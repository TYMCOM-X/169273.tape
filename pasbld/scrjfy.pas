	       (***********************************
		*                                 *
		* SCRIBE-10 JUSTIFIED LINE READER *
		*                                 *
		***********************************)

CONST
  WORDMAX = 80;					(*MAXIMUM WORD LENGTH*)
  STKMAX = 20;					(*MAXIMUM WORDS/JUSTIFIED LINE*)
  SPACE = CHR(40B);

$INCLUDE STDTYP.inc

TYPE
  UNPACKEDWORD = ARRAY[1..WORDMAX] OF CHTYPE;
  TXTTYPE = 0..WORDMAX;
  INWORD = RECORD
	     CASE TXTTYPE OF
	     1..4:(W4: ARRAY[1..4] OF CHTYPE);
	     5..8:(W8: ARRAY[1..8] OF CHTYPE);
	     9..12:(W12: ARRAY[1..12] OF CHTYPE);
	     13..20:(W20: ARRAY[1..20] OF CHTYPE);
	     21..32:(W32: ARRAY[1..32] OF CHTYPE);
	     33..48:(W48: ARRAY[1..48] OF CHTYPE);
	     49..WORDMAX:(WORDTXT: UNPACKEDWORD)
	   END;
  WORDDESC = RECORD
	       WORDPTR: ^INWORD;
	       WORDLEN: 0..WORDMAX;
	       WORDSP: INTEGER			(*NUMBER OF SPACES AFTER WORD IN LINE*)
	     END;

VAR
  CURWORD: WORDDESC;
  RDLINE: LINE;  RDPTR,RDLEN: LINEPTR;
  INSERTRIGHT: BOOLEAN;

PUBLIC PROCEDURE INITJUSTIFY;
BEGIN
  RDPTR:= 0; RDLEN:=0;				(*NULL RDLINE*)
  CURWORD.WORDLEN:= 0; INSERTRIGHT:= TRUE
END;

EXTERNAL PROCEDURE ERROR(CODE: INTEGER);

EXTERNAL PROCEDURE READLINE(VAR F:TEXT; VAR RDLINE:LINE; VAR RDLEN:LINEPTR;
  VAR CMDFLG: BOOLEAN);

PUBLIC PROCEDURE JUSTLINE(VAR F:TEXT; LENGTH: LINEPTR; VAR JSTLINE:LINE;
  VAR JSTLEN: LINEPTR; VAR CMDLINE:LINE; VAR CMDLEN:LINEPTR; CURSTATE:STATE);

VAR
  TESTLEN: LINEPTR;  EXTRASP: INTEGER; INDEX: 1..WORDMAX;
  WORDSTK:  ARRAY[1..STKMAX] OF WORDDESC;
  STKTOP,STKPTR: 0..STKMAX;
  I: LINEPTR;

  PROCEDURE NEXTWORD;				(*RETURNS NEXT WORD IN RDLINE IN CURWORD*)
  VAR CH: CHTYPE;  CMND: BOOLEAN;
    WORDBUF: UNPACKEDWORD; BUFLEN: TXTTYPE;

    PROCEDURE NEWTXT;				(*ALLOCATES WORD TEXT IN HEAP*)

      PROCEDURE PACK(LENGTH: TXTTYPE);
	VAR I: TXTTYPE;
      BEGIN
       WITH CURWORD, WORDPTR^ DO
	FOR I:= 1 TO LENGTH DO
	  WORDTXT[I]:= WORDBUF[I]
      END (*PACK*);

    BEGIN
      WITH CURWORD DO
	CASE BUFLEN OF
	 1..4:  NEW(WORDPTR,4);
	 5..8:  NEW(WORDPTR,8);
	 9..12:  NEW(WORDPTR,12);
	 13..20:  NEW(WORDPTR,20);
	 21..32:  NEW(WORDPTR,32);
	 33..48:  NEW(WORDPTR,48);
	 OTHERS:  NEW(WORDPTR,WORDMAX)
	 END;					(*CASE*)
      PACK(BUFLEN)
    END;					(*NEWTXT*)

    PROCEDURE MINSP;				(*SETS WORDSP IN CURWORD*)
    VAR CHRCTR: CHAR;
    BEGIN
      WITH CURWORD DO
      BEGIN
	CHRCTR := WORDBUF[BUFLEN].VALUE;
	CASE CHRCTR OF
	'.','?','!',':': WORDSP:= 2;
	OTHERS: WORDSP:= 1
	END
      END
    END;					(*MINSP*)

    PROCEDURE NEXTCH;				(*RETURNS NEXT CHAR FROM RDLINE IN CH*)
    LABEL 1;					(*INSTEAD OF RECURSIVE CALL*)
    BEGIN
    1:RDPTR:= RDPTR+1; CMND:= FALSE;
      IF (RDPTR-RDLEN)<=0 THEN
      BEGIN CH:= RDLINE[RDPTR];
	IF CH.VALUE < SPACE THEN GOTO 1
      END
      ELSE IF (RDPTR-RDLEN) = 1 THEN
      BEGIN
	CH.VALUE := SPACE;
	CH.ATTR := []
      END
      ELSE
      BEGIN
	READLINE(F,RDLINE,RDLEN,CMND);
	RDPTR:=0;
	IF CMND THEN
	BEGIN
	  CH.VALUE := '$';
	  CH.ATTR := []
	END
	ELSE GOTO 1				(*NOTE:  WILL NOT READ AGAIN*)
      END
    END;					(*NEXTCH*)

  BEGIN						(*NEXTWORD*)
    BUFLEN:= 0;
    NEXTCH;
    WHILE CH.VALUE = SPACE DO NEXTCH;
    IF NOT CMND THEN
    BEGIN
      WHILE CH.VALUE <> SPACE DO
      BEGIN
	IF BUFLEN<WORDMAX THEN
	BEGIN
	  BUFLEN:= BUFLEN+1; WORDBUF[BUFLEN]:= CH; NEXTCH
	END
	ELSE BEGIN
	  ERROR(7);				(*WORD TOO LONG*)
	  WHILE CH.VALUE <> SPACE DO NEXTCH
	END
      END
    END;					(*NULL CURWORD => COMMAND READ*)
    WITH CURWORD DO
    BEGIN WORDLEN:= BUFLEN;
      IF BUFLEN>0 THEN BEGIN MINSP; NEWTXT END
    END
  END;						(*NEXTWORD*)

BEGIN						(*JUSTLINE*)
  IF CURWORD.WORDLEN = 0 THEN NEXTWORD;
  IF CURWORD.WORDLEN > 0 THEN			(*STACK THE WORD AND GET ANOTHER*)
  BEGIN
    STKTOP:= 1; WORDSTK[1]:= CURWORD;
    WITH CURWORD DO TESTLEN:= WORDLEN+WORDSP;
    NEXTWORD
  END
  ELSE BEGIN STKTOP:= 0; TESTLEN:= 0 END;
  WHILE (CURWORD.WORDLEN>0) ANDIF ((TESTLEN+CURWORD.WORDLEN)<=LENGTH)
    ANDIF (STKTOP<STKMAX) DO
  BEGIN
    STKTOP:= STKTOP+1;				(*ADD WORD*)
    WORDSTK[STKTOP]:= CURWORD;
    WITH CURWORD DO TESTLEN:= TESTLEN+WORDLEN+WORDSP;
    NEXTWORD
  END;

  (*EITHER WE HAVE READ COMMAND, IN WHICH CASE WE MUST RETURN WHOLE
    STACK (WHICH MAY BE EMPTY) WITHOUT JUSTIFICATION, OR WE
    HAVE EXTRA WORD, WHICH WILL BE SAVED FOR NEXT TIME IN CURWORD*)

  IF STKTOP>0 THEN				(*CONSTRUCT JSTLINE*)
  BEGIN
    TESTLEN:= TESTLEN-WORDSTK[STKTOP].WORDSP;	(*NO TRAILING SPACES*)
    IF (STKTOP>1) AND (CURWORD.WORDLEN>0) AND (CURSTATE=JUSTIFYING) THEN    (*INSERT SPACES TO JUSTIFY*)
    BEGIN
      EXTRASP:= 2;
      IF INSERTRIGHT THEN			(*INSERT FROM RIGHT TO LEFT*)
      BEGIN
	STKPTR:= STKTOP-1;			(*BUT NOT AFTER LAST WORD*)
	WHILE TESTLEN<LENGTH DO
	BEGIN
	  WITH WORDSTK[STKPTR] DO
	  IF WORDSP<EXTRASP THEN		(*DONT OVERFILL AFTER PUNCTUATION*)
	  BEGIN
	    WORDSP:= EXTRASP; TESTLEN:= TESTLEN+1
	  END;
	  STKPTR:= STKPTR-1;
	  IF STKPTR = 0 THEN			(*DO IT AGAIN*)
	   BEGIN EXTRASP:= EXTRASP+1; STKPTR:= STKTOP-1 END
	END					(*INSERTION LOOP*)
      END
      ELSE					(*INSERT FROM LEFT TO RIGHT*)
      BEGIN
	STKPTR:= 1;
	WHILE TESTLEN<LENGTH DO
	BEGIN
	  WITH WORDSTK[STKPTR] DO
	  IF WORDSP<EXTRASP THEN
	  BEGIN
	    WORDSP:= EXTRASP; TESTLEN:= TESTLEN+1
	  END;
	  STKPTR:= STKPTR+1;
	  IF STKPTR=STKTOP THEN
	   BEGIN EXTRASP:= EXTRASP+1; STKPTR:= 1 END
	END
      END;					(*JUSTIFICATION*)
      INSERTRIGHT:= NOT INSERTRIGHT		(*INSERT OTHER WAY NEXT TIME*)
    END;
    JSTLEN:= TESTLEN;				(*SET UP JSTLINE*)
    TESTLEN:= 1;
    FOR STKPTR:= 1 TO STKTOP DO
    WITH WORDSTK[STKPTR],WORDPTR^ DO
    BEGIN
      FOR INDEX:= 1 TO WORDLEN DO
      BEGIN
	JSTLINE[TESTLEN]:= WORDTXT[INDEX];
	TESTLEN:= TESTLEN+1
      END;
      DISPOSE(WORDPTR);				(*RELEASE WORD STORAGE NOW*)
      IF STKPTR<STKTOP THEN
      FOR INDEX:= 1 TO WORDSP DO
      BEGIN
	JSTLINE[TESTLEN].VALUE := SPACE;
	JSTLINE[TESTLEN].ATTR := [];
	TESTLEN:= TESTLEN+1
      END
    END
  END						(*JSTLINE CONSTRUCTION*)
  ELSE JSTLEN:= 0;				(*EMPTY JSTLINE*)
  IF CURWORD.WORDLEN = 0 THEN
  BEGIN
    CMDLINE := RDLINE;
    CMDLEN := RDLEN;
    RDLEN := 0;
    RDPTR := 0
  END
  ELSE
    CMDLEN := 0;
END.
    