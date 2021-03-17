

                                                  (* Osman Guven
                                                     PROJECT VOCABULARY
                                                     CSE 110, MW, 19:00
                                                     Date due: 12/2/85 *)



PROGRAM Vocabulary;

CONST STRLENGTH=      20;   (*Number of character in each word*)
      FILENAMELENGTH= 10;   (*Number of character in file name*)
      ARRAYDIM=       1000; (*Size of array*)

TYPE  Name=          STRING[ STRLENGTH ];
      Vect=          ARRAY[ 1..ARRAYDIM ] OF Name;
      FileName=      PACKED ARRAY[ 1..FILENAMELENGTH ] OF CHAR;
      AssembledWord= PACKED ARRAY[ 1..STRLENGTH ] OF CHAR;

VAR   LowerBound:       INTEGER;
      ArrayCount:       INTEGER;
      UpperBound:       INTEGER;
      TheWord:          AssembledWord;
      UnsortedFileName: FileName;
      UnsortedFileName1:FileName;
      SortedFileName:   FileName;
      ListA:            VECT;
      AllDone:          BOOLEAN;
      ArrayFull:        BOOLEAN;
      InFile:           TEXT;
      OutFile:          TEXT;


(* *******************************************************************
   *                                                                 *
   *   GetFileName   Gets file name from TTY.                        *
   *                                                                 *
   ******************************************************************* *)


PROCEDURE GetFileName (VAR NameOfFile: FileName);

VAR
I, J: INTEGER;

BEGIN
  I:= 1;
  READLN;
  WHILE NOT EOLN DO
    BEGIN
      READ (NameOfFile[I]);
      I:= I + 1;
    END;
  FOR J:= I TO FILENAMELENGTH DO
  NameOfFile[I]:= ' ';
END;


(* *******************************************************************
   *                                                                 *
   *   LowerCaseCh This function converts character in "Ch" to       *
   *               lowercase.                                        *
   *                                                                 *
   ******************************************************************* *)


FUNCTION LowerCaseCh (Ch: CHAR): CHAR;

BEGIN
  LowerCaseCh:= CHR((ORD(Ch)-ORD('A')+ORD('a')))
END;


(* *******************************************************************
   *                                                                 *
   *   GetAWord   Packs characters into a PACKED ARRAY until it      *
   *              gets a word. While packing characters it converts  *
   *              the characters into lowercase.                     *
   *                                                                 *
   ******************************************************************* *)


PROCEDURE GetAWord (VAR PackedWord: AssembledWord; VAR AllDone: BOOLEAN);

VAR
    Ch:         CHAR;
    I, J:       INTEGER;
    GotAWord:   BOOLEAN;

BEGIN
  GotAWord:= FALSE;
  I:= 1;
  REPEAT
    IF EOF(inFile) THEN
      AllDone:= TRUE
    ELSE
      BEGIN
        READ (InFIle, Ch);
        IF Ch IN['A'..'z'] THEN
          BEGIN
            IF Ch <= 'Z' THEN
              PackedWord[I]:= LowerCaseCh(Ch)
            ELSE
              PackedWord[I]:= Ch;
            I:= I + 1;
            IF NOT (InFile ^ IN['A'..'z']) THEN
              BEGIN
                FOR J:= I TO STRLENGTH DO
                    PackedWord[J]:= ' ';
                GotAWord:= TRUE;
              END;
          END;
      END;
    UNTIL AllDone OR GotAWord
END;


(* *******************************************************************
   *                                                                 *
   *   Sort   It requires three parameters, a variable parameter     *
   *          array, lower array limit and upper array limit. It     *
   *          returns variable array sorted.                         *
   *                                                                 *
   *          R= Variable vector, Lo= Low limit of the vector and    *
   *          UpIn= Upper limit of the vector.                       *
   *                                                                 *
   ******************************************************************* *)


PROCEDURE Sort(VAR R: VECT; Lo: INTEGER; UpIn: INTEGER);

VAR
    I,J,Up: INTEGER;
    TempR:  NAME;

BEGIN
  Up:= UpIn;
  WHILE Up > Lo DO
    BEGIN
      J:= Lo;
      FOR I:= Lo TO Up - 1 DO
        BEGIN
          IF R[I] > R[I + 1] THEN
            BEGIN
              TempR:= R[I];
              R[I]:= R[I + 1];
              R[I + 1]:= TempR;
              J:= I;
            END;
        END;
      Up:= J;
    END;
END;


(* *******************************************************************
   *                                                                 *
   *   GetTextFile   Assembles words into array from input text file *
   *                 until EOF or array gets full.                   *
   *                                                                 *
   ******************************************************************* *)


PROCEDURE GetTextFile (VAR TopOfArray: INTEGER);

BEGIN
UpperBound:= 0;
AllDone:= FALSE;
ArrayFull:= FALSE;
WHILE NOT AllDone AND NOT ArrayFull DO
  BEGIN
    UpperBound:= UpperBound + 1;
    GetAWord (TheWord, AllDone);
    ListA[ UpperBound ]:= TheWord;
    IF UpperBound= ARRAYDIM THEN
      BEGIN
        WRITELN;
        WRITELN ('ARRAY FULL, will only sort ', UpperBound:4, ' items.');
        TopOfArray:= UpperBound;
        BREAK;
        ArrayFull:= TRUE;
      END
    ELSE TopOfArray:= UpperBound;
  END;
END;


(* *******************************************************************
   *                                                                 *
   *   SortIt   Sorts the array and writes the sorted array into     *
   *            output vocabulary file.                              *
   *                                                                 *
   ******************************************************************* *)


PROCEDURE SortIt (ArrayCount: INTEGER);

VAR
     I: INTEGER;
BEGIN
  I:= 1;
  UpperBound:= ArrayCount;
  WRITE ('Starting Sort...');BREAK;
  Sort (ListA,1,UpperBound);
  WRITELN (UpperBound:5,' items... sorted.');BREAK;
  REWRITE (OutFile,SortedFileName);
  FOR I:= 1 TO UpperBound DO
    IF ListA[I] <> ListA[I+1] THEN
      WRITELN (OutFile,ListA[I]);
END;


(*PROCEDURE GetNextLine (VAR TheFile: TEXT; VAR TheLine: AssembledWord);

CONST BLANKLINE= '                    ';
VAR   Count: INTEGER;

BEGIN
TheLine:= BLANKLINE;
Count:= 1;
WHILE NOT EOF (TheFile) DO
  BEGIN
    WHILE (Count <= STRLENGTH) AND NOT EOLN (TheFile) DO
      BEGIN
        READ (TheFile,TheLine[Count]);
writeln('in getnextline ',count);
        Count:= Count + 1;
      END;(*end while count<= strlength*)
  END;(*end while not eof*)
READLN (TheFile);(*flash out the crlf from the end of the line*)
END;(*end getnextline*)

PROCEDURE MergeFiles;

BEGIN
writeln('in mergefiles');
RESET (InFile,'Vocab1');
RESET (InFile,'Vocab2');
REWRITE (OutFile,'Listed');
    GetNextLine ('Vocab1',Vocab1Line);
writeln(vocab1line);
    GetNextLine (Vocab2,Vocab2Line);
    CASE Vocab1Line < Vocab2Line OF
      TRUE:  WRITELN (Listed,Vocab1Line);
      FALSE: WRITELN (Listed,Vocab2Line);
    END;(*end case*)
WHILE NOT EOF (Vocab1) DO
  BEGIN
    GetNextLine (Vocab1, Vocab1Line);
    WRITELN (Listed,Vocab1Line);
  END;(*end while not eof*)
WHILE NOT EOF (Vocab2) DO
  BEGIN
    GetNextLine (Vocab2,Vocab2Line);
    WRITELN (Listed,Vocab2Line);
  END;(*end while not eof*)
END;(*end mergefiles*)*)



BEGIN
      (* OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe. *)

OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "Vocabulary"');
WRITELN;
WRITE ('Input Text File 1 = ');BREAK;
GetFileName (UnsortedFileName);
RESET (InFile, UnsortedFileName);
(*WRITE ('Input Text File 2 = ');BREAK;
GetFileName (UnsortedFileName1);*)
WRITE ('Output Sort File = ');BREAK;
GetFileName (SortedFileName);
GetTextFile (ArrayCount);
SortIt (ArrayCount);
WRITELN;
WRITELN ('End of program "Vocabulary"');
WRITELN;
WRITELN ('************************************************************');
END.
    