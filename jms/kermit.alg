 $SET NOBINDINFO
 $VERSION 1.019              % [KS] 5-86
 $SET ASCII                 %  BURROUGHS USES 8 BITS FOR ASCII
BEGIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  K E R M I T  File Transfer Utility
%
%  Burroughs 7800, University of California at Davis, 1986
%    Larry Johnson, Dave Squire, Katie Stevens
%
%
%%%%% REVISIONS
%       1.019 [KS] 4-86
%                  FIXED RE-TRY BUG IN SENDSW/SBREAK
%                  OVERHAULED HELP PROCEDURES
%                  ENABLED 8-TH BIT QUOTING
%                  ENABLED VARIABLE START-OF-PACKET CHAR
%                  FIXED SERVER-SPAR RETRY BUG IN RECSW/RFILE
%       1.018 [KS] 3-86
%                  FIXED PACKET # BUG CAUSED BY DUP VARIABLE NAMES
%       1.017 [KS] 1-86
%                  ENABLED REPEAT PROCESSING
%                  ELIMINATE BLANK RECORD ADDED TO END OF RCV FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
 % SYMBOL DEFINITIONS
 
DEFINE MAXPACKSIZ     =  94#,   % MAXIMUM PACKET SIZE
       MAXPACKWDS     =  15#,   % (MAXPACKSIZ+5)DIV 6-1
       MAXSENDFILESIZ =  11#,   % LARGEST FILE NAME I SHOULD SEND
       MAXREPT        =  94#,   % LARGEST REPEAT COUNT (126-32)
       EOF            = 4"201"#,% EOF FOR BUFILL
       NULC           = 48"00"#,% ASCII NULL CHARACTER
       DEFSOH         =   1#,   % [1.019] START OF HEADER
%      SOHC           = 48"01"#,% SOH CHARACTER
       ETXC           = 48"03"#,% ETX CHARACTER
       BEL            =   7#,   % ASCII BELL
       HT             =   9#,   % ASCII HORIZONTAL TAB
       LF             =  10#,   % ASCII LINE FEED
       NL             =  LF#,   % NEWLINE CHARACTER
       CR             =  13#,   % ASCII CARRIAGE RETURN
       SP             =  32#,   % ASCII SPACE
       DEL            = 127#,   % ASCII DELETE (RUBOUT)
 
       REPTTHRESH     =   5#,   % CHARACTER REPEAT THRESHOLD
       DEFINITRETRY   =  20#,   % TIMES TO RETRY INITIALIZATION
       DEFPACKETRETRY =  10#,   % TIMES TO RETRY A PACKET
       TABLEN         =   8#,   % LENGTH OF A TAB IF EXPANDED
       DEFRECSIZE     =  15#,   % MAXRECSIZE IN WORDS OF RECEIVED FILE
       DEFBLOCKSIZE   = 420#,   % BLOCKSIZE IN WORDS OF RECEIVED FILE
       DEFUNITS       = VALUE(WORDS)#, % UNITS OF RECEIVED FILE
       DEFPAD         =   0#,   % DEFAULT # OF PADDING CHARACTERS
       DEFPCHAR       =   0#,   % DEFAULT PADDING CHARACTER
       DEFEOL         =  CR#,   % DEFAULT END OF LINE CHAR FOR BURROUGHS
       DEFQUOTE       = "#"#,   % DEFAULT QUOTE CHARACTER
       DEFQBIN        = "&"#,   % DEFAULT BINARY QUOTE CHARACTER
       DEFCHKTYPE     = "1"#,   % DEFAULT CHECKSUM TYPE
       DEFREPT        = "~"#,   % DEFAULT REPEAT CHARACTER
       DEFPAUSE       =   0#,   % DEFAULT PAUSE BEFORE ACK
       DEFDELAY       =   5#,   % DEFAULT DELAY FOR FIRST SEND
       DEFESCCHR      = "^"#,   % DEFAULT ESCAPE CHARACTER FOR CONNECT
       DEFTIME        =   5#,   % DEFAULT TIMEOUT INTERVAL
       MAXTIM         =  60#,   % MAXIMUM TIMEOUT INTERVAL
       MINTIM         =   2#;   % MINUMUM TIMEOUT INTERVAL
 
 
 % MACRO DEFINITIONS
 
 %
 % TOCHAR: CONVERTS A CONTROL CHARACTER TO A PRINTABLE ONE BY ADDING A S
 %
 % UNCHAR: UNDOES TOCHAR.
 %
 % CTL:    CONVERTS BETWEEN CONTROL CHARACTERS AND PRINTABLE CHARACTERS
 %    TOGGLING THE CONTROL BIT (IE. ^A BECOMES A AND A BECOMES ^A).
 
DEFINE TOCHAR(CH)     = ((CH) + 32)  #;
DEFINE UNCHAR(CH)     = ((CH) - 32)  #;
DEFINE CTL(CH)        = ((CH) & (1-(CH).[6:1])[6:1]) #;
 
 
 % GLOBAL VARIABLES
 
REAL
        BSIZE,          % SIZE OF PRESENT DATA
        RPSIZ,          % MAXIMUM RECEIVE PACKET SIZE
        SPSIZ,          % MAXIMUM SEND PACKET SIZE
        TIMINT,         % TIMEOUT FOR FOREIGN HOST ON SENDS
        PAD,            % HOW MUCH PADDING TO SEND
        PCHAR,          % PADDING CHARACTER TO SEND
        EOL,            % END-OF-LINE CHARACTER TO SEND
        SOHCHAR,        % [1.019] START-OF-PACKET CHAR TO SEND
        QUOTE,          % QUOTE CHARACTER IN INCOMING DATA
        QBIN,           % BINARY QUOTE CHARACTER IN INCOMING DATA
        CHKTYPE,        % ERROR DETECTION TYPE IN INCOMING DATA
        REPT,           % REPEAT CHARACTER IN INCOMING DATA
        N,              % PACKET NUMBER
        NUMTRY,         % TIMES THIS PACKET RETRIED
        OLDTRY;         % TIMES PREVIOUS PACKET RETRIED
 
BOOLEAN
        SERVER,         % MEANS WE'RE A KERMIT SERVER
        BINARYON,       % [1.019] MEANS 8-BIT QUOTING MODE ENABLED
        HIBITOK,        % MEANS 8-BIT MODE IN ACTION
        CALL1,          % [1.017] KEEPS TRACK OF RPAR/SPAR SEQUENCE
        REPTOK,         % [1.017] TRUE MEANS REPEAT ENCRIPTION OK
        DEBUG,          % INDICATES LEVEL OF DEBUGGING OUTPUT (0=NONE)
        EXPTABS,        % EXPAND TABS ON INPUT
        FIXEDRECS,      % SEND FIXEDRECS LENGTH RECORDS
        RAW,            % DONT USE NL AS RECORD SEPARATOR
        KEEPFILE;       % KEEP THE OUTPUT FILE
 
REAL
        INITRETRY,      % NUMBER OF RETRIES ON INITIALIZATION
        PACKETRETRY,    % NUMBER OF RETRIES FOR A DATA PACKET
        FILERECSIZE,    % MAXRECSIZE OF RECEIVED FILE
        FILEBLOCKSIZE,  % BLOCKSIZE OF RECEIVED FILE
        FILEUNITS,      % UNITS OF RECEIVED FILE
        FILECOUNT,      % NUMBER OF FILES LEFT TO SEND
        STATE,          % PRESENT STATE OF THE AUTOMATON
        MYPACKSIZ,      % MY MAXIMUM PACKET SIZE
        MYTIME,         % MY TIMEOUT INTERVAL
        MYPAD,          % MY NUMBER OF PADDING CHARACTERS
        MYPCHAR,        % MY PADDING CHARACTER
        MYEOL,          % MY END OF LINE CHARACTER
        MYSOH,          % [1.019] MY START-OF-PACKET CHAR
        MYQUOTE,        % MY QUOTE CHARACTER
        MYQBIN,         % MY BINARY QUOTE CHARACTER
        MYCHKTYPE,      % MY CHECKSUM TYPE
        MYREPT,         % MY REPEAT CHARACTER
        MYPAUSE,        % MY PAUSE AFTER ACK TIME
        MYDELAY,        % MY DELAY FOR FIRST SEND TIME
        MYESCCHR;       % MY ESCAPE CHARACTER FROM CONNECT
 
 
ARRAY
        FILNAM[0:MAXPACKWDS];   % TITLE OF CURRENT DISK FILE
 
POINTER
        PFILNAM;        % POINTER TO FILNAM
 
ARRAY
        RECPKT[0:MAXPACKWDS],% RECEIVE PACKET BUFFER
        PACKET[0:MAXPACKWDS];% PACKET BUFFER
 
FILE
        REM             %  FILE FOR REMOTE INPUT / OUTPUT
          (KIND=REMOTE,MYUSE=IO,UNITS=CHARACTERS,BUFFERS=1,
                 MAXRECSIZE=300,FILETYPE=3),
        LOG             %  FILE POINTER FOR LOGFILE
           (KIND=DISK,UNITS=CHARACTERS,MAXRECSIZE=96,BLOCKSIZE=2880,
                    PROTECTION=SAVE,NEWFILE,SAVEFACTOR=1,BUFFERS=1,
                    TITLE=8"KERMIT/LOG.");
 
TRANSLATETABLE TOUPPER(  ASCII TO ASCII,
       "abcdefghijklmnopqrstuvwxyz" TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
               TOLOWER(  ASCII TO ASCII,
       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" TO "abcdefghijklmnopqrstuvwxyz"),
               TOBURROUGHS( ASCII TO ".",
       "abcdefghijklmnopqrstuvwxyz" TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
       "0123456789"                 TO "0123456789" );
TRANSLATETABLE ASCTOEBC(
   47"000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F"
TO 48"00010203372D2E2F1605250B0C0D0E0F101112133C3D322618193F271C1D1E1F"
  ,47"202122232425262728292A2B2C2D2E2F303132333435363738393A3B3C3D3E3F"
TO 48"404F7F7B5B6C507D4D5D5C4E6B604B61F0F1F2F3F4F5F6F7F8F97A5E4C7E6E6F"
  ,47"404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F"
TO 48"7CC1C2C3C4C5C6C7C8C9D1D2D3D4D5D6D7D8D9E2E3E4E5E6E7E8E94AE05A5F6D"
  ,47"606162636465666768696A6B6C6D6E6F707172737475767778797A7B7C7D7E7F"
TO 48"79818283848586878889919293949596979899A2A3A4A5A6A7A8A9C06AD0A107"
            );
TRANSLATETABLE FIXSLASHES( ASCII TO ASCII , "/" TO "_" );
TRUTHSET NUMBERS(   "0" OR "1" OR "2" OR "3" OR "4" OR
                    "5" OR "6" OR "7" OR "8" OR "9");
TRUTHSET QUOTECHARS( "!" OR 48"7F" OR "#" OR "$" OR "%" OR "&"
   OR "'" OR "(" OR ")" OR "*" OR "+" OR "," OR "-" OR "."
   OR "/" OR "0" OR NUMBERS OR ":" OR ";" OR "<" OR "=" OR ">"
   OR "`" OR "{" OR "|" OR "}" OR "~" );
ARRAY ACNTRL[0:15],              %  TRUTHSET FOR ALL CONTROL CHARS
      BCNTRL[0:15];              %  TRUTHSET FOR JUST QUOTE,QBIN,REPT
 %     4"0000FFFFFFFF",          %  ADD IN FROM 0 THRU 31
 %     0,0,                      %  LEAVE OUT  32 THRU 95
 %     4"000000000001",          %  ADD IN BIT FOR 127
 %     0,0,0,0                   %  ZERO OUT END (MAY NEED FOR EBCDIC)
 %  TABLE ALGORITHM:
 %    BOOLEAN(TABLE[CHAR.[7:3]].[(31-CHAR.[4:5]):1]) => IN TABLE
 %
DEFINE TABLEIT(TAB,C)  = TAB[C.[7:3]].[(31-C.[4:5]):1] := 1#,
       UNTABLE(TAB,C)  = TAB[C.[7:3]].[(31-C.[4:5]):1] := 0#;
 
ARRAY FBUF_[0:29],      %  USED BY FPRINT
      EBUF_[0:15],      %  USED BY ERROR
      TBUF_[0:15],      %  TEMPORARY BUFFER FOR DIGITS CONVERSION
      GBUF_[0:99],     %  USED BY GETC
      PBUF_[0:99];     %  USED BY PUTC
POINTER PG_,            %  POINTS TO GBUF_
        PP_;            %  POINTS TO PBUF_
REAL RD,                %  RESULT DESCRIPTOR FOR EVERYBODY
     GCNT_,             %  NUMBER OF CHARACTERS IN GBUF_
     PCNT_,             %  NUMBER OF CHARACTERS IN PBUF_
     RECSIZ_,           %  MAXRECSIZE OF FP
     UNITS_,            %  CHARACTERS PER "UNIT" OF FP
     HUH_;              %  SILLY LITTLE FILLER
BOOLEAN BRD = RD;       %  BOOLEAN RD
DEFINE                  %  SOME BURROUGHS FIELD DEFINES
    LENGTHF = [47:20]#, %  CHAR. COUNT RETURNED FROM RESLT. DESCR.
    EOFBIT  = [ 9: 1]#, %  EOF ON I/O FROM RESLT. DESCR.
    BRKBIT  = [13: 1]#, %  BREAK ON I/O FROM RESLT. DESCR.
 TIMEOUTBIT = [15: 1]#, %  TIMEOUT ON I/O FROM RESLT. DESCR.
    ERRORF  = [16:17]#, %  THE WHOLE ERROR FIELD
    MOD64   = .[5:6]#;  %  N MOD 64 == N.[5:6]
 
DEFINE
    INDENT   = TRUE#,   % BOOLEAN CONSTANTS
    NOINDENT = FALSE#;
 
ARRAY NULLDATA[0:0];
 
DEFINE CH(NUMBER,N)  = (NUMBER).[ 7:48] FOR N#,% TO USE NUMBER AS A CHAR
      SAY(STR)       =
        BEGIN
         REPLACE POINTER(FBUF_) BY STR;
         IF SERVER THEN
           ERROR(FBUF_)
         ELSE
           BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
        END#,
      SAY1(STR,NUMBER)=
       BEGIN
         REPLACE TBUF_ BY 8"-" FOR REAL(NUMBER LSS 0),
             NUMBER FOR * DIGITS,8" " FOR 12;
         REPLACE POINTER(FBUF_) BY STR,TBUF_ FOR 12
             WITH EBCDICTOASCII;
         IF SERVER THEN
           ERROR(FBUF_)
         ELSE
           BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
       END#,
      SAYC(STR,NUMBER)=
       BEGIN
          IF TBUF_[0] := NUMBER LSS SP THEN
            REPLACE POINTER(FBUF_) BY STR,"CTRL-",CH(NUMBER+64,1),
             " (HEX ",POINTER(TBUF_,4)+10 FOR 2 WITH HEXTOASCII,")"
          ELSE
           REPLACE POINTER(FBUF_) BY STR, CH(NUMBER,1), " (HEX ",
              POINTER(TBUF_,4)+10 FOR 2 WITH HEXTOASCII,")";
         IF SERVER THEN
           ERROR(FBUF_)
         ELSE
           BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
       END#,
      SAYN(STR,PTR)   =
       BEGIN
         REPLACE POINTER(FBUF_) BY STR,
              PTR FOR MAXPACKSIZ UNTIL = NULC;
         IF SERVER THEN
           ERROR(FBUF_)
         ELSE
           BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
       END#,
      SAYP(PTR,WHITESPACE)   =
       BEGIN
         IF WHITESPACE THEN
            REPLACE POINTER(FBUF_) BY "   ",
                                      PTR FOR MAXPACKSIZ-3 WHILE GEQ " "
         ELSE
            REPLACE POINTER(FBUF_) BY PTR FOR MAXPACKSIZ WHILE GEQ " ";
         IF SERVER THEN
           ERROR(FBUF_)
         ELSE
           BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
       END#,
 
      SAYQ(STR)      =
       BEGIN
         REPLACE POINTER(FBUF_) BY " " FOR COL_BASE+12-COL_OK_TIL,
                                   "?";
         BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
         REPLACE POINTER(FBUF_) BY "  missing or invalid ",
                                   STR,
                                   " parameter";
         BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
       END#,
 
      SAYQOPT(STR)      =
       BEGIN
         COL_OK_TIL := COL_BASE + 12 - COL_OK_TIL;
         REPLACE POINTER(FBUF_) BY " " FOR COL_OK_TIL,
                                   "?";
         BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
         REPLACE POINTER(FBUF_) BY "  missing or invalid ",
                                   STR,
                                   " parameter - options are:";
         BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
       END#,
 
 
      BUG(STR)       =
        BEGIN
          REPLACE POINTER(FBUF_) BY STR;
          BRD := WRITE(LOG,96,FBUF_[*]);
          REPLACE FBUF_ BY " " FOR 16 WORDS;
        END#,
      BUG1(STR,NUMBER)=
       BEGIN
         REPLACE TBUF_ BY 8"-" FOR REAL(NUMBER LSS 0),
             NUMBER FOR * DIGITS,8" " FOR 12;
         REPLACE POINTER(FBUF_) BY STR,TBUF_ FOR 12
             WITH EBCDICTOASCII;
         BRD := WRITE(LOG,96,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
       END#,
      BUGH(STR,NUMBER)=
       BEGIN
         TBUF_[0] := NUMBER;
         REPLACE POINTER(FBUF_) BY STR,
              POINTER(TBUF_,4) FOR 12 WITH HEXTOASCII;
         BRD := WRITE(LOG,96,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
       END#,
      BUGC(STR,NUMBER)=
       BEGIN
          IF TBUF_[0] := NUMBER LSS SP THEN
            REPLACE POINTER(FBUF_) BY STR,"CTRL-",CH(CTL(NUMBER),1)
          ELSE
           REPLACE POINTER(FBUF_) BY STR, CH(NUMBER,1);
         BRD := WRITE(LOG,96,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
       END#,
      BUGN(STR,PTR)   =
       BEGIN
         REPLACE POINTER(FBUF_) BY STR,
              PTR FOR MAXPACKSIZ UNTIL = NULC;
         BRD := WRITE(LOG,96,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
       END#,
      BUGP(PTR)   =
       BEGIN
         REPLACE POINTER(FBUF_) BY PTR FOR MAXPACKSIZ UNTIL = NULC;
         BRD := WRITE(LOG,96,FBUF_[*]);
         REPLACE FBUF_ BY " " FOR 16 WORDS;
       END#;
 
DEFINE ERROR(ARA) =
      BEGIN
        REPLACE EBUF_ BY ARA FOR HUH_:MAXPACKSIZ-5 WHILE GEQ " ";
        HUH_ := MAXPACKSIZ - HUH_ - 5;
        REPLACE FBUF_ BY " " FOR 16 WORDS; % SO DEBUGGING IN SPACK IS OK
        SPACK("E",N:=(N+1) MOD64,HUH_,EBUF_);
        REPLACE EBUF_ BY " " FOR 16 WORDS;
      END#;
REAL PROCEDURE GETC(FID);
 FILE FID;
 BEGIN
   POINTER P,Q;
   REAL R;
 
   IF GCNT_ LEQ 0 THEN
     IF BRD := READ(FID,RECSIZ_,GBUF_) THEN
       GETC := EOF
     ELSE
      BEGIN
        PG_ := POINTER(GBUF_);     %  INITIALIZE POINTER
        GCNT_ := RECSIZ_ * UNITS_; %  AND COUNT
        IF FIXEDRECS THEN
         IF NOT RAW THEN            % [1.019]
          REPLACE PG_+GCNT_ BY CH(NL,1)%  ADD IN A NL AT END
         ELSE
          GCNT_ := *-1              % [1.019] NO RECORD TERMINATOR
        ELSE
         BEGIN
           Q := PG_;
           DO BEGIN
             SCAN P:Q FOR R:GCNT_ UNTIL LEQ " ";
             SCAN Q:P FOR GCNT_:R WHILE LEQ " ";
           END UNTIL GCNT_ LEQ 0;
           REPLACE P BY CH(NL,1);
           GCNT_ := RECSIZ_ * UNITS_ - R;
         END;
        GETC := REAL(PG_,1);       %  GET FIRST CHAR
        PG_ := *+1;                %  BUMP PTR BUT COUNT IS STILL OK
      END                          %  SINCE WE ADDED THE NL EARLIER
   ELSE
    BEGIN
      GCNT_ := *-1;
      GETC := REAL(PG_,1);
      PG_ := *+1;
    END;
 END GETC;
 
REAL PROCEDURE PUTC(C,FID);
 VALUE C;
 REAL C;
 FILE FID;
 BEGIN
 
   IF C = NL AND NOT RAW THEN
     PCNT_ := -1
   ELSE
     REPLACE PP_:PP_ BY CH(C,1);
   IF PCNT_ := *-1 LEQ 0 THEN
     BEGIN
       BRD := WRITE(FID,RECSIZ_,PBUF_);
       REPLACE PP_:=POINTER(PBUF_) BY " " FOR PCNT_:=RECSIZ_*UNITS_+1;
       IF C NEQ NL OR RAW THEN      %  WE JUST LOST A CHARACTER
         BEGIN                      %  LET'S GET IT BACK
           REPLACE PP_:PP_ BY CH(C,1);
           PCNT_ := *-1;            %  AND DECREMENT COUNTER
         END;
     END;
   PUTC := C;
 END PUTC;
 
 
 %
 %  M A I N
 %
 %  MAIN ROUTINE - PARSE COMMAND AND OPTIONS, SET UP THE
 %  TTY LINES, AND DISPATCH TO THE APPROPRIATE ROUTINE.
 
 
PROCEDURE MAIN;
 BEGIN
  POINTER CP;                      %  CHAR POINTER
  REAL COL;                        %  COLUMN COUNTER FOR SCANNER
  REAL COL_BASE;                   % BEGINNING COLUMN COUNT FOR SCANNER
  REAL COL_OK_TIL;                 %  COLUMN COUNTER FOR PREVIOUS SCAN
  ARRAY NEXTSEND[0:MAXPACKWDS];    %  NEXT FILE(S) TO SEND
  BOOLEAN MORETOSEND;              %  SOMETHING IN NEXTSEND
  EBCDIC ARRAY MACHINE[0:3];       %  WHAT MACHINE (7800, 6700, ETC)
  ARRAY KPROMPT[0:15];             %  THE KERMIT PROMPT
  ARRAY REFERENCE SPECARA[0];      %  CURRENT COMMAND ARRAY
 
 
  VALUE ARRAY SPECIAL(             %  ALL THE COMMANDS
    48"01" "?    "  ,              %  ? FOR HELP
    48"04" "EXIT " ,
    48"04" "HELP " ,
    48"04" "QUIT " ,
    48"07" "RECEIVE    " ,
    48"04" "SEND " ,
    48"06" "SERVER     " ,
    48"03" "SET  " ,
    48"04" "SHOW " ,
    48"06" "STATUS     " ,
              0);
 
%    DEFINES FOR COMMANDS
 
  DEFINE
    QMARKV              =   0#,
    EXITV               =   1#,
    HELPV               =   2#,
    QUITV               =   3#,
    RECEIVEV            =   4#,
    SENDV               =   6#,
    SERVERV             =   7#,
    SETV                =   9#,
    SHOWV               =  10#,
    STATUSV             =  11#,
    QUESTIONV           =  -98#,
    INVALIDV            =  -99#,
    NOERRORV            =  101#;
 
  VALUE ARRECSET(             %  ALL THE SPECIAL 'SET' COMMANDS
    48"01" "?    "  ,
    48"09" "DEBUGGING  " ,
    48"05" "DELAY" ,
    48"04" "FILE " ,
    48"0A" "INCOMPLETE " ,
    48"05" "RETRY" ,
    48"07" "RECEIVE    " ,
    48"04" "SEND " ,
    48"06" "BINARY     " ,
              0);
 
%    DEFINES FOR SET COMMAND
 
  DEFINE
%   QMARKV              =   0#,
    DEBUGV              =   1#,
    DELAYV              =   3#,
    FILEV               =   4#,
    INCOMPLETEV         =   5#,
    RETRYV              =   7#,
    SETRECEIVEV         =   8#,
    SETSENDV            =  10#,
    BINARYV             =  11#;
 
  VALUE ARRAY SPECFILE(            %  FOR SET FILE COMMANDS
    48"01" "?    " ,
    48"0A" "BLOCK-SIZE " ,
    48"0B" "EXPAND-TABS" ,
    48"05" "FIXED" ,
    48"03" "RAW  " ,
    48"0B" "RECORD-SIZE" ,
    48"05" "UNITS" ,
              0);
 
%         DEFINES FOR SET FILE COMMANDS
  DEFINE
%   QMARK                   =  0#,
    BLOCKSIZEV              =  1#,
    EXPTABSV                =  3#,
    FIXEDV                  =  5#,
    RAWV                    =  6#,
    RECORDSIZEV             =  7#,
    UNITSV                  =  9#;
 
  VALUE ARRAY SPECABORT(           %  FOR SET ABORTED-FILE
    48"01" "?    " ,
    48"07" "DISCARD    " ,
    48"04" "KEEP " ,
              0);
 
%    DEFINES FOR SPECABORT
 
  DEFINE
%   QMARKV              =   0#,
    DISCARDV            =   1#,
    KEEPV               =   3#;
 
  VALUE ARRAY SPECDEBUG(           %  FOR SET DEBUGGING
    48"01" "?    " ,
    48"06" "STATES     " ,
    48"07" "PACKETS    " ,
    48"08" "LOG-FILE   " ,
    48"03" "OFF  ",
              0);
 
%    DEFINES FOR SPECDEBUG
 
  DEFINE
%   QMARKV              =   0#,
    STATESV             =   1#,
    PACKETSV            =   3#,
    LOGFILEV            =   5#,
    DOFFV               =   7#;
 
  VALUE ARRAY SPECRETRY(           %  FOR SET RETRY
    48"01" "?    " ,
    48"12" "INITIAL-CONNECTION     " ,
    48"07" "PACKETS    " ,
              0);
 
%    DEFINES FOR SPECRETRY
 
  DEFINE
%   QMARKV              =   0#,
    INITCONNV           =   1#,
    RETRYPACKETSV       =   5#;
 
  VALUE ARRAY SPECONOFF(           %  FOR ON/OFF
    48"01" "?    " ,
    48"02" "ON   " ,
    48"03" "OFF  " ,
              0);
 
%    DEFINES FOR SPECONOFF
 
  DEFINE
%   QMARKV              =   0#,
    ONV                 =   1#,
    OFFV                =   2#;
 
  VALUE ARRAY SPECRECEIVE(         %  FOR SET RECEIVE, SET SEND
    48"01" "?    " ,
    48"0B" "END-OF-LINE" ,
    48"0D" "PACKET-LENGTH    " ,
    48"07" "PADDING    " ,
    48"07" "PADCHAR    " ,
    48"05" "PAUSE" ,
    48"05" "QUOTE" ,
    48"0F" "START-OF-PACKET  " ,
    48"07" "TIMEOUT    " ,
              0);
 
%    DEFINES FOR SPECRECEIVE
 
  DEFINE
%   QMARKV              =   0#,
    EOLV                =   1#,
    LENV                =   3#,
    PADV                =   6#,
    PCHARV              =   8#,
    PAUSEV              =  10#,
    QUOTEV              =  11#,
    STARTOFPACKV        =  12#,
    TIMEOUTV            =  15#;
 
  VALUE ARRAY SPECUNITS(           %  FOR SET RECEIVE UNITS
    48"01" "?    ",
    48"05" "WORDS",
    48"0A" "CHARACTERS ",
              0);
 
%    DEFINES FOR SPECUNITS
 
  DEFINE
%   QMARKV              =   0#,
    UWORDSV             =   1#,
    UCHARACTERSV        =   2#;
 
  VALUE ARRAY SPECSHOW(            % FOR SHOW SEND/RECEIVE
    48"04" "SEND " ,
            0);
 
  DEFINE
    SHOSENDV            =   0#;
 
 
 
  VALUE ARRAY PLAINHELP(           %  GLOBAL HELP STUFF
    48"0D" "EXIT to CANDE    ",
    48"1B" "HELP by giving this message  ",
    48"10" "QUIT (like EXIT) ",
    48"16" "RECEIVE file from host ",
    48"11" "SEND file to host",
    48"20" "SERVER   make me a Kermit Server   ",
    48"0F" "SET a parameter  ",
    48"0D" "SHOW settings    ",
    48"12" "STATUS (like SHOW)     ",
              0),
              SETHELP(             %  SET HELP STUFF
    48"20" "   BINARY (do 8th bit transfers)   " ,
    48"19" "   DEBUGGING level option    ",
    48"1F" "   DELAY seconds for first SEND    ",
    48"11" "   FILE parameter" ,
    48"19" "   INCOMPLETE disposition    " ,
    48"0E" "   RETRY count   ",
    48"14" "   RECEIVE parameter   ",
    48"11" "   SEND parameter",
              0),
              SETFILEHELP(         % SET FILE  HELP STUFF
    48"14" "   BLOCK-SIZE length   ",
    48"17" "   EXPAND-TABS on input",
    48"32" "   FIXED (send blanks found at the end of records)" ,
    48"2A" "   RAW (without any line delimiting chars)     ",
    48"15" "   RECORD-SIZE length  ",
    48"1E" "   UNITS (words or characters)    ",
              0),
              SENDHELP(            %  SET RECEIVE/SEND HELP
    48"1C" "   END-OF-LINE (number 0-31) ",
    48"17" "   PACKET-LENGTH length",
    48"20" "   PADDING  (number of PADCHARS)   ",
    48"19" "   PADCHAR  (number 0-31)    ",
    48"1B" "   PAUSE seconds before ACK  ",
    48"12" "   QUOTE character     ",
    48"20" "   START-OF-PACKET (number 0-31)   " ,
    48"15" "   TIMEOUT in seconds  ",
              0),
              UNITSHELP(           %  SET RECEIVE UNITS HELP
    48"08" "   WORDS   ",
    48"0D" "   CHARACTERS    ",
              0),
              ABORTHELP(           %  SET ABORTED-FILE HELP
    48"1C" "   DISCARD the file on abort ",
    48"19" "   KEEP the file on abort    ",
              0),
              DEBUGHELP(           %  SET DEBUGGING HELP
    48"1E" "   STATES - flag state changes     ",
    48"19" "   PACKETS- flag all data    ",
    48"20" "   LOG-FILE changes log filename   ",
    48"1E" "   OFF    - turn off all flags     ",
              0),
              RETRYHELP(           %  SET RETRY HELP
    48"1C" "    INITIAL-CONNECTION count ",
    48"11" "    PACKETS count",
              0),
              ONOFFHELP(           %  ONLY ON OR OFF
    48"05" "   ON",
    48"06" "   OFF     ",
              0),
              LONUMBERHELP(         %  ONLY NUMBERS ALLOWED
    48"24" "   must be an integer from 0 thru 31    ",
              0),
              QUOTEHELP(          %  ONLY 32 < N < 127
    48"2B" "   must be an ASCII character from ! thru ~    ",
    48"2E" "                              (HEX 21 thru 7E) ",
              0),
              NUMBERHELP(         %  ANY NUMBERS ALLOWED
    48"21" "   can be any decimal digit > 0  ",
              0);
 
   DEFINE
     PLAINH    =  0#,
     SETH      =  1#,
     ABORTH    =  2#,
     DEBUGH    =  3#,
     RETRYH    =  4#,
     RECEIVEH  =  5#,
     SENDH     =  5#,
     NUMBERH   =  6#,
     ONOFFH    =  7#,
     QUOTEH    =  8#,
     UNITSH    =  9#,
     LONUMH    = 10#,
     SETFILEH  = 11#;
 
 
 BOOLEAN PROCEDURE SENDSW;                    FORWARD;
 BOOLEAN PROCEDURE RECSW(ISTATE);                          % [1.017]
  REAL ISTATE;                                FORWARD;     % [1.017]
 PROCEDURE SPACK(TYPE,NUM,LEN,DATA);
  VALUE TYPE,NUM,LEN;
  REAL TYPE;
  REAL NUM,LEN;
  ARRAY DATA[0];                              FORWARD;
 REAL PROCEDURE RPACK(LEN,NUM,DATA);
  REAL LEN,NUM;
  ARRAY DATA[0];                              FORWARD;
 REAL PROCEDURE BUFILL(FID,BUFFER);
  FILE FID;
  ARRAY BUFFER[0];                            FORWARD;
 PROCEDURE BUFEMP(FID,BUFFER,LEN);
  VALUE LEN;
  REAL LEN;
  FILE FID;
  ARRAY BUFFER[0];                            FORWARD;
 PROCEDURE SPAR(LEN,DATA);
 REAL LEN;
  ARRAY DATA[0];                              FORWARD;
 PROCEDURE RPAR(LEN,DATA);
 REAL LEN;
  ARRAY DATA[0];                              FORWARD;
 PROCEDURE FLUSHINPUT;                        FORWARD;
 PROCEDURE PRERRPKT(MSG);
  ARRAY MSG[0];                               FORWARD;
 
 %
 %   A B O R T R U N
 %
 %   SENDS AN ERROR PACKET AND ABORTS
 %
   PROCEDURE ABORTRUN;
    BEGIN
      REPLACE TBUF_[0] BY COL FOR * DIGITS,"      ";
      REPLACE EBUF_ BY "KERMIT ABORTING DUE TO FAULT # ",
        TBUF_ FOR 2 WITH EBCDICTOASCII," @ ",
        KPROMPT FOR 50 WITH EBCDICTOASCII;
      SPACK("E",( N := *+1 ) MOD64, MAXPACKSIZ-5,EBUF_);
      IF NOT SERVER THEN
        SAYP(EBUF_,NOINDENT);
      IF (MYSELF.OPTION).[VALUE(FAULT) : 1]=1 THEN
        PROGRAMDUMP(ARRAYS,FILES);
      WHEN(10);
      MYSELF.STATUS := VALUE(TERMINATED);
    END ABORTRUN;
 
%
%  I N I T I A L I Z E
%
%  INITIALIZE SETS UP INITIAL VALUES
%
  PROCEDURE INITIALIZE;
   BEGIN
    ARRAY GREETING[0:13];
    EBCDIC ARRAY VERSION[0:7];
 
    REPLACE MACHINE BY TIME(23).[7:8]*100 FOR 4 DIGITS;
    REPLACE VERSION BY COMPILETIME(20) FOR 1 DIGITS,8".",
       COMPILETIME(21) FOR 3 DIGITS;
    REPLACE GREETING BY "UCD BURROUGHS KERMIT-",
        MACHINE FOR 4 WITH EBCDICTOASCII,
       " - VERSION ",VERSION FOR 5 WITH EBCDICTOASCII,NULC;
    REPLACE FBUF_ BY " " FOR 30 WORDS;
    REPLACE EBUF_ BY " " FOR 16 WORDS;
    SAYP(GREETING,NOINDENT);
    REPLACE KPROMPT BY "KERMIT-",MACHINE FOR 2
       WITH EBCDICTOASCII," ",ETXC; %  PUT ETXC AT END TO KEEP BLANK
 
%   INITIALIZE THESE VALUES AND HOPE THE FIRST PACKET WILL GET ACROSS OK
 
    EOL        := CR;              %  EOL FOR OUTGOING PACKETS
    SOHCHAR    := DEFSOH;          % SOH FOR OUTGOING PACKETS
    QUOTE      := "#";             %  STANDARD CONTROL-QUOTE CHAR "#"
    PAD        := 0;               %  NO PADDING
    PCHAR      := NULC;            %  USE NULC IF ANY PADDING WANTED
    QBIN       := "N";             %  DEFAULT TO NO BINARY QUOTING
    REPT       := SP;              %  DEFAULT TO SPACE
    CHKTYPE    := "1";             %  DEFAULT CHKTYPE
    MYPACKSIZ  := MAXPACKSIZ;      %  SET MINE TO DEFAULTS
    INITRETRY  := DEFINITRETRY;    %  INITIALIZE RETRIES
    PACKETRETRY:= DEFPACKETRETRY;
    FILERECSIZE:= DEFRECSIZE;
    FILEBLOCKSIZE:= DEFBLOCKSIZE;
    FILEUNITS   := DEFUNITS;
    MYTIME     := DEFTIME;
    MYPAD      := DEFPAD;
    MYPCHAR    := DEFPCHAR;
    MYEOL      := DEFEOL;
    MYSOH      := DEFSOH;              % [1.019]
    MYQUOTE    := DEFQUOTE;
    MYQBIN     := DEFQBIN;             % [1.019]
    MYCHKTYPE  := DEFCHKTYPE;
    MYREPT     := DEFREPT;
    MYPAUSE    := DEFPAUSE;     % SECONDS ( INPUT IS IN 10THS )
    MYDELAY    := DEFDELAY;
    MYESCCHR   := DEFESCCHR;
 
    FIXEDRECS  := FALSE;           %  DEFAULT
    EXPTABS    := TRUE;            %  DEFAULT -> EXPAND THEM
    HIBITOK    := FALSE;           % [1.017] 8-BIT ONLY WHEN REQUESTED
    BINARYON   := FALSE;           % [1.019] CHANGED BY SET BINARY COMMA
    REPTOK     := FALSE;           % [1.017] NO REPEAT PROCESSING
    KEEPFILE    := TRUE;           %  DEFAULT TO KEEP ALL FILES MADE
    RAW         := FALSE;          %  USE CR FOR END-OF-LINE
                                   %  INITIALIZE ACNTRL TABLE
    REPLACE ACNTRL BY 48"0000FFFFFFFF",0,0,48"000000000001",0,0,0,0;
    REPLACE BCNTRL BY 0 FOR 8 WORDS;
 
   END INITIALIZE;
 
%
%  S C A N I T
%
%  SCANS INPUT AND PUTS ITEMS INTO ARRAY AC IN KUNKER-FORM.  PLACES
%  ITEM LENGTH INTO LEN AND RETURNS THE ITEM'S INDEX IN THE SPECIAL
%  ARRAY.
%
 
    REAL PROCEDURE SCANIT;
      BEGIN
        ARRAY AC[0:3];
        REAL I,J,SAVEJ,CNT;
 
        SCANIT := -1;
        COL_OK_TIL := COL;
        SCAN CP:CP FOR COL:COL UNTIL GTR " ";
        IF COL GTR 0 THEN
          BEGIN
            SCAN CP FOR I:COL WHILE GTR " ";
            IF CP+((I := COL-I)-1) = "?" THEN
             IF I GTR 1 THEN
              I := *-1;
            REPLACE POINTER(AC) BY I.[7:48] FOR 1,
                  CP FOR I;
            J := SIZE(SPECARA);
            SAVEJ := CNT := -1;
            WHILE J:=*-1 GEQ 0 DO
              IF J := MASKSEARCH(AC[0],
               40"E0" & REAL(NOT FALSE)[39:MIN(40,I*8)],SPECARA[J])
               GEQ 0 THEN
              IF CP = POINTER(SPECARA[J])+1 FOR I THEN
                TBUF_[CNT:=*+1] := SAVEJ := J
            ;
            IF (CNT:=*+1 GTR 1)OR(CP+I = "?") THEN
              BEGIN
                IF CP+I NEQ "?" THEN
              SAY("  ambiguous command, please supply more characters");
                SAY("  possible commands:");
                WHILE CNT GTR 0 DO
                    IF SAVEJ := TBUF_[CNT := *-1] GTR 0 THEN
                      SAYP(POINTER(SPECARA[SAVEJ])+1,INDENT);
                SCANIT := NOERRORV;
              END
            ELSE
                SCANIT := SAVEJ;
            CP  := *+I;
            COL := *-I;
          END;
      END SCANIT;
 
%
%  S C A N U M
%
   REAL PROCEDURE SCANUM;
    BEGIN
      ARRAY AC[0:3];
      REAL I,J,SAVEJ,CNT;
 
      SCANUM := INVALIDV;
      COL_OK_TIL := COL;
      SCAN CP:CP FOR COL:COL UNTIL GTR " ";
      IF COL GTR 0 THEN
         IF CP IN NUMBERS THEN
           BEGIN
             SCAN CP FOR I:COL WHILE IN NUMBERS;
             IF I := COL-I LSS 12 THEN
               BEGIN
                 REPLACE AC BY CP FOR I WITH ASCIITOEBCDIC;
                 SCANUM := INTEGER(AC,I);
               END
             ELSE
               SCANUM := INVALIDV;
           END
         ELSE
           IF CP = "?" THEN
              SCANUM := QUESTIONV;
     END OF PROCEDURE SCANUM;
%
%  H E L P E R
%
%  DOES ALL THE HELP STUFF FROM ? OR HELP INPUT
%
 
 $BEGINSEGMENT
 
PROCEDURE HELPER(TYPE);
 VALUE TYPE;
 REAL TYPE;
 BEGIN
   ARRAY REFERENCE HELPARA[0];
   POINTER P;
   REAL LENGTH;
   CASE TYPE OF
   BEGIN
   PLAINH:                           %  PLAN OLD HELP
        HELPARA := PLAINHELP;
   SETH:                           %  SET HELP
        HELPARA := SETHELP;
   ABORTH:                         %  ABORT HELP
        HELPARA := ABORTHELP;
   DEBUGH:                         %  DEBUG HELP
        HELPARA := DEBUGHELP;
   RETRYH:                         %  RETRY HELP
        HELPARA := RETRYHELP;
   SENDH:                          %  SEND HELP
        HELPARA := SENDHELP;
   NUMBERH:                        %  NUMBER HELP
        HELPARA := NUMBERHELP;
   ONOFFH:                         %  ON/OFF HELP
        HELPARA := ONOFFHELP;
   QUOTEH:                         %  QUOTE HELP
        HELPARA := QUOTEHELP;
   UNITSH:                         %  RECEIVE UNITS HELP
        HELPARA := UNITSHELP;
   LONUMH:                         %  LOW NUMBER HELP (0-31)
        HELPARA := LONUMBERHELP;
   SETFILEH:
        HELPARA := SETFILEHELP;    %  SET FILE HELP
   ELSE:
        LENGTH := -1;
   END CASE;
   IF LENGTH GEQ 0 THEN
    BEGIN
     P := POINTER(HELPARA);
     WHILE LENGTH := REAL(P,1) GTR 0 DO
      BEGIN
        BRD := WRITE(REM,LENGTH,P+1);
        P := *+(((LENGTH + 6) DIV 6) *6);
      END;
    END;
 END HELPER;
 
 
%
%  S E T S T U F F
%
%  SETS THE VARIOUS THINGS
%
PROCEDURE SETSTUFF;
 BEGIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE ABORTER;
    BEGIN
      SPECARA := SPECABORT;
      CASE SCANIT OF
      BEGIN
    QMARKV:
SAY("  determines what to do if RECEIVE transfer fails - options are:");
           HELPER(ABORTH);
    DISCARDV:
           KEEPFILE := FALSE;
    KEEPV:
           KEEPFILE := TRUE;
    ELSE:
           SAYQOPT("INCOMPLETE");
           HELPER(ABORTH);
      END CASE;
    END ABORTER;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE DEBUGGER;
    BEGIN
      POINTER P;
      SPECARA := SPECDEBUG;
      CASE SCANIT OF
      BEGIN
    QMARKV:
           SAY("  sets level of DEBUGGING output -- options are:");
           HELPER(DEBUGH);
    STATESV:
           DEBUG := TRUE;
    PACKETSV:
           DEBUG := BOOLEAN(3);
    LOGFILEV:
           IF NOT DEBUG THEN DEBUG := TRUE;
           SCAN CP:CP FOR COL:COL WHILE LEQ " ";
           IF COL GTR 0 THEN
            BEGIN
              SCAN P:CP FOR COL WHILE GEQ "A";
              REPLACE P BY "."48"00";
              IF LOG.OPEN THEN LOCK(LOG,CRUNCH);
              REPLACE CP BY CP FOR COL+1 WITH ASCIITOEBCDIC;
              REPLACE LOG.TITLE BY CP;
            END;
    DOFFV:
           DEBUG := FALSE;
    ELSE:
           SAYQOPT("DEBUGGING");
           HELPER(DEBUGH);
      END CASE;
      IF DEBUG THEN
        IF NOT LOG.OPEN THEN LOG.OPEN := TRUE
        ELSE
      ELSE
        IF LOG.OPEN THEN LOCK(LOG,CRUNCH);
    END DEBUGGER;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE DELAYER;
    BEGIN
      REAL N;
      N := SCANUM;
      IF (N LSS 0)OR(N GTR 31) THEN
        IF (N = QUESTIONV) THEN
        BEGIN
             SAY("   sets time to delay (in secs) before");
             SAY("   sending first packet during file SEND");
             HELPER(LONUMH);
        END
        ELSE
        BEGIN
             SAYQ("DELAY");
             HELPER(LONUMH);
        END
      ELSE
        MYDELAY := N
    END DELAYER;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE RETRYER;
    BEGIN
      REAL N;
      SPECARA := SPECRETRY;
      CASE SCANIT OF
      BEGIN
    QMARKV:
           SAY("   sets number of times to retry an operation");
           SAY("   before giving up -  options are:");
           HELPER(RETRYH);
    INITCONNV:
           N := SCANUM;
           IF (N LSS 0) THEN
              IF (N = QUESTIONV) THEN
              BEGIN
             SAY("   sets number of times to retry initial connection");
                HELPER(NUMBERH)
              END
              ELSE
              BEGIN
                SAYQ("INITIAL-CONNECTION");
                HELPER(NUMBERH);
              END
           ELSE
              INITRETRY := N;
    RETRYPACKETSV:
           N := SCANUM;
           IF (N LSS 0) THEN
              IF (N = QUESTIONV) THEN
              BEGIN
             SAY("   sets number of times to retry regular connection");
                HELPER(NUMBERH)
              END
              ELSE
              BEGIN
                SAYQ("PACKETS");
                HELPER(NUMBERH);
              END
           ELSE
              PACKETRETRY := N;
    ELSE:
           SAYQOPT("RETRY");
           HELPER(RETRYH);
      END CASE
    END RETRYER;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE BLOCKER;
    BEGIN
      REAL N;
      N := SCANUM;
      IF (N LSS 1) THEN
         IF (N = QUESTIONV) THEN
         BEGIN
             SAY("  sets BLOCKSIZE attribute of RECEIVED files");
             HELPER(NUMBERH);
         END
         ELSE
         BEGIN
             SAYQ("BLOCK-SIZE");
             HELPER(NUMBERH);
         END
       ELSE
       BEGIN
         FILEBLOCKSIZE := N;
         IF (FILEBLOCKSIZE MOD FILERECSIZE) NEQ 0 THEN
         BEGIN
           SAY("Warning: BLOCK-SIZE must be a multiple of RECORD-SIZE");
        SAY1("  current settings:       RECORD-SIZE = ",FILERECSIZE);
        SAY1("                           BLOCK-SIZE = ",FILEBLOCKSIZE);
         END
       END
    END BLOCKER;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE RECSIZER;
    BEGIN
      REAL N;
      N := SCANUM;
      IF (N LSS 1) THEN
         IF (N = QUESTIONV) THEN
         BEGIN
             SAY("  sets MAXRECSIZE attribute of RECEIVED files");
             HELPER(NUMBERH);
         END
         ELSE
         BEGIN
             SAYQ("RECORD-SIZE");
             HELPER(NUMBERH);
         END
       ELSE
       BEGIN
         FILERECSIZE := N;
         IF (FILEBLOCKSIZE MOD FILERECSIZE) NEQ 0 THEN
         BEGIN
           SAY("Warning: BLOCK-SIZE must be a multiple of RECORD-SIZE");
        SAY1("  current settings:       RECORD-SIZE = ",FILERECSIZE);
        SAY1("                           BLOCK-SIZE = ",FILEBLOCKSIZE);
         END
       END
    END RECSIZER;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  PROCEDURE UNITER;
   BEGIN
      SPECARA := SPECUNITS;
      CASE SCANIT OF
      BEGIN
      QMARKV:
 SAY("   set UNITS file attribute for received files -- options are:");
             HELPER(UNITSH);
     UWORDSV:
             FILEUNITS := VALUE(WORDS);
     UCHARACTERSV:
             FILEUNITS := VALUE(CHARACTERS);
        ELSE:
             SAYQOPT("UNITS");
             HELPER(UNITSH);
     END CASE;
   END OF PROCEDURE UNITER;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE SENDRECEIVER(WHICH);
    VALUE WHICH;
    REAL WHICH;
    BEGIN
      REAL N;     % [1.018] NEED TO DECLARE LOCALLY
      INTEGER NDX;
      SPECARA := SPECRECEIVE;
       CASE NDX := SCANIT OF
      BEGIN
    QMARKV:
            SAY("  sets various packet parameters - options are:");
           HELPER(SENDH);
    EOLV:
           N := SCANUM;
           IF (N LSS 1)OR(N GTR 31) THEN
              IF (N = QUESTIONV) THEN
              BEGIN
                IF WHICH=SETRECEIVEV THEN
       SAY("  sets the packet terminator character to expect")
                ELSE
       SAY("  sets the packet terminator character to send");
                HELPER(LONUMH)
              END
              ELSE
              BEGIN
                SAYQ("END-OF-LINE");
                HELPER(LONUMH);
              END
           ELSE
             IF WHICH=SETRECEIVEV THEN
               MYEOL := N
             ELSE
               EOL := N;
    QUOTEV:
          IF WHICH=SETRECEIVEV THEN
   SAY("  not implemented, no need to set QUOTE to expect")
          ELSE
          BEGIN
           COL_OK_TIL := COL;
           SCAN CP:CP FOR COL:COL WHILE LEQ " ";
           IF COL GTR 0 THEN
           BEGIN
            IF CP = "?" THEN
            BEGIN
                SAY("  sets QUOTE character to send");
                HELPER(QUOTEH)
            END
            ELSE
             IF N := REAL(CP,1) LSS 33 OR N GTR 126 THEN  % ! < N < ~
             BEGIN
                SAY("  invalid QUOTE character - must be an");
                HELPER(QUOTEH)
             END
             ELSE
               IF N=MYQBIN THEN               %  NO WAY!
  SAY("  QUOTE not set, that character is your binary quote")
               ELSE
                 IF N=MYREPT THEN             % NO WAY!
  SAY("  QUOTE not set, that character is your repeat quote")
                 ELSE
                    MYQUOTE := N
            END
            ELSE
            BEGIN
              SAYQ("QUOTE");
              HELPER(QUOTEH);
            END;
          END;
    LENV:
           N := SCANUM;
           IF (N LSS 10)OR(N GTR 94) THEN
              IF (N = QUESTIONV) THEN
              BEGIN
                IF WHICH=SETRECEIVEV THEN
                  SAY("  set PACKET-LENGTH for incoming packets")
                ELSE
                  SAY("  sets PACKET-LENGTH for outgoing packets");
                SAY("   must be an integer from 10 to 94");
              END
              ELSE
              BEGIN
                SAYQ("PACKET-LENGTH");
                SAY("   must be an integer from 10 to 94");
              END
           ELSE
             IF WHICH=SETRECEIVEV THEN
               MYPACKSIZ := N
             ELSE
               SPSIZ := N;
    PADV:
    PCHARV:
    PAUSEV:
    STARTOFPACKV:
    TIMEOUTV:
           N := SCANUM;
           IF (N LSS 0)OR(N GTR 31) THEN
              IF (N = QUESTIONV) THEN
              BEGIN
                IF WHICH=SETRECEIVEV THEN
    SAY("  sets a packet parameter for incoming packets")
                ELSE
    SAY("  sets a packet parameter for outgoing packets");
                HELPER(LONUMH);
              END
              ELSE
              BEGIN
                SAYQ("packet");
                HELPER(LONUMH);
              END
           ELSE
            CASE NDX OF
            BEGIN
    PADV:
             IF WHICH=SETRECEIVEV THEN
               MYPAD := N
             ELSE
               PAD := N;
    PCHARV:
             IF WHICH=SETRECEIVEV THEN
               MYPCHAR := N
             ELSE
               PCHAR := N;
    PAUSEV:
           MYPAUSE := N/10;
    STARTOFPACKV:
           IF WHICH=SETRECEIVEV THEN
              MYSOH := N
           ELSE
              SOHCHAR := N;
    TIMEOUTV:
           IF N = 0 THEN
             SAY("  TIMEOUT must be greater than zero")
           ELSE
             IF WHICH=SETRECEIVEV THEN
               MYTIME := N
             ELSE
                TIMINT := N;
             END CASE;
    NOERRORV:
           ;
    ELSE:
           IF WHICH=SETRECEIVEV THEN
              SAYQOPT("RECEIVE")
           ELSE
              SAYQOPT("SEND");
           HELPER(SENDH);
      END CASE;
    END SENDRECEIVER;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE BINARER;
    BEGIN
      SPECARA := SPECONOFF;
      CASE SCANIT OF
      BEGIN
    QMARKV:
         SAY("  transfer all 8 bits of each character - options are:");
           HELPER(ONOFFH);
       ONV:
           BINARYON := TRUE;
      OFFV:
           BINARYON := FALSE;
      ELSE:
           SAYQOPT("BINARY");
           HELPER(ONOFFH);
      END CASE;
    END BINARER;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE FIXER;
    BEGIN
      SPECARA := SPECONOFF;
      CASE SCANIT OF
      BEGIN
    QMARKV:
           SAY("  send trailing blanks found at the end of");
           SAY("  fixed-length records -- options are:");
           HELPER(ONOFFH);
       ONV:
           FIXEDRECS := TRUE;
      OFFV:
           FIXEDRECS := FALSE;
      ELSE:
           SAYQOPT("FIXED");
           HELPER(ONOFFH);
      END CASE;
    END FIXER;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE EXPANDTABBER;
    BEGIN
      SPECARA := SPECONOFF;
      CASE SCANIT OF
      BEGIN
    QMARKV:
SAY("  expand TABs to spaces when RECEIVING files - options are:");
           HELPER(ONOFFH);
       ONV:
           EXPTABS := TRUE;
      OFFV:
           EXPTABS := FALSE;
      ELSE:
           SAYQOPT("EXPAND-TABS");
           HELPER(ONOFFH);
      END CASE;
    END EXPANDTABBER;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE RAWER;
    BEGIN
      SPECARA := SPECONOFF;
      CASE SCANIT OF
      BEGIN
    QMARKV:
SAY("  fill each record to MAXRECSIZE when RECEIVING files -");
           SAY("  options are:");
           HELPER(ONOFFH);
       ONV:
           RAW := TRUE;
           EXPTABS := FALSE;
           SAY("EXPAND-TABS now set to OFF");
      OFFV:
           RAW := FALSE;
           IF EXPTABS THEN SAY("EXPAND-TABS is ON")
           ELSE            SAY("EXPAND-TABS is OFF");
      ELSE:
           SAYQ("RAW");
           HELPER(ONOFFH);
      END CASE;
    END RAWER;
 
%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE SETFILER;
    BEGIN
 
      INTEGER NDX;
      SPECARA := SPECFILE;
      CASE (NDX := SCANIT) OF
      BEGIN
  QMARKV:
        HELPER(SETFILEH);
  BLOCKSIZEV:
        BLOCKER;
  EXPTABSV:
        EXPANDTABBER;
  FIXEDV:
        FIXER;
  RAWV:
        RAWER;
  RECORDSIZEV:
        RECSIZER;
  UNITSV:
        UNITER;
  NOERRORV:
           ;
  ELSE:
        SAYQOPT("FILE");
        HELPER(SETFILEH);
      END OF CASE;
    END OF PROCEDURE SETFILER;
%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% mainline for procedure SETSTUFF
   SPECARA := SPECSET;
   CASE SCANIT OF
   BEGIN
  QMARKV:
    SAY("  sets various KERMIT environment variables -- options are:");
        HELPER(SETH);
  INCOMPLETEV:
        ABORTER;
  BINARYV:
        BINARER;
  DEBUGV:
        DEBUGGER;
  DELAYV:
        DELAYER;
  FILEV:
        SETFILER;
  RETRYV:
        RETRYER;
  SETRECEIVEV:
        SENDRECEIVER(SETRECEIVEV);
  SETSENDV:
        SENDRECEIVER(SETSENDV);
  NOERRORV:
        ;
  ELSE:
        SAYQOPT("SET");
        HELPER(SETH);
   END CASE;
 END SETSTUFF;
 
 $ENDSEGMENT
 
 
%
%  S T A T U S
%
%  DISPLAY THE STATUS OF ALL THE VARIOUS THINGS
%
PROCEDURE STATUS;
 BEGIN
   SAY("parameters which can be changed by the SET command");
   IF (BINARYON) THEN
      SAY("  BINARY ON (8th bit quoting will be requested)")
   ELSE
      SAY("  BINARY OFF (No 8th bit quoting will be done)");
   IF DEBUG THEN
    BEGIN
      REPLACE PFILNAM:=POINTER(FILNAM) BY LOG.TITLE,NULC;
      REPLACE PFILNAM BY PFILNAM FOR 80 WITH EBCDICTOASCII;
      IF REAL(DEBUG) GTR 1 THEN
        SAYN("  DEBUG STATES and PACKETS to file ",PFILNAM)
      ELSE
        SAYN("  DEBUG STATES to file ",PFILNAM);
    END
   ELSE
     SAY("  DEBUG OFF");
   SAY1("  DELAY before first send (in seconds) = ",MYDELAY);
   IF KEEPFILE THEN
     SAY("  if INCOMPLETE, KEEP partial file")
   ELSE
     SAY("  if INCOMPLETE, DISCARD partial file");
   SAY1("  RETRY INITIAL-CONNECTION = ",INITRETRY);
   SAY1("  RETRY PACKETS = ",PACKETRETRY);
   SAY("parameters which can be changed by the SET FILE command");
   SAY1("  RECORD-SIZE = ",FILERECSIZE);
   SAY1("  BLOCK-SIZE  = ",FILEBLOCKSIZE);
   IF FILEUNITS = VALUE(WORDS) THEN
     SAY("  UNITS       = WORDS")
   ELSE
     SAY("  UNITS       = CHARACTERS");
   IF EXPTABS THEN
     SAY("  EXPAND-TABS ON")
   ELSE
     SAY("  EXPAND-TABS OFF");
   IF FIXEDRECS THEN
     SAY("  FIXED ON (send blanks found at the end of records)")
   ELSE
     SAY("  FIXED OFF (strip blanks from the end of records)");
   IF RAW THEN
     SAY("  RAW ON ( Burroughs records delimited by size only )")
   ELSE
     SAY("  RAW OFF ( Burroughs records delimited by CR )");
   SPECARA := SPECSHOW;
   CASE SCANIT OF
   BEGIN
SHOSENDV:
     SAY("parameters which can be changed by the SET SEND command");
     SAYC("  END-OF-LINE character = ",EOL);
     SAY1("  maximum PACKET-LENGTH = ",SPSIZ);
     SAY1("  number of PADDING characters = ",PAD);
     IF PAD GTR 0 THEN
       SAYC("  PADDING CHARACTER = ",PCHAR);
 SAY1("  PAUSE before packet send (in tenths of second) = ",MYPAUSE*10);
     SAYC("  START-OF-PACKET charcter = ",SOHCHAR);
     SAY1("  packet TIMEOUT (in seconds) = ",TIMINT);
ELSE:
     SAY("parameters which can be changed by the SET RECEIVE command");
     SAYC("  END-OF-LINE character = ",MYEOL);
     SAY1("  maximum PACKET-LENGTH = ",MYPACKSIZ);
     SAY1("  number of PADDING characters = ",MYPAD);
     IF MYPAD GTR 0 THEN
       SAYC("  PADDING CHARACTER = ",MYPCHAR);
 SAY1("  PAUSE before packet send (in tenths of second) = ",MYPAUSE*10);
     SAYC("  QUOTE character = ",MYQUOTE);
     SAYC("  START-OF-PACKET character = ",MYSOH);
     SAY1("  packet TIMEOUT (in seconds) = ",MYTIME);
   END CASE;
 END STATUS;
 
 
BOOLEAN PROCEDURE PROCESSIT;
BEGIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PROCEDURE SERVANT;
 BEGIN
   BOOLEAN DONTQUIT;               %  LOOP CONTROL
   ARRAY BUFFER[0:MAXPACKWDS];     %  TEMPORARY FILE TITLE BUFFER
   FILE DUMMY(KIND=PACK,FILETYPE=7);%  TEMPORARY DUMMY FILE
   REAL NUM,LEN,TIMER;             %  PACKET NUMBER, LENGTH, TIMEOUT
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PROCEDURE GENERICTHINGS;        %  HANDLE "G" REQUESTS
    BEGIN
      POINTER PR;                  %  POINTER TO PACKET
 
      PR := POINTER(PACKET);       %  INITIALIZE IT
      CASE REAL(PR,1) OF
      BEGIN
      "F":                         %  FINISH, BUT DON'T LOGOUT
           SPACK("Y",N,0,NULLDATA);%  ACK TO PC AND THEN...
           DONTQUIT := FALSE;      %  EXIT FROM WHILE LOOP
           BRD := TRUE;            %  AND EXIT FROM MAIN LOOP
      "L":                         %  FINISH AND LOG OUT, TOO
%          SPACK("Y",N,0,NULLDATA);%  ACK TO PC AND THEN...
           SAYN("BYE IS NOT IMPLEMENTED:  ",PACKET);
           DONTQUIT := FALSE;      %  EXIT FROM WHILE LOOP
           BRD := TRUE;            %  AND EXIT FROM MAIN LOOP
           %                       %  THIS PART ISN'T IMPLEMENTED
     ELSE:                         %  SOME OTHER NON-IMPLEMENTED THING
           SAYN("THIS IS NOT IMPLEMENTED:  ",PACKET);
      END CASE;
    END GENERICTHINGS;
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% mainline statements for SERVANT
   STATE := "T";                   %  JUST TO INITIALIZE FOR DEBUG
   DONTQUIT := SERVER := TRUE;     %  INITIALIZE BOOLEANS
   REPLACE PFILNAM:=POINTER(FILNAM) BY NULC FOR MAXPACKWDS WORDS;
   TIMER := IF TIMINT LSS MINTIM OR TIMINT GTR MAXTIM THEN
                          MYTIME ELSE TIMINT;
 
   WHILE DONTQUIT DO
    BEGIN
      REM.TIMELIMIT := TIMER;    %  SET UP FOR IO TIMEOUT
      IF DEBUG THEN BUGC("SERVANT STATE:  ",STATE);
      CASE RPACK(LEN,NUM,PACKET) OF%  DO WHICHEVER ONE WE NEED
      BEGIN
      "R":                         %  GET OR RECEIVE A FILE(US TO PC)
           N := NUM;               %  RESTART PACKET NUMBERS
           REPLACE PFILNAM BY PACKET FOR LEN WITH TOUPPER,
                 ".",NULC;
           REPLACE BUFFER BY PFILNAM FOR LEN+1 WITH ASCIITOEBCDIC;
           IF DUMMY.OPEN THEN CLOSE(DUMMY);
           REPLACE DUMMY.TITLE BY BUFFER;
           IF NOT DUMMY.RESIDENT THEN
             SAYN("NO FILE:  ",FILNAM)
           ELSE
           IF SENDSW THEN
%            SAYN("SEND DONE FOR:  ",PFILNAM)
%          ELSE
%            SAYN("SEND FAILED FOR:  ",PFILNAM);
           STATE := "R";           %  FOR DEBUG
      "S":                         %  SEND A FILE (FROM PC TO US)
           CALL1 := TRUE;                   % [1.017]
           RPAR(LEN,PACKET);                % [1.017] EXCHANGE
           SPAR(LEN,PACKET);                % [1.017]  PARAMETERS
           SPACK("Y",NUM,LEN,PACKET);       % [1.017]
           OLDTRY := NUMTRY;                % [1.017] RESET COUNTERS
           NUMTRY := 0;                     % [1.017]
           N := (NUM+1) MOD64;              % [1.017]
           IF (NOT RECSW("F")) THEN         % [1.017] ATTEMPT TO RECEIVE
           BEGIN                            % [1.017]
             REPLACE FBUF_ BY "RECEIVE FAILED.";
             ERROR(FBUF_);                  % [1.017]
           END;                             % [1.017]
%            SAYN("RECEIVE DONE FOR:  ",PFILNAM)
%          ELSE                    %  NO SUCH LUCK
%          IF KEEPFILE THEN        %  WE GOT PART OF IT...
%            SAYN("RECEIVE FAILED, BUT SAVED PART OF:  ",PFILNAM)
%          ELSE
%            SAYN("RECEIVE FAILED FOR:  ",PFILNAM);
           STATE := "S";           %  FOR DEBUG
      "T":                        %  TIMED OUT
           SPACK("N",N,0,NULLDATA);%  NAK ON TIMEOUT
           STATE := "T";          %  FOR DEBUG
      "G":                         %  GENERIC COMMAND
           GENERICTHINGS;          %  TAKE CARE OF THEM ELSEWHERE
      "I":                         %  INITIALIZE PACKETS
           CALL1 := TRUE;          % [1.017] RPAR CALLED FIRST
           RPAR(LEN,PACKET);       %  GET HIS INIT DATA
           SPAR(LEN,PACKET);       %  FILL UP PACKET WITH MY INIT DATA
           SPACK("Y",N,LEN,PACKET);%  ACK WITH MY PARAMETERS
           OLDTRY := NUMTRY;       %  SAVE OLD TRY COUNT
           NUMTRY := 0;            %  INITIALIZE NUMTRY
     ELSE:                         %  WHO KNOWS
           SPACK("N",N,6,PACKET);  %  NAK IT
      END CASE;
    END WHILE;
   WHEN(5);                        %  MAKE SURE ACK GETS OUT
   BRD := TRUE;                    %  EXIT THRU TO EOT
  END SERVANT;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  G E T F I L E T I T L E
%
%  GETS THE DISK FILE TITLE FOR SENDING
%  AND MAKES SURE THE FILE TITLE WILL FIT INSIDE A PACKET
%
PROCEDURE GETFILETITLE;
 BEGIN
   POINTER P,Q;
   REAL I,J;
 
   SCAN CP:CP FOR COL:COL WHILE LEQ " ";
   IF COL GTR 0 THEN
     BEGIN
       SCAN P:CP FOR I:COL WHILE GTR " ";
       COL := *-I;
       REPLACE PFILNAM:=POINTER(FILNAM) BY CP FOR COL,".",NULC;
       CP := POINTER(PACKET)+COL;
       IF (COL:=I)-1 GTR 0 THEN
         SCAN Q:P+1 FOR J:I-1 UNTIL GTR " ";
       IF MORETOSEND := J GTR 0 THEN
         REPLACE NEXTSEND BY Q FOR J,NULC;
       REPLACE PACKET BY NULC FOR MAXPACKWDS WORDS;
     END;
 END GETFILETITLE;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% mainline statements for PROCESSIT
  REPLACE CP:=PACKET BY NULC FOR MAXPACKWDS WORDS;
  COL:=80;
  SPECARA := SPECIAL;
  BRD := WRITE(REM[STOP],11,KPROMPT);
  IF NOT BRD := READ(REM,COL,PACKET) THEN
  BEGIN
   REPLACE CP BY CP FOR COL := RD.LENGTHF WITH TOUPPER;
   COL_BASE := COL;
   SCAN CP:CP FOR COL:COL WHILE LEQ " ";
   IF COL GTR 0 THEN
      CASE SCANIT OF
      BEGIN
 QMARKV:
 HELPV:
        HELPER(PLAINH);        %  ?, HELP
 SERVERV:
        SERVANT;               %  SERVER
 SENDV:
        GETFILETITLE;          %  SEND
        IF SENDSW THEN
%         SAY("SEND DONE")
;%      ELSE
%         SAY("SEND FAILED");
 RECEIVEV:
        REPLACE PFILNAM := POINTER(FILNAM) BY NULC FOR MAXPACKWDS WORDS;
       IF RECSW("R") THEN                    % [1.017]
%         SAY("RECEIVE DONE")
;%       ELSE
%         IF KEEPFILE THEN
%           SAY("RECEIVE FAILED - PARTIAL FILE SAVED")
%         ELSE
%           SAY("RECEIVE FAILED");
 SETV:
        SETSTUFF;              %  SET
 SHOWV:
 STATUSV:
        STATUS;                %  STATUS
 QUITV:
 EXITV:
        BRD := TRUE;
 NOERRORV:                     %  WE ALREADY GAVE THE ERROR
        ;
 ELSE:                         %  GARBAGE?
        SAYQOPT(" ");
        HELPER(PLAINH);
      END CASE;
      IF MORETOSEND THEN
        BEGIN
          MORETOSEND := FALSE;
          REPLACE CP := PACKET BY "SEND ",NEXTSEND FOR
              COL:MAXPACKSIZ-5 UNTIL=NULC, NULC FOR COL;
        END;
  END IF;
END PROCESSIT;
 
 
 %
 %  S E N D S W
 %
 %  SENDSW IS THE STATE TABLE SWITCHER FOR SENDING FILES.  IT LOOPS UNTI
 %  EITHER IT FINISHES, OR AN ERROR IS ENCOUNTERED.  THE ROUTINES CALLED
 %  BY SENDSW ARE RESPONSIBLE FOR CHANGING THE STATE.
 %
 
 
 $BEGINSEGMENT
 
BOOLEAN PROCEDURE SENDSW;
 BEGIN
  BOOLEAN DONTQUIT;            %  LOOP CONTROL
  FILE FP(KIND=DISK,FILETYPE=8,        %  CURRENT DISK FILE
          INTMODE=ASCII,               %  SO CHECKSUM, ETC, WILL WORK
          TRANSLATE=FORCESOFT,INPUTTABLE=EBCDICTOASCII,
          MYUSE=IN);
 
 
 
 %
 %  S I N I T
 %
 %  SEND INITIATE: SEND THIS HOST'S PARAMETERS AND GET OTHER SIDE'S BACK
 
 
REAL PROCEDURE SINIT;
 BEGIN
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH
 
  IF (NUMTRY:=*+1 LEQ INITRETRY) THEN
  BEGIN
    CALL1 := TRUE;                 % [1.017] SPAR CALLED FIRST
    SPAR(LEN,PACKET);              %  FILL UP INIT INFO PACKET
 
    IF NOT SERVER AND NUMTRY = 1 THEN % WAIT A BIT BEFORE SENDING THE
      WHEN(MYDELAY);               %  INIT PACKET...
    FLUSHINPUT;                    %  FLUSH PENDING INPUT
 
    SPACK("S",N,LEN,PACKET);       %  SEND AN S PACKET
    CASE RPACK(LEN,NUM,RECPKT) OF  %  WHAT WAS THE REPLY?
     BEGIN
     "N":  SINIT := STATE;         %  NAK, TRY IT AGAIN
 
     "Y":                          %  ACK
           IF (N = NUM) THEN
             BEGIN
               RPAR(LEN,RECPKT);   %  GET OTHER SIDE'S INIT INFO
               IF EOL = 0 THEN     %  CHECK AND SET DEFAULTS
                 EOL := MYEOL;
               IF QUOTE = 0 THEN
                 QUOTE := MYQUOTE;
               NUMTRY := 0;        %  RESET TRY COUNTER
               N := (N+1) MOD64;   %  BUMP PACKET COUNT
               SINIT := "F";       %  OK, CASE STATE TO F
             END ELSE
               SINIT := STATE;     %  IF WRONG ACK, STAY IN S STATE
 
     "E":                          %  ERROR PACKET RECEIVED
           PRERRPKT(RECPKT);       %  PRINT IT OUT AND
           SINIT := "A";           %  ABORT
 
     "T":                         %  RECEIVE FAILURE, TRY AGAIN
           SINIT := STATE;
 
    ELSE:                          %  ANYTHING ELSE, JUST ABORT
           SINIT := "A";
    END CASE;
  END ELSE
      SINIT := "A";                %  IF TOO MANY TRIES, GIVE UP
 END SINIT;
 
 
 %
 %  S F I L E
 %
 %  SEND FILE HEADER.
 
 
REAL PROCEDURE SFILE;
 BEGIN
  LABEL ACKHERE,QUIT;
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH
  ARRAY FILNAM1[0:MAXPACKWDS];             %  CONVERTED FILE NAME
  POINTER NEWFILNAM,               %  POINTER TO FILE NAME TO SEND
          CP;                      %  CHAR POINTER
 
  IF (NUMTRY:=*+1 LEQ PACKETRETRY+1) THEN
  BEGIN
 
    IF FP.OPEN THEN CLOSE(FP);     %  MAKE SURE IT'S CLOSED
    SCAN CP := PFILNAM FOR LEN:MAXPACKSIZ UNTIL = NULC;
    LEN := MAXPACKSIZ - LEN;       %  LENGTH OF BURROUGHS TITLE
    REPLACE FILNAM1 BY PFILNAM FOR LEN WITH ASCIITOEBCDIC;
    REPLACE FP.TITLE BY FILNAM1;   %  GIVE IT THE NEW NAME
    IF NOT FP.PRESENT THEN         %  FILE ISN'T THERE
      BEGIN
        REPLACE FBUF_ BY "CANNOT FIND FILE: ",PFILNAM FOR MAXSENDFILESIZ
            WHILE GEQ " ",NULC;
        ERROR(FBUF_);
        SFILE := "A";
        GO QUIT;
      END;
    IF FP.EXTMODE = VALUE(EBCDIC) THEN % DEFAULT IS TO TRANSLATE IT...
      BEGIN
        CLOSE(FP);
        FP.EXTMODE := VALUE(EBCDIC);
        FP.OPEN := TRUE;
      END
    ELSE
    IF FP.EXTMODE = VALUE(ASCII) THEN % DONT TRANSLATE IT...
      BEGIN
        CLOSE(FP);
        FP.EXTMODE := VALUE(ASCII);
        FP.TRANSLATE := VALUE(FULLTRANS);
        FP.OPEN := TRUE;
      END
    ELSE
          ;            %  GIVE UP...?
    IF DEBUG THEN BUGN("OPENING FOR SENDING:  ",PFILNAM);
    RECSIZ_ := FP.MAXRECSIZE;
    UNITS_  := IF FP.UNITS=VALUE(CHARACTERS) THEN 1 ELSE 6;
    RESIZE(GBUF_,(RECSIZ_ * UNITS_ + 6) DIV 6);% MAKE BUFFER CORRECT SIZ
    LEN := *-1;        % GET RID OF THE EXTRA PERIOD...
 
    NUM := LEN;
    WHILE NUM GTR 0 AND LEN GTR MAXSENDFILESIZ DO % PARE DOWN TITLE
     BEGIN
       SCAN NEWFILNAM:CP FOR NUM:LEN UNTIL ="/";
       IF NUM GTR 0 THEN
         SCAN CP:NEWFILNAM FOR LEN:NUM WHILE = "/";
     END;
    NUM := LEN;
    NEWFILNAM := FILNAM1;
    WHILE NUM GTR 0 DO
     BEGIN
       REPLACE NEWFILNAM:NEWFILNAM BY CP:CP FOR NUM:NUM WHILE NEQ """;
       IF NUM GTR 0 THEN
         BEGIN
           SCAN CP:CP FOR NUM:NUM WHILE = """;
           LEN := *-1;
         END;
     END;
    IF LEN GTR 8 THEN             %  WE'LL HAVE TO INSERT A DOT
      BEGIN
        LEN := *+1;
        REPLACE PFILNAM BY CP:FILNAM1 FOR 8 WITH FIXSLASHES,
           "." , CP FOR LEN-8 WITH FIXSLASHES
      END
    ELSE
      REPLACE PFILNAM BY FILNAM1 FOR LEN WITH FIXSLASHES;
    REPLACE FILNAM1 BY PFILNAM FOR LEN, NULC;
 
 
    IF DEBUG THEN
      BUGN("SENDING:  ",FILNAM1);
 
    SPACK("F",N,LEN,FILNAM1);      %  SEND AN F PACKET
    CASE RPACK(LEN,NUM,RECPKT) OF  % WHAT WAS THE REPLY?
     BEGIN
     "N":                          %  NAK, JUST STAY IN THIS STATE,
           NUM := (NUM+63) MOD64;  %  UNLESS IT'S NAK FOR NEXT PACKET
           IF N NEQ NUM THEN       %  WHICH IS JUST LIKE AN ACK FOR
             SFILE := STATE        %  THIS PACKET SO FALL THRU TO...
           ELSE GO TO ACKHERE;
 
     "Y":                          %  ACK
 ACKHERE:  IF  N = NUM THEN        %  PACKET NUMBER MATCHES
            BEGIN
             NUMTRY := 0;          %  RESET TRY COUNTER
             N := (N+1) MOD64;     %  BUMP PACKET COUNT
             IF BSIZE := BUFILL(FP,PACKET)=0
              THEN                 %  GET FIRST DATA FROM FILE, ERROR?
                SFILE := "Z"       %  YES, QUIT NOW
             ELSE                  %  A GOOD READ
              SFILE := "D";        %  CASE STATE TO D
            END
           ELSE
             SFILE := STATE;       %  WRONG ACK, STAY IN F STATE
 
     "E":                          %  ERROR PACKET RECEIVED
           PRERRPKT(RECPKT);       %  PRINT IT OUT AND
           SFILE := "A";           %  ABORT
 
     "T":                         %  RECEIVE FAILURE, STAY IN F STATE
           SFILE := STATE;
 
    ELSE:  SFILE := "A";           %  SOMETHING ELSE, JUST "ABORT"
      END CASE;
   END ELSE
     SFILE := "A";                 %  IF TOO MANY TRIES, GIVE UP
 QUIT:
  END SFILE;
 
 
 %
 %  S D A T A
 %
 %  SEND FILE DATA
 
 
REAL PROCEDURE SDATA;
 BEGIN
  LABEL ACKHERE;
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH
 
  IF (NUMTRY:=*+1 LEQ PACKETRETRY+1) THEN
  BEGIN
 
    SPACK("D",N,BSIZE,PACKET);      %  SEND A D PACKET
    CASE RPACK(LEN,NUM,RECPKT) OF  %  WHAT WAS THE REPLY?
     BEGIN
     "N":                          %  NAK, JUST STAY IN THIS STATE,
                                   %  UNLESS IT'S NAK FOR NEXT PACKET
           NUM := (NUM+63) MOD64;  %  UNLESS IT'S NAK FOR NEXT PACKET
           IF N NEQ NUM THEN       %  WHICH IS JUST LIKE AN ACK FOR
             SDATA := STATE        %  THIS PACKET SO FALL THRU TO...
           ELSE GO TO ACKHERE;
 
     "Y":                          %  ACK
 ACKHERE:   IF N = NUM THEN        %  IF WRONG ACK, FAIL
             BEGIN
              NUMTRY := 0;         %  RESET TRY COUNTER
              N := (N+1)MOD64;     %  BUMP PACKET COUNT
              IF ((BSIZE := BUFILL(FP,PACKET)) = 0)
                    THEN           %  GET DATA FROM FILE
                SDATA := "Z"       %  IF EOF SET STATE TO THAT
              ELSE SDATA := "D";   %  GOT DATA, STAY IN STATE D
             END
            ELSE
             SDATA := STATE;
 
     "E":                          %  ERROR PACKET RECEIVED
           PRERRPKT(RECPKT);       %  PRINT IT OUT AND
           SDATA := "A";           %  ABORT
 
     "T": SDATA := STATE;         %  RECEIVE FAILURE, STAY IN D
 
    ELSE:  SDATA := "A";           %  ANYTHING ELSE, "ABORT"
     END CASE;
  END ELSE
    SDATA := "A";                  %  IF TOO MANY TRIES, GIVE UP
 END SDATA;
 
 
 %
 %  S E O F
 %
 %  SEND END-OF-FILE.
 
 
REAL PROCEDURE SEOF;
 BEGIN
  LABEL ACKHERE;
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH
  IF (NUMTRY:=*+1 LEQ PACKETRETRY+1) THEN
  BEGIN
 
    SPACK("Z",N,0,PACKET);         %  SEND A "Z" PACKET
    CASE RPACK(LEN,NUM,RECPKT) OF  %  WHAT WAS THE REPLY?
     BEGIN
     "N":                          %  NAK, JUST STAY IN THIS STATE,
                                   %  UNLESS IT'S NAK FOR NEXT PACKET,
           NUM := (NUM+63) MOD64;  %  UNLESS IT'S NAK FOR NEXT PACKET
           IF N NEQ NUM THEN       %  WHICH IS JUST LIKE AN ACK FOR
             SEOF := STATE         %  THIS PACKET SO FALL THRU TO...
           ELSE GO TO ACKHERE;
 
     "Y":                          %  ACK
 ACKHERE:
           IF N = NUM THEN
             BEGIN
               NUMTRY := 0;        %  RESET TRY COUNTER
               N := (N+1) MOD64;   %  AND BUMP PACKET COUNT
               IF DEBUG THEN BUGN("CLOSING INPUT FILE:  ",PFILNAM);
               CLOSE(FP);          %  CLOSE THE INPUT FILE
                   SEOF := "B";    %  BREAK, EOT, ALL DONE
             END ELSE              %  IF WRONG ACK, HOLD OUT
               SEOF := STATE;
 
     "E":                          %  ERROR PACKET RECEIVED
           PRERRPKT(RECPKT);       %  PRINT IT OUT AND
           SEOF := "A";            %  ABORT
 
     "T": SEOF := STATE;          %  RECEIVE FAILURE, STAY IN Z
 
     ELSE: SEOF := "A";            %  SOMETHING ELSE, "ABORT"
     END CASE;
  END ELSE
    SEOF := "A";                   %  IF TOO MANY TRIES, ABORT
 END SEOF;
 
 
 %
 %  S B R E A K
 %
 %  SEND BREAK (EOT)
 
 
REAL PROCEDURE SBREAK;
 BEGIN
  LABEL ACKHERE;
  REAL NUM, LEN;   %  PACKET NUMBER, LENGTH
  IF (NUMTRY:=*+1 LEQ PACKETRETRY+1) THEN
  BEGIN
 
    SPACK("B",N,0,PACKET);         %  SEND A B PACKET
    CASE RPACK(LEN,NUM,RECPKT) OF  %  WHAT WAS THE REPLY?
     BEGIN
     "N":                          %  NAK, JUST STAY IN THIS STATE,
                                   %  UNLESS NAK FOR PREVIOUS PACKET,
           NUM := (NUM+63) MOD64;  %  UNLESS IT'S NAK FOR NEXT PACKET
           IF N NEQ NUM THEN       %  WHICH IS JUST LIKE AN ACK FOR
             SBREAK := STATE       %  THIS PACKET SO FALL THRU TO...
           ELSE
             GO TO ACKHERE;
 
     "Y":                          %  ACK
 ACKHERE:
           IF N = NUM THEN         %  IF WRONG ACK, FAIL
             BEGIN
           NUMTRY := 0;            %  RESET TRY COUNTER
           N := (N+1) MOD64;       %  AND BUMP PACKET COUNT
           SBREAK := "C";          %  CASE STATE TO COMPLETE
             END ELSE
             SBREAK := STATE;
 
     "E":                          %  ERROR PACKET RECEIVED
           PRERRPKT(RECPKT);       %  PRINT IT OUT AND
           SBREAK := "A";          %  ABORT
 
     "T": SBREAK := STATE;        %  RECEIVE FAILURE, STAY IN B
%     [KS019] NEED TO RETRY ON TIME OUT
%    "T": SBREAK := "C";          %  TIMED OUT WAITING FOR LAST ACK
 
    ELSE:  SBREAK := "A";          %  OTHER, "ABORT"
    END CASE;
  END ELSE
    SBREAK := "A";                 %  IF TOO MANY TRIES, ABORT
 END SBREAK;
 
%    MAIN LINE TO SENDSW
 
 
 
  STATE := "S";                    %  SEND INITIATE IS THE START STATE
  N := 0;                          %  INITIALIZE MESSAGE NUMBER
  GCNT_ := -1;                     %  INITIALIZE GETCHAR POINTER, ETC
  NUMTRY := 0;                     %  BUG NO TRIES YET
  DONTQUIT := TRUE;                %  INITIALIZE FOR LOOP
  REM.TIMELIMIT := IF TIMINT LSS MINTIM OR TIMINT GTR MAXTIM THEN
                          MYTIME ELSE TIMINT;
 
  WHILE DONTQUIT DO                %  DO THIS AS LONG AS NECESSARY
   BEGIN
     IF DEBUG THEN BUGC("SENDSW STATE:  ",STATE);
     CASE STATE OF
      BEGIN
      "S": STATE := SINIT;         %  SEND-INIT
      "F": STATE := SFILE;         %  SEND-FILE
      "D": STATE := SDATA;         %  SEND-DATA
      "Z": STATE := SEOF;          %  SEND-END-OF-FILE
      "B": STATE := SBREAK;        %  SEND-BREAK
      "C": SENDSW := TRUE;         %  COMPLETE
           DONTQUIT:=FALSE;        %  LET'S QUIT
      "A": SENDSW := FALSE;        %  "ABORT"
           DONTQUIT:=FALSE;        %  LET'S QUIT
     ELSE: SENDSW := FALSE;        %  UNKNOWN, FAIL
           DONTQUIT:=FALSE;        %  LET'S QUIT
      END CASE;
   END WHILE;
   REM.TIMELIMIT := 0;          %  DISABLE REMOTE INPUT TIMELIMIT
 END SENDSW;
 
 $ENDSEGMENT
 
 
 %
 %  R E C S W
 %
 %  THIS IS THE STATE TABLE SWITCHER FOR RECEIVING FILES.
 
 
 $BEGINSEGMENT
 
BOOLEAN PROCEDURE RECSW(ISTATE);   % [1.017]
  REAL ISTATE;                     % [1.017]
 BEGIN
  BOOLEAN DONTQUIT;
  FILE FP(KIND=DISK,MYUSE=OUT,     %  FILE POINTER FOR CURRENT DISK FILE
          INTMODE=ASCII,EXTMODE=EBCDIC,UNITS=FILEUNITS,
          TRANSLATE=FULLTRANS,OUTPUTTABLE=ASCIITOEBCDIC,
          MAXRECSIZE=FILERECSIZE,BLOCKSIZE=FILEBLOCKSIZE,
          AREASIZE=FILEBLOCKSIZE DIV FILERECSIZE * 10);
 
 
 
 %
 %  R I N I T
 %
 %  RECEIVE INITIALIZATION
 
 
REAL PROCEDURE RINIT;
 BEGIN
  REAL LEN, NUM;                   %  PACKET LENGTH, NUMBER
 
  IF (NUMTRY:=*+1 LEQ INITRETRY) THEN
  BEGIN
 
    CASE IF SERVER AND NUMTRY=1 THEN "S"
         ELSE
           RPACK(LEN,NUM,PACKET) OF%  GET A PACKET
     BEGIN
     "S":                          %  SEND-INIT
           CALL1 := TRUE;          % [1.017] RPAR CALLED FIRST
           RPAR(LEN,PACKET);       %  GET THE OTHER SIDE'S INIT DATA
           SPAR(LEN,PACKET);       %  FILL UP PACKET WITH MY INIT
           SPACK("Y",N,LEN,PACKET);%  ACK WITH MY PARAMETERS
           OLDTRY := NUMTRY;       %  SAVE OLD TRY COUNT
           NUMTRY := 0;            %  START A NEW COUNTER
           N := (N+1) MOD64;       %  BUMP PACKET NUMBER, MOD 64
           RINIT := "F";           %  ENTER FILE-RECEIVE STATE
 
     "E":                          %  ERROR PACKET RECEIVED
           PRERRPKT(PACKET);       %  PRINT IT OUT AND
           RINIT := "A";           %  ABORT
 
     "T":                         %  DIDN'T GET PACKET
           SPACK("N",N,0,NULLDATA);%  RETURN A NAK
           RINIT := STATE;         %  KEEP TRYING
 
    ELSE:  RINIT := "A";           %  SOME OTHER PACKET TYPE, "ABORT"
   END CASE;
END ELSE
  RINIT := "A";                    %  SOME OTHER PACKET TYPE, ABORT
 END RINIT;
 
 
 %
 %  R F I L E
 %
 %  RECEIVE FILE HEADER
 
 
REAL PROCEDURE RFILE;
 BEGIN
  LABEL QUIT;
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH
  ARRAY FILNAM1[0:MAXPACKWDS];     %  HOLDS THE CONVERTED FILE NAME
  POINTER NEWFILNAM;
 
  IF (NUMTRY:=*+1 LEQ PACKETRETRY+1) THEN
  BEGIN
 
    CASE RPACK(LEN,NUM,PACKET) OF%  GET A PACKET
     BEGIN
     "S":                          %  SEND-INIT, MAYBE OUR ACK LOST
          IF OLDTRY := *+1 LEQ PACKETRETRY+1 THEN
          BEGIN
           IF NUM = (N+63) MOD64
                THEN               %  PREVIOUS PACKET, MOD 64 ?
             BEGIN                 %  YES, ACK IT AGAIN WITH
               CALL1 := FALSE;     % [1.017] RPAR WAS ALREADY CALLED
               SPAR(LEN,PACKET);       %  OUR SEND-INIT PARAMETERS
               SPACK("Y",NUM,LEN,PACKET); % [1.019] FIX LENGTH PARAMETER
               NUMTRY := 0;        %  RESET TRY COUNTER
               RFILE := STATE;     %  STAY IN THIS STATE
             END
           ELSE RFILE := "A";      %  NOT PREVIOUS PACKET, "ABORT"
          END ELSE
            RFILE := "A";
 
     "Z":                          %  END-OF-FILE
          IF (OLDTRY := *+1 LEQ PACKETRETRY+1) THEN
           BEGIN
             IF NUM = (N+63) MOD64
              THEN                 %  PREVIOUS PACKET, MOD 64 ?
               BEGIN               %  YES, ACK IT AGAIN.
                SPACK("Y",NUM,0,NULLDATA);
                NUMTRY := 0;
                RFILE := STATE;    %  STAY IN THIS STATE
               END
             ELSE RFILE := "A";    %  NOT PREVIOUS PACKET, "ABORT"
           END ELSE
             RFILE := "A";         %  ABORT IT
 
     "F":                          %  FILE HEADER (JUST WHAT WE WANT)
          IF NUM = N THEN          %  THE PACKET NUMBER MUST BE RIGHT
            BEGIN
             IF LEN LEQ 17 THEN
             REPLACE PFILNAM BY """,PACKET FOR LEN WITH TOBURROUGHS,
                   "".",NULC
             ELSE
             REPLACE PFILNAM BY """,POINTER(PACKET)+(LEN-17) FOR 17 WITH
                    TOBURROUGHS, "".",NULC;
             IF FP.OPEN THEN CLOSE(FP);
             IF KEEPFILE THEN FP.PROTECTION := VALUE(SAVE)
             ELSE             FP.PROTECTION := VALUE(TEMPORARY);
             REPLACE FILNAM1 BY PFILNAM FOR LEN+4 WITH ASCIITOEBCDIC;
             REPLACE FP.TITLE BY FILNAM1;
             IF NOT FP.PRESENT THEN%  DIDN'T OPEN THE FILE
               BEGIN
                REPLACE FBUF_ BY "CANNOT CREATE: ",PFILNAM FOR LEN,NULC;
                 ERROR(FBUF_);
                 RFILE := "A";
                 GO QUIT;
               END
             ELSE                  %  OK, GIVE MESSAGE
              IF DEBUG THEN
                BUGN("RECEIVING:  ",PFILNAM);
          RECSIZ_ := FP.MAXRECSIZE;
          UNITS_  := IF FP.UNITS=VALUE(CHARACTERS) THEN 1 ELSE 6;
          RESIZE(PBUF_,(RECSIZ_ * UNITS_ +6) DIV 6);% SET UP BUFFER SIZE
          REPLACE PP_ := POINTER(PBUF_) BY " " FOR
               PCNT_ := (RECSIZ_ * UNITS_) + 1;
          SPACK("Y",N,0,NULLDATA); %  ACKNOWLEDGE THE FILE HEADER
          OLDTRY := NUMTRY;        %  RESET TRY COUNTERS
          NUMTRY := 0;             %  ...
          N := (N+1) MOD64;        %  BUMP PACKET NUMBER, MOD 64
          RFILE := "D";            %  CASE TO DATA STATE
            END ELSE
              RFILE := "A";
 
     "B":                          %  BREAK TRANSMISSION (EOT)
          IF NUM = N THEN          %  NEED RIGHT PACKET NUMBER HERE
          BEGIN
           SPACK("Y",N,0,NULLDATA);%  BUG OK
           RFILE := "C";           %  GO TO COMPLETE STATE
          END ELSE
            RFILE := "A";
 
     "E":                          %  ERROR PACKET RECEIVED
          PRERRPKT(PACKET);        %  PRINT IT OUT AND
          RFILE := "A";            %  ABORT
 
     "T":                         %  DIDN'T GET PACKET
          SPACK("N",N,0,NULLDATA); %  RETURN A NAK
          RFILE := STATE;          %  KEEP TRYING
 
    ELSE: RFILE := "A";            %  SOME OTHER PACKET, "ABORT"
  END CASE;
    END ELSE
 RFILE := "A";                     %  ABORT IF TOO MANY TRIES
QUIT:
 END RFILE;
 
 
 %
 %  R D A T A
 %
 %  RECEIVE DATA
 
 
REAL PROCEDURE RDATA;
 BEGIN
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH
  IF NUMTRY:=*+1 LEQ PACKETRETRY+1 THEN
  BEGIN
 
    CASE RPACK(LEN,NUM,PACKET) OF%  GET PACKET
     BEGIN
     "D":                          %  GOT DATA PACKET
          IF NUM NEQ N THEN        %  RIGHT PACKET?
            BEGIN                  %  NO
             IF OLDTRY := *+1 LEQ PACKETRETRY+1 THEN
              BEGIN
               IF NUM = (N+63) MOD64
                THEN               %  ELSE CHECK PACKET NUMBER
                BEGIN              %  PREVIOUS PACKET AGAIN?
                 SPACK("Y",NUM,6,PACKET);   %  YES, RE-ACK IT
                 NUMTRY := 0;      %  RESET TRY COUNTER
                 RDATA := STATE;   %  DON'T WRITE OUT DATA!
                END
               ELSE RDATA := "A";  %  SORRY, WRONG NUMBER
              END
             ELSE RDATA := "A";
            END ELSE
           BEGIN
                                   %  GOT DATA WITH RIGHT PACKET NUMBER
             BUFEMP(FP,PACKET,LEN);%  WRITE THE DATA TO THE FILE
             SPACK("Y",N,0,NULLDATA);%  ACKNOWLEDGE THE PACKET
             OLDTRY := NUMTRY;     %  RESET THE TRY COUNTERS
             NUMTRY := 0;          %  ...
             N := (N+1) MOD64;     %  BUMP PACKET NUMBER, MOD 64
             RDATA := "D";         %  REMAIN IN DATA STATE
           END;
 
     "F":                          %  GOT A FILE HEADER
          IF OLDTRY := *+1 LEQ PACKETRETRY+1 THEN
          BEGIN
           IF NUM = (N+63) MOD64
            THEN                   %  ELSE CHECK PACKET NUMBER
            BEGIN                  %  IT WAS THE PREVIOUS ONE
              SPACK("Y",NUM,0,NULLDATA);  %  ACK IT AGAIN
              NUMTRY := 0;         %  RESET TRY COUNTER
              RDATA := STATE;      %  STAY IN DATA STATE
            END
           ELSE RDATA := "A";      %  NOT PREVIOUS PACKET, "ABORT"
          END ELSE
            RDATA := "A";          %  ABORT IT
 
     "Z":                          %  END-OF-FILE
          IF NUM = N THEN          %  MUST HAVE RIGHT PACKET NUMBER
          BEGIN
           SPACK("Y",N,0,NULLDATA);%  OK, ACK IT.
           IF PCNT_ LSS RECSIZ_*UNITS_+1 THEN
             BRD:=WRITE(FP,RECSIZ_,PBUF_);%    FLUSH THE BUFFER
           LOCK(FP,CRUNCH);        %  LOCK THE FILE
           N := (N+1) MOD64;       %  BUMP PACKET NUMBER
           RDATA := "F";           %  GO BACK TO RECEIVE FILE STATE
          END ELSE
           RDATA := "A";
 
     "E":                          %  ERROR PACKET RECEIVED
           PRERRPKT(PACKET);       %  PRINT IT OUT AND
           RDATA := "A";           %  ABORT
 
     "T":                         %  DIDN'T GET PACKET
           SPACK("N",N,0,NULLDATA);%  RETURN A NAK
           RDATA := STATE;         %  KEEP TRYING
 
    ELSE:  RDATA := "A";           %  SOME OTHER PACKET, "ABORT"
END CASE;
    END ELSE
 RDATA := "A";                     %  ABORT IF TOO MANY TRIES
 END RDATA;
 
%   MAIN LINE TO RECSW
 
 
 
  STATE := ISTATE;                 % [1.017] START STATE IS PASSED IN
  % [1.017] N := 0;                  %  INITIALIZE MESSAGE NUMBER
  NUMTRY := 0;                     %  BUG NO TRIES YET
  DONTQUIT := TRUE;                %  LOOP INITIALIZATION
  REM.TIMELIMIT := IF TIMINT LSS MINTIM OR TIMINT GTR MAXTIM THEN
                          MYTIME ELSE TIMINT;
 
  WHILE DONTQUIT DO
    BEGIN
      IF DEBUG THEN BUGC("RECSW STATE:  ",STATE);
      CASE STATE OF
       BEGIN
       "R": STATE := RINIT;        %  RECEIVE-INIT
       "F": STATE := RFILE;        %  RECEIVE-FILE
       "D": STATE := RDATA;        %  RECEIVE-DATA
       "C": RECSW := TRUE;         %  COMPLETE STATE
            DONTQUIT := FALSE;     %  LET'S QUIT
       "A": RECSW := FALSE;        %  "ABORT" STATE
            DONTQUIT := FALSE;     %  LET'S QUIT
      ELSE: RECSW := FALSE;        %  UNKNOWN STATE
            DONTQUIT := FALSE;     %  LET'S QUIT
       END CASE;
   END WHILE;
   REM.TIMELIMIT := 0;          %  DISABLE REMOTE INPUT TIMELIMIT
 END RECSW;
 
 $ENDSEGMENT
 
 
 %
 % KERMIT UTILITIES.
 %
 
 
 %
 %  S P A C K
 %
 %  SEND A PACKET
 
 
 $BEGINSEGMENT
 
PROCEDURE SPACK(TYPE,NUM,LEN,DATA);
VALUE TYPE,NUM,LEN;
REAL TYPE;
REAL NUM, LEN;
ARRAY DATA[0];
 BEGIN
  REAL I;                          %  CHARACTER LOOP COUNTER
  REAL CHKSUM;                     %  CHECKSUM
  ARRAY BUFFER[0:MAXPACKWDS+1+(PAD+5)DIV 6];  %  PACKET BUFFER
  POINTER PB,PD;                   %  BUFFER POINTERS
 
  PD := POINTER(DATA);
  REPLACE PD + LEN BY NULC;
  IF REAL(DEBUG) GTR 1 THEN        %  DISPLAY OUTGOING PACKET
   BEGIN
     BUGC("SPACK TYPE:  ",TYPE);
     BUG1("NUM:  ",NUM);
     BUG1("LEN:  ",LEN);
     IF LEN GTR 0 THEN
       BUGN("DATA:  ",DATA);
   END;
 
  IF PAD GTR 0 THEN
    REPLACE PB:PB:=POINTER(BUFFER) BY CH(PCHAR,PAD)
  ELSE
    PB := POINTER(BUFFER);
 
  REPLACE PB:PB BY CH(SOHCHAR,1),  %  PACKET MARKER, ASCII 1 (SOH)
         CH(TOCHAR(LEN+3),1),      %  SEND THE CHARACTER COUNT
           CH(TOCHAR(NUM),1),      %  PACKET NUMBER
                  CH(TYPE,1);      %  PACKET TYPE
  CHKSUM := TOCHAR(LEN+3)          %  CHECKSUM CHARACTER COUNT
          + TOCHAR(NUM)            %  CHECKSUM PACKET NUMBER
          + TYPE;                  %  CHECKSUM PACKET TYPE
 
  FOR I:=0 STEP 1 UNTIL LEN-1 DO   %  LOOP FOR ALL DATA CHARACTERS
    BEGIN
      CHKSUM := *+REAL(PD,1);      %  UPDATE CHECKSUM
      REPLACE PB:PB BY PD:PD FOR 1;% GET A CHARACTER
    END;
  CHKSUM := *.[7:8];               %  SIMULATE CHAR ARITHMETIC
  CHKSUM := * + CHKSUM.[7:2];
  CHKSUM := *.[5:6];               %  COMPUTE FINAL CHECKSUM
  IF CHKSUM=0 THEN                 %  WE MAY STRIP TRAILING BLANKS...
    BEGIN
    REPLACE PB:PB BY CH(TOCHAR(CHKSUM),1),% SO LET'S PUT IN A DUMMY
                    "?",CH(EOL,1); %  ? BEFORE ANOTHER EOL...
    LEN := *+2;                    %  WE HAVE TO WRITE OUT 2 MORE, NOW
    END
  ELSE                             %  NO NEED TO WORRY ABOUT IT
    REPLACE PB:PB BY CH(TOCHAR(CHKSUM),1);%  PUT IT IN THE PACKET
  REPLACE PB:PB BY CH(EOL,1);      %  PUT IN LINE TERMINATOR
  IF BRD := WRITE(REM[STOP],LEN+6+PAD,BUFFER) THEN
    BEGIN
      I := 3+1;                    %  TRY 3 TIMES
      WHILE BRD AND I := *-1 GTR 0 DO
        BEGIN
          IF DEBUG THEN BUGH("SPACK WRITE ERROR (HEX) = ",RD);
          WHEN(.5);                %  WAIT A HALF SECOND
          BRD := WRITE(REM,LEN+6+PAD,BUFFER);
        END;                       %  TRY THE IO AGAIN
    END;
  IF REAL(DEBUG) GTR 1 THEN
    BUG1("CSUM:  ",CHKSUM);
  WHEN(.1);                        %  ALLOW FOR TURN AROUND DELAY
 END SPACK;
 
 %
 %  R P A C K
 %
 %  READ A PACKET
 
 
REAL PROCEDURE RPACK(LEN,NUM,DATA);
REAL LEN, NUM;                    %  PACKET LENGTH, NUMBER
ARRAY DATA[0];                    %  PACKET DATA
 BEGIN
  LABEL GOTSOH,QUIT;
  REAL I;                         %  DATA CHARACTER NUMBER, LOOP EXIT
  REAL     R,                     %  CURRENT INPUT CHARACTER
        TYPE,                     %  PACKET TYPE
     CCHKSUM,                     %  OUR (COMPUTED) CHECKSUM
     RCHKSUM,                     %  CHECKSUM RECEIVED FROM OTHER HOST
     COL;                         %  COLUMN COUNTER FOR PB
  BOOLEAN DONE;                   %  ARE WE THRU?
  ARRAY BUFFER[0:49];
  POINTER PB,PD;
  DEFINE ABORT =                   %  TIMED OUT OR DEFICIENT PACKET
       BEGIN
         RPACK := "T";             %  DEFAULT TO TIMED OUT OR SHORT PACK
         IF BRD.TIMEOUTBIT THEN
           IF REAL(DEBUG) GTR 1 THEN
             BUG("TIMED OUT")
           ELSE
         ELSE
         IF BRD THEN               %  SOME OTHER ERROR
           BEGIN
             IF DEBUG THEN
               BUGH("ERROR ON READ (HEX) = ",RD);
             RPACK := 0;           %  ABORT ... NOW
           END
         ELSE                      %  NO ERROR - MUST BE A SHORT PACKET
         IF DEBUG THEN
           BEGIN
             BUG("SHORT PACKET");
             BUG1("CHARACTERS LEFT=",COL);
             IF COL GTR 0 THEN
               BUGN("WHICH ARE      :",PB);
             BUGN("BUFFER IS      :",BUFFER);
           END;
         RD := 0;                  %  RESET RESULT DESCRIPTOR
         GO QUIT;
       END#;
 
 
  DO
    BEGIN
      IF BRD := READ(REM,300,BUFFER) THEN ABORT;
      SCAN PB:PB:=POINTER(BUFFER)  %  FIND A SOH HEADER
          FOR COL:RD.LENGTHF UNTIL = MYSOH;
      IF COL := *-1 GTR 0 THEN     %  ADJUST POINTER AFTER SOH
          PB := *+1;
    END UNTIL COL GTR 0;
 
  DONE := FALSE;                  %  GOT SOH, INIT LOOP
GOTSOH:
 
  IF COL LSS 4 THEN ABORT;        %  NOT A WHOLE PACKET LEFT
  WHILE NOT DONE DO               %  LOOP TO GET A PACKET
   BEGIN
% [1.017]     IF HIBITOK THEN
% [1.017]       R := REAL(PB,1)            %  TAKE 8 BITS
% [1.017]     ELSE
       R := REAL(PB,1).[6:7];     %  HANDLE PARITY
     PB := *+1; COL := *-1;       %  BUMP THE POINTER
     IF R = MYSOH THEN GO TO GOTSOH;%  RESYNCHRONIZE IF SOH
     CCHKSUM := R;                %  START THE CHECKSUM
     LEN := UNCHAR(R)-3;          %  CHARACTER COUNT
 
% [1.017]     IF HIBITOK THEN
% [1.017]       R := REAL(PB,1)            %  TAKE 8 BITS
% [1.017]     ELSE
       R := REAL(PB,1).[6:7];     %  HANDLE PARITY
     PB := *+1; COL := *-1;       %  BUMP THE POINTER
     IF R = MYSOH THEN GO TO GOTSOH;%  RESYNCHRONIZE IF SOH
     CCHKSUM := * + R;            %  UPDATE CHECKSUM
     NUM := UNCHAR(R);            %  PACKET NUMBER
 
% [1.017]     IF HIBITOK THEN
% [1.017]       R := REAL(PB,1)            %  TAKE 8 BITS
% [1.017]     ELSE
       R := REAL(PB,1).[6:7];     %  HANDLE PARITY
     PB := *+1; COL := *-1;       %  BUMP THE POINTER
     IF R = MYSOH THEN GO TO GOTSOH;%  SYNCHRONIZE IF SOH
     CCHKSUM := * + R;            %  UPDATE CHECKSUM
     TYPE := R;                   %  PACKET TYPE
 
     IF COL LSS LEN+1 THEN ABORT; %  NOT A WHOLE PACKET LEFT
     REPLACE PD := POINTER(DATA) BY NULC FOR MAXPACKSIZ;
     FOR I := 0 STEP 1 UNTIL LEN-1 DO
       BEGIN                      %  LOOP FOR CHARACTER COUNT
% [1.017]   IF HIBITOK THEN
% [1.017]     R := REAL(PB,1)        %  TAKE 8 BITS
% [1.017]   ELSE
           R := REAL(PB,1).[6:7]; %  HANDLE PARITY
         IF R = MYSOH THEN GO GOTSOH;%  RESYNCH IF SOH
         CCHKSUM := * + R;        %  UPDATE CHECKSUM
         REPLACE PD:PD BY PB:PB FOR 1;%PUT IT IN THE DATA BUFFER
         COL := *-1;              %  BUMP THE POINTER
       END;
 
     R := REAL(PB,1);             %  TAKE 8 BITS
     PB := *+1; COL := *-1;       %  BUMP THE POINTER
     RCHKSUM := UNCHAR(R);        %  CONVERT TO NUMERIC
% [1.017] IF HIBITOK THEN           %  GET EOL CHARACTER AND TOSS IT
% [1.017]   R := REAL(PB,1)            %  TAKE 8 BITS
% [1.017] ELSE
       R := REAL(PB,1).[6:7];     %  HANDLE PARITY
     PB := *+1; COL := *-1;       %  BUMP THE POINTER
     IF R = MYSOH THEN GO TO GOTSOH;%  RESYNCHRONIZE IF SOH
     DONE := TRUE;                %  GOT CHECKSUM, DONE
   END;
 
   IF REAL(DEBUG) GTR 1 THEN      %  DISPLAY INCOMING PACKET
    BEGIN
     BUGC("RPACK TYPE:  ",TYPE);
     BUG1("NUM:  ",NUM);
     BUG1("LEN:  ",LEN);
     IF LEN GTR 0 THEN
       BUGN("DATA:  ",DATA);
    END;
                                  %  FOLD IN BITS 7:8 TO COMPUTE
   CCHKSUM := * + CCHKSUM.[7:2];  %  FINAL CHECKSUM
   CCHKSUM := *.[5:6];
 
   IF CCHKSUM NEQ RCHKSUM THEN
     RPACK := 0
   ELSE
     RPACK := TYPE;               %  ALL OK, RETURN PACKET TYPE
  IF REAL(DEBUG) GTR 1 THEN
    BEGIN
      BUG1("CCSUM:  ",CCHKSUM);
      BUG1("RCSUM:  ",RCHKSUM);
    END;
 QUIT:
   WHEN(MYPAUSE);                 %  WAIT BEFORE SENDING ACK
 END RPACK;
 
 $ENDSEGMENT
 
 
 %
 %  B U F I L L
 %
 %  GET A BUFFERFUL OF DATA FROM THE FILE THAT'S BEING SENT.
 %  CONTROL-QUOTING,  8-BIT & REPEAT COUNT PREFIXES ARE ALL
 %  HANDLED.
 
 
REAL PROCEDURE BUFILL(FID,BUFFER);
FILE FID;                        %  DISK FILE TO FILL FROM
ARRAY BUFFER[0];                 %  BUFFER
 BEGIN
  LABEL QUIT;
  REAL I,                         %  LOOP INDEX
       LASTT,                     %  PREVIOUS CHARACTER READ FROM FILE
       CNT,                       %  COUNT OF IDENTICAL PREVIOUS CHARS
       T;                         %  CHAR READ FROM FILE
  REAL T7;                        %  7-BIT VERSION OF ABOVE
  BOOLEAN BT = T,                 %  A BOOLEAN VERSION OF T
          REPTON;                 % [1.017] TRUE WHEN REPEAT IN ACTION
  POINTER PB,                     %  POINTER TO BUFFER
          PBSAVE;                 % [1.017] PTS TO SLOT FOR REPEAT COUNT
 
  REAL PROCEDURE FIXIT;        %  FIXES UP BINARY QUOTING, ETC
   BEGIN
      IF QBIN NEQ "N" THEN         % [1.019]  WE MAY USE 8-BIT STUFF
       IF BT.[7:1] THEN            % [1.019]  HIGH BIT IS ON
        BEGIN                      % [1.019]
          REPLACE PB:PB BY CH(QBIN,1);
          FIXIT := *+1;            % [1.019]
        END;                       % [1.019]
     IF T7 IN ACNTRL[0] THEN % CONTROL, QUOTE, QBIN, REPT
      BEGIN
        REPLACE PB:PB BY CH(QUOTE,1);
        IF T7 IN BCNTRL[0] THEN    %  DON'T CTL IT
          REPLACE PB:PB BY CH(T7,1)%  PUT IT OUT STRAIGHT
        ELSE                      %  IT'S 0 <= N <= 31 OR 127
        IF T7 = NL THEN            %  TREAT NL SPECIALLY
         IF NOT HIBITOK THEN       % [1.019]
          BEGIN
            REPLACE PB:PB BY CH(CTL(CR),1),
                             CH(QUOTE,1),
                             CH(CTL(T7),1);
            FIXIT := *+2;         %  WE'LL COUNT THE CHARACTER LATER
          END
         ELSE                     %  CAN'T ADD CR IF USING 8 BITS
          REPLACE PB:PB BY CH(CTL(T7),1)
        ELSE                      %  IT WASN'T A CR, ANYWAY
          REPLACE PB:PB BY CH(CTL(T7),1);
        FIXIT := *+1;             %  ADD 1 FOR THE QUOTE
      END
     ELSE                         %  NOT A CONTROL CHARACTER
      REPLACE PB:PB BY CH(T7,1);   %  PUT IT OUT NORMALLY
     FIXIT := *+1;                %  FINALLY ADD IN THE LAST ONE
  END FIXIT;
 
 
  PB := POINTER(BUFFER);
  I := 0;                         %  INIT DATA BUFFER POINTER
  WHILE (T:=GETC(FID)) NEQ EOF DO %  GET THE NEXT CHARACTER
   BEGIN
     T7 := T.[6:7];               %  GET LOW ORDER 7 BITS
 
     IF (T = LASTT)AND(REPTOK) THEN    % [1.017] CHECK FOR REPEAT
     BEGIN                             % [1.017]
       IF (I >0) THEN                  % [1.017] COUNT UNLESS FIRST CHAR
       BEGIN                           % [1.017]   IN PACKET
         CNT := *+1;                   % [1.017]
       END;                            % [1.017]
          % PROCESS CHAR NORMALLY UNTIL REPEAT THRESHOLD EXCEEDED
       IF (NOT REPTON) THEN            % [1.017]
       BEGIN                           % [1.017]
          % WHEN THRESHOLD EXCEEDED, BACKUP AND INSERT REPEAT PREFIX
         IF (CNT > REPTTHRESH) THEN    % [1.017]
         BEGIN                         % [1.017]
           IF (T7 IN ACNTRL[0]) THEN   % [1.017]
           BEGIN                       % [1.017]
             PB := *-5;                % [1.017] BACKUP FOR CTL QUOTE
             I := *-5;                 % [1.017]
           END;                        % [1.017]
           PB := *-5;                  % [1.017] BACKUP FOR CHAR ITSELF
           I := *-5;                   % [1.017]
           REPLACE PB:PB BY CH(REPT,1);% [1.017] INSERT REPT QUOTE
           I := *+1;                   % [1.017]
           PBSAVE := PB;               % [1.017] LEAVE SLOT FOR COUNT
           PB := *+1;                  % [1.017]
           I := *+1;                   % [1.017]
           REPTON := TRUE;             % [1.017] SET REPEAT FLAG
         END;                          % [1.017]
         I := *+FIXIT;                 % [1.017] INSERT CHAR WITH QUOTES
       END;                            % [1.017]
       IF (CNT > MAXREPT) THEN         % [1.017] CHECK FOR REPEAT LIMIT
       BEGIN                           % [1.017]
         REPLACE PBSAVE BY CH(TOCHAR(CNT),1); % FILL COUNT SLOT
         REPTON := FALSE;              % [1.017] RESET REPEAT FLAG
         CNT := 0;                     % [1.017]
       END;                            % [1.017]
     END                               % [1.017]
     ELSE                              % [1.017]
     BEGIN                             % [1.017]
       IF (REPTON) THEN                % [1.017] CHECK FOR END OF REPEAT
       BEGIN                           % [1.017]
         REPLACE PBSAVE BY CH(TOCHAR(CNT),1); % FILL COUNT SLOT
         REPTON := FALSE;              % [1.017]
       END;                            % [1.017]
       CNT := 1;                       % [1.017]
       I := *+FIXIT;                 % [1.017] INSERT CHAR WITH QUOTING
       LASTT := T;                     % [1.017]
     END;                              % [1.017]
 
     IF I GEQ SPSIZ-11 THEN  %  ALLOW FOR SOH,LEN,#,TYP,CHK,?,EOL,EOL,UP
       BEGIN
         BUFILL := I;             %  CHECK LENGTH
         GO TO QUIT;
       END;
   END WHILE;
  BUFILL := I;                    %  HANDLE PARTIAL BUFFER
QUIT:
 END BUFILL;
 
 
 %
 % B U F E M P
 %
 %  PUT DATA FROM AN INCOMING PACKET INTO A FILE.
 
 
PROCEDURE BUFEMP(FID,BUFFER,LEN);
VALUE LEN;
REAL LEN;
FILE FID;
ARRAY BUFFER[0];                  %  BUFFER
 BEGIN
  REAL I;                         %  COUNTER
  REAL T;                         %  CHARACTER HOLDER
  BOOLEAN HIBIT;                  %  8 BIT STUFF
  REAL CNT;                       %  REPEAT COUNT
  POINTER PB;                     %  BUFFER POINTER
 
  PB := POINTER(BUFFER);
  FOR I:=0 STEP 1 UNTIL LEN-1 DO  % LOOP THRU THE DATA FIELD
   BEGIN
    HIBIT := FALSE;               %  INITIALIZE IT
    CNT := 1;                     %  WE HAVE 1 CHARACTER AT LEAST
    T := REAL(PB,1);              %  GET CHARACTER
    PB := *+1;
    IF REPTOK THEN                % WE CAN USE REPEAT COUNTS
     IF T = MYREPT THEN           %  WE ARE REPEATING
      BEGIN
        CNT := UNCHAR(REAL(PB,1));%  GET THE COUNT
        PB := *+1; I := *+1;      %  BUMP THE POINTER
        T := REAL(PB,1);          %  GET THE NEXT CHARACTER
        PB := *+1; I := *+1;      %  BUMP THE POINTER
      END;
    IF HIBITOK THEN               %  WE CAN QUOTE 8-BIT STUFF
     IF T = MYQBIN THEN           %  WE HAVE AN 8-BIT THING
      BEGIN
        HIBIT := TRUE;            %  SET THE FLAG
        T := REAL(PB,1);          %  GET THE NEXT CHARACTER
        PB := *+1; I := *+1;      %  BUMP THE POINTER
      END;
    IF T = MYQUOTE THEN           %  WE HAVE A QUOTED THING
      BEGIN
        T := REAL(PB,1);          %  GET THE NEXT CHARACTER
        PB := *+1; I := *+1;      %  BUMP THE POINTER
        IF NOT T IN BCNTRL[0] THEN%  IT'S NOT QUOTE, QBIN OR REPT
          T := CTL(T);            %  UNCONTROLIFY IT
      END;
    IF HIBIT THEN T := * & 1[7:1];%  SET THE 8-TH BIT
    THRU CNT DO
     IF T = HT THEN               %  IS IT A TAB?
      IF EXPTABS THEN             %  WE NEED TO EXPAND IT
        THRU (TABLEN-((RECSIZ_*UNITS_-PCNT_) MOD TABLEN)) DO
               PUTC(SP,FID)       %  FILL IN WITH SPACES
      ELSE
        PUTC(T,FID)               %  JUST PUT OUT THE TAB
     ELSE                         %  IT'S NOT A TAB
     IF  T = CR THEN           % [1.017]  IT'S A CR
      IF (HIBITOK)OR(RAW) THEN             %  DON'T FIDDLE WITH IT
        PUTC(T,FID)               %  PUT OUT A CR
      ELSE                        %  IT'S PROBABLY EXTRA, SO
                                  %  JUST EAT IT!
     ELSE                         %  NOT A CR, EITHER
       PUTC(T,FID);               %  PUT IT OUT
   END FOR LOOP;
 
 END BUFEMP;
 
 
 %
 %  S P A R
 %
 %  FILL THE DATA ARRAY WITH MY SEND-INIT PARAMETERS
 %
 
 
 $BEGINSEGMENT
 
PROCEDURE SPAR(LEN,DATA);
REAL LEN;
ARRAY DATA[0];
 BEGIN
   DEFINE FORCESEGMENT=#;         % SO BEGINSEGMENT WILL WORK
  POINTER PD;                     % [1.017] TEMPORARY POINTER
  REPLACE PD:PD := POINTER(DATA) BY
        CH(TOCHAR(MYPACKSIZ),1) , %  BIGGEST PACKET I CAN RECEIVE
        CH(TOCHAR(MYTIME),1)    , %  WHEN I WANT TO BE TIMED OUT
        CH(TOCHAR(MYPAD),1)     , %  HOW MUCH PADDING I NEED
        CH(CTL(MYPCHAR),1)      , %  PADDING CHARACTER I WANT
        CH(TOCHAR(MYEOL),1)     , %  END-OF-LINE CHARACTER I WANT
        CH(MYQUOTE,1)           ; %  CONTROL-QUOTE CHARACTER I SEND
  IF CALL1 THEN                      % [1.019]
    % IF SPAR IS CALLED FIRST (BEFORE RPAR) WE CONTROL
    %  WHETHER OR NOT 8TH BIT QUOTING CAN BE DONE
    IF (BINARYON) THEN               % [1.019]
       REPLACE PD:PD BY CH(MYQBIN,1) % [1.019] REQUEST 8TH BIT QUOTING
    ELSE                             % [1.019]
       REPLACE PD:PD BY "N"          % [1.019] PREVENT 8TH BIT QUOTING
  ELSE                               % [1.019]
    % IF SPAR IS CALLED SECOND (AFTER RPAR) WE
    % RESPOND TO THE REQUEST FROM THE REMOTE KERMIT
    IF (BINARYON)AND(HIBITOK) THEN   % [1.019]
      % IF 8TH BIT QUOTING REQUESTED, ACCEPT IF WE ARE IN BINARY MODE
      IF (QBIN = "Y") THEN           % [1.019] USE OUR QBIN CHAR
      BEGIN                          % [1.019]
        REPLACE PD:PD BY CH(MYQBIN,1); % [1.019]
        QBIN := MYQBIN;              % [1.019]
      END                            % [1.019]
      ELSE                           % [1.019]
      BEGIN                          % [1.019]
        REPLACE PD:PD BY "Y";        % [1.019] ACK 8BIT QUOTE REQUEST
        MYQBIN := QBIN;              % [1.019] USE INCOMING QBIN CHAR
      END                            % [1.019]
    ELSE                             % [1.019]
    BEGIN                            % [1.019]
      % 8TH BIT QUOTING WILL NOT BE DONE
      REPLACE PD:PD BY "N";          % [1.019] NAK 8TH BIT QUOTING
      HIBITOK := FALSE;              % [1.019]
    END;                             % [1.019]
                                     % [1.019]
  REPLACE PD:PD BY
        CH(MYCHKTYPE,1)         ; % [1.017] STANDARD CHECKTYPE
  IF CALL1 THEN                             % [1.017]
  BEGIN                                     % [1.017]
     % REQUEST REPEAT CHAR PROCESSING
    REPLACE PD BY CH(MYREPT,1);             % [1.017]
    CALL1 := FALSE;                         % [1.017]
  END                                       % [1.017]
  ELSE                                      % [1.017]
  BEGIN                                     % [1.017]
     % ACKNOWLEDGE REPEAT PROCESSING IF IT WAS REQUESTED
    IF (REPTOK) THEN                        % [1.017]
      REPLACE PD BY CH(REPT,1)          % [1.017]
    ELSE                                    % [1.017]
      REPLACE PD BY CH(SP,1);               % [1.017]
    CALL1 := TRUE;                          % [1.017]
  END;                                      % [1.017]
  LEN := 9;                                 % [1.017]
  IF REAL(DEBUG) GTR 1 THEN       %  EXPAND IT ALL
    BEGIN
      BUG1("My packet size = ",MYPACKSIZ);
      BUG1("My timeout = ",MYTIME);
      BUG1("My padding = ",MYPAD);
      BUGH("My padding character = ",MYPCHAR);
      BUGH("My end of line character = ",MYEOL);
      BUGC("My quote character = ",MYQUOTE);
      BUGC("My binary quote character = ",MYQBIN);
      BUGC("My checksum type = ",MYCHKTYPE);
      BUGC("My repeat character = ",MYREPT);
      IF REPTOK THEN
        BUG("WE ARE REPEATING")
      ELSE
        BUG("NO REPEAT CHARACTER");
      IF HIBITOK THEN
        BUG("WE ARE BINARY QUOTING")
      ELSE
        BUG("NOT BINARY QUOTING");
     END;
 END SPAR;
 
 
 %   R P A R
 %
 %  GET THE OTHER HOST'S SEND-INIT PARAMETERS
 %
 
 
PROCEDURE RPAR(LEN,DATA);
REAL LEN;
ARRAY DATA[0];
 BEGIN
  POINTER PD;
  PD := POINTER(DATA);
 
 
  SPSIZ  := UNCHAR(REAL(PD,1)); %  MAXIMUM SEND PACKET SIZE
  PD := *+1;
  TIMINT := UNCHAR(REAL(PD,1)); %  WHEN I SHOULD TIME OUT
  PD := *+1;
  PAD    := UNCHAR(REAL(PD,1)); %  NUMBER OF PADS TO SEND
  PD := *+1;
  PCHAR:= CTL(REAL(PD,1));      %  PADDING CHARACTER TO SEND
  PD := *+1;
  EOL    := UNCHAR(REAL(PD,1)); %  EOL CHARACTER I MUST SEND
  PD := *+1;
  UNTABLE(ACNTRL,QUOTE);           %  TAKE IT OUT OF THE ATABLE
  UNTABLE(BCNTRL,QUOTE);           %  TAKE IT OUT OF THE BTABLE
  QUOTE  := REAL(PD,1);            %  INCOMING DATA QUOTE CHARACTER
  TABLEIT(ACNTRL,QUOTE);           %  PUT NEW ONE IN THE ATABLE
  TABLEIT(BCNTRL,QUOTE);           %  PUT NEW ONE IN THE BTABLE
     % CHECK FOR REQUEST/ACKNOWLEDGE FOR 8TH BIT QUOTING
  IF LEN := *-6 GTR 0 THEN         % [1.019]
  BEGIN                            % [1.019]
    PD := *+1;                     % [1.019] SKIP PAST QUOTE
    UNTABLE(ACNTRL,QBIN);          % [1.019] TAKE OUT OF ATABLE
    UNTABLE(BCNTRL,QBIN);          % [1.019] TAKE OUT OF BTABLE
    QBIN := REAL(PD,1);            % [1.019] INCOMING 8BIT QUOTE
    IF (CALL1) THEN                % [1.019]
    BEGIN                          % [1.019]
      % [1.019] IF 8TH BIT MODE IS ENABLED, SEE IF INCOMING QBIN
      % [1.019] CHAR REQUESTS 8TH BIT QUOTING
      IF (BINARYON)AND((PD IN QUOTECHARS)OR(PD = "Y")) THEN
      BEGIN                        % [1.019]
        HIBITOK := TRUE;           % [1.019] YES, SET OK FLAG
        IF (PD = "Y") THEN         % [1.019]
        BEGIN                      % [1.019]
          TABLEIT(ACNTRL,MYQBIN);  % [1.019] TABLE MY QBIN CHAR
          TABLEIT(BCNTRL,MYQBIN);  % [1.019]
        END                        % [1.019]
        ELSE                       % [1.019]
        BEGIN                      % [1.019]
          TABLEIT(ACNTRL,QBIN);    % [1.019] TABLE INCOMING QBIN
          TABLEIT(BCNTRL,QBIN);    % [1.019]
        END;                       % [1.019]
      END                          % [1.019]
      ELSE                         % [1.019] 8TH BIT QUOTING WILL
        HIBITOK := FALSE;           % [1.019] NOT BE DONE
    END
    ELSE                 % [1.019] CALL 2, SPAR WAS CALLED FIRST
    BEGIN                          % [1.019]
      % [1.019] IF 8TH BIT MODE IS ENABLED, SEE IF WE
      % [1.019]  GOT AN ACK TO OUR 8TH BIT QUOTE REQUEST
      IF (BINARYON)AND((QBIN = "Y")OR(QBIN = MYQBIN)) THEN
      BEGIN                        % [1.019]
        HIBITOK := TRUE;           % [1.019] WILL DO 8TH BIT QUOTING
        TABLEIT(ACNTRL,MYQBIN);    % [1.019] TABLE MY QBIN CHAR
        TABLEIT(BCNTRL,MYQBIN);    % [1.019]
      END                          % [1.019]
      ELSE                         % [1.019]
        HIBITOK := FALSE;          % [1.019] 8TH BIT QUOTING WILL
    END;                           % [1.019]  NOT BE DONE
  END;                             % [1.019]
  IF LEN := *-2 GTR 0 THEN
  BEGIN
    PD := *+2;                     % [1.017] SKIP PAST QBIN,CHKTYPE
    UNTABLE(ACNTRL,REPT);          % [1.017] TAKE IT OUT OF ATABLE
    UNTABLE(BCNTRL,REPT);          % [1.017] TAKE IT OUT OF BTABLE
    REPT := REAL(PD,1);            % [1.017] INCOMING REPEAT CHAR
    IF CALL1 THEN                  % [1.017]
    BEGIN                          % [1.017]
         % IF CHAR SENT IS A VALID QUOTE CHAR, WE ARE REPEATING
      IF (PD IN QUOTECHARS) THEN   % [1.017] VALID CHAR ?
      BEGIN                        % [1.017]
        REPTOK := TRUE;            % [1.017]
        MYREPT := REPT;            % [1.017]
        TABLEIT(ACNTRL,REPT);      % [1.017]
        TABLEIT(BCNTRL,REPT);      % [1.017]
      END                          % [1.017]
      ELSE                         % [1.017]
        REPTOK := FALSE;           % [1.017]
      CALL1 := FALSE;              % [1.017]
    END                            % [1.017]
    ELSE                           % [1.017]
    BEGIN                          % [1.017]
         % IF CHAR MATCHES CHAR WE SENT, WE ARE REPEATING
      IF (REPT = MYREPT) THEN      % [1.017]
      BEGIN                        % [1.017]
        REPTOK := TRUE;            % [1.017]
        TABLEIT(ACNTRL,REPT);      % [1.017]
        TABLEIT(BCNTRL,REPT);      % [1.017]
      END                          % [1.017]
      ELSE                         % [1.017]
        REPTOK := FALSE;           % [1.017]
      CALL1 := TRUE;               % [1.017]
    END                            % [1.017]
  END                              % [1.017]
  ELSE                                      % [1.017]
         % DEFAULT TO NO REPEAT PROCESSING
  BEGIN                                     % [1.017]
    REPTOK := FALSE;                        % [1.017]
  END;                                      % [1.017]
  IF REAL(DEBUG) GTR 1 THEN       %  EXPAND IT ALL
    BEGIN
      BUG1("Your packet size = ",SPSIZ);
      BUG1("Your timeout = ",TIMINT);
      BUG1("Your padding = ",PAD);
      BUGH("Your padding character = ",PCHAR);
      BUGH("Your end of line character = ",EOL);
      BUGC("Your quote character = ",QUOTE);
      BUGC("Your binary quote character = ",QBIN);
      BUGC("Your checksum type = ",CHKTYPE);
      BUGC("Your repeat character = ",REPT);
      IF REPTOK THEN
        BUG("WE ARE REPEATING")
      ELSE
        BUG("NO REPEAT CHARACTER");
      IF HIBITOK THEN
        BUG("WE ARE BINARY QUOTING")
      ELSE
        BUG("NOT BINARY QUOTING");
     END;
 END RPAR;
 
 
 %
 %  F L U S H I N P U T
 %
 %  DUMP ALL PENDING INPUT TO CLEAR STACKED UP NAKS.
 %
 
 
PROCEDURE FLUSHINPUT;
 BEGIN
 
   WHILE REM.CENSUS GTR 0 DO
     BRD := READ(REM);
 END FLUSHINPUT;
 
 $ENDSEGMENT
 
 
 %
 %  KERMIT PRINTING ROUTINES:
 %
 %  PRERRPKT - PRINT CONTENTS OF ERROR PACKET RECEIVED FROM REMOTE HOST
 
 
 %
 %  E R R O R
 %
 %  PRINT ERROR MESSAGE.
 %
 %  IF LOCAL, PRINT ERROR MESSAGE WITH PRINTMSG.
 %  IF REMOTE, SEND AN ERROR PACKET WITH THE MESSAGE.
 
 
 %
 %  P R E R R P K T
 %
 %  PRINT CONTENTS OF ERROR PACKET RECEIVED FROM REMOTE HOST.
 
PROCEDURE PRERRPKT(MSG);
 ARRAY MSG[0];
 BEGIN
    BUG("KERMIT ABORTING WITH FOLLOWING ERROR FROM REMOTE HOST:");
    BUGP(MSG);
 END PRERRPKT;
 
  INITIALIZE;
    ON ANYFAULT [ KPROMPT[*] : COL] , ABORTRUN;
 
  WHILE NOT BRD DO PROCESSIT;
 
 END MAIN;
 
 %
 %  O U T E R   B L O C K
 %
 %  OUTER BLOCK OF KERMIT
 
MAIN;
END.
 
   V2