 $SET NOBINDINFO                                                        00100000
 $VERSION 1.019              % [KS] 5-86                                00101000
 $SET ASCII                 %  BURROUGHS USES 8 BITS FOR ASCII          00102000
BEGIN                                                                   00103000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       00104000
%                                                                       00105000
%  K E R M I T  File Transfer Utility                                   00106000
%                                                                       00107000
%  Burroughs 7800, University of California at Davis, 1986              00108000
%    Larry Johnson, Dave Squire, Katie Stevens                          00109000
%                                                                       00110000
%                                                                       00111000
%%%%% REVISIONS                                                         00112000
%       1.019 [KS] 4-86                                                 00113000
%                  FIXED RE-TRY BUG IN SENDSW/SBREAK                    00114000
%                  OVERHAULED HELP PROCEDURES                           00115000
%                  ENABLED 8-TH BIT QUOTING                             00116000
%                  ENABLED VARIABLE START-OF-PACKET CHAR                00117000
%                  FIXED SERVER-SPAR RETRY BUG IN RECSW/RFILE           00117100
%       1.018 [KS] 3-86                                                 00118000
%                  FIXED PACKET # BUG CAUSED BY DUP VARIABLE NAMES      00119000
%       1.017 [KS] 1-86                                                 00120000
%                  ENABLED REPEAT PROCESSING                            00121000
%                  ELIMINATE BLANK RECORD ADDED TO END OF RCV FILES     00122000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     00123000
                                                                        00124000
 % SYMBOL DEFINITIONS                                                   00125000
                                                                        00126000
DEFINE MAXPACKSIZ     =  94#,   % MAXIMUM PACKET SIZE                   00127000
       MAXPACKWDS     =  15#,   % (MAXPACKSIZ+5)DIV 6-1                 00128000
       MAXSENDFILESIZ =  11#,   % LARGEST FILE NAME I SHOULD SEND       00129000
       MAXREPT        =  94#,   % LARGEST REPEAT COUNT (126-32)         00130000
       EOF            = 4"201"#,% EOF FOR BUFILL                        00131000
       NULC           = 48"00"#,% ASCII NULL CHARACTER                  00132000
       DEFSOH         =   1#,   % [1.019] START OF HEADER               00133000
%      SOHC           = 48"01"#,% SOH CHARACTER                         00134000
       ETXC           = 48"03"#,% ETX CHARACTER                         00135000
       BEL            =   7#,   % ASCII BELL                            00136000
       HT             =   9#,   % ASCII HORIZONTAL TAB                  00137000
       LF             =  10#,   % ASCII LINE FEED                       00138000
       NL             =  LF#,   % NEWLINE CHARACTER                     00139000
       CR             =  13#,   % ASCII CARRIAGE RETURN                 00140000
       SP             =  32#,   % ASCII SPACE                           00141000
       DEL            = 127#,   % ASCII DELETE (RUBOUT)                 00142000
                                                                        00143000
       REPTTHRESH     =   5#,   % CHARACTER REPEAT THRESHOLD            00144000
       DEFINITRETRY   =  20#,   % TIMES TO RETRY INITIALIZATION         00145000
       DEFPACKETRETRY =  10#,   % TIMES TO RETRY A PACKET               00146000
       TABLEN         =   8#,   % LENGTH OF A TAB IF EXPANDED           00147000
       DEFRECSIZE     =  15#,   % MAXRECSIZE IN WORDS OF RECEIVED FILE  00148000
       DEFBLOCKSIZE   = 420#,   % BLOCKSIZE IN WORDS OF RECEIVED FILE   00149000
       DEFUNITS       = VALUE(WORDS)#, % UNITS OF RECEIVED FILE         00150000
       DEFPAD         =   0#,   % DEFAULT # OF PADDING CHARACTERS       00151000
       DEFPCHAR       =   0#,   % DEFAULT PADDING CHARACTER             00152000
       DEFEOL         =  CR#,   % DEFAULT END OF LINE CHAR FOR BURROUGHS00153000
       DEFQUOTE       = "#"#,   % DEFAULT QUOTE CHARACTER               00154000
       DEFQBIN        = "&"#,   % DEFAULT BINARY QUOTE CHARACTER        00155000
       DEFCHKTYPE     = "1"#,   % DEFAULT CHECKSUM TYPE                 00156000
       DEFREPT        = "~"#,   % DEFAULT REPEAT CHARACTER              00157000
       DEFPAUSE       =   0#,   % DEFAULT PAUSE BEFORE ACK              00158000
       DEFDELAY       =   5#,   % DEFAULT DELAY FOR FIRST SEND          00159000
       DEFESCCHR      = "^"#,   % DEFAULT ESCAPE CHARACTER FOR CONNECT  00160000
       DEFTIME        =   5#,   % DEFAULT TIMEOUT INTERVAL              00161000
       MAXTIM         =  60#,   % MAXIMUM TIMEOUT INTERVAL              00162000
       MINTIM         =   2#;   % MINUMUM TIMEOUT INTERVAL              00163000
                                                                        00164000
                                                                        00165000
 % MACRO DEFINITIONS                                                    00166000
                                                                        00167000
 %                                                                      00168000
 % TOCHAR: CONVERTS A CONTROL CHARACTER TO A PRINTABLE ONE BY ADDING A S00169000
 %                                                                      00170000
 % UNCHAR: UNDOES TOCHAR.                                               00171000
 %                                                                      00172000
 % CTL:    CONVERTS BETWEEN CONTROL CHARACTERS AND PRINTABLE CHARACTERS 00173000
 %    TOGGLING THE CONTROL BIT (IE. ^A BECOMES A AND A BECOMES ^A).     00174000
                                                                        00175000
DEFINE TOCHAR(CH)     = ((CH) + 32)  #;                                 00176000
DEFINE UNCHAR(CH)     = ((CH) - 32)  #;                                 00177000
DEFINE CTL(CH)        = ((CH) & (1-(CH).[6:1])[6:1]) #;                 00178000
                                                                        00179000
                                                                        00180000
 % GLOBAL VARIABLES                                                     00181000
                                                                        00182000
REAL                                                                    00183000
        BSIZE,          % SIZE OF PRESENT DATA                          00184000
        RPSIZ,          % MAXIMUM RECEIVE PACKET SIZE                   00185000
        SPSIZ,          % MAXIMUM SEND PACKET SIZE                      00186000
        TIMINT,         % TIMEOUT FOR FOREIGN HOST ON SENDS             00187000
        PAD,            % HOW MUCH PADDING TO SEND                      00188000
        PCHAR,          % PADDING CHARACTER TO SEND                     00189000
        EOL,            % END-OF-LINE CHARACTER TO SEND                 00190000
        SOHCHAR,        % [1.019] START-OF-PACKET CHAR TO SEND          00191000
        QUOTE,          % QUOTE CHARACTER IN INCOMING DATA              00192000
        QBIN,           % BINARY QUOTE CHARACTER IN INCOMING DATA       00193000
        CHKTYPE,        % ERROR DETECTION TYPE IN INCOMING DATA         00194000
        REPT,           % REPEAT CHARACTER IN INCOMING DATA             00195000
        N,              % PACKET NUMBER                                 00196000
        NUMTRY,         % TIMES THIS PACKET RETRIED                     00197000
        OLDTRY;         % TIMES PREVIOUS PACKET RETRIED                 00198000
                                                                        00199000
BOOLEAN                                                                 00200000
        SERVER,         % MEANS WE'RE A KERMIT SERVER                   00201000
        BINARYON,       % [1.019] MEANS 8-BIT QUOTING MODE ENABLED      00202000
        HIBITOK,        % MEANS 8-BIT MODE IN ACTION                    00203000
        CALL1,          % [1.017] KEEPS TRACK OF RPAR/SPAR SEQUENCE     00204000
        REPTOK,         % [1.017] TRUE MEANS REPEAT ENCRIPTION OK       00205000
        DEBUG,          % INDICATES LEVEL OF DEBUGGING OUTPUT (0=NONE)  00206000
        EXPTABS,        % EXPAND TABS ON INPUT                          00207000
        FIXEDRECS,      % SEND FIXEDRECS LENGTH RECORDS                 00208000
        RAW,            % DONT USE NL AS RECORD SEPARATOR               00209000
        KEEPFILE;       % KEEP THE OUTPUT FILE                          00210000
                                                                        00211000
REAL                                                                    00212000
        INITRETRY,      % NUMBER OF RETRIES ON INITIALIZATION           00213000
        PACKETRETRY,    % NUMBER OF RETRIES FOR A DATA PACKET           00214000
        FILERECSIZE,    % MAXRECSIZE OF RECEIVED FILE                   00215000
        FILEBLOCKSIZE,  % BLOCKSIZE OF RECEIVED FILE                    00216000
        FILEUNITS,      % UNITS OF RECEIVED FILE                        00217000
        FILECOUNT,      % NUMBER OF FILES LEFT TO SEND                  00218000
        STATE,          % PRESENT STATE OF THE AUTOMATON                00219000
        MYPACKSIZ,      % MY MAXIMUM PACKET SIZE                        00220000
        MYTIME,         % MY TIMEOUT INTERVAL                           00221000
        MYPAD,          % MY NUMBER OF PADDING CHARACTERS               00222000
        MYPCHAR,        % MY PADDING CHARACTER                          00223000
        MYEOL,          % MY END OF LINE CHARACTER                      00224000
        MYSOH,          % [1.019] MY START-OF-PACKET CHAR               00225000
        MYQUOTE,        % MY QUOTE CHARACTER                            00226000
        MYQBIN,         % MY BINARY QUOTE CHARACTER                     00227000
        MYCHKTYPE,      % MY CHECKSUM TYPE                              00228000
        MYREPT,         % MY REPEAT CHARACTER                           00229000
        MYPAUSE,        % MY PAUSE AFTER ACK TIME                       00230000
        MYDELAY,        % MY DELAY FOR FIRST SEND TIME                  00231000
        MYESCCHR;       % MY ESCAPE CHARACTER FROM CONNECT              00232000
                                                                        00233000
                                                                        00234000
ARRAY                                                                   00235000
        FILNAM[0:MAXPACKWDS];   % TITLE OF CURRENT DISK FILE            00236000
                                                                        00237000
POINTER                                                                 00238000
        PFILNAM;        % POINTER TO FILNAM                             00239000
                                                                        00240000
ARRAY                                                                   00241000
        RECPKT[0:MAXPACKWDS],% RECEIVE PACKET BUFFER                    00242000
        PACKET[0:MAXPACKWDS];% PACKET BUFFER                            00243000
                                                                        00244000
FILE                                                                    00245000
        REM             %  FILE FOR REMOTE INPUT / OUTPUT               00246000
          (KIND=REMOTE,MYUSE=IO,UNITS=CHARACTERS,BUFFERS=1,             00247000
                 MAXRECSIZE=300,FILETYPE=3),                            00248000
        LOG             %  FILE POINTER FOR LOGFILE                     00249000
           (KIND=DISK,UNITS=CHARACTERS,MAXRECSIZE=96,BLOCKSIZE=2880,    00250000
                    PROTECTION=SAVE,NEWFILE,SAVEFACTOR=1,BUFFERS=1,     00251000
                    TITLE=8"KERMIT/LOG.");                              00252000
                                                                        00253000
TRANSLATETABLE TOUPPER(  ASCII TO ASCII,                                00254000
       "abcdefghijklmnopqrstuvwxyz" TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),   00255000
               TOLOWER(  ASCII TO ASCII,                                00256000
       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" TO "abcdefghijklmnopqrstuvwxyz"),   00257000
               TOBURROUGHS( ASCII TO ".",                               00258000
       "abcdefghijklmnopqrstuvwxyz" TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ",    00259000
       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ",    00260000
       "0123456789"                 TO "0123456789" );                  00261000
TRANSLATETABLE ASCTOEBC(                                                00262000
   47"000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F" 00263000
TO 48"00010203372D2E2F1605250B0C0D0E0F101112133C3D322618193F271C1D1E1F" 00264000
  ,47"202122232425262728292A2B2C2D2E2F303132333435363738393A3B3C3D3E3F" 00265000
TO 48"404F7F7B5B6C507D4D5D5C4E6B604B61F0F1F2F3F4F5F6F7F8F97A5E4C7E6E6F" 00266000
  ,47"404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F" 00267000
TO 48"7CC1C2C3C4C5C6C7C8C9D1D2D3D4D5D6D7D8D9E2E3E4E5E6E7E8E94AE05A5F6D" 00268000
  ,47"606162636465666768696A6B6C6D6E6F707172737475767778797A7B7C7D7E7F" 00269000
TO 48"79818283848586878889919293949596979899A2A3A4A5A6A7A8A9C06AD0A107" 00270000
            );                                                          00271000
TRANSLATETABLE FIXSLASHES( ASCII TO ASCII , "/" TO "_" );               00272000
TRUTHSET NUMBERS(   "0" OR "1" OR "2" OR "3" OR "4" OR                  00273000
                    "5" OR "6" OR "7" OR "8" OR "9");                   00274000
TRUTHSET QUOTECHARS( "!" OR 48"7F" OR "#" OR "$" OR "%" OR "&"          00275000
   OR "'" OR "(" OR ")" OR "*" OR "+" OR "," OR "-" OR "."              00276000
   OR "/" OR "0" OR NUMBERS OR ":" OR ";" OR "<" OR "=" OR ">"          00277000
   OR "`" OR "{" OR "|" OR "}" OR "~" );                                00278000
ARRAY ACNTRL[0:15],              %  TRUTHSET FOR ALL CONTROL CHARS      00279000
      BCNTRL[0:15];              %  TRUTHSET FOR JUST QUOTE,QBIN,REPT   00280000
 %     4"0000FFFFFFFF",          %  ADD IN FROM 0 THRU 31               00281000
 %     0,0,                      %  LEAVE OUT  32 THRU 95               00282000
 %     4"000000000001",          %  ADD IN BIT FOR 127                  00283000
 %     0,0,0,0                   %  ZERO OUT END (MAY NEED FOR EBCDIC)  00284000
 %  TABLE ALGORITHM:                                                    00285000
 %    BOOLEAN(TABLE[CHAR.[7:3]].[(31-CHAR.[4:5]):1]) => IN TABLE        00286000
 %                                                                      00287000
DEFINE TABLEIT(TAB,C)  = TAB[C.[7:3]].[(31-C.[4:5]):1] := 1#,           00288000
       UNTABLE(TAB,C)  = TAB[C.[7:3]].[(31-C.[4:5]):1] := 0#;           00289000
                                                                        00290000
ARRAY FBUF_[0:29],      %  USED BY FPRINT                               00291000
      EBUF_[0:15],      %  USED BY ERROR                                00292000
      TBUF_[0:15],      %  TEMPORARY BUFFER FOR DIGITS CONVERSION       00293000
      GBUF_[0:99],     %  USED BY GETC                                  00294000
      PBUF_[0:99];     %  USED BY PUTC                                  00295000
POINTER PG_,            %  POINTS TO GBUF_                              00296000
        PP_;            %  POINTS TO PBUF_                              00297000
REAL RD,                %  RESULT DESCRIPTOR FOR EVERYBODY              00298000
     GCNT_,             %  NUMBER OF CHARACTERS IN GBUF_                00299000
     PCNT_,             %  NUMBER OF CHARACTERS IN PBUF_                00300000
     RECSIZ_,           %  MAXRECSIZE OF FP                             00301000
     UNITS_,            %  CHARACTERS PER "UNIT" OF FP                  00302000
     HUH_;              %  SILLY LITTLE FILLER                          00303000
BOOLEAN BRD = RD;       %  BOOLEAN RD                                   00304000
DEFINE                  %  SOME BURROUGHS FIELD DEFINES                 00305000
    LENGTHF = [47:20]#, %  CHAR. COUNT RETURNED FROM RESLT. DESCR.      00306000
    EOFBIT  = [ 9: 1]#, %  EOF ON I/O FROM RESLT. DESCR.                00307000
    BRKBIT  = [13: 1]#, %  BREAK ON I/O FROM RESLT. DESCR.              00308000
 TIMEOUTBIT = [15: 1]#, %  TIMEOUT ON I/O FROM RESLT. DESCR.            00309000
    ERRORF  = [16:17]#, %  THE WHOLE ERROR FIELD                        00310000
    MOD64   = .[5:6]#;  %  N MOD 64 == N.[5:6]                          00311000
                                                                        00312000
DEFINE                                                                  00313000
    INDENT   = TRUE#,   % BOOLEAN CONSTANTS                             00314000
    NOINDENT = FALSE#;                                                  00315000
                                                                        00316000
ARRAY NULLDATA[0:0];                                                    00317000
                                                                        00318000
DEFINE CH(NUMBER,N)  = (NUMBER).[ 7:48] FOR N#,% TO USE NUMBER AS A CHAR00319000
      SAY(STR)       =                                                  00320000
        BEGIN                                                           00321000
         REPLACE POINTER(FBUF_) BY STR;                                 00322000
    IF SERVER THEN                                                 00323000
           ERROR(FBUF_)                                                 00324000
         ELSE                                                           00325000
           BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);                       00326000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00327000
        END#,                                                           00328000
      SAY1(STR,NUMBER)=                                                 00329000
       BEGIN                                                            00330000
         REPLACE TBUF_ BY 8"-" FOR REAL(NUMBER LSS 0),                  00331000
             NUMBER FOR * DIGITS,8" " FOR 12;                           00332000
         REPLACE POINTER(FBUF_) BY STR,TBUF_ FOR 12                     00333000
             WITH EBCDICTOASCII;                                        00334000
         IF SERVER THEN                                                 00335000
           ERROR(FBUF_)                                                 00336000
         ELSE                                                           00337000
           BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);                       00338000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00339000
       END#,                                                            00340000
      SAYC(STR,NUMBER)=                                                 00341000
       BEGIN                                                            00342000
          IF TBUF_[0] := NUMBER LSS SP THEN                             00343000
            REPLACE POINTER(FBUF_) BY STR,"CTRL-",CH(NUMBER+64,1),      00344000
             " (HEX ",POINTER(TBUF_,4)+10 FOR 2 WITH HEXTOASCII,")"     00345000
          ELSE                                                          00346000
           REPLACE POINTER(FBUF_) BY STR, CH(NUMBER,1), " (HEX ",       00347000
              POINTER(TBUF_,4)+10 FOR 2 WITH HEXTOASCII,")";            00348000
         IF SERVER THEN                                                 00349000
           ERROR(FBUF_)                                                 00350000
         ELSE                                                           00351000
           BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);                       00352000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00353000
       END#,                                                            00354000
      SAYN(STR,PTR)   =                                                 00355000
       BEGIN                                                            00356000
         REPLACE POINTER(FBUF_) BY STR,                                 00357000
              PTR FOR MAXPACKSIZ UNTIL = NULC;                          00358000
         IF SERVER THEN                                                 00359000
           ERROR(FBUF_)                                                 00360000
         ELSE                                                           00361000
           BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);                       00362000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00363000
       END#,                                                            00364000
      SAYP(PTR,WHITESPACE)   =                                          00365000
       BEGIN                                                            00366000
         IF WHITESPACE THEN                                             00367000
            REPLACE POINTER(FBUF_) BY "   ",                            00368000
                                      PTR FOR MAXPACKSIZ-3 WHILE GEQ " "00369000
         ELSE                                                           00370000
            REPLACE POINTER(FBUF_) BY PTR FOR MAXPACKSIZ WHILE GEQ " "; 00371000
         IF SERVER THEN                                                 00372000
           ERROR(FBUF_)                                                 00373000
         ELSE                                                           00374000
           BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);                       00375000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00376000
       END#,                                                            00377000
                                                                        00378000
      SAYQ(STR)      =                                                  00379000
       BEGIN                                                            00380000
         REPLACE POINTER(FBUF_) BY " " FOR COL_BASE+12-COL_OK_TIL,      00381000
                                   "?";                                 00382000
         BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);                         00383000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00384000
         REPLACE POINTER(FBUF_) BY "  missing or invalid ",             00385000
                                   STR,                                 00386000
                                   " parameter";                        00387000
         BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);                         00388000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00389000
       END#,                                                            00390000
                                                                        00391000
      SAYQOPT(STR)      =                                               00392000
       BEGIN                                                            00393000
         COL_OK_TIL := COL_BASE + 12 - COL_OK_TIL;                      00394000
         REPLACE POINTER(FBUF_) BY " " FOR COL_OK_TIL,                  00395000
                                   "?";                                 00396000
         BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);                         00397000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00398000
         REPLACE POINTER(FBUF_) BY "  missing or invalid ",             00399000
                                   STR,                                 00400000
                                   " parameter - options are:";         00401000
         BRD := WRITE(REM,MAXPACKSIZ,FBUF_[*]);                         00402000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00403000
       END#,                                                            00404000
                                                                        00405000
                                                                        00406000
      BUG(STR)       =                                                  00407000
        BEGIN                                                           00408000
          REPLACE POINTER(FBUF_) BY STR;                                00409000
          BRD := WRITE(LOG,96,FBUF_[*]);                                00410000
          REPLACE FBUF_ BY " " FOR 16 WORDS;                            00411000
        END#,                                                           00412000
      BUG1(STR,NUMBER)=                                                 00413000
       BEGIN                                                            00414000
         REPLACE TBUF_ BY 8"-" FOR REAL(NUMBER LSS 0),                  00415000
             NUMBER FOR * DIGITS,8" " FOR 12;                           00416000
         REPLACE POINTER(FBUF_) BY STR,TBUF_ FOR 12                     00417000
             WITH EBCDICTOASCII;                                        00418000
         BRD := WRITE(LOG,96,FBUF_[*]);                                 00419000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00420000
       END#,                                                            00421000
      BUGH(STR,NUMBER)=                                                 00422000
       BEGIN                                                            00423000
         TBUF_[0] := NUMBER;                                            00424000
         REPLACE POINTER(FBUF_) BY STR,                                 00425000
              POINTER(TBUF_,4) FOR 12 WITH HEXTOASCII;                  00426000
         BRD := WRITE(LOG,96,FBUF_[*]);                                 00427000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00428000
       END#,                                                            00429000
      BUGC(STR,NUMBER)=                                                 00430000
       BEGIN                                                            00431000
          IF TBUF_[0] := NUMBER LSS SP THEN                             00432000
            REPLACE POINTER(FBUF_) BY STR,"CTRL-",CH(CTL(NUMBER),1)     00433000
          ELSE                                                          00434000
           REPLACE POINTER(FBUF_) BY STR, CH(NUMBER,1);                 00435000
         BRD := WRITE(LOG,96,FBUF_[*]);                                 00436000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00437000
       END#,                                                            00438000
      BUGN(STR,PTR)   =                                                 00439000
       BEGIN                                                            00440000
         REPLACE POINTER(FBUF_) BY STR,                                 00441000
              PTR FOR MAXPACKSIZ UNTIL = NULC;                          00442000
         BRD := WRITE(LOG,96,FBUF_[*]);                                 00443000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00444000
       END#,                                                            00445000
      BUGP(PTR)   =                                                     00446000
       BEGIN                                                            00447000
         REPLACE POINTER(FBUF_) BY PTR FOR MAXPACKSIZ UNTIL = NULC;     00448000
         BRD := WRITE(LOG,96,FBUF_[*]);                                 00449000
         REPLACE FBUF_ BY " " FOR 16 WORDS;                             00450000
       END#;                                                            00451000
                                                                        00452000
DEFINE ERROR(ARA) =                                                     00453000
      BEGIN                                                             00454000
        REPLACE EBUF_ BY ARA FOR HUH_:MAXPACKSIZ-5 WHILE GEQ " ";       00455000
        HUH_ := MAXPACKSIZ - HUH_ - 5;                                  00456000
        REPLACE FBUF_ BY " " FOR 16 WORDS; % SO DEBUGGING IN SPACK IS OK00457000
        SPACK("E",N:=(N+1) MOD64,HUH_,EBUF_);                           00458000
        REPLACE EBUF_ BY " " FOR 16 WORDS;                              00459000
      END#;                                                             00460000
REAL PROCEDURE GETC(FID);                                               00461000
 FILE FID;                                                              00462000
 BEGIN                                                                  00463000
   POINTER P,Q;                                                         00464000
   REAL R;                                                              00465000
                                                                        00466000
   IF GCNT_ LEQ 0 THEN                                                  00467000
     IF BRD := READ(FID,RECSIZ_,GBUF_) THEN                             00468000
       GETC := EOF                                                      00469000
     ELSE                                                               00470000
      BEGIN                                                             00471000
        PG_ := POINTER(GBUF_);     %  INITIALIZE POINTER                00472000
        GCNT_ := RECSIZ_ * UNITS_; %  AND COUNT                         00473000
        IF FIXEDRECS THEN                                               00474000
         IF NOT RAW THEN            % [1.019]                           00475000
          REPLACE PG_+GCNT_ BY CH(NL,1)%  ADD IN A NL AT END            00476000
         ELSE                                                           00477000
          GCNT_ := *-1              % [1.019] NO RECORD TERMINATOR      00478000
        ELSE                                                            00479000
         BEGIN                                                          00480000
           Q := PG_;                                                    00481000
           DO BEGIN                                                     00482000
             SCAN P:Q FOR R:GCNT_ UNTIL LEQ " ";                        00483000
             SCAN Q:P FOR GCNT_:R WHILE LEQ " ";                        00484000
           END UNTIL GCNT_ LEQ 0;                                       00485000
           REPLACE P BY CH(NL,1);                                       00486000
           GCNT_ := RECSIZ_ * UNITS_ - R;                               00487000
         END;                                                           00488000
        GETC := REAL(PG_,1);       %  GET FIRST CHAR                    00489000
        PG_ := *+1;                %  BUMP PTR BUT COUNT IS STILL OK    00490000
      END                          %  SINCE WE ADDED THE NL EARLIER     00491000
   ELSE                                                                 00492000
    BEGIN                                                               00493000
      GCNT_ := *-1;                                                     00494000
      GETC := REAL(PG_,1);                                              00495000
      PG_ := *+1;                                                       00496000
    END;                                                                00497000
 END GETC;                                                              00498000
                                                                        00499000
REAL PROCEDURE PUTC(C,FID);                                             00500000
 VALUE C;                                                               00501000
 REAL C;                                                                00502000
 FILE FID;                                                              00503000
 BEGIN                                                                  00504000
                                                                        00505000
   IF C = NL AND NOT RAW THEN                                           00506000
     PCNT_ := -1                                                        00507000
   ELSE                                                                 00508000
     REPLACE PP_:PP_ BY CH(C,1);                                        00509000
   IF PCNT_ := *-1 LEQ 0 THEN                                           00510000
     BEGIN                                                              00511000
       BRD := WRITE(FID,RECSIZ_,PBUF_);                                 00512000
       REPLACE PP_:=POINTER(PBUF_) BY " " FOR PCNT_:=RECSIZ_*UNITS_+1;  00513000
       IF C NEQ NL OR RAW THEN      %  WE JUST LOST A CHARACTER         00514000
         BEGIN                      %  LET'S GET IT BACK                00515000
           REPLACE PP_:PP_ BY CH(C,1);                                  00516000
           PCNT_ := *-1;            %  AND DECREMENT COUNTER            00517000
         END;                                                           00518000
     END;                                                               00519000
   PUTC := C;                                                           00520000
 END PUTC;                                                              00521000
                                                                        00522000
                                                                        00523000
 %                                                                      00524000
 %  M A I N                                                             00525000
 %                                                                      00526000
 %  MAIN ROUTINE - PARSE COMMAND AND OPTIONS, SET UP THE                00527000
 %  TTY LINES, AND DISPATCH TO THE APPROPRIATE ROUTINE.                 00528000
                                                                        00529000
                                                                        00530000
PROCEDURE MAIN;                                                         00531000
 BEGIN                                                                  00532000
  POINTER CP;                      %  CHAR POINTER                      00533000
  REAL COL;                        %  COLUMN COUNTER FOR SCANNER        00534000
  REAL COL_BASE;                   % BEGINNING COLUMN COUNT FOR SCANNER 00535000
  REAL COL_OK_TIL;                 %  COLUMN COUNTER FOR PREVIOUS SCAN  00536000
  ARRAY NEXTSEND[0:MAXPACKWDS];    %  NEXT FILE(S) TO SEND              00537000
  BOOLEAN MORETOSEND;              %  SOMETHING IN NEXTSEND             00538000
  EBCDIC ARRAY MACHINE[0:3];       %  WHAT MACHINE (7800, 6700, ETC)    00539000
  ARRAY KPROMPT[0:15];             %  THE KERMIT PROMPT                 00540000
  ARRAY REFERENCE SPECARA[0];      %  CURRENT COMMAND ARRAY             00541000
                                                                        00542000
                                                                        00543000
  VALUE ARRAY SPECIAL(             %  ALL THE COMMANDS                  00544000
    48"01" "?    "  ,              %  ? FOR HELP                        00545000
    48"04" "EXIT " ,                                                    00546000
    48"04" "HELP " ,                                                    00547000
    48"04" "QUIT " ,                                                    00548000
    48"07" "RECEIVE    " ,                                              00549000
    48"04" "SEND " ,                                                    00550000
    48"06" "SERVER     " ,                                              00551000
    48"03" "SET  " ,                                                    00552000
    48"04" "SHOW " ,                                                    00553000
    48"06" "STATUS     " ,                                              00554000
              0);                                                       00555000
                                                                        00556000
%    DEFINES FOR COMMANDS                                               00557000
                                                                        00558000
  DEFINE                                                                00559000
    QMARKV              =   0#,                                         00560000
    EXITV               =   1#,                                         00561000
    HELPV               =   2#,                                         00562000
    QUITV               =   3#,                                         00563000
    RECEIVEV            =   4#,                                         00564000
    SENDV               =   6#,                                         00565000
    SERVERV             =   7#,                                         00566000
    SETV                =   9#,                                         00567000
    SHOWV               =  10#,                                         00568000
    STATUSV             =  11#,                                         00569000
    QUESTIONV           =  -98#,                                        00570000
    INVALIDV            =  -99#,                                        00571000
    NOERRORV            =  101#;                                        00572000
                                                                        00573000
  VALUE ARRAY SPECSET(             %  ALL THE SPECIAL 'SET' COMMANDS    00574000
    48"01" "?    "  ,                                                   00575000
    48"09" "DEBUGGING  " ,                                              00576000
    48"05" "DELAY" ,                                                    00577000
    48"04" "FILE " ,                                                    00578000
    48"0A" "INCOMPLETE " ,                                              00579000
    48"05" "RETRY" ,                                                    00580000
    48"07" "RECEIVE    " ,                                              00581000
    48"04" "SEND " ,                                                    00582000
    48"06" "BINARY     " ,                                              00583000
              0);                                                       00584000
                                                                        00585000
%    DEFINES FOR SET COMMAND                                            00586000
                                                                        00587000
  DEFINE                                                                00588000
%   QMARKV              =   0#,                                         00589000
    DEBUGV              =   1#,                                         00590000
    DELAYV              =   3#,                                         00591000
    FILEV               =   4#,                                         00592000
    INCOMPLETEV         =   5#,                                         00593000
    RETRYV              =   7#,                                         00594000
    SETRECEIVEV         =   8#,                                         00595000
    SETSENDV            =  10#,                                         00596000
    BINARYV             =  11#;                                         00597000
                                                                        00598000
  VALUE ARRAY SPECFILE(            %  FOR SET FILE COMMANDS             00599000
    48"01" "?    " ,                                                    00600000
    48"0A" "BLOCK-SIZE " ,                                              00601000
    48"0B" "EXPAND-TABS" ,                                              00602000
    48"05" "FIXED" ,                                                    00603000
    48"03" "RAW  " ,                                                    00604000
    48"0B" "RECORD-SIZE" ,                                              00605000
    48"05" "UNITS" ,                                                    00606000
              0);                                                       00607000
                                                                        00608000
%         DEFINES FOR SET FILE COMMANDS                                 00609000
  DEFINE                                                                00610000
%   QMARK                   =  0#,                                      00611000
    BLOCKSIZEV              =  1#,                                      00612000
    EXPTABSV                =  3#,                                      00613000
    FIXEDV                  =  5#,                                      00614000
    RAWV                    =  6#,                                      00615000
    RECORDSIZEV             =  7#,                                      00616000
    UNITSV                  =  9#;                                      00617000
                                                                        00618000
  VALUE ARRAY SPECABORT(           %  FOR SET ABORTED-FILE              00619000
    48"01" "?    " ,                                                    00620000
    48"07" "DISCARD    " ,                                              00621000
    48"04" "KEEP " ,                                                    00622000
              0);                                                       00623000
                                                                        00624000
%    DEFINES FOR SPECABORT                                              00625000
                                                                        00626000
  DEFINE                                                                00627000
%   QMARKV              =   0#,                                         00628000
    DISCARDV            =   1#,                                         00629000
    KEEPV               =   3#;                                         00630000
                                                                        00631000
  VALUE ARRAY SPECDEBUG(           %  FOR SET DEBUGGING                 00632000
    48"01" "?    " ,                                                    00633000
    48"06" "STATES     " ,                                              00634000
    48"07" "PACKETS    " ,                                              00635000
    48"08" "LOG-FILE   " ,                                              00636000
    48"03" "OFF  ",                                                     00637000
              0);                                                       00638000
                                                                        00639000
%    DEFINES FOR SPECDEBUG                                              00640000
                                                                        00641000
  DEFINE                                                                00642000
%   QMARKV              =   0#,                                         00643000
    STATESV             =   1#,                                         00644000
    PACKETSV            =   3#,                                         00645000
    LOGFILEV            =   5#,                                         00646000
    DOFFV               =   7#;                                         00647000
                                                                        00648000
  VALUE ARRAY SPECRETRY(           %  FOR SET RETRY                     00649000
    48"01" "?    " ,                                                    00650000
    48"12" "INITIAL-CONNECTION     " ,                                  00651000
    48"07" "PACKETS    " ,                                              00652000
              0);                                                       00653000
                                                                        00654000
%    DEFINES FOR SPECRETRY                                              00655000
                                                                        00656000
  DEFINE                                                                00657000
%   QMARKV              =   0#,                                         00658000
    INITCONNV           =   1#,                                         00659000
    RETRYPACKETSV       =   5#;                                         00660000
                                                                        00661000
  VALUE ARRAY SPECONOFF(           %  FOR ON/OFF                        00662000
    48"01" "?    " ,                                                    00663000
    48"02" "ON   " ,                                                    00664000
    48"03" "OFF  " ,                                                    00665000
              0);                                                       00666000
                                                                        00667000
%    DEFINES FOR SPECONOFF                                              00668000
                                                                        00669000
  DEFINE                                                                00670000
%   QMARKV              =   0#,                                         00671000
    ONV                 =   1#,                                         00672000
    OFFV                =   2#;                                         00673000
                                                                        00674000
  VALUE ARRAY SPECRECEIVE(         %  FOR SET RECEIVE, SET SEND         00675000
    48"01" "?    " ,                                                    00676000
    48"0B" "END-OF-LINE" ,                                              00677000
    48"0D" "PACKET-LENGTH    " ,                                        00678000
    48"07" "PADDING    " ,                                              00679000
    48"07" "PADCHAR    " ,                                              00680000
    48"05" "PAUSE" ,                                                    00681000
    48"05" "QUOTE" ,                                                    00682000
    48"0F" "START-OF-PACKET  " ,                                        00683000
    48"07" "TIMEOUT    " ,                                              00684000
              0);                                                       00685000
                                                                        00686000
%    DEFINES FOR SPECRECEIVE                                            00687000
                                                                        00688000
  DEFINE                                                                00689000
%   QMARKV              =   0#,                                         00690000
    EOLV                =   1#,                                         00691000
    LENV                =   3#,                                         00692000
    PADV                =   6#,                                         00693000
    PCHARV              =   8#,                                         00694000
    PAUSEV              =  10#,                                         00695000
    QUOTEV              =  11#,                                         00696000
    STARTOFPACKV        =  12#,                                         00697000
    TIMEOUTV            =  15#;                                         00698000
                                                                        00699000
  VALUE ARRAY SPECUNITS(           %  FOR SET RECEIVE UNITS             00700000
    48"01" "?    ",                                                     00701000
    48"05" "WORDS",                                                     00702000
    48"0A" "CHARACTERS ",                                               00703000
              0);                                                       00704000
                                                                        00705000
%    DEFINES FOR SPECUNITS                                              00706000
                                                                        00707000
  DEFINE                                                                00708000
%   QMARKV              =   0#,                                         00709000
    UWORDSV             =   1#,                                         00710000
    UCHARACTERSV        =   2#;                                         00711000
                                                                        00712000
  VALUE ARRAY SPECSHOW(            % FOR SHOW SEND/RECEIVE              00713000
    48"04" "SEND " ,                                                    00714000
            0);                                                         00715000
                                                                        00716000
  DEFINE                                                                00717000
    SHOSENDV            =   0#;                                         00718000
                                                                        00719000
                                                                        00720000
                                                                        00721000
  VALUE ARRAY PLAINHELP(           %  GLOBAL HELP STUFF                 00722000
    48"0D" "EXIT to CANDE    ",                                         00723000
    48"1B" "HELP by giving this message  ",                             00724000
    48"10" "QUIT (like EXIT) ",                                         00725000
    48"16" "RECEIVE file from host ",                                   00726000
    48"11" "SEND file to host",                                         00727000
    48"20" "SERVER   make me a Kermit Server   ",                       00728000
    48"0F" "SET a parameter  ",                                         00729000
    48"0D" "SHOW settings    ",                                         00730000
    48"12" "STATUS (like SHOW)     ",                                   00731000
              0),                                                       00732000
              SETHELP(             %  SET HELP STUFF                    00733000
    48"20" "   BINARY (do 8th bit transfers)   " ,                      00734000
    48"19" "   DEBUGGING level option    ",                             00735000
    48"1F" "   DELAY seconds for first SEND    ",                       00736000
    48"11" "   FILE parameter" ,                                        00737000
    48"19" "   INCOMPLETE disposition    " ,                            00738000
    48"0E" "   RETRY count   ",                                         00739000
    48"14" "   RECEIVE parameter   ",                                   00740000
    48"11" "   SEND parameter",                                         00741000
              0),                                                       00742000
              SETFILEHELP(         % SET FILE  HELP STUFF               00743000
    48"14" "   BLOCK-SIZE length   ",                                   00744000
    48"17" "   EXPAND-TABS on input",                                   00745000
    48"32" "   FIXED (send blanks found at the end of records)" ,       00746000
    48"2A" "   RAW (without any line delimiting chars)     ",           00747000
    48"15" "   RECORD-SIZE length  ",                                   00748000
    48"1E" "   UNITS (words or characters)    ",                        00749000
              0),                                                       00750000
              SENDHELP(            %  SET RECEIVE/SEND HELP             00751000
    48"1C" "   END-OF-LINE (number 0-31) ",                             00752000
    48"17" "   PACKET-LENGTH length",                                   00753000
    48"20" "   PADDING  (number of PADCHARS)   ",                       00754000
    48"19" "   PADCHAR  (number 0-31)    ",                             00755000
    48"1B" "   PAUSE seconds before ACK  ",                             00756000
    48"12" "   QUOTE character     ",                                   00757000
    48"20" "   START-OF-PACKET (number 0-31)   " ,                      00758000
    48"15" "   TIMEOUT in seconds  ",                                   00759000
              0),                                                       00760000
              UNITSHELP(           %  SET RECEIVE UNITS HELP            00761000
    48"08" "   WORDS   ",                                               00762000
    48"0D" "   CHARACTERS    ",                                         00763000
              0),                                                       00764000
              ABORTHELP(           %  SET ABORTED-FILE HELP             00765000
    48"1C" "   DISCARD the file on abort ",                             00766000
    48"19" "   KEEP the file on abort    ",                             00767000
              0),                                                       00768000
              DEBUGHELP(           %  SET DEBUGGING HELP                00769000
    48"1E" "   STATES - flag state changes     ",                       00770000
    48"19" "   PACKETS- flag all data    ",                             00771000
    48"20" "   LOG-FILE changes log filename   ",                       00772000
    48"1E" "   OFF    - turn off all flags     ",                       00773000
              0),                                                       00774000
              RETRYHELP(           %  SET RETRY HELP                    00775000
    48"1C" "    INITIAL-CONNECTION count ",                             00776000
    48"11" "    PACKETS count",                                         00777000
              0),                                                       00778000
              ONOFFHELP(           %  ONLY ON OR OFF                    00779000
    48"05" "   ON",                                                     00780000
    48"06" "   OFF     ",                                               00781000
              0),                                                       00782000
              LONUMBERHELP(         %  ONLY NUMBERS ALLOWED             00783000
    48"24" "   must be an integer from 0 thru 31    ",                  00784000
              0),                                                       00785000
              QUOTEHELP(          %  ONLY 32 < N < 127                  00786000
    48"2B" "   must be an ASCII character from ! thru ~    ",           00787000
    48"2E" "                              (HEX 21 thru 7E) ",           00788000
              0),                                                       00789000
              NUMBERHELP(         %  ANY NUMBERS ALLOWED                00790000
    48"21" "   can be any decimal digit > 0  ",                         00791000
              0);                                                       00792000
                                                                        00793000
   DEFINE                                                               00794000
     PLAINH    =  0#,                                                   00795000
     SETH      =  1#,                                                   00796000
     ABORTH    =  2#,                                                   00797000
     DEBUGH    =  3#,                                                   00798000
     RETRYH    =  4#,                                                   00799000
     RECEIVEH  =  5#,                                                   00800000
     SENDH     =  5#,                                                   00801000
     NUMBERH   =  6#,                                                   00802000
     ONOFFH    =  7#,                                                   00803000
     QUOTEH    =  8#,                                                   00804000
     UNITSH    =  9#,                                                   00805000
     LONUMH    = 10#,                                                   00806000
     SETFILEH  = 11#;                                                   00807000
                                                                        00808000
                                                                        00809000
 BOOLEAN PROCEDURE SENDSW;                    FORWARD;                  00810000
 BOOLEAN PROCEDURE RECSW(ISTATE);                          % [1.017]    00811000
  REAL ISTATE;                                FORWARD;     % [1.017]    00812000
 PROCEDURE SPACK(TYPE,NUM,LEN,DATA);                                    00813000
  VALUE TYPE,NUM,LEN;                                                   00814000
  REAL TYPE;                                                            00815000
  REAL NUM,LEN;                                                         00816000
  ARRAY DATA[0];                              FORWARD;                  00817000
 REAL PROCEDURE RPACK(LEN,NUM,DATA);                                    00818000
  REAL LEN,NUM;                                                         00819000
  ARRAY DATA[0];                              FORWARD;                  00820000
 REAL PROCEDURE BUFILL(FID,BUFFER);                                     00821000
  FILE FID;                                                             00822000
  ARRAY BUFFER[0];                            FORWARD;                  00823000
 PROCEDURE BUFEMP(FID,BUFFER,LEN);                                      00824000
  VALUE LEN;                                                            00825000
  REAL LEN;                                                             00826000
  FILE FID;                                                             00827000
  ARRAY BUFFER[0];                            FORWARD;                  00828000
 PROCEDURE SPAR(LEN,DATA);                                              00829000
 REAL LEN;                                                              00830000
  ARRAY DATA[0];                              FORWARD;                  00831000
 PROCEDURE RPAR(LEN,DATA);                                              00832000
 REAL LEN;                                                              00833000
  ARRAY DATA[0];                              FORWARD;                  00834000
 PROCEDURE FLUSHINPUT;                        FORWARD;                  00835000
 PROCEDURE PRERRPKT(MSG);                                               00836000
  ARRAY MSG[0];                               FORWARD;                  00837000
                                                                        00838000
 %                                                                      00839000
 %   A B O R T R U N                                                    00840000
 %                                                                      00841000
 %   SENDS AN ERROR PACKET AND ABORTS                                   00842000
 %                                                                      00843000
   PROCEDURE ABORTRUN;                                                  00844000
    BEGIN                                                               00845000
      REPLACE TBUF_[0] BY COL FOR * DIGITS,"      ";                    00846000
      REPLACE EBUF_ BY "KERMIT ABORTING DUE TO FAULT # ",               00847000
        TBUF_ FOR 2 WITH EBCDICTOASCII," @ ",                           00848000
        KPROMPT FOR 50 WITH EBCDICTOASCII;                              00849000
      SPACK("E",( N := *+1 ) MOD64, MAXPACKSIZ-5,EBUF_);                00850000
      IF NOT SERVER THEN                                                00851000
        SAYP(EBUF_,NOINDENT);                                           00852000
      IF (MYSELF.OPTION).[VALUE(FAULT) : 1]=1 THEN                      00853000
        PROGRAMDUMP(ARRAYS,FILES);                                      00854000
      WHEN(10);                                                         00855000
      MYSELF.STATUS := VALUE(TERMINATED);                               00856000
    END ABORTRUN;                                                       00857000
                                                                        00858000
%                                                                       00859000
%  I N I T I A L I Z E                                                  00860000
%                                                                       00861000
%  INITIALIZE SETS UP INITIAL VALUES                                    00862000
%                                                                       00863000
  PROCEDURE INITIALIZE;                                                 00864000
   BEGIN                                                                00865000
    ARRAY GREETING[0:13];                                               00866000
    EBCDIC ARRAY VERSION[0:7];                                          00867000
                                                                        00868000
    REPLACE MACHINE BY TIME(23).[7:8]*100 FOR 4 DIGITS;                 00869000
    REPLACE VERSION BY COMPILETIME(20) FOR 1 DIGITS,8".",               00870000
       COMPILETIME(21) FOR 3 DIGITS;                                    00871000
    REPLACE GREETING BY "UCD BURROUGHS KERMIT-",                        00872000
        MACHINE FOR 4 WITH EBCDICTOASCII,                               00873000
       " - VERSION ",VERSION FOR 5 WITH EBCDICTOASCII,NULC;             00874000
    REPLACE FBUF_ BY " " FOR 30 WORDS;                                  00875000
    REPLACE EBUF_ BY " " FOR 16 WORDS;                                  00876000
    SAYP(GREETING,NOINDENT);                                            00877000
    REPLACE KPROMPT BY "KERMIT-",MACHINE FOR 2                          00878000
       WITH EBCDICTOASCII," ",ETXC; %  PUT ETXC AT END TO KEEP BLANK    00879000
                                                                        00880000
%   INITIALIZE THESE VALUES AND HOPE THE FIRST PACKET WILL GET ACROSS OK00881000
                                                                        00882000
    EOL        := CR;              %  EOL FOR OUTGOING PACKETS          00883000
    SOHCHAR    := DEFSOH;          % SOH FOR OUTGOING PACKETS           00884000
    QUOTE      := "#";             %  STANDARD CONTROL-QUOTE CHAR "#"   00885000
    PAD        := 0;               %  NO PADDING                        00886000
    PCHAR      := NULC;            %  USE NULC IF ANY PADDING WANTED    00887000
    QBIN       := "N";             %  DEFAULT TO NO BINARY QUOTING      00888000
    REPT       := SP;              %  DEFAULT TO SPACE                  00889000
    CHKTYPE    := "1";             %  DEFAULT CHKTYPE                   00890000
    MYPACKSIZ  := MAXPACKSIZ;      %  SET MINE TO DEFAULTS              00891000
    INITRETRY  := DEFINITRETRY;    %  INITIALIZE RETRIES                00892000
    PACKETRETRY:= DEFPACKETRETRY;                                       00893000
    FILERECSIZE:= DEFRECSIZE;                                           00894000
    FILEBLOCKSIZE:= DEFBLOCKSIZE;                                       00895000
    FILEUNITS   := DEFUNITS;                                            00896000
    MYTIME     := DEFTIME;                                              00897000
    MYPAD      := DEFPAD;                                               00898000
    MYPCHAR    := DEFPCHAR;                                             00899000
    MYEOL      := DEFEOL;                                               00900000
    MYSOH      := DEFSOH;              % [1.019]                        00901000
    MYQUOTE    := DEFQUOTE;                                             00902000
    MYQBIN     := DEFQBIN;             % [1.019]                        00903000
    MYCHKTYPE  := DEFCHKTYPE;                                           00904000
    MYREPT     := DEFREPT;                                              00905000
    MYPAUSE    := DEFPAUSE;     % SECONDS ( INPUT IS IN 10THS )         00906000
    MYDELAY    := DEFDELAY;                                             00907000
    MYESCCHR   := DEFESCCHR;                                            00908000
                                                                        00909000
    FIXEDRECS  := FALSE;           %  DEFAULT                           00910000
    EXPTABS    := TRUE;            %  DEFAULT -> EXPAND THEM            00911000
    HIBITOK    := FALSE;           % [1.017] 8-BIT ONLY WHEN REQUESTED  00912000
    BINARYON   := FALSE;           % [1.019] CHANGED BY SET BINARY COMMA00913000
    REPTOK     := FALSE;           % [1.017] NO REPEAT PROCESSING       00914000
    KEEPFILE    := TRUE;           %  DEFAULT TO KEEP ALL FILES MADE    00915000
    RAW         := FALSE;          %  USE CR FOR END-OF-LINE            00916000
                                   %  INITIALIZE ACNTRL TABLE           00917000
    REPLACE ACNTRL BY 48"0000FFFFFFFF",0,0,48"000000000001",0,0,0,0;    00918000
    REPLACE BCNTRL BY 0 FOR 8 WORDS;                                    00919000
                                                                        00920000
   END INITIALIZE;                                                      00921000
                                                                        00922000
%                                                                       00923000
%  S C A N I T                                                          00924000
%                                                                       00925000
%  SCANS INPUT AND PUTS ITEMS INTO ARRAY AC IN KUNKER-FORM.  PLACES     00926000
%  ITEM LENGTH INTO LEN AND RETURNS THE ITEM'S INDEX IN THE SPECIAL     00927000
%  ARRAY.                                                               00928000
%                                                                       00929000
                                                                        00930000
    REAL PROCEDURE SCANIT;                                              00931000
      BEGIN                                                             00932000
        ARRAY AC[0:3];                                                  00933000
        REAL I,J,SAVEJ,CNT;                                             00934000
                                                                        00935000
        SCANIT := -1;                                                   00936000
        COL_OK_TIL := COL;                                              00937000
        SCAN CP:CP FOR COL:COL UNTIL GTR " ";                           00938000
        IF COL GTR 0 THEN                                               00939000
          BEGIN                                                         00940000
            SCAN CP FOR I:COL WHILE GTR " ";                            00941000
            IF CP+((I := COL-I)-1) = "?" THEN                           00942000
             IF I GTR 1 THEN                                            00943000
              I := *-1;                                                 00944000
            REPLACE POINTER(AC) BY I.[7:48] FOR 1,                      00945000
                  CP FOR I;                                             00946000
            J := SIZE(SPECARA);                                         00947000
            SAVEJ := CNT := -1;                                         00948000
            WHILE J:=*-1 GEQ 0 DO                                       00949000
              IF J := MASKSEARCH(AC[0],                                 00950000
               40"E0" & REAL(NOT FALSE)[39:MIN(40,I*8)],SPECARA[J])     00951000
               GEQ 0 THEN                                               00952000
              IF CP = POINTER(SPECARA[J])+1 FOR I THEN                  00953000
                TBUF_[CNT:=*+1] := SAVEJ := J                           00954000
            ;                                                           00955000
            IF (CNT:=*+1 GTR 1)OR(CP+I = "?") THEN                      00956000
              BEGIN                                                     00957000
                IF CP+I NEQ "?" THEN                                    00958000
              SAY("  ambiguous command, please supply more characters");00959000
                SAY("  possible commands:");                            00960000
                WHILE CNT GTR 0 DO                                      00961000
                    IF SAVEJ := TBUF_[CNT := *-1] GTR 0 THEN            00962000
                      SAYP(POINTER(SPECARA[SAVEJ])+1,INDENT);           00963000
                SCANIT := NOERRORV;                                     00964000
              END                                                       00965000
            ELSE                                                        00966000
                SCANIT := SAVEJ;                                        00967000
            CP  := *+I;                                                 00968000
            COL := *-I;                                                 00969000
          END;                                                          00970000
      END SCANIT;                                                       00971000
                                                                        00972000
%                                                                       00973000
%  S C A N U M                                                          00974000
%                                                                       00975000
   REAL PROCEDURE SCANUM;                                               00976000
    BEGIN                                                               00977000
      ARRAY AC[0:3];                                                    00978000
      REAL I,J,SAVEJ,CNT;                                               00979000
                                                                        00980000
      SCANUM := INVALIDV;                                               00981000
      COL_OK_TIL := COL;                                                00982000
      SCAN CP:CP FOR COL:COL UNTIL GTR " ";                             00983000
      IF COL GTR 0 THEN                                                 00984000
         IF CP IN NUMBERS THEN                                          00985000
           BEGIN                                                        00986000
             SCAN CP FOR I:COL WHILE IN NUMBERS;                        00987000
             IF I := COL-I LSS 12 THEN                                  00988000
               BEGIN                                                    00989000
                 REPLACE AC BY CP FOR I WITH ASCIITOEBCDIC;             00990000
                 SCANUM := INTEGER(AC,I);                               00991000
               END                                                      00992000
             ELSE                                                       00993000
               SCANUM := INVALIDV;                                      00994000
           END                                                          00995000
         ELSE                                                           00996000
           IF CP = "?" THEN                                             00997000
              SCANUM := QUESTIONV;                                      00998000
     END OF PROCEDURE SCANUM;                                           00999000
%                                                                       01000000
%  H E L P E R                                                          01001000
%                                                                       01002000
%  DOES ALL THE HELP STUFF FROM ? OR HELP INPUT                         01003000
%                                                                       01004000
                                                                        01005000
 $BEGINSEGMENT                                                          01006000
                                                                        01007000
PROCEDURE HELPER(TYPE);                                                 01008000
 VALUE TYPE;                                                            01009000
 REAL TYPE;                                                             01010000
 BEGIN                                                                  01011000
   ARRAY REFERENCE HELPARA[0];                                          01012000
   POINTER P;                                                           01013000
   REAL LENGTH;                                                         01014000
   CASE TYPE OF                                                         01015000
   BEGIN                                                                01016000
   PLAINH:                           %  PLAN OLD HELP                   01017000
        HELPARA := PLAINHELP;                                           01018000
   SETH:                           %  SET HELP                          01019000
        HELPARA := SETHELP;                                             01020000
   ABORTH:                         %  ABORT HELP                        01021000
        HELPARA := ABORTHELP;                                           01022000
   DEBUGH:                         %  DEBUG HELP                        01023000
        HELPARA := DEBUGHELP;                                           01024000
   RETRYH:                         %  RETRY HELP                        01025000
        HELPARA := RETRYHELP;                                           01026000
   SENDH:                          %  SEND HELP                         01027000
        HELPARA := SENDHELP;                                            01028000
   NUMBERH:                        %  NUMBER HELP                       01029000
        HELPARA := NUMBERHELP;                                          01030000
   ONOFFH:                         %  ON/OFF HELP                       01031000
        HELPARA := ONOFFHELP;                                           01032000
   QUOTEH:                         %  QUOTE HELP                        01033000
        HELPARA := QUOTEHELP;                                           01034000
   UNITSH:                         %  RECEIVE UNITS HELP                01035000
        HELPARA := UNITSHELP;                                           01036000
   LONUMH:                         %  LOW NUMBER HELP (0-31)            01037000
        HELPARA := LONUMBERHELP;                                        01038000
   SETFILEH:                                                            01039000
        HELPARA := SETFILEHELP;    %  SET FILE HELP                     01040000
   ELSE:                                                                01041000
        LENGTH := -1;                                                   01042000
   END CASE;                                                            01043000
   IF LENGTH GEQ 0 THEN                                                 01044000
    BEGIN                                                               01045000
     P := POINTER(HELPARA);                                             01046000
     WHILE LENGTH := REAL(P,1) GTR 0 DO                                 01047000
      BEGIN                                                             01048000
        BRD := WRITE(REM,LENGTH,P+1);                                   01049000
        P := *+(((LENGTH + 6) DIV 6) *6);                               01050000
      END;                                                              01051000
    END;                                                                01052000
 END HELPER;                                                            01053000
                                                                        01054000
                                                                        01055000
%                                                                       01056000
%  S E T S T U F F                                                      01057000
%                                                                       01058000
%  SETS THE VARIOUS THINGS                                              01059000
%                                                                       01060000
PROCEDURE SETSTUFF;                                                     01061000
 BEGIN                                                                  01062000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                           01063000
   PROCEDURE ABORTER;                                                   01064000
    BEGIN                                                               01065000
      SPECARA := SPECABORT;                                             01066000
      CASE SCANIT OF                                                    01067000
      BEGIN                                                             01068000
    QMARKV:                                                             01069000
SAY("  determines what to do if RECEIVE transfer fails - options are:");01070000
           HELPER(ABORTH);                                              01071000
    DISCARDV:                                                           01072000
           KEEPFILE := FALSE;                                           01073000
    KEEPV:                                                              01074000
           KEEPFILE := TRUE;                                            01075000
    ELSE:                                                               01076000
           SAYQOPT("INCOMPLETE");                                       01077000
           HELPER(ABORTH);                                              01078000
      END CASE;                                                         01079000
    END ABORTER;                                                        01080000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                           01081000
   PROCEDURE DEBUGGER;                                                  01082000
    BEGIN                                                               01083000
      POINTER P;                                                        01084000
      SPECARA := SPECDEBUG;                                             01085000
      CASE SCANIT OF                                                    01086000
      BEGIN                                                             01087000
    QMARKV:                                                             01088000
           SAY("  sets level of DEBUGGING output -- options are:");     01089000
           HELPER(DEBUGH);                                              01090000
    STATESV:                                                            01091000
           DEBUG := TRUE;                                               01092000
    PACKETSV:                                                           01093000
           DEBUG := BOOLEAN(3);                                         01094000
    LOGFILEV:                                                           01095000
           IF NOT DEBUG THEN DEBUG := TRUE;                             01096000
           SCAN CP:CP FOR COL:COL WHILE LEQ " ";                        01097000
           IF COL GTR 0 THEN                                            01098000
            BEGIN                                                       01099000
              SCAN P:CP FOR COL WHILE GEQ "A";                          01100000
              REPLACE P BY "."48"00";                                   01101000
              IF LOG.OPEN THEN LOCK(LOG,CRUNCH);                        01102000
              REPLACE CP BY CP FOR COL+1 WITH ASCIITOEBCDIC;            01103000
              REPLACE LOG.TITLE BY CP;                                  01104000
            END;                                                        01105000
    DOFFV:                                                              01106000
           DEBUG := FALSE;                                              01107000
    ELSE:                                                               01108000
           SAYQOPT("DEBUGGING");                                        01109000
           HELPER(DEBUGH);                                              01110000
      END CASE;                                                         01111000
      IF DEBUG THEN                                                     01112000
        IF NOT LOG.OPEN THEN LOG.OPEN := TRUE                           01113000
        ELSE                                                            01114000
      ELSE                                                              01115000
        IF LOG.OPEN THEN LOCK(LOG,CRUNCH);                              01116000
    END DEBUGGER;                                                       01117000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                           01118000
   PROCEDURE DELAYER;                                                   01119000
    BEGIN                                                               01120000
      REAL N;                                                           01121000
      N := SCANUM;                                                      01122000
      IF (N LSS 0)OR(N GTR 31) THEN                                     01123000
        IF (N = QUESTIONV) THEN                                         01124000
        BEGIN                                                           01125000
             SAY("   sets time to delay (in secs) before");             01126000
             SAY("   sending first packet during file SEND");           01127000
             HELPER(LONUMH);                                            01128000
        END                                                             01129000
        ELSE                                                            01130000
        BEGIN                                                           01131000
             SAYQ("DELAY");                                             01132000
             HELPER(LONUMH);                                            01133000
        END                                                             01134000
      ELSE                                                              01135000
        MYDELAY := N                                                    01136000
    END DELAYER;                                                        01137000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                           01138000
   PROCEDURE RETRYER;                                                   01139000
    BEGIN                                                               01140000
      REAL N;                                                           01141000
      SPECARA := SPECRETRY;                                             01142000
      CASE SCANIT OF                                                    01143000
      BEGIN                                                             01144000
    QMARKV:                                                             01145000
           SAY("   sets number of times to retry an operation");        01146000
           SAY("   before giving up -  options are:");                  01147000
           HELPER(RETRYH);                                              01148000
    INITCONNV:                                                          01149000
           N := SCANUM;                                                 01150000
           IF (N LSS 0) THEN                                            01151000
              IF (N = QUESTIONV) THEN                                   01152000
              BEGIN                                                     01153000
             SAY("   sets number of times to retry initial connection");01154000
                HELPER(NUMBERH)                                         01155000
              END                                                       01156000
              ELSE                                                      01157000
              BEGIN                                                     01158000
                SAYQ("INITIAL-CONNECTION");                             01159000
                HELPER(NUMBERH);                                        01160000
              END                                                       01161000
           ELSE                                                         01162000
              INITRETRY := N;                                           01163000
    RETRYPACKETSV:                                                      01164000
           N := SCANUM;                                                 01165000
           IF (N LSS 0) THEN                                            01166000
              IF (N = QUESTIONV) THEN                                   01167000
              BEGIN                                                     01168000
             SAY("   sets number of times to retry regular connection");01169000
                HELPER(NUMBERH)                                         01170000
              END                                                       01171000
              ELSE                                                      01172000
              BEGIN                                                     01173000
                SAYQ("PACKETS");                                        01174000
                HELPER(NUMBERH);                                        01175000
              END                                                       01176000
           ELSE                                                         01177000
              PACKETRETRY := N;                                         01178000
    ELSE:                                                               01179000
           SAYQOPT("RETRY");                                            01180000
           HELPER(RETRYH);                                              01181000
      END CASE                                                          01182000
    END RETRYER;                                                        01183000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                        01184000
   PROCEDURE BLOCKER;                                                   01185000
    BEGIN                                                               01186000
      REAL N;                                                           01187000
      N := SCANUM;                                                      01188000
      IF (N LSS 1) THEN                                                 01189000
         IF (N = QUESTIONV) THEN                                        01190000
         BEGIN                                                          01191000
             SAY("  sets BLOCKSIZE attribute of RECEIVED files");       01192000
             HELPER(NUMBERH);                                           01193000
         END                                                            01194000
         ELSE                                                           01195000
         BEGIN                                                          01196000
             SAYQ("BLOCK-SIZE");                                        01197000
             HELPER(NUMBERH);                                           01198000
         END                                                            01199000
       ELSE                                                             01200000
       BEGIN                                                            01201000
         FILEBLOCKSIZE := N;                                            01202000
         IF (FILEBLOCKSIZE MOD FILERECSIZE) NEQ 0 THEN                  01203000
         BEGIN                                                          01204000
           SAY("Warning: BLOCK-SIZE must be a multiple of RECORD-SIZE");01205000
        SAY1("  current settings:       RECORD-SIZE = ",FILERECSIZE);   01206000
        SAY1("                           BLOCK-SIZE = ",FILEBLOCKSIZE); 01207000
         END                                                            01208000
       END                                                              01209000
    END BLOCKER;                                                        01210000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                           01211000
   PROCEDURE RECSIZER;                                                  01212000
    BEGIN                                                               01213000
      REAL N;                                                           01214000
      N := SCANUM;                                                      01215000
      IF (N LSS 1) THEN                                                 01216000
         IF (N = QUESTIONV) THEN                                        01217000
         BEGIN                                                          01218000
             SAY("  sets MAXRECSIZE attribute of RECEIVED files");      01219000
             HELPER(NUMBERH);                                           01220000
         END                                                            01221000
         ELSE                                                           01222000
         BEGIN                                                          01223000
             SAYQ("RECORD-SIZE");                                       01224000
             HELPER(NUMBERH);                                           01225000
         END                                                            01226000
       ELSE                                                             01227000
       BEGIN                                                            01228000
         FILERECSIZE := N;                                              01229000
         IF (FILEBLOCKSIZE MOD FILERECSIZE) NEQ 0 THEN                  01230000
         BEGIN                                                          01231000
           SAY("Warning: BLOCK-SIZE must be a multiple of RECORD-SIZE");01232000
        SAY1("  current settings:       RECORD-SIZE = ",FILERECSIZE);   01233000
        SAY1("                           BLOCK-SIZE = ",FILEBLOCKSIZE); 01234000
         END                                                            01235000
       END                                                              01236000
    END RECSIZER;                                                       01237000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                           01238000
  PROCEDURE UNITER;                                                     01239000
   BEGIN                                                                01240000
      SPECARA := SPECUNITS;                                             01241000
      CASE SCANIT OF                                                    01242000
      BEGIN                                                             01243000
      QMARKV:                                                           01244000
 SAY("   set UNITS file attribute for received files -- options are:"); 01245000
             HELPER(UNITSH);                                            01246000
     UWORDSV:                                                           01247000
             FILEUNITS := VALUE(WORDS);                                 01248000
     UCHARACTERSV:                                                      01249000
             FILEUNITS := VALUE(CHARACTERS);                            01250000
        ELSE:                                                           01251000
             SAYQOPT("UNITS");                                          01252000
             HELPER(UNITSH);                                            01253000
     END CASE;                                                          01254000
   END OF PROCEDURE UNITER;                                             01255000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                        01256000
   PROCEDURE SENDRECEIVER(WHICH);                                       01257000
    VALUE WHICH;                                                        01258000
    REAL WHICH;                                                         01259000
    BEGIN                                                               01260000
      REAL N;     % [1.018] NEED TO DECLARE LOCALLY                     01261000
      INTEGER NDX;                                                      01262000
      SPECARA := SPECRECEIVE;                                           01263000
       CASE NDX := SCANIT OF                                            01264000
      BEGIN                                                             01265000
    QMARKV:                                                             01266000
            SAY("  sets various packet parameters - options are:");     01267000
           HELPER(SENDH);                                               01268000
    EOLV:                                                               01269000
           N := SCANUM;                                                 01270000
           IF (N LSS 1)OR(N GTR 31) THEN                                01271000
              IF (N = QUESTIONV) THEN                                   01272000
              BEGIN                                                     01273000
                IF WHICH=SETRECEIVEV THEN                               01274000
       SAY("  sets the packet terminator character to expect")          01275000
                ELSE                                                    01276000
       SAY("  sets the packet terminator character to send");           01277000
                HELPER(LONUMH)                                          01278000
              END                                                       01279000
              ELSE                                                      01280000
              BEGIN                                                     01281000
                SAYQ("END-OF-LINE");                                    01282000
                HELPER(LONUMH);                                         01283000
              END                                                       01284000
           ELSE                                                         01285000
             IF WHICH=SETRECEIVEV THEN                                  01286000
               MYEOL := N                                               01287000
             ELSE                                                       01288000
               EOL := N;                                                01289000
    QUOTEV:                                                             01290000
          IF WHICH=SETRECEIVEV THEN                                     01291000
   SAY("  not implemented, no need to set QUOTE to expect")             01292000
          ELSE                                                          01293000
          BEGIN                                                         01294000
           COL_OK_TIL := COL;                                           01295000
           SCAN CP:CP FOR COL:COL WHILE LEQ " ";                        01296000
           IF COL GTR 0 THEN                                            01297000
           BEGIN                                                        01298000
            IF CP = "?" THEN                                            01299000
            BEGIN                                                       01300000
                SAY("  sets QUOTE character to send");                  01301000
                HELPER(QUOTEH)                                          01302000
            END                                                         01303000
            ELSE                                                        01304000
             IF N := REAL(CP,1) LSS 33 OR N GTR 126 THEN  % ! < N < ~   01305000
             BEGIN                                                      01306000
                SAY("  invalid QUOTE character - must be an");          01307000
                HELPER(QUOTEH)                                          01308000
             END                                                        01309000
             ELSE                                                       01310000
               IF N=MYQBIN THEN               %  NO WAY!                01311000
  SAY("  QUOTE not set, that character is your binary quote")           01312000
               ELSE                                                     01313000
                 IF N=MYREPT THEN             % NO WAY!                 01314000
  SAY("  QUOTE not set, that character is your repeat quote")           01315000
                 ELSE                                                   01316000
                    MYQUOTE := N                                        01317000
            END                                                         01318000
            ELSE                                                        01319000
            BEGIN                                                       01320000
              SAYQ("QUOTE");                                            01321000
              HELPER(QUOTEH);                                           01322000
            END;                                                        01323000
          END;                                                          01324000
    LENV:                                                               01325000
           N := SCANUM;                                                 01326000
           IF (N LSS 10)OR(N GTR 94) THEN                               01327000
              IF (N = QUESTIONV) THEN                                   01328000
              BEGIN                                                     01329000
                IF WHICH=SETRECEIVEV THEN                               01330000
                  SAY("  set PACKET-LENGTH for incoming packets")       01331000
                ELSE                                                    01332000
                  SAY("  sets PACKET-LENGTH for outgoing packets");     01333000
                SAY("   must be an integer from 10 to 94");             01334000
              END                                                       01335000
              ELSE                                                      01336000
              BEGIN                                                     01337000
                SAYQ("PACKET-LENGTH");                                  01338000
                SAY("   must be an integer from 10 to 94");             01339000
              END                                                       01340000
           ELSE                                                         01341000
             IF WHICH=SETRECEIVEV THEN                                  01342000
               MYPACKSIZ := N                                           01343000
             ELSE                                                       01344000
               SPSIZ := N;                                              01345000
    PADV:                                                               01346000
    PCHARV:                                                             01347000
    PAUSEV:                                                             01348000
    STARTOFPACKV:                                                       01349000
    TIMEOUTV:                                                           01350000
           N := SCANUM;                                                 01351000
           IF (N LSS 0)OR(N GTR 31) THEN                                01352000
              IF (N = QUESTIONV) THEN                                   01353000
              BEGIN                                                     01354000
                IF WHICH=SETRECEIVEV THEN                               01355000
    SAY("  sets a packet parameter for incoming packets")               01356000
                ELSE                                                    01357000
    SAY("  sets a packet parameter for outgoing packets");              01358000
                HELPER(LONUMH);                                         01359000
              END                                                       01360000
              ELSE                                                      01361000
              BEGIN                                                     01362000
                SAYQ("packet");                                         01363000
                HELPER(LONUMH);                                         01364000
              END                                                       01365000
           ELSE                                                         01366000
            CASE NDX OF                                                 01367000
            BEGIN                                                       01368000
    PADV:                                                               01369000
             IF WHICH=SETRECEIVEV THEN                                  01370000
               MYPAD := N                                               01371000
             ELSE                                                       01372000
               PAD := N;                                                01373000
    PCHARV:                                                             01374000
             IF WHICH=SETRECEIVEV THEN                                  01375000
               MYPCHAR := N                                             01376000
             ELSE                                                       01377000
               PCHAR := N;                                              01378000
    PAUSEV:                                                             01379000
           MYPAUSE := N/10;                                             01380000
    STARTOFPACKV:                                                       01381000
           IF WHICH=SETRECEIVEV THEN                                    01382000
              MYSOH := N                                                01383000
           ELSE                                                         01384000
              SOHCHAR := N;                                             01385000
    TIMEOUTV:                                                           01386000
           IF N = 0 THEN                                                01387000
             SAY("  TIMEOUT must be greater than zero")                 01388000
           ELSE                                                         01389000
             IF WHICH=SETRECEIVEV THEN                                  01390000
               MYTIME := N                                              01391000
             ELSE                                                       01392000
                TIMINT := N;                                            01393000
             END CASE;                                                  01394000
    NOERRORV:                                                           01395000
           ;                                                            01396000
    ELSE:                                                               01397000
           IF WHICH=SETRECEIVEV THEN                                    01398000
              SAYQOPT("RECEIVE")                                        01399000
           ELSE                                                         01400000
              SAYQOPT("SEND");                                          01401000
           HELPER(SENDH);                                               01402000
      END CASE;                                                         01403000
    END SENDRECEIVER;                                                   01404000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                           01405000
   PROCEDURE BINARER;                                                   01406000
    BEGIN                                                               01407000
      SPECARA := SPECONOFF;                                             01408000
      CASE SCANIT OF                                                    01409000
      BEGIN                                                             01410000
    QMARKV:                                                             01411000
         SAY("  transfer all 8 bits of each character - options are:"); 01412000
           HELPER(ONOFFH);                                              01413000
       ONV:                                                             01414000
           BINARYON := TRUE;                                            01415000
      OFFV:                                                             01416000
           BINARYON := FALSE;                                           01417000
      ELSE:                                                             01418000
           SAYQOPT("BINARY");                                           01419000
           HELPER(ONOFFH);                                              01420000
      END CASE;                                                         01421000
    END BINARER;                                                        01422000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                           01423000
   PROCEDURE FIXER;                                                     01424000
    BEGIN                                                               01425000
      SPECARA := SPECONOFF;                                             01426000
      CASE SCANIT OF                                                    01427000
      BEGIN                                                             01428000
    QMARKV:                                                             01429000
           SAY("  send trailing blanks found at the end of");           01430000
           SAY("  fixed-length records -- options are:");               01431000
           HELPER(ONOFFH);                                              01432000
       ONV:                                                             01433000
           FIXEDRECS := TRUE;                                           01434000
      OFFV:                                                             01435000
           FIXEDRECS := FALSE;                                          01436000
      ELSE:                                                             01437000
           SAYQOPT("FIXED");                                            01438000
           HELPER(ONOFFH);                                              01439000
      END CASE;                                                         01440000
    END FIXER;                                                          01441000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                           01442000
   PROCEDURE EXPANDTABBER;                                              01443000
    BEGIN                                                               01444000
      SPECARA := SPECONOFF;                                             01445000
      CASE SCANIT OF                                                    01446000
      BEGIN                                                             01447000
    QMARKV:                                                             01448000
SAY("  expand TABs to spaces when RECEIVING files - options are:");     01449000
           HELPER(ONOFFH);                                              01450000
       ONV:                                                             01451000
           EXPTABS := TRUE;                                             01452000
      OFFV:                                                             01453000
           EXPTABS := FALSE;                                            01454000
      ELSE:                                                             01455000
           SAYQOPT("EXPAND-TABS");                                      01456000
           HELPER(ONOFFH);                                              01457000
      END CASE;                                                         01458000
    END EXPANDTABBER;                                                   01459000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                           01460000
   PROCEDURE RAWER;                                                     01461000
    BEGIN                                                               01462000
      SPECARA := SPECONOFF;                                             01463000
      CASE SCANIT OF                                                    01464000
      BEGIN                                                             01465000
    QMARKV:                                                             01466000
SAY("  fill each record to MAXRECSIZE when RECEIVING files -");         01467000
           SAY("  options are:");                                       01468000
           HELPER(ONOFFH);                                              01469000
       ONV:                                                             01470000
           RAW := TRUE;                                                 01471000
           EXPTABS := FALSE;                                            01472000
           SAY("EXPAND-TABS now set to OFF");                           01473000
      OFFV:                                                             01474000
           RAW := FALSE;                                                01475000
           IF EXPTABS THEN SAY("EXPAND-TABS is ON")                     01476000
           ELSE            SAY("EXPAND-TABS is OFF");                   01477000
      ELSE:                                                             01478000
           SAYQ("RAW");                                                 01479000
           HELPER(ONOFFH);                                              01480000
      END CASE;                                                         01481000
    END RAWER;                                                          01482000
                                                                        01483000
%%%%%%%%%%%%%%%%%%%%%%                                                  01484000
   PROCEDURE SETFILER;                                                  01485000
    BEGIN                                                               01486000
                                                                        01487000
      INTEGER NDX;                                                      01488000
      SPECARA := SPECFILE;                                              01489000
      CASE (NDX := SCANIT) OF                                           01490000
      BEGIN                                                             01491000
  QMARKV:                                                               01492000
        HELPER(SETFILEH);                                               01493000
  BLOCKSIZEV:                                                           01494000
        BLOCKER;                                                        01495000
  EXPTABSV:                                                             01496000
        EXPANDTABBER;                                                   01497000
  FIXEDV:                                                               01498000
        FIXER;                                                          01499000
  RAWV:                                                                 01500000
        RAWER;                                                          01501000
  RECORDSIZEV:                                                          01502000
        RECSIZER;                                                       01503000
  UNITSV:                                                               01504000
        UNITER;                                                         01505000
  NOERRORV:                                                             01506000
           ;                                                            01507000
  ELSE:                                                                 01508000
        SAYQOPT("FILE");                                                01509000
        HELPER(SETFILEH);                                               01510000
      END OF CASE;                                                      01511000
    END OF PROCEDURE SETFILER;                                          01512000
%%%%%%%%%%%%%%%%%%%%%%                                                  01513000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% mainline for procedure SETSTUFF    01514000
   SPECARA := SPECSET;                                                  01515000
   CASE SCANIT OF                                                       01516000
   BEGIN                                                                01517000
  QMARKV:                                                               01518000
    SAY("  sets various KERMIT environment variables -- options are:"); 01519000
        HELPER(SETH);                                                   01520000
  INCOMPLETEV:                                                          01521000
        ABORTER;                                                        01522000
  BINARYV:                                                              01523000
        BINARER;                                                        01524000
  DEBUGV:                                                               01525000
        DEBUGGER;                                                       01526000
  DELAYV:                                                               01527000
        DELAYER;                                                        01528000
  FILEV:                                                                01529000
        SETFILER;                                                       01530000
  RETRYV:                                                               01531000
        RETRYER;                                                        01532000
  SETRECEIVEV:                                                          01533000
        SENDRECEIVER(SETRECEIVEV);                                      01534000
  SETSENDV:                                                             01535000
        SENDRECEIVER(SETSENDV);                                         01536000
  NOERRORV:                                                             01537000
        ;                                                               01538000
  ELSE:                                                                 01539000
        SAYQOPT("SET");                                                 01540000
        HELPER(SETH);                                                   01541000
   END CASE;                                                            01542000
 END SETSTUFF;                                                          01543000
                                                                        01544000
 $ENDSEGMENT                                                            01545000
                                                                        01546000
                                                                        01547000
%                                                                       01548000
%  S T A T U S                                                          01549000
%                                                                       01550000
%  DISPLAY THE STATUS OF ALL THE VARIOUS THINGS                         01551000
%                                                                       01552000
PROCEDURE STATUS;                                                       01553000
 BEGIN                                                                  01554000
   SAY("parameters which can be changed by the SET command");           01555000
   IF (BINARYON) THEN                                                   01556000
      SAY("  BINARY ON (8th bit quoting will be requested)")            01557000
   ELSE                                                                 01558000
      SAY("  BINARY OFF (No 8th bit quoting will be done)");            01559000
   IF DEBUG THEN                                                        01560000
    BEGIN                                                               01561000
      REPLACE PFILNAM:=POINTER(FILNAM) BY LOG.TITLE,NULC;               01562000
      REPLACE PFILNAM BY PFILNAM FOR 80 WITH EBCDICTOASCII;             01563000
      IF REAL(DEBUG) GTR 1 THEN                                         01564000
        SAYN("  DEBUG STATES and PACKETS to file ",PFILNAM)             01565000
      ELSE                                                              01566000
        SAYN("  DEBUG STATES to file ",PFILNAM);                        01567000
    END                                                                 01568000
   ELSE                                                                 01569000
     SAY("  DEBUG OFF");                                                01570000
   SAY1("  DELAY before first send (in seconds) = ",MYDELAY);           01571000
   IF KEEPFILE THEN                                                     01572000
     SAY("  if INCOMPLETE, KEEP partial file")                          01573000
   ELSE                                                                 01574000
     SAY("  if INCOMPLETE, DISCARD partial file");                      01575000
   SAY1("  RETRY INITIAL-CONNECTION = ",INITRETRY);                     01576000
   SAY1("  RETRY PACKETS = ",PACKETRETRY);                              01577000
   SAY("parameters which can be changed by the SET FILE command");      01578000
   SAY1("  RECORD-SIZE = ",FILERECSIZE);                                01579000
   SAY1("  BLOCK-SIZE  = ",FILEBLOCKSIZE);                              01580000
   IF FILEUNITS = VALUE(WORDS) THEN                                     01581000
     SAY("  UNITS       = WORDS")                                       01582000
   ELSE                                                                 01583000
     SAY("  UNITS       = CHARACTERS");                                 01584000
   IF EXPTABS THEN                                                      01585000
     SAY("  EXPAND-TABS ON")                                            01586000
   ELSE                                                                 01587000
     SAY("  EXPAND-TABS OFF");                                          01588000
   IF FIXEDRECS THEN                                                    01589000
     SAY("  FIXED ON (send blanks found at the end of records)")        01590000
   ELSE                                                                 01591000
     SAY("  FIXED OFF (strip blanks from the end of records)");         01592000
   IF RAW THEN                                                          01593000
     SAY("  RAW ON ( Burroughs records delimited by size only )")       01594000
   ELSE                                                                 01595000
     SAY("  RAW OFF ( Burroughs records delimited by CR )");            01596000
   SPECARA := SPECSHOW;                                                 01597000
   CASE SCANIT OF                                                       01598000
   BEGIN                                                                01599000
SHOSENDV:                                                               01600000
     SAY("parameters which can be changed by the SET SEND command");    01601000
     SAYC("  END-OF-LINE character = ",EOL);                            01602000
     SAY1("  maximum PACKET-LENGTH = ",SPSIZ);                          01603000
     SAY1("  number of PADDING characters = ",PAD);                     01604000
     IF PAD GTR 0 THEN                                                  01605000
       SAYC("  PADDING CHARACTER = ",PCHAR);                            01606000
 SAY1("  PAUSE before packet send (in tenths of second) = ",MYPAUSE*10);01607000
     SAYC("  START-OF-PACKET charcter = ",SOHCHAR);                     01608000
     SAY1("  packet TIMEOUT (in seconds) = ",TIMINT);                   01609000
ELSE:                                                                   01610000
     SAY("parameters which can be changed by the SET RECEIVE command"); 01611000
     SAYC("  END-OF-LINE character = ",MYEOL);                          01612000
     SAY1("  maximum PACKET-LENGTH = ",MYPACKSIZ);                      01613000
     SAY1("  number of PADDING characters = ",MYPAD);                   01614000
     IF MYPAD GTR 0 THEN                                                01615000
       SAYC("  PADDING CHARACTER = ",MYPCHAR);                          01616000
 SAY1("  PAUSE before packet send (in tenths of second) = ",MYPAUSE*10);01617000
     SAYC("  QUOTE character = ",MYQUOTE);                              01618000
     SAYC("  START-OF-PACKET character = ",MYSOH);                      01619000
     SAY1("  packet TIMEOUT (in seconds) = ",MYTIME);                   01620000
   END CASE;                                                            01621000
 END STATUS;                                                            01622000
                                                                        01623000
                                                                        01624000
BOOLEAN PROCEDURE PROCESSIT;                                            01625000
BEGIN                                                                   01626000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                          01627000
PROCEDURE SERVANT;                                                      01628000
 BEGIN                                                                  01629000
   BOOLEAN DONTQUIT;               %  LOOP CONTROL                      01630000
   ARRAY BUFFER[0:MAXPACKWDS];     %  TEMPORARY FILE TITLE BUFFER       01631000
   FILE DUMMY(KIND=PACK,FILETYPE=7);%  TEMPORARY DUMMY FILE             01632000
   REAL NUM,LEN,TIMER;             %  PACKET NUMBER, LENGTH, TIMEOUT    01633000
                                                                        01634000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                          01635000
   PROCEDURE GENERICTHINGS;        %  HANDLE "G" REQUESTS               01636000
    BEGIN                                                               01637000
      POINTER PR;                  %  POINTER TO PACKET                 01638000
                                                                        01639000
      PR := POINTER(PACKET);       %  INITIALIZE IT                     01640000
      CASE REAL(PR,1) OF                                                01641000
      BEGIN                                                             01642000
      "F":                         %  FINISH, BUT DON'T LOGOUT          01643000
           SPACK("Y",N,0,NULLDATA);%  ACK TO PC AND THEN...             01644000
           DONTQUIT := FALSE;      %  EXIT FROM WHILE LOOP              01645000
           BRD := TRUE;            %  AND EXIT FROM MAIN LOOP           01646000
      "L":                         %  FINISH AND LOG OUT, TOO           01647000
%          SPACK("Y",N,0,NULLDATA);%  ACK TO PC AND THEN...             01648000
           SAYN("BYE IS NOT IMPLEMENTED:  ",PACKET);                    01649000
           DONTQUIT := FALSE;      %  EXIT FROM WHILE LOOP              01650000
           BRD := TRUE;            %  AND EXIT FROM MAIN LOOP           01651000
           %                       %  THIS PART ISN'T IMPLEMENTED       01652000
     ELSE:                         %  SOME OTHER NON-IMPLEMENTED THING  01653000
           SAYN("THIS IS NOT IMPLEMENTED:  ",PACKET);                   01654000
      END CASE;                                                         01655000
    END GENERICTHINGS;                                                  01656000
                                                                        01657000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% mainline statements for SERVANT           01658000
   STATE := "T";                   %  JUST TO INITIALIZE FOR DEBUG      01659000
   DONTQUIT := SERVER := TRUE;     %  INITIALIZE BOOLEANS               01660000
   REPLACE PFILNAM:=POINTER(FILNAM) BY NULC FOR MAXPACKWDS WORDS;       01661000
   TIMER := IF TIMINT LSS MINTIM OR TIMINT GTR MAXTIM THEN              01662000
                          MYTIME ELSE TIMINT;                           01663000
                                                                        01664000
   WHILE DONTQUIT DO                                                    01665000
    BEGIN                                                               01666000
      REM.TIMELIMIT := TIMER;    %  SET UP FOR IO TIMEOUT               01667000
      IF DEBUG THEN BUGC("SERVANT STATE:  ",STATE);                     01668000
      CASE RPACK(LEN,NUM,PACKET) OF%  DO WHICHEVER ONE WE NEED          01669000
      BEGIN                                                             01670000
      "R":                         %  GET OR RECEIVE A FILE(US TO PC)   01671000
           N := NUM;               %  RESTART PACKET NUMBERS            01672000
           REPLACE PFILNAM BY PACKET FOR LEN WITH TOUPPER,              01673000
                 ".",NULC;                                              01674000
           REPLACE BUFFER BY PFILNAM FOR LEN+1 WITH ASCIITOEBCDIC;      01675000
           IF DUMMY.OPEN THEN CLOSE(DUMMY);                             01676000
           REPLACE DUMMY.TITLE BY BUFFER;                               01677000
           IF NOT DUMMY.RESIDENT THEN                                   01678000
             SAYN("NO FILE:  ",FILNAM)                                  01679000
           ELSE                                                         01680000
           IF SENDSW THEN                                               01681000
%            SAYN("SEND DONE FOR:  ",PFILNAM)                           01682000
%          ELSE                                                         01683000
%            SAYN("SEND FAILED FOR:  ",PFILNAM);                        01684000
           STATE := "R";           %  FOR DEBUG                         01685000
      "S":                         %  SEND A FILE (FROM PC TO US)       01686000
           CALL1 := TRUE;                   % [1.017]                   01687000
           RPAR(LEN,PACKET);                % [1.017] EXCHANGE          01688000
           SPAR(LEN,PACKET);                % [1.017]  PARAMETERS       01689000
           SPACK("Y",NUM,LEN,PACKET);       % [1.017]                   01690000
           OLDTRY := NUMTRY;                % [1.017] RESET COUNTERS    01691000
           NUMTRY := 0;                     % [1.017]                   01692000
           N := (NUM+1) MOD64;              % [1.017]                   01693000
           IF (NOT RECSW("F")) THEN         % [1.017] ATTEMPT TO RECEIVE01694000
           BEGIN                            % [1.017]                   01695000
             REPLACE FBUF_ BY "RECEIVE FAILED.";                        01696000
             ERROR(FBUF_);                  % [1.017]                   01697000
           END;                             % [1.017]                   01698000
%            SAYN("RECEIVE DONE FOR:  ",PFILNAM)                        01699000
%          ELSE                    %  NO SUCH LUCK                      01700000
%          IF KEEPFILE THEN        %  WE GOT PART OF IT...              01701000
%            SAYN("RECEIVE FAILED, BUT SAVED PART OF:  ",PFILNAM)       01702000
%          ELSE                                                         01703000
%            SAYN("RECEIVE FAILED FOR:  ",PFILNAM);                     01704000
           STATE := "S";           %  FOR DEBUG                         01705000
      "T":                        %  TIMED OUT                          01706000
           SPACK("N",N,0,NULLDATA);%  NAK ON TIMEOUT                    01707000
           STATE := "T";          %  FOR DEBUG                          01708000
      "G":                         %  GENERIC COMMAND                   01709000
           GENERICTHINGS;          %  TAKE CARE OF THEM ELSEWHERE       01710000
      "I":                         %  INITIALIZE PACKETS                01711000
           CALL1 := TRUE;          % [1.017] RPAR CALLED FIRST          01712000
           RPAR(LEN,PACKET);       %  GET HIS INIT DATA                 01713000
           SPAR(LEN,PACKET);       %  FILL UP PACKET WITH MY INIT DATA  01714000
           SPACK("Y",N,LEN,PACKET);%  ACK WITH MY PARAMETERS            01715000
           OLDTRY := NUMTRY;       %  SAVE OLD TRY COUNT                01716000
           NUMTRY := 0;            %  INITIALIZE NUMTRY                 01717000
     ELSE:                         %  WHO KNOWS                         01718000
           SPACK("N",N,6,PACKET);  %  NAK IT                            01719000
      END CASE;                                                         01720000
    END WHILE;                                                          01721000
   WHEN(5);                        %  MAKE SURE ACK GETS OUT            01722000
   BRD := TRUE;                    %  EXIT THRU TO EOT                  01723000
  END SERVANT;                                                          01724000
%                                                                       01725000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                         01726000
%  G E T F I L E T I T L E                                              01727000
%                                                                       01728000
%  GETS THE DISK FILE TITLE FOR SENDING                                 01729000
%  AND MAKES SURE THE FILE TITLE WILL FIT INSIDE A PACKET               01730000
%                                                                       01731000
PROCEDURE GETFILETITLE;                                                 01732000
 BEGIN                                                                  01733000
   POINTER P,Q;                                                         01734000
   REAL I,J;                                                            01735000
                                                                        01736000
   SCAN CP:CP FOR COL:COL WHILE LEQ " ";                                01737000
   IF COL GTR 0 THEN                                                    01738000
     BEGIN                                                              01739000
       SCAN P:CP FOR I:COL WHILE GTR " ";                               01740000
       COL := *-I;                                                      01741000
       REPLACE PFILNAM:=POINTER(FILNAM) BY CP FOR COL,".",NULC;         01742000
       CP := POINTER(PACKET)+COL;                                       01743000
       IF (COL:=I)-1 GTR 0 THEN                                         01744000
         SCAN Q:P+1 FOR J:I-1 UNTIL GTR " ";                            01745000
       IF MORETOSEND := J GTR 0 THEN                                    01746000
         REPLACE NEXTSEND BY Q FOR J,NULC;                              01747000
       REPLACE PACKET BY NULC FOR MAXPACKWDS WORDS;                     01748000
     END;                                                               01749000
 END GETFILETITLE;                                                      01750000
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% mainline statements for PROCESSIT     01751000
  REPLACE CP:=PACKET BY NULC FOR MAXPACKWDS WORDS;                      01752000
  COL:=80;                                                              01753000
  SPECARA := SPECIAL;                                                   01754000
  BRD := WRITE(REM[STOP],11,KPROMPT);                                   01755000
  IF NOT BRD := READ(REM,COL,PACKET) THEN                               01756000
  BEGIN                                                                 01757000
   REPLACE CP BY CP FOR COL := RD.LENGTHF WITH TOUPPER;                 01758000
   COL_BASE := COL;                                                     01759000
   SCAN CP:CP FOR COL:COL WHILE LEQ " ";                                01760000
   IF COL GTR 0 THEN                                                    01761000
      CASE SCANIT OF                                                    01762000
      BEGIN                                                             01763000
 QMARKV:                                                                01764000
 HELPV:                                                                 01765000
        HELPER(PLAINH);        %  ?, HELP                               01766000
 SERVERV:                                                               01767000
        SERVANT;               %  SERVER                                01768000
 SENDV:                                                                 01769000
        GETFILETITLE;          %  SEND                                  01770000
        IF SENDSW THEN                                                  01771000
%         SAY("SEND DONE")                                              01772000
;%      ELSE                                                            01773000
%         SAY("SEND FAILED");                                           01774000
 RECEIVEV:                                                              01775000
        REPLACE PFILNAM := POINTER(FILNAM) BY NULC FOR MAXPACKWDS WORDS;01776000
       IF RECSW("R") THEN                    % [1.017]                  01777000
%         SAY("RECEIVE DONE")                                           01778000
;%       ELSE                                                           01779000
%         IF KEEPFILE THEN                                              01780000
%           SAY("RECEIVE FAILED - PARTIAL FILE SAVED")                  01781000
%         ELSE                                                          01782000
%           SAY("RECEIVE FAILED");                                      01783000
 SETV:                                                                  01784000
        SETSTUFF;              %  SET                                   01785000
 SHOWV:                                                                 01786000
 STATUSV:                                                               01787000
        STATUS;                %  STATUS                                01788000
 QUITV:                                                                 01789000
 EXITV:                                                                 01790000
        BRD := TRUE;                                                    01791000
 NOERRORV:                     %  WE ALREADY GAVE THE ERROR             01792000
        ;                                                               01793000
 ELSE:                         %  GARBAGE?                              01794000
        SAYQOPT(" ");                                                   01795000
        HELPER(PLAINH);                                                 01796000
      END CASE;                                                         01797000
      IF MORETOSEND THEN                                                01798000
        BEGIN                                                           01799000
          MORETOSEND := FALSE;                                          01800000
          REPLACE CP := PACKET BY "SEND ",NEXTSEND FOR                  01801000
              COL:MAXPACKSIZ-5 UNTIL=NULC, NULC FOR COL;                01802000
        END;                                                            01803000
  END IF;                                                               01804000
END PROCESSIT;                                                          01805000
                                                                        01806000
                                                                        01807000
 %                                                                      01808000
 %  S E N D S W                                                         01809000
 %                                                                      01810000
 %  SENDSW IS THE STATE TABLE SWITCHER FOR SENDING FILES.  IT LOOPS UNTI01811000
 %  EITHER IT FINISHES, OR AN ERROR IS ENCOUNTERED.  THE ROUTINES CALLED01812000
 %  BY SENDSW ARE RESPONSIBLE FOR CHANGING THE STATE.                   01813000
 %                                                                      01814000
                                                                        01815000
                                                                        01816000
 $BEGINSEGMENT                                                          01817000
                                                                        01818000
BOOLEAN PROCEDURE SENDSW;                                               01819000
 BEGIN                                                                  01820000
  BOOLEAN DONTQUIT;            %  LOOP CONTROL                          01821000
  FILE FP(KIND=DISK,FILETYPE=8,        %  CURRENT DISK FILE             01822000
          INTMODE=ASCII,               %  SO CHECKSUM, ETC, WILL WORK   01823000
          TRANSLATE=FORCESOFT,INPUTTABLE=EBCDICTOASCII,                 01824000
          MYUSE=IN);                                                    01825000
                                                                        01826000
                                                                        01827000
                                                                        01828000
 %                                                                      01829000
 %  S I N I T                                                           01830000
 %                                                                      01831000
 %  SEND INITIATE: SEND THIS HOST'S PARAMETERS AND GET OTHER SIDE'S BACK01832000
                                                                        01833000
                                                                        01834000
REAL PROCEDURE SINIT;                                                   01835000
 BEGIN                                                                  01836000
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH             01837000
                                                                        01838000
  IF (NUMTRY:=*+1 LEQ INITRETRY) THEN                                   01839000
  BEGIN                                                                 01840000
    CALL1 := TRUE;                 % [1.017] SPAR CALLED FIRST          01841000
    SPAR(LEN,PACKET);              %  FILL UP INIT INFO PACKET          01842000
                                                                        01843000
    IF NOT SERVER AND NUMTRY = 1 THEN % WAIT A BIT BEFORE SENDING THE   01844000
      WHEN(MYDELAY);               %  INIT PACKET...                    01845000
    FLUSHINPUT;                    %  FLUSH PENDING INPUT               01846000
                                                                        01847000
    SPACK("S",N,LEN,PACKET);       %  SEND AN S PACKET                  01848000
    CASE RPACK(LEN,NUM,RECPKT) OF  %  WHAT WAS THE REPLY?               01849000
     BEGIN                                                              01850000
     "N":  SINIT := STATE;         %  NAK, TRY IT AGAIN                 01851000
                                                                        01852000
     "Y":                          %  ACK                               01853000
           IF (N = NUM) THEN                                            01854000
             BEGIN                                                      01855000
               RPAR(LEN,RECPKT);   %  GET OTHER SIDE'S INIT INFO        01856000
               IF EOL = 0 THEN     %  CHECK AND SET DEFAULTS            01857000
                 EOL := MYEOL;                                          01858000
               IF QUOTE = 0 THEN                                        01859000
                 QUOTE := MYQUOTE;                                      01860000
               NUMTRY := 0;        %  RESET TRY COUNTER                 01861000
               N := (N+1) MOD64;   %  BUMP PACKET COUNT                 01862000
               SINIT := "F";       %  OK, CASE STATE TO F               01863000
             END ELSE                                                   01864000
               SINIT := STATE;     %  IF WRONG ACK, STAY IN S STATE     01865000
                                                                        01866000
     "E":                          %  ERROR PACKET RECEIVED             01867000
           PRERRPKT(RECPKT);       %  PRINT IT OUT AND                  01868000
           SINIT := "A";           %  ABORT                             01869000
                                                                        01870000
     "T":                         %  RECEIVE FAILURE, TRY AGAIN         01871000
           SINIT := STATE;                                              01872000
                                                                        01873000
    ELSE:                          %  ANYTHING ELSE, JUST ABORT         01874000
           SINIT := "A";                                                01875000
    END CASE;                                                           01876000
  END ELSE                                                              01877000
      SINIT := "A";                %  IF TOO MANY TRIES, GIVE UP        01878000
 END SINIT;                                                             01879000
                                                                        01880000
                                                                        01881000
 %                                                                      01882000
 %  S F I L E                                                           01883000
 %                                                                      01884000
 %  SEND FILE HEADER.                                                   01885000
                                                                        01886000
                                                                        01887000
REAL PROCEDURE SFILE;                                                   01888000
 BEGIN                                                                  01889000
  LABEL ACKHERE,QUIT;                                                   01890000
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH             01891000
  ARRAY FILNAM1[0:MAXPACKWDS];             %  CONVERTED FILE NAME       01892000
  POINTER NEWFILNAM,               %  POINTER TO FILE NAME TO SEND      01893000
          CP;                      %  CHAR POINTER                      01894000
                                                                        01895000
  IF (NUMTRY:=*+1 LEQ PACKETRETRY+1) THEN                               01896000
  BEGIN                                                                 01897000
                                                                        01898000
    IF FP.OPEN THEN CLOSE(FP);     %  MAKE SURE IT'S CLOSED             01899000
    SCAN CP := PFILNAM FOR LEN:MAXPACKSIZ UNTIL = NULC;                 01900000
    LEN := MAXPACKSIZ - LEN;       %  LENGTH OF BURROUGHS TITLE         01901000
    REPLACE FILNAM1 BY PFILNAM FOR LEN WITH ASCIITOEBCDIC;              01902000
    REPLACE FP.TITLE BY FILNAM1;   %  GIVE IT THE NEW NAME              01903000
    IF NOT FP.PRESENT THEN         %  FILE ISN'T THERE                  01904000
      BEGIN                                                             01905000
        REPLACE FBUF_ BY "CANNOT FIND FILE: ",PFILNAM FOR MAXSENDFILESIZ01906000
            WHILE GEQ " ",NULC;                                         01907000
        ERROR(FBUF_);                                                   01908000
        SFILE := "A";                                                   01909000
        GO QUIT;                                                        01910000
      END;                                                              01911000
    IF FP.EXTMODE = VALUE(EBCDIC) THEN % DEFAULT IS TO TRANSLATE IT...  01912000
      BEGIN                                                             01913000
        CLOSE(FP);                                                      01914000
        FP.EXTMODE := VALUE(EBCDIC);                                    01915000
        FP.OPEN := TRUE;                                                01916000
      END                                                               01917000
    ELSE                                                                01918000
    IF FP.EXTMODE = VALUE(ASCII) THEN % DONT TRANSLATE IT...            01919000
      BEGIN                                                             01920000
        CLOSE(FP);                                                      01921000
        FP.EXTMODE := VALUE(ASCII);                                     01922000
        FP.TRANSLATE := VALUE(FULLTRANS);                               01923000
        FP.OPEN := TRUE;                                                01924000
      END                                                               01925000
    ELSE                                                                01926000
          ;            %  GIVE UP...?                                   01927000
    IF DEBUG THEN BUGN("OPENING FOR SENDING:  ",PFILNAM);               01928000
    RECSIZ_ := FP.MAXRECSIZE;                                           01929000
    UNITS_  := IF FP.UNITS=VALUE(CHARACTERS) THEN 1 ELSE 6;             01930000
    RESIZE(GBUF_,(RECSIZ_ * UNITS_ + 6) DIV 6);% MAKE BUFFER CORRECT SIZ01931000
    LEN := *-1;        % GET RID OF THE EXTRA PERIOD...                 01932000
                                                                        01933000
    NUM := LEN;                                                         01934000
    WHILE NUM GTR 0 AND LEN GTR MAXSENDFILESIZ DO % PARE DOWN TITLE     01935000
     BEGIN                                                              01936000
       SCAN NEWFILNAM:CP FOR NUM:LEN UNTIL ="/";                        01937000
       IF NUM GTR 0 THEN                                                01938000
         SCAN CP:NEWFILNAM FOR LEN:NUM WHILE = "/";                     01939000
     END;                                                               01940000
    NUM := LEN;                                                         01941000
    NEWFILNAM := FILNAM1;                                               01942000
    WHILE NUM GTR 0 DO                                                  01943000
     BEGIN                                                              01944000
       REPLACE NEWFILNAM:NEWFILNAM BY CP:CP FOR NUM:NUM WHILE NEQ """;  01945000
       IF NUM GTR 0 THEN                                                01946000
         BEGIN                                                          01947000
           SCAN CP:CP FOR NUM:NUM WHILE = """;                          01948000
           LEN := *-1;                                                  01949000
         END;                                                           01950000
     END;                                                               01951000
    IF LEN GTR 8 THEN             %  WE'LL HAVE TO INSERT A DOT         01952000
      BEGIN                                                             01953000
        LEN := *+1;                                                     01954000
        REPLACE PFILNAM BY CP:FILNAM1 FOR 8 WITH FIXSLASHES,            01955000
           "." , CP FOR LEN-8 WITH FIXSLASHES                           01956000
      END                                                               01957000
    ELSE                                                                01958000
      REPLACE PFILNAM BY FILNAM1 FOR LEN WITH FIXSLASHES;               01959000
    REPLACE FILNAM1 BY PFILNAM FOR LEN, NULC;                           01960000
                                                                        01961000
                                                                        01962000
    IF DEBUG THEN                                                       01963000
      BUGN("SENDING:  ",FILNAM1);                                       01964000
                                                                        01965000
    SPACK("F",N,LEN,FILNAM1);      %  SEND AN F PACKET                  01966000
    CASE RPACK(LEN,NUM,RECPKT) OF  % WHAT WAS THE REPLY?                01967000
     BEGIN                                                              01968000
     "N":                          %  NAK, JUST STAY IN THIS STATE,     01969000
           NUM := (NUM+63) MOD64;  %  UNLESS IT'S NAK FOR NEXT PACKET   01970000
           IF N NEQ NUM THEN       %  WHICH IS JUST LIKE AN ACK FOR     01971000
             SFILE := STATE        %  THIS PACKET SO FALL THRU TO...    01972000
           ELSE GO TO ACKHERE;                                          01973000
                                                                        01974000
     "Y":                          %  ACK                               01975000
 ACKHERE:  IF  N = NUM THEN        %  PACKET NUMBER MATCHES             01976000
            BEGIN                                                       01977000
             NUMTRY := 0;          %  RESET TRY COUNTER                 01978000
             N := (N+1) MOD64;     %  BUMP PACKET COUNT                 01979000
             IF BSIZE := BUFILL(FP,PACKET)=0                            01980000
              THEN                 %  GET FIRST DATA FROM FILE, ERROR?  01981000
                SFILE := "Z"       %  YES, QUIT NOW                     01982000
             ELSE                  %  A GOOD READ                       01983000
              SFILE := "D";        %  CASE STATE TO D                   01984000
            END                                                         01985000
           ELSE                                                         01986000
             SFILE := STATE;       %  WRONG ACK, STAY IN F STATE        01987000
                                                                        01988000
     "E":                          %  ERROR PACKET RECEIVED             01989000
           PRERRPKT(RECPKT);       %  PRINT IT OUT AND                  01990000
           SFILE := "A";           %  ABORT                             01991000
                                                                        01992000
     "T":                         %  RECEIVE FAILURE, STAY IN F STATE   01993000
           SFILE := STATE;                                              01994000
                                                                        01995000
    ELSE:  SFILE := "A";           %  SOMETHING ELSE, JUST "ABORT"      01996000
      END CASE;                                                         01997000
   END ELSE                                                             01998000
     SFILE := "A";                 %  IF TOO MANY TRIES, GIVE UP        01999000
 QUIT:                                                                  02000000
  END SFILE;                                                            02001000
                                                                        02002000
                                                                        02003000
 %                                                                      02004000
 %  S D A T A                                                           02005000
 %                                                                      02006000
 %  SEND FILE DATA                                                      02007000
                                                                        02008000
                                                                        02009000
REAL PROCEDURE SDATA;                                                   02010000
 BEGIN                                                                  02011000
  LABEL ACKHERE;                                                        02012000
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH             02013000
                                                                        02014000
  IF (NUMTRY:=*+1 LEQ PACKETRETRY+1) THEN                               02015000
  BEGIN                                                                 02016000
                                                                        02017000
    SPACK("D",N,BSIZE,PACKET);      %  SEND A D PACKET                  02018000
    CASE RPACK(LEN,NUM,RECPKT) OF  %  WHAT WAS THE REPLY?               02019000
     BEGIN                                                              02020000
     "N":                          %  NAK, JUST STAY IN THIS STATE,     02021000
                                   %  UNLESS IT'S NAK FOR NEXT PACKET   02022000
           NUM := (NUM+63) MOD64;  %  UNLESS IT'S NAK FOR NEXT PACKET   02023000
           IF N NEQ NUM THEN       %  WHICH IS JUST LIKE AN ACK FOR     02024000
             SDATA := STATE        %  THIS PACKET SO FALL THRU TO...    02025000
           ELSE GO TO ACKHERE;                                          02026000
                                                                        02027000
     "Y":                          %  ACK                               02028000
 ACKHERE:   IF N = NUM THEN        %  IF WRONG ACK, FAIL                02029000
             BEGIN                                                      02030000
              NUMTRY := 0;         %  RESET TRY COUNTER                 02031000
              N := (N+1)MOD64;     %  BUMP PACKET COUNT                 02032000
              IF ((BSIZE := BUFILL(FP,PACKET)) = 0)                     02033000
                    THEN           %  GET DATA FROM FILE                02034000
                SDATA := "Z"       %  IF EOF SET STATE TO THAT          02035000
              ELSE SDATA := "D";   %  GOT DATA, STAY IN STATE D         02036000
             END                                                        02037000
            ELSE                                                        02038000
             SDATA := STATE;                                            02039000
                                                                        02040000
     "E":                          %  ERROR PACKET RECEIVED             02041000
           PRERRPKT(RECPKT);       %  PRINT IT OUT AND                  02042000
           SDATA := "A";           %  ABORT                             02043000
                                                                        02044000
     "T": SDATA := STATE;         %  RECEIVE FAILURE, STAY IN D         02045000
                                                                        02046000
    ELSE:  SDATA := "A";           %  ANYTHING ELSE, "ABORT"            02047000
     END CASE;                                                          02048000
  END ELSE                                                              02049000
    SDATA := "A";                  %  IF TOO MANY TRIES, GIVE UP        02050000
 END SDATA;                                                             02051000
                                                                        02052000
                                                                        02053000
 %                                                                      02054000
 %  S E O F                                                             02055000
 %                                                                      02056000
 %  SEND END-OF-FILE.                                                   02057000
                                                                        02058000
                                                                        02059000
REAL PROCEDURE SEOF;                                                    02060000
 BEGIN                                                                  02061000
  LABEL ACKHERE;                                                        02062000
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH             02063000
  IF (NUMTRY:=*+1 LEQ PACKETRETRY+1) THEN                               02064000
  BEGIN                                                                 02065000
                                                                        02066000
    SPACK("Z",N,0,PACKET);         %  SEND A "Z" PACKET                 02067000
    CASE RPACK(LEN,NUM,RECPKT) OF  %  WHAT WAS THE REPLY?               02068000
     BEGIN                                                              02069000
     "N":                          %  NAK, JUST STAY IN THIS STATE,     02070000
                                   %  UNLESS IT'S NAK FOR NEXT PACKET,  02071000
           NUM := (NUM+63) MOD64;  %  UNLESS IT'S NAK FOR NEXT PACKET   02072000
           IF N NEQ NUM THEN       %  WHICH IS JUST LIKE AN ACK FOR     02073000
             SEOF := STATE         %  THIS PACKET SO FALL THRU TO...    02074000
           ELSE GO TO ACKHERE;                                          02075000
                                                                        02076000
     "Y":                          %  ACK                               02077000
 ACKHERE:                                                               02078000
           IF N = NUM THEN                                              02079000
             BEGIN                                                      02080000
               NUMTRY := 0;        %  RESET TRY COUNTER                 02081000
               N := (N+1) MOD64;   %  AND BUMP PACKET COUNT             02082000
               IF DEBUG THEN BUGN("CLOSING INPUT FILE:  ",PFILNAM);     02083000
               CLOSE(FP);          %  CLOSE THE INPUT FILE              02084000
                   SEOF := "B";    %  BREAK, EOT, ALL DONE              02085000
             END ELSE              %  IF WRONG ACK, HOLD OUT            02086000
               SEOF := STATE;                                           02087000
                                                                        02088000
     "E":                          %  ERROR PACKET RECEIVED             02089000
           PRERRPKT(RECPKT);       %  PRINT IT OUT AND                  02090000
           SEOF := "A";            %  ABORT                             02091000
                                                                        02092000
     "T": SEOF := STATE;          %  RECEIVE FAILURE, STAY IN Z         02093000
                                                                        02094000
     ELSE: SEOF := "A";            %  SOMETHING ELSE, "ABORT"           02095000
     END CASE;                                                          02096000
  END ELSE                                                              02097000
    SEOF := "A";                   %  IF TOO MANY TRIES, ABORT          02098000
 END SEOF;                                                              02099000
                                                                        02100000
                                                                        02101000
 %                                                                      02102000
 %  S B R E A K                                                         02103000
 %                                                                      02104000
 %  SEND BREAK (EOT)                                                    02105000
                                                                        02106000
                                                                        02107000
REAL PROCEDURE SBREAK;                                                  02108000
 BEGIN                                                                  02109000
  LABEL ACKHERE;                                                        02110000
  REAL NUM, LEN;   %  PACKET NUMBER, LENGTH                             02111000
  IF (NUMTRY:=*+1 LEQ PACKETRETRY+1) THEN                               02112000
  BEGIN                                                                 02113000
                                                                        02114000
    SPACK("B",N,0,PACKET);         %  SEND A B PACKET                   02115000
    CASE RPACK(LEN,NUM,RECPKT) OF  %  WHAT WAS THE REPLY?               02116000
     BEGIN                                                              02117000
     "N":                          %  NAK, JUST STAY IN THIS STATE,     02118000
                                   %  UNLESS NAK FOR PREVIOUS PACKET,   02119000
           NUM := (NUM+63) MOD64;  %  UNLESS IT'S NAK FOR NEXT PACKET   02120000
           IF N NEQ NUM THEN       %  WHICH IS JUST LIKE AN ACK FOR     02121000
             SBREAK := STATE       %  THIS PACKET SO FALL THRU TO...    02122000
           ELSE                                                         02123000
             GO TO ACKHERE;                                             02124000
                                                                        02125000
     "Y":                          %  ACK                               02126000
 ACKHERE:                                                               02127000
           IF N = NUM THEN         %  IF WRONG ACK, FAIL                02128000
             BEGIN                                                      02129000
           NUMTRY := 0;            %  RESET TRY COUNTER                 02130000
           N := (N+1) MOD64;       %  AND BUMP PACKET COUNT             02131000
           SBREAK := "C";          %  CASE STATE TO COMPLETE            02132000
             END ELSE                                                   02133000
             SBREAK := STATE;                                           02134000
                                                                        02135000
     "E":                          %  ERROR PACKET RECEIVED             02136000
           PRERRPKT(RECPKT);       %  PRINT IT OUT AND                  02137000
           SBREAK := "A";          %  ABORT                             02138000
                                                                        02139000
     "T": SBREAK := STATE;        %  RECEIVE FAILURE, STAY IN B         02140000
%     [KS019] NEED TO RETRY ON TIME OUT                                 02141000
%    "T": SBREAK := "C";          %  TIMED OUT WAITING FOR LAST ACK     02142000
                                                                        02143000
    ELSE:  SBREAK := "A";          %  OTHER, "ABORT"                    02144000
    END CASE;                                                           02145000
  END ELSE                                                              02146000
    SBREAK := "A";                 %  IF TOO MANY TRIES, ABORT          02147000
 END SBREAK;                                                            02148000
                                                                        02149000
%    MAIN LINE TO SENDSW                                                02150000
                                                                        02151000
                                                                        02152000
                                                                        02153000
  STATE := "S";                    %  SEND INITIATE IS THE START STATE  02154000
  N := 0;                          %  INITIALIZE MESSAGE NUMBER         02155000
  GCNT_ := -1;                     %  INITIALIZE GETCHAR POINTER, ETC   02156000
  NUMTRY := 0;                     %  BUG NO TRIES YET                  02157000
  DONTQUIT := TRUE;                %  INITIALIZE FOR LOOP               02158000
  REM.TIMELIMIT := IF TIMINT LSS MINTIM OR TIMINT GTR MAXTIM THEN       02159000
                          MYTIME ELSE TIMINT;                           02160000
                                                                        02161000
  WHILE DONTQUIT DO                %  DO THIS AS LONG AS NECESSARY      02162000
   BEGIN                                                                02163000
     IF DEBUG THEN BUGC("SENDSW STATE:  ",STATE);                       02164000
     CASE STATE OF                                                      02165000
      BEGIN                                                             02166000
      "S": STATE := SINIT;         %  SEND-INIT                         02167000
      "F": STATE := SFILE;         %  SEND-FILE                         02168000
      "D": STATE := SDATA;         %  SEND-DATA                         02169000
      "Z": STATE := SEOF;          %  SEND-END-OF-FILE                  02170000
      "B": STATE := SBREAK;        %  SEND-BREAK                        02171000
      "C": SENDSW := TRUE;         %  COMPLETE                          02172000
           DONTQUIT:=FALSE;        %  LET'S QUIT                        02173000
      "A": SENDSW := FALSE;        %  "ABORT"                           02174000
           DONTQUIT:=FALSE;        %  LET'S QUIT                        02175000
     ELSE: SENDSW := FALSE;        %  UNKNOWN, FAIL                     02176000
           DONTQUIT:=FALSE;        %  LET'S QUIT                        02177000
      END CASE;                                                         02178000
   END WHILE;                                                           02179000
   REM.TIMELIMIT := 0;          %  DISABLE REMOTE INPUT TIMELIMIT       02180000
 END SENDSW;                                                            02181000
                                                                        02182000
 $ENDSEGMENT                                                            02183000
                                                                        02184000
                                                                        02185000
 %                                                                      02186000
 %  R E C S W                                                           02187000
 %                                                                      02188000
 %  THIS IS THE STATE TABLE SWITCHER FOR RECEIVING FILES.               02189000
                                                                        02190000
                                                                        02191000
 $BEGINSEGMENT                                                          02192000
                                                                        02193000
BOOLEAN PROCEDURE RECSW(ISTATE);   % [1.017]                            02194000
  REAL ISTATE;                     % [1.017]                            02195000
 BEGIN                                                                  02196000
  BOOLEAN DONTQUIT;                                                     02197000
  FILE FP(KIND=DISK,MYUSE=OUT,     %  FILE POINTER FOR CURRENT DISK FILE02198000
          INTMODE=ASCII,EXTMODE=EBCDIC,UNITS=FILEUNITS,                 02199000
          TRANSLATE=FULLTRANS,OUTPUTTABLE=ASCIITOEBCDIC,                02200000
          MAXRECSIZE=FILERECSIZE,BLOCKSIZE=FILEBLOCKSIZE,               02201000
          AREASIZE=FILEBLOCKSIZE DIV FILERECSIZE * 10);                 02202000
                                                                        02203000
                                                                        02204000
                                                                        02205000
 %                                                                      02206000
 %  R I N I T                                                           02207000
 %                                                                      02208000
 %  RECEIVE INITIALIZATION                                              02209000
                                                                        02210000
                                                                        02211000
REAL PROCEDURE RINIT;                                                   02212000
 BEGIN                                                                  02213000
  REAL LEN, NUM;                   %  PACKET LENGTH, NUMBER             02214000
                                                                        02215000
  IF (NUMTRY:=*+1 LEQ INITRETRY) THEN                                   02216000
  BEGIN                                                                 02217000
                                                                        02218000
    CASE IF SERVER AND NUMTRY=1 THEN "S"                                02219000
         ELSE                                                           02220000
           RPACK(LEN,NUM,PACKET) OF%  GET A PACKET                      02221000
     BEGIN                                                              02222000
     "S":                          %  SEND-INIT                         02223000
           CALL1 := TRUE;          % [1.017] RPAR CALLED FIRST          02224000
           RPAR(LEN,PACKET);       %  GET THE OTHER SIDE'S INIT DATA    02225000
           SPAR(LEN,PACKET);       %  FILL UP PACKET WITH MY INIT       02226000
           SPACK("Y",N,LEN,PACKET);%  ACK WITH MY PARAMETERS            02227000
           OLDTRY := NUMTRY;       %  SAVE OLD TRY COUNT                02228000
           NUMTRY := 0;            %  START A NEW COUNTER               02229000
           N := (N+1) MOD64;       %  BUMP PACKET NUMBER, MOD 64        02230000
           RINIT := "F";           %  ENTER FILE-RECEIVE STATE          02231000
                                                                        02232000
     "E":                          %  ERROR PACKET RECEIVED             02233000
           PRERRPKT(PACKET);       %  PRINT IT OUT AND                  02234000
           RINIT := "A";           %  ABORT                             02235000
                                                                        02236000
     "T":                         %  DIDN'T GET PACKET                  02237000
           SPACK("N",N,0,NULLDATA);%  RETURN A NAK                      02238000
           RINIT := STATE;         %  KEEP TRYING                       02239000
                                                                        02240000
    ELSE:  RINIT := "A";           %  SOME OTHER PACKET TYPE, "ABORT"   02241000
   END CASE;                                                            02242000
END ELSE                                                                02243000
  RINIT := "A";                    %  SOME OTHER PACKET TYPE, ABORT     02244000
 END RINIT;                                                             02245000
                                                                        02246000
                                                                        02247000
 %                                                                      02248000
 %  R F I L E                                                           02249000
 %                                                                      02250000
 %  RECEIVE FILE HEADER                                                 02251000
                                                                        02252000
                                                                        02253000
REAL PROCEDURE RFILE;                                                   02254000
 BEGIN                                                                  02255000
  LABEL QUIT;                                                           02256000
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH             02257000
  ARRAY FILNAM1[0:MAXPACKWDS];     %  HOLDS THE CONVERTED FILE NAME     02258000
  POINTER NEWFILNAM;                                                    02259000
                                                                        02260000
  IF (NUMTRY:=*+1 LEQ PACKETRETRY+1) THEN                               02261000
  BEGIN                                                                 02262000
                                                                        02263000
    CASE RPACK(LEN,NUM,PACKET) OF%  GET A PACKET                        02264000
     BEGIN                                                              02265000
     "S":                          %  SEND-INIT, MAYBE OUR ACK LOST     02266000
          IF OLDTRY := *+1 LEQ PACKETRETRY+1 THEN                       02267000
          BEGIN                                                         02268000
           IF NUM = (N+63) MOD64                                        02269000
                THEN               %  PREVIOUS PACKET, MOD 64 ?         02270000
             BEGIN                 %  YES, ACK IT AGAIN WITH            02271000
               CALL1 := FALSE;     % [1.017] RPAR WAS ALREADY CALLED    02272000
               SPAR(LEN,PACKET);       %  OUR SEND-INIT PARAMETERS      02273000
               SPACK("Y",NUM,LEN,PACKET); % [1.019] FIX LENGTH PARAMETER02274000
               NUMTRY := 0;        %  RESET TRY COUNTER                 02275000
               RFILE := STATE;     %  STAY IN THIS STATE                02276000
             END                                                        02277000
           ELSE RFILE := "A";      %  NOT PREVIOUS PACKET, "ABORT"      02278000
          END ELSE                                                      02279000
            RFILE := "A";                                               02280000
                                                                        02281000
     "Z":                          %  END-OF-FILE                       02282000
          IF (OLDTRY := *+1 LEQ PACKETRETRY+1) THEN                     02283000
           BEGIN                                                        02284000
             IF NUM = (N+63) MOD64                                      02285000
              THEN                 %  PREVIOUS PACKET, MOD 64 ?         02286000
               BEGIN               %  YES, ACK IT AGAIN.                02287000
                SPACK("Y",NUM,0,NULLDATA);                              02288000
                NUMTRY := 0;                                            02289000
                RFILE := STATE;    %  STAY IN THIS STATE                02290000
               END                                                      02291000
             ELSE RFILE := "A";    %  NOT PREVIOUS PACKET, "ABORT"      02292000
           END ELSE                                                     02293000
             RFILE := "A";         %  ABORT IT                          02294000
                                                                        02295000
     "F":                          %  FILE HEADER (JUST WHAT WE WANT)   02296000
          IF NUM = N THEN          %  THE PACKET NUMBER MUST BE RIGHT   02297000
            BEGIN                                                       02298000
             IF LEN LEQ 17 THEN                                         02299000
             REPLACE PFILNAM BY """,PACKET FOR LEN WITH TOBURROUGHS,    02300000
                   "".",NULC                                            02301000
             ELSE                                                       02302000
             REPLACE PFILNAM BY """,POINTER(PACKET)+(LEN-17) FOR 17 WITH02303000
                    TOBURROUGHS, "".",NULC;                             02304000
             IF FP.OPEN THEN CLOSE(FP);                                 02305000
             IF KEEPFILE THEN FP.PROTECTION := VALUE(SAVE)              02306000
             ELSE             FP.PROTECTION := VALUE(TEMPORARY);        02307000
             REPLACE FILNAM1 BY PFILNAM FOR LEN+4 WITH ASCIITOEBCDIC;   02308000
             REPLACE FP.TITLE BY FILNAM1;                               02309000
             IF NOT FP.PRESENT THEN%  DIDN'T OPEN THE FILE              02310000
               BEGIN                                                    02311000
                REPLACE FBUF_ BY "CANNOT CREATE: ",PFILNAM FOR LEN,NULC;02312000
                 ERROR(FBUF_);                                          02313000
                 RFILE := "A";                                          02314000
                 GO QUIT;                                               02315000
               END                                                      02316000
             ELSE                  %  OK, GIVE MESSAGE                  02317000
              IF DEBUG THEN                                             02318000
                BUGN("RECEIVING:  ",PFILNAM);                           02319000
          RECSIZ_ := FP.MAXRECSIZE;                                     02320000
          UNITS_  := IF FP.UNITS=VALUE(CHARACTERS) THEN 1 ELSE 6;       02321000
          RESIZE(PBUF_,(RECSIZ_ * UNITS_ +6) DIV 6);% SET UP BUFFER SIZE02322000
          REPLACE PP_ := POINTER(PBUF_) BY " " FOR                      02323000
               PCNT_ := (RECSIZ_ * UNITS_) + 1;                         02324000
          SPACK("Y",N,0,NULLDATA); %  ACKNOWLEDGE THE FILE HEADER       02325000
          OLDTRY := NUMTRY;        %  RESET TRY COUNTERS                02326000
          NUMTRY := 0;             %  ...                               02327000
          N := (N+1) MOD64;        %  BUMP PACKET NUMBER, MOD 64        02328000
          RFILE := "D";            %  CASE TO DATA STATE                02329000
            END ELSE                                                    02330000
              RFILE := "A";                                             02331000
                                                                        02332000
     "B":                          %  BREAK TRANSMISSION (EOT)          02333000
          IF NUM = N THEN          %  NEED RIGHT PACKET NUMBER HERE     02334000
          BEGIN                                                         02335000
           SPACK("Y",N,0,NULLDATA);%  BUG OK                            02336000
           RFILE := "C";           %  GO TO COMPLETE STATE              02337000
          END ELSE                                                      02338000
            RFILE := "A";                                               02339000
                                                                        02340000
     "E":                          %  ERROR PACKET RECEIVED             02341000
          PRERRPKT(PACKET);        %  PRINT IT OUT AND                  02342000
          RFILE := "A";            %  ABORT                             02343000
                                                                        02344000
     "T":                         %  DIDN'T GET PACKET                  02345000
          SPACK("N",N,0,NULLDATA); %  RETURN A NAK                      02346000
          RFILE := STATE;          %  KEEP TRYING                       02347000
                                                                        02348000
    ELSE: RFILE := "A";            %  SOME OTHER PACKET, "ABORT"        02349000
  END CASE;                                                             02350000
    END ELSE                                                            02351000
 RFILE := "A";                     %  ABORT IF TOO MANY TRIES           02352000
QUIT:                                                                   02353000
 END RFILE;                                                             02354000
                                                                        02355000
                                                                        02356000
 %                                                                      02357000
 %  R D A T A                                                           02358000
 %                                                                      02359000
 %  RECEIVE DATA                                                        02360000
                                                                        02361000
                                                                        02362000
REAL PROCEDURE RDATA;                                                   02363000
 BEGIN                                                                  02364000
  REAL NUM, LEN;                   %  PACKET NUMBER, LENGTH             02365000
  IF NUMTRY:=*+1 LEQ PACKETRETRY+1 THEN                                 02366000
  BEGIN                                                                 02367000
                                                                        02368000
    CASE RPACK(LEN,NUM,PACKET) OF%  GET PACKET                          02369000
     BEGIN                                                              02370000
     "D":                          %  GOT DATA PACKET                   02371000
          IF NUM NEQ N THEN        %  RIGHT PACKET?                     02372000
            BEGIN                  %  NO                                02373000
             IF OLDTRY := *+1 LEQ PACKETRETRY+1 THEN                    02374000
              BEGIN                                                     02375000
               IF NUM = (N+63) MOD64                                    02376000
                THEN               %  ELSE CHECK PACKET NUMBER          02377000
                BEGIN              %  PREVIOUS PACKET AGAIN?            02378000
                 SPACK("Y",NUM,6,PACKET);   %  YES, RE-ACK IT           02379000
                 NUMTRY := 0;      %  RESET TRY COUNTER                 02380000
                 RDATA := STATE;   %  DON'T WRITE OUT DATA!             02381000
                END                                                     02382000
               ELSE RDATA := "A";  %  SORRY, WRONG NUMBER               02383000
              END                                                       02384000
             ELSE RDATA := "A";                                         02385000
            END ELSE                                                    02386000
           BEGIN                                                        02387000
                                   %  GOT DATA WITH RIGHT PACKET NUMBER 02388000
             BUFEMP(FP,PACKET,LEN);%  WRITE THE DATA TO THE FILE        02389000
             SPACK("Y",N,0,NULLDATA);%  ACKNOWLEDGE THE PACKET          02390000
             OLDTRY := NUMTRY;     %  RESET THE TRY COUNTERS            02391000
             NUMTRY := 0;          %  ...                               02392000
             N := (N+1) MOD64;     %  BUMP PACKET NUMBER, MOD 64        02393000
             RDATA := "D";         %  REMAIN IN DATA STATE              02394000
           END;                                                         02395000
                                                                        02396000
     "F":                          %  GOT A FILE HEADER                 02397000
          IF OLDTRY := *+1 LEQ PACKETRETRY+1 THEN                       02398000
          BEGIN                                                         02399000
           IF NUM = (N+63) MOD64                                        02400000
            THEN                   %  ELSE CHECK PACKET NUMBER          02401000
            BEGIN                  %  IT WAS THE PREVIOUS ONE           02402000
              SPACK("Y",NUM,0,NULLDATA);  %  ACK IT AGAIN               02403000
              NUMTRY := 0;         %  RESET TRY COUNTER                 02404000
              RDATA := STATE;      %  STAY IN DATA STATE                02405000
            END                                                         02406000
           ELSE RDATA := "A";      %  NOT PREVIOUS PACKET, "ABORT"      02407000
          END ELSE                                                      02408000
            RDATA := "A";          %  ABORT IT                          02409000
                                                                        02410000
     "Z":                          %  END-OF-FILE                       02411000
          IF NUM = N THEN          %  MUST HAVE RIGHT PACKET NUMBER     02412000
          BEGIN                                                         02413000
           SPACK("Y",N,0,NULLDATA);%  OK, ACK IT.                       02414000
           IF PCNT_ LSS RECSIZ_*UNITS_+1 THEN                           02415000
             BRD:=WRITE(FP,RECSIZ_,PBUF_);%    FLUSH THE BUFFER         02416000
           LOCK(FP,CRUNCH);        %  LOCK THE FILE                     02417000
           N := (N+1) MOD64;       %  BUMP PACKET NUMBER                02418000
           RDATA := "F";           %  GO BACK TO RECEIVE FILE STATE     02419000
          END ELSE                                                      02420000
           RDATA := "A";                                                02421000
                                                                        02422000
     "E":                          %  ERROR PACKET RECEIVED             02423000
           PRERRPKT(PACKET);       %  PRINT IT OUT AND                  02424000
           RDATA := "A";           %  ABORT                             02425000
                                                                        02426000
     "T":                         %  DIDN'T GET PACKET                  02427000
           SPACK("N",N,0,NULLDATA);%  RETURN A NAK                      02428000
           RDATA := STATE;         %  KEEP TRYING                       02429000
                                                                        02430000
    ELSE:  RDATA := "A";           %  SOME OTHER PACKET, "ABORT"        02431000
END CASE;                                                               02432000
    END ELSE                                                            02433000
 RDATA := "A";                     %  ABORT IF TOO MANY TRIES           02434000
 END RDATA;                                                             02435000
                                                                        02436000
%   MAIN LINE TO RECSW                                                  02437000
                                                                        02438000
                                                                        02439000
                                                                        02440000
  STATE := ISTATE;                 % [1.017] START STATE IS PASSED IN   02441000
  % [1.017] N := 0;                  %  INITIALIZE MESSAGE NUMBER       02442000
  NUMTRY := 0;                     %  BUG NO TRIES YET                  02443000
  DONTQUIT := TRUE;                %  LOOP INITIALIZATION               02444000
  REM.TIMELIMIT := IF TIMINT LSS MINTIM OR TIMINT GTR MAXTIM THEN       02445000
                          MYTIME ELSE TIMINT;                           02446000
                                                                        02447000
  WHILE DONTQUIT DO                                                     02448000
    BEGIN                                                               02449000
      IF DEBUG THEN BUGC("RECSW STATE:  ",STATE);                       02450000
      CASE STATE OF                                                     02451000
       BEGIN                                                            02452000
       "R": STATE := RINIT;        %  RECEIVE-INIT                      02453000
       "F": STATE := RFILE;        %  RECEIVE-FILE                      02454000
       "D": STATE := RDATA;        %  RECEIVE-DATA                      02455000
       "C": RECSW := TRUE;         %  COMPLETE STATE                    02456000
            DONTQUIT := FALSE;     %  LET'S QUIT                        02457000
       "A": RECSW := FALSE;        %  "ABORT" STATE                     02458000
            DONTQUIT := FALSE;     %  LET'S QUIT                        02459000
      ELSE: RECSW := FALSE;        %  UNKNOWN STATE                     02460000
            DONTQUIT := FALSE;     %  LET'S QUIT                        02461000
       END CASE;                                                        02462000
   END WHILE;                                                           02463000
   REM.TIMELIMIT := 0;          %  DISABLE REMOTE INPUT TIMELIMIT       02464000
 END RECSW;                                                             02465000
                                                                        02466000
 $ENDSEGMENT                                                            02467000
                                                                        02468000
                                                                        02469000
 %                                                                      02470000
 % KERMIT UTILITIES.                                                    02471000
 %                                                                      02472000
                                                                        02473000
                                                                        02474000
 %                                                                      02475000
 %  S P A C K                                                           02476000
 %                                                                      02477000
 %  SEND A PACKET                                                       02478000
                                                                        02479000
                                                                        02480000
 $BEGINSEGMENT                                                          02481000
                                                                        02482000
PROCEDURE SPACK(TYPE,NUM,LEN,DATA);                                     02483000
VALUE TYPE,NUM,LEN;                                                     02484000
REAL TYPE;                                                              02485000
REAL NUM, LEN;                                                          02486000
ARRAY DATA[0];                                                          02487000
 BEGIN                                                                  02488000
  REAL I;                          %  CHARACTER LOOP COUNTER            02489000
  REAL CHKSUM;                     %  CHECKSUM                          02490000
  ARRAY BUFFER[0:MAXPACKWDS+1+(PAD+5)DIV 6];  %  PACKET BUFFER          02491000
  POINTER PB,PD;                   %  BUFFER POINTERS                   02492000
                                                                        02493000
  PD := POINTER(DATA);                                                  02494000
  REPLACE PD + LEN BY NULC;                                             02495000
  IF REAL(DEBUG) GTR 1 THEN        %  DISPLAY OUTGOING PACKET           02496000
   BEGIN                                                                02497000
     BUGC("SPACK TYPE:  ",TYPE);                                        02498000
     BUG1("NUM:  ",NUM);                                                02499000
     BUG1("LEN:  ",LEN);                                                02500000
     IF LEN GTR 0 THEN                                                  02501000
       BUGN("DATA:  ",DATA);                                            02502000
   END;                                                                 02503000
                                                                        02504000
  IF PAD GTR 0 THEN                                                     02505000
    REPLACE PB:PB:=POINTER(BUFFER) BY CH(PCHAR,PAD)                     02506000
  ELSE                                                                  02507000
    PB := POINTER(BUFFER);                                              02508000
                                                                        02509000
  REPLACE PB:PB BY CH(SOHCHAR,1),  %  PACKET MARKER, ASCII 1 (SOH)      02510000
         CH(TOCHAR(LEN+3),1),      %  SEND THE CHARACTER COUNT          02511000
           CH(TOCHAR(NUM),1),      %  PACKET NUMBER                     02512000
                  CH(TYPE,1);      %  PACKET TYPE                       02513000
  CHKSUM := TOCHAR(LEN+3)          %  CHECKSUM CHARACTER COUNT          02514000
          + TOCHAR(NUM)            %  CHECKSUM PACKET NUMBER            02515000
          + TYPE;                  %  CHECKSUM PACKET TYPE              02516000
                                                                        02517000
  FOR I:=0 STEP 1 UNTIL LEN-1 DO   %  LOOP FOR ALL DATA CHARACTERS      02518000
    BEGIN                                                               02519000
      CHKSUM := *+REAL(PD,1);      %  UPDATE CHECKSUM                   02520000
      REPLACE PB:PB BY PD:PD FOR 1;% GET A CHARACTER                    02521000
    END;                                                                02522000
  CHKSUM := *.[7:8];               %  SIMULATE CHAR ARITHMETIC          02523000
  CHKSUM := * + CHKSUM.[7:2];                                           02524000
  CHKSUM := *.[5:6];               %  COMPUTE FINAL CHECKSUM            02525000
  IF CHKSUM=0 THEN                 %  WE MAY STRIP TRAILING BLANKS...   02526000
    BEGIN                                                               02527000
    REPLACE PB:PB BY CH(TOCHAR(CHKSUM),1),% SO LET'S PUT IN A DUMMY     02528000
                    "?",CH(EOL,1); %  ? BEFORE ANOTHER EOL...           02529000
    LEN := *+2;                    %  WE HAVE TO WRITE OUT 2 MORE, NOW  02530000
    END                                                                 02531000
  ELSE                             %  NO NEED TO WORRY ABOUT IT         02532000
    REPLACE PB:PB BY CH(TOCHAR(CHKSUM),1);%  PUT IT IN THE PACKET       02533000
  REPLACE PB:PB BY CH(EOL,1);      %  PUT IN LINE TERMINATOR            02534000
  IF BRD := WRITE(REM[STOP],LEN+6+PAD,BUFFER) THEN                      02535000
    BEGIN                                                               02536000
      I := 3+1;                    %  TRY 3 TIMES                       02537000
      WHILE BRD AND I := *-1 GTR 0 DO                                   02538000
        BEGIN                                                           02539000
          IF DEBUG THEN BUGH("SPACK WRITE ERROR (HEX) = ",RD);          02540000
          WHEN(.5);                %  WAIT A HALF SECOND                02541000
          BRD := WRITE(REM,LEN+6+PAD,BUFFER);                           02542000
        END;                       %  TRY THE IO AGAIN                  02543000
    END;                                                                02544000
  IF REAL(DEBUG) GTR 1 THEN                                             02545000
    BUG1("CSUM:  ",CHKSUM);                                             02546000
  WHEN(.1);                        %  ALLOW FOR TURN AROUND DELAY       02547000
 END SPACK;                                                             02548000
                                                                        02549000
 %                                                                      02550000
 %  R P A C K                                                           02551000
 %                                                                      02552000
 %  READ A PACKET                                                       02553000
                                                                        02554000
                                                                        02555000
REAL PROCEDURE RPACK(LEN,NUM,DATA);                                     02556000
REAL LEN, NUM;                    %  PACKET LENGTH, NUMBER              02557000
ARRAY DATA[0];                    %  PACKET DATA                        02558000
 BEGIN                                                                  02559000
  LABEL GOTSOH,QUIT;                                                    02560000
  REAL I;                         %  DATA CHARACTER NUMBER, LOOP EXIT   02561000
  REAL     R,                     %  CURRENT INPUT CHARACTER            02562000
        TYPE,                     %  PACKET TYPE                        02563000
     CCHKSUM,                     %  OUR (COMPUTED) CHECKSUM            02564000
     RCHKSUM,                     %  CHECKSUM RECEIVED FROM OTHER HOST  02565000
     COL;                         %  COLUMN COUNTER FOR PB              02566000
  BOOLEAN DONE;                   %  ARE WE THRU?                       02567000
  ARRAY BUFFER[0:49];                                                   02568000
  POINTER PB,PD;                                                        02569000
  DEFINE ABORT =                   %  TIMED OUT OR DEFICIENT PACKET     02570000
       BEGIN                                                            02571000
         RPACK := "T";             %  DEFAULT TO TIMED OUT OR SHORT PACK02572000
         IF BRD.TIMEOUTBIT THEN                                         02573000
           IF REAL(DEBUG) GTR 1 THEN                                    02574000
             BUG("TIMED OUT")                                           02575000
           ELSE                                                         02576000
         ELSE                                                           02577000
         IF BRD THEN               %  SOME OTHER ERROR                  02578000
           BEGIN                                                        02579000
             IF DEBUG THEN                                              02580000
               BUGH("ERROR ON READ (HEX) = ",RD);                       02581000
             RPACK := 0;           %  ABORT ... NOW                     02582000
           END                                                          02583000
         ELSE                      %  NO ERROR - MUST BE A SHORT PACKET 02584000
         IF DEBUG THEN                                                  02585000
           BEGIN                                                        02586000
             BUG("SHORT PACKET");                                       02587000
             BUG1("CHARACTERS LEFT=",COL);                              02588000
             IF COL GTR 0 THEN                                          02589000
               BUGN("WHICH ARE      :",PB);                             02590000
             BUGN("BUFFER IS      :",BUFFER);                           02591000
           END;                                                         02592000
         RD := 0;                  %  RESET RESULT DESCRIPTOR           02593000
         GO QUIT;                                                       02594000
       END#;                                                            02595000
                                                                        02596000
                                                                        02597000
  DO                                                                    02598000
    BEGIN                                                               02599000
      IF BRD := READ(REM,300,BUFFER) THEN ABORT;                        02600000
      SCAN PB:PB:=POINTER(BUFFER)  %  FIND A SOH HEADER                 02601000
          FOR COL:RD.LENGTHF UNTIL = MYSOH;                             02602000
      IF COL := *-1 GTR 0 THEN     %  ADJUST POINTER AFTER SOH          02603000
          PB := *+1;                                                    02604000
    END UNTIL COL GTR 0;                                                02605000
                                                                        02606000
  DONE := FALSE;                  %  GOT SOH, INIT LOOP                 02607000
GOTSOH:                                                                 02608000
                                                                        02609000
  IF COL LSS 4 THEN ABORT;        %  NOT A WHOLE PACKET LEFT            02610000
  WHILE NOT DONE DO               %  LOOP TO GET A PACKET               02611000
   BEGIN                                                                02612000
% [1.017]     IF HIBITOK THEN                                           02613000
% [1.017]       R := REAL(PB,1)            %  TAKE 8 BITS               02614000
% [1.017]     ELSE                                                      02615000
       R := REAL(PB,1).[6:7];     %  HANDLE PARITY                      02616000
     PB := *+1; COL := *-1;       %  BUMP THE POINTER                   02617000
     IF R = MYSOH THEN GO TO GOTSOH;%  RESYNCHRONIZE IF SOH             02618000
     CCHKSUM := R;                %  START THE CHECKSUM                 02619000
     LEN := UNCHAR(R)-3;          %  CHARACTER COUNT                    02620000
                                                                        02621000
% [1.017]     IF HIBITOK THEN                                           02622000
% [1.017]       R := REAL(PB,1)            %  TAKE 8 BITS               02623000
% [1.017]     ELSE                                                      02624000
       R := REAL(PB,1).[6:7];     %  HANDLE PARITY                      02625000
     PB := *+1; COL := *-1;       %  BUMP THE POINTER                   02626000
     IF R = MYSOH THEN GO TO GOTSOH;%  RESYNCHRONIZE IF SOH             02627000
     CCHKSUM := * + R;            %  UPDATE CHECKSUM                    02628000
     NUM := UNCHAR(R);            %  PACKET NUMBER                      02629000
                                                                        02630000
% [1.017]     IF HIBITOK THEN                                           02631000
% [1.017]       R := REAL(PB,1)            %  TAKE 8 BITS               02632000
% [1.017]     ELSE                                                      02633000
       R := REAL(PB,1).[6:7];     %  HANDLE PARITY                      02634000
     PB := *+1; COL := *-1;       %  BUMP THE POINTER                   02635000
     IF R = MYSOH THEN GO TO GOTSOH;%  SYNCHRONIZE IF SOH               02636000
     CCHKSUM := * + R;            %  UPDATE CHECKSUM                    02637000
     TYPE := R;                   %  PACKET TYPE                        02638000
                                                                        02639000
     IF COL LSS LEN+1 THEN ABORT; %  NOT A WHOLE PACKET LEFT            02640000
     REPLACE PD := POINTER(DATA) BY NULC FOR MAXPACKSIZ;                02641000
     FOR I := 0 STEP 1 UNTIL LEN-1 DO                                   02642000
       BEGIN                      %  LOOP FOR CHARACTER COUNT           02643000
% [1.017]   IF HIBITOK THEN                                             02644000
% [1.017]     R := REAL(PB,1)        %  TAKE 8 BITS                     02645000
% [1.017]   ELSE                                                        02646000
           R := REAL(PB,1).[6:7]; %  HANDLE PARITY                      02647000
         IF R = MYSOH THEN GO GOTSOH;%  RESYNCH IF SOH                  02648000
         CCHKSUM := * + R;        %  UPDATE CHECKSUM                    02649000
         REPLACE PD:PD BY PB:PB FOR 1;%PUT IT IN THE DATA BUFFER        02650000
         COL := *-1;              %  BUMP THE POINTER                   02651000
       END;                                                             02652000
                                                                        02653000
     R := REAL(PB,1);             %  TAKE 8 BITS                        02654000
     PB := *+1; COL := *-1;       %  BUMP THE POINTER                   02655000
     RCHKSUM := UNCHAR(R);        %  CONVERT TO NUMERIC                 02656000
% [1.017] IF HIBITOK THEN           %  GET EOL CHARACTER AND TOSS IT    02657000
% [1.017]   R := REAL(PB,1)            %  TAKE 8 BITS                   02658000
% [1.017] ELSE                                                          02659000
       R := REAL(PB,1).[6:7];     %  HANDLE PARITY                      02660000
     PB := *+1; COL := *-1;       %  BUMP THE POINTER                   02661000
     IF R = MYSOH THEN GO TO GOTSOH;%  RESYNCHRONIZE IF SOH             02662000
     DONE := TRUE;                %  GOT CHECKSUM, DONE                 02663000
   END;                                                                 02664000
                                                                        02665000
   IF REAL(DEBUG) GTR 1 THEN      %  DISPLAY INCOMING PACKET            02666000
    BEGIN                                                               02667000
     BUGC("RPACK TYPE:  ",TYPE);                                        02668000
     BUG1("NUM:  ",NUM);                                                02669000
     BUG1("LEN:  ",LEN);                                                02670000
     IF LEN GTR 0 THEN                                                  02671000
       BUGN("DATA:  ",DATA);                                            02672000
    END;                                                                02673000
                                  %  FOLD IN BITS 7:8 TO COMPUTE        02674000
   CCHKSUM := * + CCHKSUM.[7:2];  %  FINAL CHECKSUM                     02675000
   CCHKSUM := *.[5:6];                                                  02676000
                                                                        02677000
   IF CCHKSUM NEQ RCHKSUM THEN                                          02678000
     RPACK := 0                                                         02679000
   ELSE                                                                 02680000
     RPACK := TYPE;               %  ALL OK, RETURN PACKET TYPE         02681000
  IF REAL(DEBUG) GTR 1 THEN                                             02682000
    BEGIN                                                               02683000
      BUG1("CCSUM:  ",CCHKSUM);                                         02684000
      BUG1("RCSUM:  ",RCHKSUM);                                         02685000
    END;                                                                02686000
 QUIT:                                                                  02687000
   WHEN(MYPAUSE);                 %  WAIT BEFORE SENDING ACK            02688000
 END RPACK;                                                             02689000
                                                                        02690000
 $ENDSEGMENT                                                            02691000
                                                                        02692000
                                                                        02693000
 %                                                                      02694000
 %  B U F I L L                                                         02695000
 %                                                                      02696000
 %  GET A BUFFERFUL OF DATA FROM THE FILE THAT'S BEING SENT.            02697000
 %  CONTROL-QUOTING,  8-BIT & REPEAT COUNT PREFIXES ARE ALL             02698000
 %  HANDLED.                                                            02699000
                                                                        02700000
                                                                        02701000
REAL PROCEDURE BUFILL(FID,BUFFER);                                      02702000
FILE FID;                        %  DISK FILE TO FILL FROM              02703000
ARRAY BUFFER[0];                 %  BUFFER                              02704000
 BEGIN                                                                  02705000
  LABEL QUIT;                                                           02706000
  REAL I,                         %  LOOP INDEX                         02707000
       LASTT,                     %  PREVIOUS CHARACTER READ FROM FILE  02708000
       CNT,                       %  COUNT OF IDENTICAL PREVIOUS CHARS  02709000
       T;                         %  CHAR READ FROM FILE                02710000
  REAL T7;                        %  7-BIT VERSION OF ABOVE             02711000
  BOOLEAN BT = T,                 %  A BOOLEAN VERSION OF T             02712000
          REPTON;                 % [1.017] TRUE WHEN REPEAT IN ACTION  02713000
  POINTER PB,                     %  POINTER TO BUFFER                  02714000
          PBSAVE;                 % [1.017] PTS TO SLOT FOR REPEAT COUNT02715000
                                                                        02716000
  REAL PROCEDURE FIXIT;        %  FIXES UP BINARY QUOTING, ETC          02717000
   BEGIN                                                                02718000
      IF QBIN NEQ "N" THEN         % [1.019]  WE MAY USE 8-BIT STUFF    02719000
       IF BT.[7:1] THEN            % [1.019]  HIGH BIT IS ON            02720000
        BEGIN                      % [1.019]                            02721000
          REPLACE PB:PB BY CH(QBIN,1);                                  02722000
          FIXIT := *+1;            % [1.019]                            02723000
        END;                       % [1.019]                            02724000
     IF T7 IN ACNTRL[0] THEN % CONTROL, QUOTE, QBIN, REPT               02725000
      BEGIN                                                             02726000
        REPLACE PB:PB BY CH(QUOTE,1);                                   02727000
        IF T7 IN BCNTRL[0] THEN    %  DON'T CTL IT                      02728000
          REPLACE PB:PB BY CH(T7,1)%  PUT IT OUT STRAIGHT               02729000
        ELSE                      %  IT'S 0 <= N <= 31 OR 127           02730000
        IF T7 = NL THEN            %  TREAT NL SPECIALLY                02731000
         IF NOT HIBITOK THEN       % [1.019]                            02732000
          BEGIN                                                         02733000
            REPLACE PB:PB BY CH(CTL(CR),1),                             02734000
                             CH(QUOTE,1),                               02735000
                             CH(CTL(T7),1);                             02736000
            FIXIT := *+2;         %  WE'LL COUNT THE CHARACTER LATER    02737000
          END                                                           02738000
         ELSE                     %  CAN'T ADD CR IF USING 8 BITS       02739000
          REPLACE PB:PB BY CH(CTL(T7),1)                                02740000
        ELSE                      %  IT WASN'T A CR, ANYWAY             02741000
          REPLACE PB:PB BY CH(CTL(T7),1);                               02742000
        FIXIT := *+1;             %  ADD 1 FOR THE QUOTE                02743000
      END                                                               02744000
     ELSE                         %  NOT A CONTROL CHARACTER            02745000
      REPLACE PB:PB BY CH(T7,1);   %  PUT IT OUT NORMALLY               02746000
     FIXIT := *+1;                %  FINALLY ADD IN THE LAST ONE        02747000
  END FIXIT;                                                            02748000
                                                                        02749000
                                                                        02750000
  PB := POINTER(BUFFER);                                                02751000
  I := 0;                         %  INIT DATA BUFFER POINTER           02752000
  WHILE (T:=GETC(FID)) NEQ EOF DO %  GET THE NEXT CHARACTER             02753000
   BEGIN                                                                02754000
     T7 := T.[6:7];               %  GET LOW ORDER 7 BITS               02755000
                                                                        02756000
     IF (T = LASTT)AND(REPTOK) THEN    % [1.017] CHECK FOR REPEAT       02757000
     BEGIN                             % [1.017]                        02758000
       IF (I >0) THEN                  % [1.017] COUNT UNLESS FIRST CHAR02759000
       BEGIN                           % [1.017]   IN PACKET            02760000
         CNT := *+1;                   % [1.017]                        02761000
       END;                            % [1.017]                        02762000
          % PROCESS CHAR NORMALLY UNTIL REPEAT THRESHOLD EXCEEDED       02763000
       IF (NOT REPTON) THEN            % [1.017]                        02764000
       BEGIN                           % [1.017]                        02765000
          % WHEN THRESHOLD EXCEEDED, BACKUP AND INSERT REPEAT PREFIX    02766000
         IF (CNT > REPTTHRESH) THEN    % [1.017]                        02767000
         BEGIN                         % [1.017]                        02768000
           IF (T7 IN ACNTRL[0]) THEN   % [1.017]                        02769000
           BEGIN                       % [1.017]                        02770000
             PB := *-5;                % [1.017] BACKUP FOR CTL QUOTE   02771000
             I := *-5;                 % [1.017]                        02772000
           END;                        % [1.017]                        02773000
           PB := *-5;                  % [1.017] BACKUP FOR CHAR ITSELF 02774000
           I := *-5;                   % [1.017]                        02775000
           REPLACE PB:PB BY CH(REPT,1);% [1.017] INSERT REPT QUOTE      02776000
           I := *+1;                   % [1.017]                        02777000
           PBSAVE := PB;               % [1.017] LEAVE SLOT FOR COUNT   02778000
           PB := *+1;                  % [1.017]                        02779000
           I := *+1;                   % [1.017]                        02780000
           REPTON := TRUE;             % [1.017] SET REPEAT FLAG        02781000
         END;                          % [1.017]                        02782000
         I := *+FIXIT;                 % [1.017] INSERT CHAR WITH QUOTES02783000
       END;                            % [1.017]                        02784000
       IF (CNT > MAXREPT) THEN         % [1.017] CHECK FOR REPEAT LIMIT 02785000
       BEGIN                           % [1.017]                        02786000
         REPLACE PBSAVE BY CH(TOCHAR(CNT),1); % FILL COUNT SLOT         02787000
         REPTON := FALSE;              % [1.017] RESET REPEAT FLAG      02788000
         CNT := 0;                     % [1.017]                        02789000
       END;                            % [1.017]                        02790000
     END                               % [1.017]                        02791000
     ELSE                              % [1.017]                        02792000
     BEGIN                             % [1.017]                        02793000
       IF (REPTON) THEN                % [1.017] CHECK FOR END OF REPEAT02794000
       BEGIN                           % [1.017]                        02795000
         REPLACE PBSAVE BY CH(TOCHAR(CNT),1); % FILL COUNT SLOT         02796000
         REPTON := FALSE;              % [1.017]                        02797000
       END;                            % [1.017]                        02798000
       CNT := 1;                       % [1.017]                        02799000
       I := *+FIXIT;                 % [1.017] INSERT CHAR WITH QUOTING 02800000
       LASTT := T;                     % [1.017]                        02801000
     END;                              % [1.017]                        02802000
                                                                        02803000
     IF I GEQ SPSIZ-11 THEN  %  ALLOW FOR SOH,LEN,#,TYP,CHK,?,EOL,EOL,UP02804000
       BEGIN                                                            02805000
         BUFILL := I;             %  CHECK LENGTH                       02806000
         GO TO QUIT;                                                    02807000
       END;                                                             02808000
   END WHILE;                                                           02809000
  BUFILL := I;                    %  HANDLE PARTIAL BUFFER              02810000
QUIT:                                                                   02811000
 END BUFILL;                                                            02812000
                                                                        02813000
                                                                        02814000
 %                                                                      02815000
 % B U F E M P                                                          02816000
 %                                                                      02817000
 %  PUT DATA FROM AN INCOMING PACKET INTO A FILE.                       02818000
                                                                        02819000
                                                                        02820000
PROCEDURE BUFEMP(FID,BUFFER,LEN);                                       02821000
VALUE LEN;                                                              02822000
REAL LEN;                                                               02823000
FILE FID;                                                               02824000
ARRAY BUFFER[0];                  %  BUFFER                             02825000
 BEGIN                                                                  02826000
  REAL I;                         %  COUNTER                            02827000
  REAL T;                         %  CHARACTER HOLDER                   02828000
  BOOLEAN HIBIT;                  %  8 BIT STUFF                        02829000
  REAL CNT;                       %  REPEAT COUNT                       02830000
  POINTER PB;                     %  BUFFER POINTER                     02831000
                                                                        02832000
  PB := POINTER(BUFFER);                                                02833000
  FOR I:=0 STEP 1 UNTIL LEN-1 DO  % LOOP THRU THE DATA FIELD            02834000
   BEGIN                                                                02835000
    HIBIT := FALSE;               %  INITIALIZE IT                      02836000
    CNT := 1;                     %  WE HAVE 1 CHARACTER AT LEAST       02837000
    T := REAL(PB,1);              %  GET CHARACTER                      02838000
    PB := *+1;                                                          02839000
    IF REPTOK THEN                % WE CAN USE REPEAT COUNTS            02840000
     IF T = MYREPT THEN           %  WE ARE REPEATING                   02841000
      BEGIN                                                             02842000
        CNT := UNCHAR(REAL(PB,1));%  GET THE COUNT                      02843000
        PB := *+1; I := *+1;      %  BUMP THE POINTER                   02844000
        T := REAL(PB,1);          %  GET THE NEXT CHARACTER             02845000
        PB := *+1; I := *+1;      %  BUMP THE POINTER                   02846000
      END;                                                              02847000
    IF HIBITOK THEN               %  WE CAN QUOTE 8-BIT STUFF           02848000
     IF T = MYQBIN THEN           %  WE HAVE AN 8-BIT THING             02849000
      BEGIN                                                             02850000
        HIBIT := TRUE;            %  SET THE FLAG                       02851000
        T := REAL(PB,1);          %  GET THE NEXT CHARACTER             02852000
        PB := *+1; I := *+1;      %  BUMP THE POINTER                   02853000
      END;                                                              02854000
    IF T = MYQUOTE THEN           %  WE HAVE A QUOTED THING             02855000
      BEGIN                                                             02856000
        T := REAL(PB,1);          %  GET THE NEXT CHARACTER             02857000
        PB := *+1; I := *+1;      %  BUMP THE POINTER                   02858000
        IF NOT T IN BCNTRL[0] THEN%  IT'S NOT QUOTE, QBIN OR REPT       02859000
          T := CTL(T);            %  UNCONTROLIFY IT                    02860000
      END;                                                              02861000
    IF HIBIT THEN T := * & 1[7:1];%  SET THE 8-TH BIT                   02862000
    THRU CNT DO                                                         02863000
     IF T = HT THEN               %  IS IT A TAB?                       02864000
      IF EXPTABS THEN             %  WE NEED TO EXPAND IT               02865000
        THRU (TABLEN-((RECSIZ_*UNITS_-PCNT_) MOD TABLEN)) DO            02866000
               PUTC(SP,FID)       %  FILL IN WITH SPACES                02867000
      ELSE                                                              02868000
        PUTC(T,FID)               %  JUST PUT OUT THE TAB               02869000
     ELSE                         %  IT'S NOT A TAB                     02870000
     IF  T = CR THEN           % [1.017]  IT'S A CR                     02871000
      IF (HIBITOK)OR(RAW) THEN             %  DON'T FIDDLE WITH IT      02872000
        PUTC(T,FID)               %  PUT OUT A CR                       02873000
      ELSE                        %  IT'S PROBABLY EXTRA, SO            02874000
                                  %  JUST EAT IT!                       02875000
     ELSE                         %  NOT A CR, EITHER                   02876000
       PUTC(T,FID);               %  PUT IT OUT                         02877000
   END FOR LOOP;                                                        02878000
                                                                        02879000
 END BUFEMP;                                                            02880000
                                                                        02881000
                                                                        02882000
 %                                                                      02883000
 %  S P A R                                                             02884000
 %                                                                      02885000
 %  FILL THE DATA ARRAY WITH MY SEND-INIT PARAMETERS                    02886000
 %                                                                      02887000
                                                                        02888000
                                                                        02889000
 $BEGINSEGMENT                                                          02890000
                                                                        02891000
PROCEDURE SPAR(LEN,DATA);                                               02892000
REAL LEN;                                                               02893000
ARRAY DATA[0];                                                          02894000
 BEGIN                                                                  02895000
   DEFINE FORCESEGMENT=#;         % SO BEGINSEGMENT WILL WORK           02896000
  POINTER PD;                     % [1.017] TEMPORARY POINTER           02897000
  REPLACE PD:PD := POINTER(DATA) BY                                     02898000
        CH(TOCHAR(MYPACKSIZ),1) , %  BIGGEST PACKET I CAN RECEIVE       02899000
        CH(TOCHAR(MYTIME),1)    , %  WHEN I WANT TO BE TIMED OUT        02900000
        CH(TOCHAR(MYPAD),1)     , %  HOW MUCH PADDING I NEED            02901000
        CH(CTL(MYPCHAR),1)      , %  PADDING CHARACTER I WANT           02902000
        CH(TOCHAR(MYEOL),1)     , %  END-OF-LINE CHARACTER I WANT       02903000
        CH(MYQUOTE,1)           ; %  CONTROL-QUOTE CHARACTER I SEND     02904000
  IF CALL1 THEN                      % [1.019]                          02905000
    % IF SPAR IS CALLED FIRST (BEFORE RPAR) WE CONTROL                  02906000
    %  WHETHER OR NOT 8TH BIT QUOTING CAN BE DONE                       02907000
    IF (BINARYON) THEN               % [1.019]                          02908000
       REPLACE PD:PD BY CH(MYQBIN,1) % [1.019] REQUEST 8TH BIT QUOTING  02909000
    ELSE                             % [1.019]                          02910000
       REPLACE PD:PD BY "N"          % [1.019] PREVENT 8TH BIT QUOTING  02911000
  ELSE                               % [1.019]                          02912000
    % IF SPAR IS CALLED SECOND (AFTER RPAR) WE                          02913000
    % RESPOND TO THE REQUEST FROM THE REMOTE KERMIT                     02914000
    IF (BINARYON)AND(HIBITOK) THEN   % [1.019]                          02915000
      % IF 8TH BIT QUOTING REQUESTED, ACCEPT IF WE ARE IN BINARY MODE   02916000
      IF (QBIN = "Y") THEN           % [1.019] USE OUR QBIN CHAR        02917000
      BEGIN                          % [1.019]                          02918000
        REPLACE PD:PD BY CH(MYQBIN,1); % [1.019]                        02919000
        QBIN := MYQBIN;              % [1.019]                          02920000
      END                            % [1.019]                          02921000
      ELSE                           % [1.019]                          02922000
      BEGIN                          % [1.019]                          02923000
        REPLACE PD:PD BY "Y";        % [1.019] ACK 8BIT QUOTE REQUEST   02924000
        MYQBIN := QBIN;              % [1.019] USE INCOMING QBIN CHAR   02925000
      END                            % [1.019]                          02926000
    ELSE                             % [1.019]                          02927000
    BEGIN                            % [1.019]                          02928000
      % 8TH BIT QUOTING WILL NOT BE DONE                                02929000
      REPLACE PD:PD BY "N";          % [1.019] NAK 8TH BIT QUOTING      02930000
      HIBITOK := FALSE;              % [1.019]                          02931000
    END;                             % [1.019]                          02932000
                                     % [1.019]                          02933000
  REPLACE PD:PD BY                                                      02934000
        CH(MYCHKTYPE,1)         ; % [1.017] STANDARD CHECKTYPE          02935000
  IF CALL1 THEN                             % [1.017]                   02936000
  BEGIN                                     % [1.017]                   02937000
     % REQUEST REPEAT CHAR PROCESSING                                   02938000
    REPLACE PD BY CH(MYREPT,1);             % [1.017]                   02939000
    CALL1 := FALSE;                         % [1.017]                   02940000
  END                                       % [1.017]                   02941000
  ELSE                                      % [1.017]                   02942000
  BEGIN                                     % [1.017]                   02943000
     % ACKNOWLEDGE REPEAT PROCESSING IF IT WAS REQUESTED                02944000
    IF (REPTOK) THEN                        % [1.017]                   02945000
      REPLACE PD BY CH(REPT,1)          % [1.017]                       02946000
    ELSE                                    % [1.017]                   02947000
      REPLACE PD BY CH(SP,1);               % [1.017]                   02948000
    CALL1 := TRUE;                          % [1.017]                   02949000
  END;                                      % [1.017]                   02950000
  LEN := 9;                                 % [1.017]                   02951000
  IF REAL(DEBUG) GTR 1 THEN       %  EXPAND IT ALL                      02952000
    BEGIN                                                               02953000
      BUG1("My packet size = ",MYPACKSIZ);                              02954000
      BUG1("My timeout = ",MYTIME);                                     02955000
      BUG1("My padding = ",MYPAD);                                      02956000
      BUGH("My padding character = ",MYPCHAR);                          02957000
      BUGH("My end of line character = ",MYEOL);                        02958000
      BUGC("My quote character = ",MYQUOTE);                            02959000
      BUGC("My binary quote character = ",MYQBIN);                      02960000
      BUGC("My checksum type = ",MYCHKTYPE);                            02961000
      BUGC("My repeat character = ",MYREPT);                            02962000
      IF REPTOK THEN                                                    02963000
        BUG("WE ARE REPEATING")                                         02964000
      ELSE                                                              02965000
        BUG("NO REPEAT CHARACTER");                                     02966000
      IF HIBITOK THEN                                                   02967000
        BUG("WE ARE BINARY QUOTING")                                    02968000
      ELSE                                                              02969000
        BUG("NOT BINARY QUOTING");                                      02970000
     END;                                                               02971000
 END SPAR;                                                              02972000
                                                                        02973000
                                                                        02974000
 %   R P A R                                                            02975000
 %                                                                      02976000
 %  GET THE OTHER HOST'S SEND-INIT PARAMETERS                           02977000
 %                                                                      02978000
                                                                        02979000
                                                                        02980000
PROCEDURE RPAR(LEN,DATA);                                               02981000
REAL LEN;                                                               02982000
ARRAY DATA[0];                                                          02983000
 BEGIN                                                                  02984000
  POINTER PD;                                                           02985000
  PD := POINTER(DATA);                                                  02986000
                                                                        02987000
                                                                        02988000
  SPSIZ  := UNCHAR(REAL(PD,1)); %  MAXIMUM SEND PACKET SIZE             02989000
  PD := *+1;                                                            02990000
  TIMINT := UNCHAR(REAL(PD,1)); %  WHEN I SHOULD TIME OUT               02991000
  PD := *+1;                                                            02992000
  PAD    := UNCHAR(REAL(PD,1)); %  NUMBER OF PADS TO SEND               02993000
  PD := *+1;                                                            02994000
  PCHAR:= CTL(REAL(PD,1));      %  PADDING CHARACTER TO SEND            02995000
  PD := *+1;                                                            02996000
  EOL    := UNCHAR(REAL(PD,1)); %  EOL CHARACTER I MUST SEND            02997000
  PD := *+1;                                                            02998000
  UNTABLE(ACNTRL,QUOTE);           %  TAKE IT OUT OF THE ATABLE         02999000
  UNTABLE(BCNTRL,QUOTE);           %  TAKE IT OUT OF THE BTABLE         03000000
  QUOTE  := REAL(PD,1);            %  INCOMING DATA QUOTE CHARACTER     03001000
  TABLEIT(ACNTRL,QUOTE);           %  PUT NEW ONE IN THE ATABLE         03002000
  TABLEIT(BCNTRL,QUOTE);           %  PUT NEW ONE IN THE BTABLE         03003000
     % CHECK FOR REQUEST/ACKNOWLEDGE FOR 8TH BIT QUOTING                03004000
  IF LEN := *-6 GTR 0 THEN         % [1.019]                            03005000
  BEGIN                            % [1.019]                            03006000
    PD := *+1;                     % [1.019] SKIP PAST QUOTE            03007000
    UNTABLE(ACNTRL,QBIN);          % [1.019] TAKE OUT OF ATABLE         03008000
    UNTABLE(BCNTRL,QBIN);          % [1.019] TAKE OUT OF BTABLE         03009000
    QBIN := REAL(PD,1);            % [1.019] INCOMING 8BIT QUOTE        03010000
    IF (CALL1) THEN                % [1.019]                            03011000
    BEGIN                          % [1.019]                            03012000
      % [1.019] IF 8TH BIT MODE IS ENABLED, SEE IF INCOMING QBIN        03013000
      % [1.019] CHAR REQUESTS 8TH BIT QUOTING                           03014000
      IF (BINARYON)AND((PD IN QUOTECHARS)OR(PD = "Y")) THEN             03015000
      BEGIN                        % [1.019]                            03016000
        HIBITOK := TRUE;           % [1.019] YES, SET OK FLAG           03017000
        IF (PD = "Y") THEN         % [1.019]                            03018000
        BEGIN                      % [1.019]                            03019000
          TABLEIT(ACNTRL,MYQBIN);  % [1.019] TABLE MY QBIN CHAR         03020000
          TABLEIT(BCNTRL,MYQBIN);  % [1.019]                            03021000
        END                        % [1.019]                            03022000
        ELSE                       % [1.019]                            03023000
        BEGIN                      % [1.019]                            03024000
          TABLEIT(ACNTRL,QBIN);    % [1.019] TABLE INCOMING QBIN        03025000
          TABLEIT(BCNTRL,QBIN);    % [1.019]                            03026000
        END;                       % [1.019]                            03027000
      END                          % [1.019]                            03028000
      ELSE                         % [1.019] 8TH BIT QUOTING WILL       03029000
        HIBITOK := FALSE;           % [1.019] NOT BE DONE               03030000
    END                                                                 03031000
    ELSE                 % [1.019] CALL 2, SPAR WAS CALLED FIRST        03032000
    BEGIN                          % [1.019]                            03033000
      % [1.019] IF 8TH BIT MODE IS ENABLED, SEE IF WE                   03034000
      % [1.019]  GOT AN ACK TO OUR 8TH BIT QUOTE REQUEST                03035000
      IF (BINARYON)AND((QBIN = "Y")OR(QBIN = MYQBIN)) THEN              03036000
      BEGIN                        % [1.019]                            03037000
        HIBITOK := TRUE;           % [1.019] WILL DO 8TH BIT QUOTING    03038000
        TABLEIT(ACNTRL,MYQBIN);    % [1.019] TABLE MY QBIN CHAR         03039000
        TABLEIT(BCNTRL,MYQBIN);    % [1.019]                            03040000
      END                          % [1.019]                            03041000
      ELSE                         % [1.019]                            03042000
        HIBITOK := FALSE;          % [1.019] 8TH BIT QUOTING WILL       03043000
    END;                           % [1.019]  NOT BE DONE               03044000
  END;                             % [1.019]                            03045000
  IF LEN := *-2 GTR 0 THEN                                              03046000
  BEGIN                                                                 03047000
    PD := *+2;                     % [1.017] SKIP PAST QBIN,CHKTYPE     03048000
    UNTABLE(ACNTRL,REPT);          % [1.017] TAKE IT OUT OF ATABLE      03049000
    UNTABLE(BCNTRL,REPT);          % [1.017] TAKE IT OUT OF BTABLE      03050000
    REPT := REAL(PD,1);            % [1.017] INCOMING REPEAT CHAR       03051000
    IF CALL1 THEN                  % [1.017]                            03052000
    BEGIN                          % [1.017]                            03053000
         % IF CHAR SENT IS A VALID QUOTE CHAR, WE ARE REPEATING         03054000
      IF (PD IN QUOTECHARS) THEN   % [1.017] VALID CHAR ?               03055000
      BEGIN                        % [1.017]                            03056000
        REPTOK := TRUE;            % [1.017]                            03057000
        MYREPT := REPT;            % [1.017]                            03058000
        TABLEIT(ACNTRL,REPT);      % [1.017]                            03059000
        TABLEIT(BCNTRL,REPT);      % [1.017]                            03060000
      END                          % [1.017]                            03061000
      ELSE                         % [1.017]                            03062000
        REPTOK := FALSE;           % [1.017]                            03063000
      CALL1 := FALSE;              % [1.017]                            03064000
    END                            % [1.017]                            03065000
    ELSE                           % [1.017]                            03066000
    BEGIN                          % [1.017]                            03067000
         % IF CHAR MATCHES CHAR WE SENT, WE ARE REPEATING               03068000
      IF (REPT = MYREPT) THEN      % [1.017]                            03069000
      BEGIN                        % [1.017]                            03070000
        REPTOK := TRUE;            % [1.017]                            03071000
        TABLEIT(ACNTRL,REPT);      % [1.017]                            03072000
        TABLEIT(BCNTRL,REPT);      % [1.017]                            03073000
      END                          % [1.017]                            03074000
      ELSE                         % [1.017]                            03075000
        REPTOK := FALSE;           % [1.017]                            03076000
      CALL1 := TRUE;               % [1.017]                            03077000
    END                            % [1.017]                            03078000
  END                              % [1.017]                            03079000
  ELSE                                      % [1.017]                   03080000
         % DEFAULT TO NO REPEAT PROCESSING                              03081000
  BEGIN                                     % [1.017]                   03082000
    REPTOK := FALSE;                        % [1.017]                   03083000
  END;                                      % [1.017]                   03084000
  IF REAL(DEBUG) GTR 1 THEN       %  EXPAND IT ALL                      03085000
    BEGIN                                                               03086000
      BUG1("Your packet size = ",SPSIZ);                                03087000
      BUG1("Your timeout = ",TIMINT);                                   03088000
      BUG1("Your padding = ",PAD);                                      03089000
      BUGH("Your padding character = ",PCHAR);                          03090000
      BUGH("Your end of line character = ",EOL);                        03091000
      BUGC("Your quote character = ",QUOTE);                            03092000
      BUGC("Your binary quote character = ",QBIN);                      03093000
      BUGC("Your checksum type = ",CHKTYPE);                            03094000
      BUGC("Your repeat character = ",REPT);                            03095000
      IF REPTOK THEN                                                    03096000
        BUG("WE ARE REPEATING")                                         03097000
      ELSE                                                              03098000
        BUG("NO REPEAT CHARACTER");                                     03099000
      IF HIBITOK THEN                                                   03100000
        BUG("WE ARE BINARY QUOTING")                                    03101000
      ELSE                                                              03102000
        BUG("NOT BINARY QUOTING");                                      03103000
     END;                                                               03104000
 END RPAR;                                                              03105000
                                                                        03106000
                                                                        03107000
 %                                                                      03108000
 %  F L U S H I N P U T                                                 03109000
 %                                                                      03110000
 %  DUMP ALL PENDING INPUT TO CLEAR STACKED UP NAKS.                    03111000
 %                                                                      03112000
                                                                        03113000
                                                                        03114000
PROCEDURE FLUSHINPUT;                                                   03115000
 BEGIN                                                                  03116000
                                                                        03117000
   WHILE REM.CENSUS GTR 0 DO                                            03118000
     BRD := READ(REM);                                                  03119000
 END FLUSHINPUT;                                                        03120000
                                                                        03121000
 $ENDSEGMENT                                                            03122000
                                                                        03123000
                                                                        03124000
 %                                                                      03125000
 %  KERMIT PRINTING ROUTINES:                                           03126000
 %                                                                      03127000
 %  PRERRPKT - PRINT CONTENTS OF ERROR PACKET RECEIVED FROM REMOTE HOST 03128000
                                                                        03129000
                                                                        03130000
 %                                                                      03131000
 %  E R R O R                                                           03132000
 %                                                                      03133000
 %  PRINT ERROR MESSAGE.                                                03134000
 %                                                                      03135000
 %  IF LOCAL, PRINT ERROR MESSAGE WITH PRINTMSG.                        03136000
 %  IF REMOTE, SEND AN ERROR PACKET WITH THE MESSAGE.                   03137000
                                                                        03138000
                                                                        03139000
 %                                                                      03140000
 %  P R E R R P K T                                                     03141000
 %                                                                      03142000
 %  PRINT CONTENTS OF ERROR PACKET RECEIVED FROM REMOTE HOST.           03143000
                                                                        03144000
PROCEDURE PRERRPKT(MSG);                                                03145000
 ARRAY MSG[0];                                                          03146000
 BEGIN                                                                  03147000
    BUG("KERMIT ABORTING WITH FOLLOWING ERROR FROM REMOTE HOST:");      03148000
    BUGP(MSG);                                                          03149000
 END PRERRPKT;                                                          03150000
                                                                        03151000
  INITIALIZE;                                                           03152000
    ON ANYFAULT [ KPROMPT[*] : COL] , ABORTRUN;                         03153000
                                                                        03154000
  WHILE NOT BRD DO PROCESSIT;                                           03155000
                                                                        03156000
 END MAIN;                                                              03157000
                                                                        03158000
 %                                                                      03159000
 %  O U T E R   B L O C K                                               03160000
 %                                                                      03161000
 %  OUTER BLOCK OF KERMIT                                               03162000
                                                                        03163000
MAIN;                                                                   03164000
END.                                                                    03165000

    `@m¿