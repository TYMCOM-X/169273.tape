IDENTIFICATION DIVISION.
PROGRAM-ID.                    PD.
AUTHOR.                IDSI.
REMARKS.               TRANSMISSION SOFTWARE: VERSION 07.          00
         RELEASE DATE 09-01-88
         IBM ANS COBOL.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER.             DECSYSTEM-10.
OBJECT-COMPUTER.             DECSYSTEM-10.
* LINES 13 AND 15 MAYBE NEEDED FOR ASCII  MACHINES
*           PROGRAM COLLATING SEQUENCE IS EBCDIC.
SPECIAL-NAMES.
    ALPHABET EBCDIC IS EBCDIC.
    C01 IS NEXT-PAGE.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
*
***********************************************************
*
    SELECT PRINT-FILE    ASSIGN TO PRTR.
    SELECT UPDATE-FILE   ASSIGN TO UPDFL.
    SELECT TRANS-FILE    ASSIGN TO TRANSFL.
    SELECT  OLDTP70-FILE ASSIGN TO OLD216.
    SELECT OUTTP70-FILE  ASSIGN TO NEW216.
DATA DIVISION.
FILE SECTION.
FD  OLDTP70-FILE
    RECORD CONTAINS 216 CHARACTERS
    BLOCK CONTAINS 5 RECORDS
    LABEL RECORDS ARE STANDARD.
01  INTP-REC.
    05  I-X-QSIP.
      10  I-ISSUER            PIC X(6).
      10  I-ISSUE             PIC X(2).
      10  I-UNQ-CD            PIC X.
      10  I-X-CD              PIC X.
    05  I-ISR-QSIP REDEFINES I-X-QSIP.
      10  I-ISSUE-R     PIC X(8).
      10  FILLER        PIC X(2).
    05  I-X-TIC.
      10  I-PRIM-TIC          PIC X(4).
      10  I-GRP-ID            PIC X.
      10  I-CLASS-ID          PIC X.
    05  I-SIC             PIC X(4).
    05  I-ISSUER-DESCRIP          PIC X(28).
    05  I-ISSUE-DESCRIP.
      10  FILLER              PIC X(13).
      10  I-FUT-DP4-CD            PIC X.
      10  FILLER              PIC X(6).
    05  I-PRICE-SECT.
      10  I-PRICE-DT          PIC 9(4).
      10  I-VOL               PIC 9(7).
      10  I-HIGH              PIC 9(4)V9(3).
      10  I-HIGH4 REDEFINES I-HIGH    PIC 9(3)V9(4).
      10  I-LOW               PIC 9(4)V9(3).
      10  I-LOW4 REDEFINES I-LOW      PIC 9(3)V9(4).
      10  I-CLOSE             PIC 9(4)V9(3).
      10  I-CLOSE4 REDEFINES I-CLOSE      PIC 9(3)V9(4).
      10  I-ADJ-PREV-CLOSE        PIC 9(4)V9(3).
      10  I-ADJ-PREV-CLOSE4 REDEFINES
      I-ADJ-PREV-CLOSE        PIC 9(3)V9(4).
    05  I-IND-SECT.
      10  I-ISS-TYPE          PIC X.
      10  I-TRD-TYPE          PIC X.
      10  I-ISS-ST            PIC X.
      10  I-XPRICE-ST         PIC X.
      10  I-SUPP-TYPE REDEFINES I-XPRICE-ST PIC X.
      10  I-SNP-RATE REDEFINES I-SUPP-TYPE PIC X.
      10  I-MAT-DT            PIC 9(8).
      10  I-EXP-DT   REDEFINES I-MAT-DT   PIC 9(8).
      10  I-COUP-RATE         PIC 9(2)V9(4).
      10  I-ADJ-FACT REDEFINES I-COUP-RATE PIC 9(2)V9(4).
      10  I-TRD-ADJ   REDEFINES I-ADJ-FACT PIC 9(3)V9(3).
      10  I-MRG-IND           PIC X.
      10  I-BOND-FORM REDEFINES I-MRG-IND PIC X.
      10  I-TRD-UN-ST   REDEFINES I-MRG-IND PIC X.
      10  I-LAST-TDT          PIC 9(6).
      10  I-XREFQSIP REDEFINES I-LAST-TDT   PIC X(6).
    05  I-EARN-IAD-SECT.
      10  I-END12-DT          PIC 9(2).
      10  I-ERN-PER-SHR           PIC 9(2)V9(3).
      10  I-NEG-IND           PIC X.
      10  I-SHRS-OUT          PIC 9(7).
      10  I-IAD-FOOT          PIC 9.
      10  I-IAD               PIC 9(1)V9(3).
    05  I-BOND-QUAL-SECT REDEFINES I-EARN-IAD-SECT.
      10  FILLER              PIC X(6).
      10  I-SNP               PIC X.
      10  I-MOODY-RATE            PIC X.
      10  I-AMT-OUT           PIC X(7).
      10  I-YIELD-2-MAT           PIC 9(2)V9(3).
    05  I-UND-STK-SECT REDEFINES I-EARN-IAD-SECT.
      10  I-UND-STK-PRICE         PIC 9(4)V9(3).
      10  FILLER              PIC X.
      10  I-OPEN-INT          PIC X(7).
      10  I-OVER-10K-ADJ.
    15  I-P-ADJ-IND           PIC 9.
    15  I-HIGH-ADJ            PIC 9.
    15  I-LOW-ADJ         PIC 9.
    15  I-CLOSE-ADJ           PIC 9.
    15  I-ADJ-PREV-CLOSE-ADJ      PIC 9.
    05  I-DIVIDEND-AREAS.
      10  I-DIVSECT-1-2 OCCURS 2 TIMES.
    15  I-DIV-AMT         PIC 9(2)V9(4).
    15  I-DIV-EXDIS-DT        PIC 9(6).
    15  I-DIV-REC-DT          PIC 9(6).
    15  I-DIV-PAY-DT          PIC 9(6).
    15  I-DIV-PAY-MTD         PIC X.
    15  I-DIV-PAY-ODR         PIC X.
    15  I-DIV-TX-BS           PIC X.
    15  I-DIV-LR-CD           PIC X.
    15  I-DIV-FQ-CD           PIC X.
    15  I-DIV2-BRR-CD REDEFINES
        I-DIV-FQ-CD           PIC X.
    15  I-DIV2-TRANS-CD       PIC X.
    15  I-DIV-RV-IND          PIC X.
    15  I-DIV-ECODE           PIC X.
FD  OUTTP70-FILE
    RECORD CONTAINS 216 CHARACTERS
    BLOCK CONTAINS 5 RECORDS
    LABEL RECORDS ARE STANDARD.
01  OUTP216-REC     PIC X(216).
FD  UPDATE-FILE
    RECORD CONTAINS 40 CHARACTERS
    BLOCK CONTAINS 2 RECORDS
    LABEL RECORDS ARE STANDARD.
01  UPD-CONT-REC.
    05  FILLER            PIC X(22).
    05  UC-CON-IND            PIC 9.
    05  FILLER            PIC X.
    05  UC-1.
      10  UC-ISSUER-1         PIC X(16).
    05  UC-2 REDEFINES UC-1.
      10  UC-ISSUER-2         PIC X(12).
      10  UC-ISSUE-1          PIC X(4).
    05  UC-3 REDEFINES UC-1.
      10  UC-ISSUE-2          PIC X(16).
    05  UC-4 REDEFINES UC-1.
      10  UC-TICK             PIC X(6).
      10  FILLER              PIC XX.
      10  UC-SIC              PIC X(4).
      10  UC-ISS-TYPE         PIC X.
      10  UC-TRD-TYPE         PIC X.
      10  UC-ISS-ST           PIC X.
      10  FILLER              PIC X.
    05  UC-5 REDEFINES UC-1.
      10  UC-EXP-DT           PIC X(8).
      10  UC-ADJ-FACT         PIC 9(2)V9(4).
      10  UC-BOND-FORM            PIC X.
      10  FILLER              PIC X.
01  UPD-REC.
    05  U-X-QSIP.
      10  U-ISSUER            PIC X(6).
      10  U-ISSUE             PIC X(2).
      10  U-UNQ-CD            PIC X.
      10  U-X-CD              PIC X.
    05  U-ISR-QSIP REDEFINES U-X-QSIP.
      10  U-ISSUE-R     PIC X(8).
      10  FILLER        PIC X(2).
    05  U-REC-TYPE            PIC X.
    88  U-IS-BULL VALUES 'D', 'E', 'F'.
    88  U-HAS-CONT VALUE 'E'.
    88  U-IS-CAP VALUE 'F'.
    05  U-RPT-DT.
      10  U-RPT-Y             PIC 9.
      10  U-RPT-MM            PIC 9(2).
      10  U-RPT-DD            PIC 9(2).
    05  U-EFCT-DT.
      10  U-EFCT-Y            PIC 9.
      10  U-EFCT-MM           PIC 9(2).
      10  U-EFCT-DD           PIC 9(2).
    05  STAT-CHNG-INFO.
      10  U-ISS-ST            PIC X.
      10  FILLER              PIC X.
      10  U-XREF-QSIP.
    15  U-XREF-ISSUER         PIC X(6).
    15  U-XREF-ISSUE          PIC X(2).
      10  U-XREF-UNQ-CD         PIC X.
      10  U-XREF-X-CD           PIC X.
      10  U-XREF-FLAG           PIC 9.
        88  U-HAS-XREF VALUE 1.
        88  U-HAS-UCUS VALUE 2.
      10  U-REF-PR            PIC 9(5).
      10  U-MG-PR REDEFINES U-REF-PR      PIC 9(3)V9(2).
      10  U-ISS-TYPE          PIC X.
    05  CAP-CHNG-INFO REDEFINES STAT-CHNG-INFO.
      10  C-CAP-IND           PIC X.
      10  C-CAP-SECT          PIC 9.
      10  C-IAD-SECT.
* CAP CHANGE 6
    15  C-IAD             PIC 9(2)V9(3).
    15  C-IAD-FOOT            PIC X.
    15  C-BOND-SUPP-TYPE REDEFINES C-IAD-FOOT
                    PIC X.
    15  C-EPS             PIC 99V999.
    15  C-EPS-IND         PIC X.
    15  C-12M-END-Y           PIC 9.
    15  C-12M-END-DT          PIC 99.
    15  FILLER            PIC X.
    15  C-ISS-TYPE            PIC X.
      10  C-RECAP-SECT REDEFINES C-IAD-SECT.
* CAP CHANGE 7
    15  C-ADJ-FAC         PIC 9(2)V9(4).
    15  C-SHRS-OUT            PIC 9(8).
    15  FILLER            PIC XXX.
      10  C-TRD-REV-SECT   REDEFINES C-IAD-SECT.
* CAP CHANGE 9
    15  C-MARG-IND            PIC X.
    15  C-TRD-UNT-ST REDEFINES
        C-MARG-IND            PIC X.
    15  C-BOND-FORM REDEFINES
        C-MARG-IND            PIC X.
    15  C-X-PR-ST         PIC X.
    15  C-TRANS-CD            PIC X.
    15  C-OPT-IND         PIC X.
    15  C-TRD-UNT-ADJ         PIC 9(3)V9(3).
    15  FILLER            PIC X(4).
    15  C-MOODY-FIN-QL        PIC X.
    15  C-SNP-BD-RATE         PIC X.
    15  FILLER            PIC X.
FD  TRANS-FILE
    RECORD CONTAINS 40 CHARACTERS
    BLOCK CONTAINS 2 RECORDS
    LABEL RECORDS ARE STANDARD.
01  TRANS-REC.
    05  T-ISSUE-R             PIC X(8).
    05  FILLER            PIC X(32).
01  TRANS-LABEL.
    05  DUMMY             PIC X(5).
    05  T-SCALE-FLAG          PIC X(1).
    05  T-LABEL               PIC X(4).
    05  T-EXT-ISTYP-FLAG          PIC X(1).
    05  FILLER            PIC X(23).
    05  T-LABEL-DT            PIC 9(6).
01  T-PRICE-REC.
    05  T-X-QSIP.
      10  T-ISSUER            PIC X(6).
      88  TNOTE-TBOND VALUES '912810', '912827'.
      10  T-ISSUE             PIC X(2).
      10  T-UNQ-CD            PIC X.
      10  T-X-CD              PIC X.
    05  T-FUT-QSIP REDEFINES T-X-QSIP.
      10  T-FUT-NINE          PIC X(3).
      10  T-FUT-NUM           PIC X(3).
      10  T-EX-MT-CD          PIC X.
      10  T-EX-YR-CD          PIC X.
      10  T-FUT-X-CD          PIC X.
      10  T-TEL-FUT-CD            PIC X.
    05  T-REC-TYPE            PIC X.
    88  T-STOCK-PRICE VALUE 'K'.
    88  T-BOND-PRICE VALUE 'L'.
    88  T-OPTION-PRICE VALUE 'M'.
    88  T-OPTION-BA VALUE 'N'.
    88  T-FUTURE-PRICE VALUE 'O'.
    88  T-DIV VALUE 'P'.
    88  T-RBI VALUE 'R'.
    88  T-MTG VALUE 'S'.
    88  T-CALL VALUE 'T'.
    88  T-FUTURE-OI VALUE 'U'.
    88  T-NOT-PRICE VALUE 'P', 'R', 'S', 'T'.
    05  T-TRD-TYPE            PIC X.
    05  T-PRICING-DT.
      10  T-PR-MM             PIC 9(2).
      10  T-PR-DD             PIC 9(2).
    05  T-VOL             PIC 9(5).
    05  T-PRICES.
      10  T-CLOSE             PIC 9(5).
      10  T-HIGH              PIC 9(4).
      10  T-LOW               PIC 9(4).
    05  T-PRC-REP-CODE            PIC X.
    88  IN-THOU VALUE '0'.
    88  IN-HUND VALUE '1'.
    88  WITH-32NDS VALUE '2'.
    88  IN-8THS VALUE '3'.
    88  IN-16THS VALUE '4'.
    88  IN-32NDS VALUE '5'.
    88  IN-64THS VALUE '6'.
    88  T-EQUITY-OPT VALUE '1'.
    88  T-INDEX-OPT VALUE '2'.
    88  T-CURRENCY-OPT VALUE '3'.
    88  T-INTEREST-OPT VALUE '4'.
    88  T-DEBT-OPT VALUE '5'.
    88  T-FUTURE-OPT VALUE '6'.
    05  T-FUT-DIVISOR REDEFINES T-PRC-REP-CODE PIC X.
    05  T-YLD-2-MAT           PIC 9(2)V9(3).
    05  T-FUT-CASH-PRICE REDEFINES T-YLD-2-MAT PIC 9(5).
    05  T-OFLOW REDEFINES T-YLD-2-MAT.
      10  T-OPT-PRC-CODE          PIC X.
      88  T-OPT-OFLOW VALUES 'A'.
      10  T-VOL-HO            PIC 9.
      10  T-CLOSE-HO          PIC 9.
      10  T-HIGH-HO           PIC 9.
      10  T-LOW-HO            PIC 9.
    05  T-UPRC REDEFINES T-YLD-2-MAT.
      10  T-UND-STK-PR            PIC 9(5).

01  REG-BOND-REC.
    05  R-QSIP.
      10  R-ISSUER            PIC X(6).
      10  R-ISSUE             PIC X(2).
      10  R-ECODE             PIC X.
      10  FILLER              PIC X.
    05  R-CHK-DG              PIC 9.
    05  R-ISS-TYPE            PIC X.
    05  R-INT-PMT             PIC 9(3)V9(3).
    05  FILLER            PIC X(5).
    05  R-REC-DT.
      10  R-REC-Y             PIC 9.
      10  R-REC-MM            PIC 9(2).
      10  R-REC-DD            PIC 9(2).
    05  R-PMT-DT.
      10  R-PMT-Y             PIC 9.
      10  R-PMT-MM            PIC 9(2).
      10  R-PMT-DD            PIC 9(2).
    05  R-PMT-FQ              PIC X.
    05  R-BOND-IND            PIC X.
    05  R-LR-CD               PIC X.
    05  R-TRANS-CD            PIC X.
    05  R-PMT-MTD             PIC X.
    05  R-PMT-CD              PIC X.
    05  R-TX-BS-CD            PIC X.
01  BID-ASK-REC.
    05  FILLER            PIC X(11).
    05  BA-DATE.
    10  BA-DT-Y           PIC 9.
    10  BA-DT-MM          PIC 99.
    10  BA-DT-DD          PIC 99.
    05  BA-IND            PIC 9.
    88  BA-UPD-OPEN-INT VALUES  0, 1, 3, 4.
    88  BA-UPD-BA VALUES 2, 3, 4.
    88  BA-ADJ-OPEN-INT VALUES 1, 4.
    05  BA-ASK            PIC 9(5)V9(3).
    05  BA-BID            PIC 9(5)V9(3).
    05  BA-OPEN-INT           PIC 9(7).
01  CALL-REC.
    05  FILLER            PIC X(8).
    05  C-ENTRY-CODE          PIC X.
    05  C-REV-CODE            PIC X.
    05  FILLER            PIC X.
    05  C-SAP-PG-NO           PIC X(5).
    05  C-CALL-Y              PIC 9.
    05  C-CALL-MM             PIC 99.
    05  C-CALL-DD             PIC 99.
    05  C-CALL-ISS-TYPE           PIC X.
    05  C-SF-CODE             PIC X.
    05  C-CALL-TYPE           PIC X.
    05  C-REDEMPTION          PIC X.
    05  C-PREMIUM-CODE            PIC X.
    05  C-CALL-AMT            PIC 9(7).
    05  C-CALL-PRICE          PIC 9(4)V9(3).
FD  PRINT-FILE
    RECORD CONTAINS 132 CHARACTERS
    LABEL RECORDS ARE STANDARD.
01  PRINT-REC.
    05  P-MESS.
      10  PRINT-MESS    PIC X(60).
      10  FILLER        PIC X(72).
    05  PRINT-LINE REDEFINES P-MESS PIC X(132).
*
***********************************************************
*
WORKING-STORAGE SECTION.
*
01  TEMP-STRG.
    05  W-IAD-FOOT      PIC X.
    05  W-IAD       PIC 9(2)V9(3).
    05  WS-10-ADJ-PRC        PIC 9(5)V9(3).
    05  WS-OVER-10-K REDEFINES WS-10-ADJ-PRC.
    10  WS-10-ADJ-FACT    PIC 9.
    10  WS-10-ADJ-SEC     PIC 9(4)V9(3).
    05  WHOLE-32NDS       PIC 9(10).
    05  FRAC-32NDS        PIC 9(5)V9(3).
    05  WS-TODAYS-DT.
    10  WS-TD-YY          PIC 99.
    10  WS-TD-MD          PIC 9(4).
    05  XDIS-DT-TEMP.
    10  XDIS-DT-YY        PIC 99.
    10  XDIS-DT-MD        PIC 9(4).
    05  WS-DT.
    10  WS-D-MM     PIC 99.
    10  WS-D-DD     PIC 99.
    10  WS-D-YY     PIC 99.
    05  WS-DT-N REDEFINES WS-DT PIC 9(6).
    05  WS-YMD.
    10  WS-YMD-YY     PIC 99.
    10  WS-YMD-MM     PIC 99.
    10  WS-YMD-DD     PIC 99.
    05  WS-YMD-N REDEFINES WS-YMD PIC 9(6).
    05  WS-XD.
    10  WS-XD-YY     PIC 99.
    10  WS-XD-MM     PIC 99.
    10  WS-XD-DD     PIC 99.
    05  WS-XD-N REDEFINES WS-XD PIC 9(6).
    05  WS-D-YYYY       PIC 9(4).
    05  WS-M-CNT        PIC 9(4)   COMP.
    05  ITN-CNT         PIC 9(4)   COMP.
    05  TODAYS-DT.
    10  TD-MM       PIC 99.
    10  TD-DD       PIC 99.
    10  TD-YY       PIC 99.
    05  TODAYS-DT-N REDEFINES TODAYS-DT.
    10  TODAYS-DT-FT    PIC 9(4).
    10  FILLER      PIC 9(2).
    05  SAVE-DATE.
    10  S-YYYY-1    PIC 9(4).
    10  S-DT-FT     PIC 9(4).
    05  CHK-DATE.
    10  YYYY-1      PIC 9(4).
    10  MM-1        PIC 99.
    10  DD-1        PIC 99.
    05  XDIV-DT1.
    10  XDIV-DT-FT1     PIC 9(4).
    10  XDIV-DT-YY1     PIC 99.
    05  TD-YYYY         PIC 9(4)   COMP.
    05  TD-M-CNT        PIC 9(4)   COMP.
    05  FIX-DT.
    10  D-MM        PIC 99.
    10  D-DD        PIC 99.
    10  D-YY        PIC 99.
    05  FIX-DT-N REDEFINES FIX-DT      PIC 9(6).
    05 FIX-FTH REDEFINES FIX-DT-N.
   10  FIX-FST      PIC 9(4).
   10  FILLER       PIC 9(2).
    05  DIV         PIC 9(5)      COMP.
    05  WS-CLOSE-INT    PIC 9(6).
    05  WS-HIGH-INT     PIC 9(6).
    05  WS-LOW-INT      PIC 9(6).
    05  TEMP-PRCS       PIC 9(4)V9(5).
    05  TEMP-HIGH       PIC 9(5)V9(5).
    05  TEMP-HIGH-R REDEFINES TEMP-HIGH.
    10  TEMP-HIGH-OFLOW PIC 9.
    10  FILLER      PIC 9(9).
    05  TEMP-LOW        PIC 9(5)V9(5).
    05  TEMP-LOW-R REDEFINES TEMP-LOW.
    10  TEMP-LOW-OFLOW  PIC 9.
    10  FILLER      PIC 9(9).
    05  TEMP-CLOSE      PIC 9(5)V9(5).
    05  TEMP-CLOSE-R REDEFINES TEMP-CLOSE.
    10  TEMP-CLOSE-OFLOW PIC 9.
    10  FILLER      PIC 9(9).
    05  TEMP-BID        PIC 9(5)V9(3).
    05  TEMP-BID-R REDEFINES TEMP-BID.
    10  TEMP-BID-ADJ    PIC 9.
    10  TEMP-OP-BID     PIC 9(4)V9(3).
    05  TEMP-ASK        PIC 9(5)V9(3).
    05  TEMP-ASK-R REDEFINES TEMP-ASK.
    10  TEMP-ASK-ADJ    PIC 9.
    10  TEMP-OP-ASK     PIC 9(4)V9(3).
    05  FUT-TEMP-PRICE      PIC 9(5)V9(3).
    05  HI-X-QSIP       PIC X(10).
    05  HI-TRD-TYPE     PIC X.
    05  HI-HIGH         PIC 9(4)V9(3).
    05  HI-LOW      PIC 9(4)V9(3).
    05  HI-CLOSE        PIC 9(4)V9(3).
    05  HI-ADJ-PREV-CLOSE   PIC 9(4)V9(3).
    05  HI-HIGH-ADJ     PIC 9.
    05  HI-LOW-ADJ      PIC 9.
    05  HI-CLOSE-ADJ    PIC 9.
    05  HI-ADJ-PREV-CLOSE-ADJ      PIC 9.
    05  HOLD-IAD-CUSIP      PIC X(8) VALUE 'XXXXXXXX'.
    05  HOLD-IAD        PIC 99V999.
    05  HOLD-LAST-TRDNDT    PIC 9(6).
    05  VOL-TEMP        PIC 9(7).
    05  TEMP-DT         PIC 9(4).
01  TEST-ISS-TYPE       PIC X.
    88  TEST-IS-STOCK VALUES '0', '1', '2', '4', '7', '8',
             '9', 'A', 'E', 'F', 'G', 'Q'.
    88  TEST-IS-BOND VALUES '3', '5', '6', 'H', 'I', 'J', 'K',
            'L', 'M', 'N', 'O', 'P', 'R', 'S',
            'T', 'U', 'V', 'W', 'X', 'Y', 'Z'.
    88  TEST-IS-OPTION VALUES 'B', 'D'.
    88  TEST-IS-EQUITY-OPTION VALUE 'B'.
    88  TEST-IS-FUTURE VALUE 'C'.
*
*
01  FLAGS.
    05  NOT-OLD-TRD     PIC X VALUE  'F'.
    88  NEW-TRD-TYPE      VALUE  'T'.
    05  REC-NOT-TRADED      PIC X VALUE  'F'.
    88  NOT-TRADED        VALUE  'T'.
    05  ADD-A-REC       PIC X VALUE 'F'.
    88  NEW-REC       VALUE 'T'.
    05  BAD-NEWS        PIC 9 VALUE 0.
    88  FATAL-ERROR       VALUE 1.
    05  CAP-7       PIC X VALUE 'F'.
    88  CAP-7-REC         VALUE 'T'.
    05  OPT-SCALE-FLAG        PIC X VALUE 'F'.
    88  PRICE-PER-SHARE VALUE 'T'.
    05  ISS-TYPE-FLAG   PIC X VALUE 'F'.
    88  NON-EXTENDED VALUE 'T'.
    05  C5-STAT-CHANGE      PIC X VALUE 'F'.
    88  C5-CHANGE       VALUE 'T'.
    05  ENDFILES.
    10  W-ENDUPD    PIC X VALUE 'F'.
        88  ENDUPD        VALUE 'T'.
    10  W-ENDTRANS      PIC X VALUE 'F'.
        88  ENDTRANS      VALUE 'T'.
    10  W-ENDOLDTP      PIC X VALUE 'F'.
        88  ENDOLDTP        VALUE 'T'.
    05  ENDFILES-2 REDEFINES ENDFILES.
    10  W-ENDALL    PIC XXX.
        88  ENDALL        VALUE 'TTT'.
*
*
01  CNTRS.
    05  W-OLDTP-CNT     PIC 9(6) VALUE 0 COMP.
    05  W-OUTP-CNT      PIC 9(6) VALUE 0 COMP.
    05  W-TRANS-CNT     PIC 9(6) VALUE 0 COMP.
    05  W-UPD-CNT       PIC 9(6) VALUE 0 COMP.
    05  CR-CNT      PIC 9(6) VALUE 0 COMP.
    05  NO-DESCRIP-CNT      PIC 9(6)  VALUE 0 COMP.
    05  SC-CNT      PIC 9(6) VA COMP.
    05  NSC-CNT         PIC 9(6) VALUE 0 COMP.
    05  SCNG-CNT        PIC 9(6) VALUE 0 COMP.
    05  CONT-CNT        PIC 9(6) VALUE 0 COMP.
    05  PR-CNT      PIC 9(6) VALUE 0 COMP.
    05  BA-CNT      PIC 9(6) VALUE 0 COMP.
    05  OI-CNT      PIC 9(6) VALUE 0 COMP.
    05  RB-CNT      PIC 9(6) VALUE 0 COMP.
    05  DR-CNT      PIC 9(6) VALUE 0 COMP.
    05  CL-CNT      PIC 9(6) VALUE 0 COMP.
    05  STAT-CON-CNT    PIC 9(2) VALUE 0  COMP.
    05  PT-BLK      PIC 9(6) VALUE 0 COMP.
    05  NO-9BLKS        PIC 9     VALUE 0 COMP.
    05  BLK-SZ      PIC 9     VALUE 5 COMP.
    05  FT-BLK      PIC 9(6)  VALUE 0 COMP.
    05  P-CNT       PIC 9(6) VALUE 0 COMP.
    05  BADPR-CNT       PIC 9(6) VALUE 0 COMP.
    05  I           PIC 9(4)      COMP.
    05  J           PIC 9(4)      COMP.
    05  P           PIC 9    VALUE 1  COMP.
    05  M           PIC 9(2)      COMP.
*
01  MESSAGES.
    05 FILLER PIC X(30) VALUE '40 TO 216 CONVERT  VERSION 7.          0'.
    05 FILLER PIC X(30) VALUE 'NORMAL END-OF-JOB         '.
    05 FILLER PIC X(30) VALUE 'ALL I/O FILES OPENED      '.
    05 FILLER PIC X(30) VALUE 'EOF BEFORE HEADER RECORD READ '.
    05 FILLER PIC X(30) VALUE 'NO HEADER RECORD ON TRANS FILE'.
     05 FILLER PIC X(30) VALUE 'NO DATE RECORD ON TRANS FILE  '.
     05 FILLER PIC X(30) VALUE 'TRANS EOF BEFORE SEPARATOR REC'.
     05 FILLER PIC X(30) VALUE 'RECS SEPARATED, (BULT, PR/DIV)'.
     05 FILLER PIC X(30) VALUE 'ERROR IN PRIMING READS    '.
     05 FILLER PIC X(30) VALUE 'INPUT BUFFERS FILLED      '.
     05 FILLER PIC X(30) VALUE 'ALL FILES CLOSED      '.
     05 FILLER PIC X(30) VALUE 'MAIN PROCESSING STARTED   '.
     05 FILLER PIC X(30) VALUE 'CONTINUATION RECORD EXPECTED  '.
     05 FILLER PIC X(30) VALUE 'CAPIT.           SECTION NUMBER ERROR   '.
     05 FILLER PIC X(30) VALUE 'EXTRA STATUS CONTIN.           RECORDS  '.
     05 FILLER PIC X(30) VALUE 'DIV.           SECTION NUMBER INVALID   '.
     05 FILLER PIC X(30) VALUE 'UNCODE 216-PNTR OUT OF BOUNDS '.
     05 FILLER PIC X(30) VALUE 'NEW DATA 216 PNTR OUT OF BNDS '.
     05 FILLER PIC X(30) VALUE 'JOB STOPPED=> CANT ADJUST DATE'.
     05 FILLER PIC X(30) VALUE 'UPD FILE EOF-TRAILER MISSING  '.
     05 FILLER PIC X(30) VALUE 'TRAN FILE EOF-TRAILER MISSING '.
     05 FILLER PIC X(30) VALUE 'OLDTP FILE EOF-TRAILER MISSING'.
     05 FILLER PIC X(30) VALUE 'TRANS-FILE LABEL READ     '.
     05 FILLER PIC X(30) VALUE 'DATE COMPUTED FROM TRANS.           REC '.
     05 FILLER PIC X(30) VALUE 'PROBLEMS >100: JOB TERMINATED.          '.
     05 FILLER PIC X(30) VALUE 'BOND CURRENT YIELD ZERO FOR:  '.
     05 FILLER PIC X(30) VALUE 'STATUS CONTIN.           OUT OF ORDER   '.
*
01  MESSAGE-TABLE REDEFINES MESSAGES.
    05 MESS         OCCURS  26 PIC X(30).
*
*
01  MESS-REC.
    05  FILLER      PIC X VALUE SPACES.
    05  FILLER      PIC X(9) VALUE '****     '.
    05  MESG        PIC X(30).
    05  FILLER      PIC X(4) VALUE '****'.
    05  FILLER      PIC X(89) VALUE SPACES.
*
01  HEAD-1.
    05  FILLER      PIC X(27) VALUE SPACES.
    05  FILLER      PIC X(7) VALUE '------ '.
    05  FILLER      PIC X(45) VALUE
    '40 TO 216 CONVERT SOFTWARE PROCESSING SUMMARY'.
    05  FILLER      PIC X(7) VALUE ' ------'.
    05  FILLER      PIC X(46) VALUE SPACES.
*
01  HDR-1.
    05  FILLER      PIC X(15) VALUE 'DATE FROM HDR :'.
    05  HDR-DT      PIC 9(6) VALUE ZERO.
    05  FILLER      PIC X(39) VALUE SPACES.
01  HDR-REC.
    05  FILLER      PIC X(15) VALUE 'HEADER RECORD :'.
    05  HDR         PIC X(40) VALUE SPACES.
    05  FILLER      PIC X(5) VALUE SPACES.
01  LAST-T-REC.
    05  FILLER      PIC X(15) VALUE 'LAST PR/DV REC:'.
    05  LT-REC      PIC X(40) VALUE SPACES.
    05  FILLER      PIC X(5) VALUE SPACES.
01  LAST-U-REC.
    05  FILLER      PIC X(15) VALUE 'LAST BULT.           REC:'.
    05  LT-U-REC        PIC X(40) VALUE SPACES.
    05  FILLER      PIC X(5) VALUE SPACES.
01  LAST-O-REC.
    05  FILLER      PIC X(15) VALUE 'LAST NEW  216 :'.
    05  OUT-QSIP-SAVE   PIC X(10).
    05  OUT-TIC-SAVE    PIC X(06).
    05  OUT-SIC-SAVE    PIC X(04).
    05  OUT-DESC-SAVE   PIC X(20).
01  LINE-2.
    05  FILLER      PIC X VALUE ' '.
    05  FILLER      PIC X(15) JUST RIGHT VALUE
    'TOTAL RECORDS '.
    05  FILLER      PIC X(30) JUST RIGHT VALUE
    '        OLDTP70 UPDATE'.
    05  FILLER      PIC X(20) JUST RIGHT VALUE
    '  TRANS   OUTTP70   '.
01  TOT-LINE.
    05  FILLER      PIC XX VALUE ' '.
    05  FILLER      PIC X(25) VALUE
               'LESS SEPARATOR/HEADER REC'.
    05  FILLER      PIC X(5) VALUE SPACES.
    05  TOT-OLD         PIC Z(4)99BB.
    05  TOT-UPD         PIC Z(4)99B.
    05  TOT-TRANS       PIC Z(4)99BBBB.
    05  TOT-OUT         PIC Z(4)99BBBB.
01  LINE-3.
    05  FILLER      PIC X VALUE ' '.
    05  FILLER      PIC X(26) JUST RIGHT VALUE
    ' PROCESSING TOTALS: UPD'.
    05  FILLER      PIC X(33) VALUE SPACES.
01  PART-TOT.
    05  FILLER      PIC X VALUE ' '.
    05  FILLER      PIC X(26) VALUE SPACES.
    05  PT-TYPE         PIC X(15).
    05  OUT-PT      PIC Z(4)99.
    05  FILLER      PIC X(12) VALUE SPACES.
01  PART-TOT-1.
    05  FILLER      PIC X VALUE ' '.
    05  FILLER      PIC X(26) VALUE SPACES.
    05  PT-TYPE-1       PIC X(15) VALUE
               'RECORDS TO DATE'.
    05  FILLER      PIC X(18) VALUE SPACES.
01  SUB-HEAD.
    05  FILLER      PIC X VALUE ' '.
    05  FILLER      PIC X(26) JUST RIGHT VALUE 'TRANS'.
    05  FILLER      PIC X(33) VALUE SPACES.
01  SUB-HEAD-1.
    05  FILLER      PIC X VALUE ' '.
    05  FILLER      PIC X(26) JUST RIGHT VALUE
                  '     OLDTP70'.
    05  FILLER      PIC X(33) VALUE SPACES.
01  SUB-HEAD-2.
    05  FILLER      PIC X VALUE ' '.
    05  FILLER      PIC X(26) JUST RIGHT VALUE
                  '     OUTTP70'.
    05  FILLER      PIC X(33) VALUE SPACES.
*
01  ERR-LINE.
    05  FILLER      PIC X VALUE ' '.
    05  FILLER      PIC X(30) JUST RIGHT VALUE
      'AT CUSIP: OUTTP70   UPD'.
    05  FILLER      PIC X(29) JUST RIGHT VALUE
      '   TRANS     DIVIDEND'.
*
01  ERR-LINE-1.
    05  FILLER      PIC X VALUE ' '.
    05  FILLER      PIC X(9) VALUE SPACES.
    05  ERR-O-QSIP      PIC X(10).
    05  FILLER      PIC X(4) VALUE SPACES.
    05  ERR-U-QSIP      PIC X(10).
    05  FILLER      PIC X(4) VALUE SPACES.
    05  ERR-T-QSIP      PIC X(10).
    05  FILLER      PIC X(4) VALUE SPACES.
    05  ERR-D-QSIP      PIC X(8).
01  ERR-LINE-2.
    05  FILLER      PIC X VALUE ' '.
    05  FILLER      PIC X(9) VALUE SPACES.
    05  O-ERR-QSIP      PIC X(10).
    05  FILLER         PIC X(103) VALUE SPACES.
*
01  ID-1.
    05  FILLER      PIC X(57) JUST RIGHT VALUE
    'INTERACTIVE DATA CORPORATION'.
    05  FILLER      PIC X(26) JUST RIGHT VALUE
    '40 TO 216 CONVERT SOFTWARE'.
    05  FILLER      PIC X(49) VALUE SPACES.
*
01  ID-2.
    05  FILLER      PIC X(30) VALUE SPACES.
    05  FILLER      PIC X(26) VALUE
      'ID:         PD;  VERSION 07.          00;'.
    05  FILLER      PIC X(27) VALUE
      ' RELEASE DATE   09-01-88   '.
*
01  DASHES.
    05  FILLER      PIC X(30) VALUE SPACES.
    05  FILLER      PIC X(53) VALUE ALL '-'.
    05  FILLER      PIC X(49) VALUE SPACES.
*
01  REVISIONS.
    05  REV-IND OCCURS 5    PIC X.
01  REV-WORD
    REDEFINES REVISIONS     PIC X(5).
*
01  REV-WORD-VALUES.
    05  FILLER  PIC X(25) VALUE '0000100100001100100001010'.
    05  FILLER  PIC X(25) VALUE '0110001110000000001000001'.
    05  FILLER  PIC X(25) VALUE '0000100001000010000100001'.
    05  FILLER  PIC X(25) VALUE '0000100001000011010010110'.
    05  FILLER  PIC X(25) VALUE '1100011010111001111010000'.
    05  FILLER  PIC X(25) VALUE '1001000001000010000100001'.
    05  FILLER  PIC X(25) VALUE '0000100001000010000100001'.
    05  FILLER  PIC X(5)  VALUE '00001'.
*
01  R-W-V  REDEFINES REV-WORD-VALUES.
    05  RW-TABLE OCCURS 36  PIC X(5).
*
01  RW-CODES        PIC X(37) VALUE
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890#'.
01  R-W-C  REDEFINES RW-CODES.
    05  REV-CODE-TABLE OCCURS 37 PIC X.
*
01  OUTP-REC.
    05  O-X-QSIP.
      10  O-ISSUER            PIC X(6).
      10  O-ISSUE             PIC X(2).
      10  O-UNQ-CD            PIC X.
      10  O-X-CD              PIC X.
    05  O-ISR-QSIP REDEFINES O-X-QSIP.
      10  O-ISSUE-R           PIC X(8).
      10  FILLER              PIC X(2).
    05  O-X-TIC.
      10  O-PRIM-TIC          PIC X(4).
      10  O-GRP-ID            PIC X.
      10  O-CLASS-ID          PIC X.
    05  O-SIC             PIC X(4).
    05  O-ISSUER-DESCRIP          PIC X(28).
    05  O-ISSUER-R REDEFINES O-ISSUER-DESCRIP.
      10  O-ISSUER-1            PIC X(16).
      10  O-ISSUER-2            PIC X(12).
    05  O-ISSUER-CHK REDEFINES O-ISSUER-DESCRIP.
      10  O-DESCRIP-CHK         PIC X(4).
      10  FILLER            PIC X(24).
    05  O-ISSUE-DESCRIP.
      10  FILLER              PIC X(13).
      10  O-FUT-DP4-CD            PIC X.
      10  FILLER              PIC X(6).
    05  O-ISSUE-R1 REDEFINES O-ISSUE-DESCRIP.
      10  O-ISSUE-1           PIC X(4).
      10  O-ISSUE-2           PIC X(16).
    05  O-ISSUE-R2 REDEFINES O-ISSUE-DESCRIP.
      10  FILLER              PIC X(19).
      10  O-OP-TYPE           PIC X.
    88  O-EQUITY-OPT VALUE '1'.
    88  O-INDEX-OPT VALUE '2'.
    88  O-CURRENCY-OPT VALUE '3'.
    88  O-INTEREST-OPT VALUE '4'.
    88  O-DEBT-OPT VALUE '5'.
    88  O-FUTURE-OPT VALUE '6'.
    05  O-PRICE-SECT.
      10  O-PRICE-DT          PIC 9(4).
      10  O-VOL               PIC 9(7).
      10  O-HIGH              PIC 9(4)V9(3).
      10  O-HIGH4 REDEFINES O-HIGH    PIC 9(3)V9(4).
      10  O-LOW               PIC 9(4)V9(3).
      10  O-LOW4 REDEFINES O-LOW      PIC 9(3)V9(4).
      10  O-CLOSE             PIC 9(4)V9(3).
      10  O-CLOSE4 REDEFINES O-CLOSE      PIC 9(3)V9(4).
      10  O-ADJ-PREV-CLOSE        PIC 9(4)V9(3).
      10  O-ADJ-PREV-CLOSE4 REDEFINES
    O-ADJ-PREV-CLOSE          PIC 9(3)V9(4).
    05  O-IND-SECT.
      10  O-ISS-TYPE          PIC X.
      10  O-TRD-TYPE          PIC X.
      10  O-ISS-ST            PIC X.
      10  O-XPRICE-ST         PIC X.
      10  O-SUPP-TYPE REDEFINES O-XPRICE-ST PIC X.
      10  O-SNP-RATE REDEFINES O-SUPP-TYPE PIC X.
      10  O-MAT-DT.
    15  O-MAT-MM          PIC 99.
    15  O-MAT-DD          PIC 99.
    15  O-MAT-YYYY            PIC 9(4).
      10  O-EXP-DT   REDEFINES O-MAT-DT   PIC 9(8).
      10  O-COUP-RATE         PIC 9(2)V9(4).
      10  O-ADJ-FACT REDEFINES O-COUP-RATE PIC 9(2)V9(4).
      10  O-TRD-ADJ   REDEFINES O-COUP-RATE PIC 9(3)V9(3).
      10  O-MRG-IND           PIC X.
      10  O-BOND-FORM REDEFINES O-MRG-IND PIC X.
      10  O-TRD-UN-ST   REDEFINES O-MRG-IND PIC X.
      10  O-LAST-TDT.
    15  O-MMDD            PIC 9(4).
    15  O-YY              PIC 99.
      10  O-XREFQSIP REDEFINES O-LAST-TDT   PIC X(6).
    05  O-EARN-IAD-SECT.
      10  O-END12-DT          PIC 9(2).
      10  O-ERN-PER-SHR           PIC 9(2)V9(3).
      10  O-NEG-IND           PIC X.
      10  O-SHRS-OUT          PIC 9(7).
      10  O-IAD-FOOT          PIC 9.
      10  O-IAD               PIC 9(1)V9(3).
    05  O-BOND-QUAL-SECT REDEFINES O-EARN-IAD-SECT.
      10  FILLER              PIC X(6).
      10  O-SNP-BD            PIC X.
      10  O-MOODY-RATE            PIC X.
      10  O-AMT-OUT           PIC X(7).
      10  O-YIELD-2-MAT           PIC 9(2)V9(3).
    05  O-FUT-CASH-PRICE-SECT REDEFINES O-EARN-IAD-SECT.
      10  O-FUT-CASH-PRICE      PIC 9(5)V9(3).
      10  O-FUT-OPEN-INTEREST       PIC 9(7).
      10  FILLER             PIC X(5).
    05  O-UND-STK-SECT REDEFINES O-EARN-IAD-SECT.
      10  O-UND-STK-PRICE         PIC 9(4)V9(3).
      10  O-OPEN-INT-ADJ-IND      PIC 9.
      10  O-OPEN-INT          PIC X(7).
      10  O-OVER-10K-ADJ.
    15  O-P-ADJ-IND           PIC 9.
    15  O-HIGH-ADJ            PIC 9.
    15  O-LOW-ADJ         PIC 9.
    15  O-CLOSE-ADJ           PIC 9.
    15  O-ADJ-PREV-CLOSE-ADJ      PIC 9.
    05  O-DIVIDEND-AREAS.
      10  O-DIVSECT-1-2 OCCURS 2 TIMES.
    15  O-DIV-AMT         PIC 9(2)V9(4).
    15  O-DIV-XDIS-DT         PIC 9(6).
    15  O-DIV-REC-DT          PIC 9(6).
    15  O-DIV-PAY-DT          PIC 9(6).
    15  O-DIV-PAY-MTD         PIC X.
    15  O-DIV-PAY-ODR         PIC X.
    15  O-DIV-TX-BS           PIC X.
    15  O-DIV-LR-CD           PIC X.
    15  O-DIV-FQ-CD           PIC X.
    15  O-DIV2-BRR-CD REDEFINES
        O-DIV-FQ-CD           PIC X.
    15  O-DIV2-TRANS-CD       PIC X.
    15  O-DIV-RV-IND          PIC X.
    15  O-DIV-ECODE           PIC X.
    05  O-ALT-2ND-DIV REDEFINES
    O-DIVIDEND-AREAS.
      10  FILLER              PIC X(29).
      10  O-OP-IAD            PIC 99V999.
      10  O-OP-UCUS           PIC X(8).
      10  O-OP-ASK-ADJ            PIC X.
      10  O-OP-BID-ADJ            PIC X.
      10  O-OP-BA-DATE            PIC 9(6).
      10  O-OP-ASK            PIC 9(4)V9(3).
      10  O-OP-BID            PIC 9(4)V9(3).
    05  O-BOND-INT-PMNT-SECT REDEFINES
             O-DIVIDEND-AREAS.
      10  O-ENTRY-CODE            PIC X.
      10  O-SAP-PG-NO         PIC X(5).
      10  O-CALL-DATE         PIC 9(6).
      10  O-REV-CODE          PIC X.
      10  O-CALL-ISS-TYPE         PIC X.
      10  O-SF-CODE           PIC X.
      10  O-CALL-TYPE         PIC X.
      10  O-REDEMPTION            PIC X.
      10  O-PREMIUM-CODE          PIC X.
      10  O-CALL-AMT          PIC 9(7).
      10  O-CALL-PRICE            PIC 9(4)V9(3).
      10  O-BD-CUR-YLD            PIC 9(2)V9(4).
      10  O-BD-INT-PMT            PIC 9(3)V9(3).
      10  O-BD-REC-DT         PIC 9(6).
      10  O-BD-PMT-DT         PIC 9(6).
      10  O-BD-PMT-MTD            PIC X.
      10  O-BD-PMT-CD         PIC X.
      10  O-BD-TX-BS-CD           PIC X.
      10  O-BD-LR-CD          PIC X.
      10  O-BD-PMT-FQ         PIC X.
      10  O-BD-TRANS-CD           PIC X.
      10  O-BD-RV-IND         PIC X.
      10  O-BD-ECODE          PIC X.
01  OUTP-216-REC REDEFINES OUTP-REC.
    05  FILLER            PIC X(10).
    05  O-DATA            PIC X(206).
*
01  W-DIV-REC             PIC X(80) VALUE SPACES.
01  W-DIV REDEFINES W-DIV-REC.
    03  DIV-REC  OCCURS 2.
    05  D-QSIP.
      10  D-ISSUE-R           PIC X(8).
      10  D-ECODE             PIC X.
      10  D-DIV-SECT-NUM          PIC 9.
    05  D-X-QSIP REDEFINES D-QSIP.
      10  D-ISSUER            PIC  X(6).
      10  D-ISSUE           PIC X(2).
      10  FILLER            PIC X(2).
    05  D-CHK-DG              PIC 9.
    05  D-ISS-TYPE            PIC X.
    05  D-AMT-RATE            PIC 9(2)V9(4).
    05  D-XDIS-AMT-DT.
      10  D-XDIS-Y            PIC 9.
      10  D-XDIS-MM           PIC 9(2).
      10  D-XDIS-DD           PIC 9(2).
    05  D-REC-DT.
      10  D-REC-Y             PIC 9.
      10  D-REC-MM            PIC 9(2).
      10  D-REC-DD            PIC 9(2).
    05  D-PAY-DT.
      10  D-PAY-Y             PIC 9.
      10  D-PAY-MM            PIC 9(2).
      10  D-PAY-DD            PIC 9(2).
    05  D-LR-CD               PIC 9.
    05  D-REV-IND             PIC X.
    05  D-BRR-CD              PIC X.
    05  D-TRANS-CD            PIC X.
    05  D-PMT-MTD             PIC X.
    05  D-PMT-ODR             PIC X.
    05  D-TX-BS-CD            PIC X.
*
01  COM-DIV-REC               PIC X(80) VALUE SPACES.
01  COM-DIV REDEFINES COM-DIV-REC.
    03  COM-OPT-DIV-REC OCCURS 2.
    05  COM-OPT-QSIP.
      10  COM-OPT-ISSUE-R         PIC X(8).
      10  COM-OPT-ECODE           PIC X.
      10  COM-OPT-DIV-SECT-NUM        PIC 9.
    05  COM-OPT-X-QSIP REDEFINES COM-OPT-QSIP.
      10  COM-OPT-ISSUER          PIC  X(6).
      10  COM-OPT-ISSUE         PIC X(2).
      10  FILLER            PIC X(2).
    05  COM-OPT-CHK-DG            PIC 9.
    05  COM-OPT-ISS-TYPE          PIC X.
    05  COM-OPT-AMT-RATE          PIC 9(2)V9(4).
    05  COM-OPT-XDIS-AMT-DT.
      10  COM-OPT-XDIS-Y          PIC 9.
      10  COM-OPT-XDIS-MM         PIC 9(2).
      10  COM-OPT-XDIS-DD         PIC 9(2).
    05  COM-OPT-REC-DT.
      10  COM-OPT-REC-Y           PIC 9.
      10  COM-OPT-REC-MM          PIC 9(2).
      10  COM-OPT-REC-DD          PIC 9(2).
    05  COM-OPT-PAY-DT.
      10  COM-OPT-PAY-Y           PIC 9.
      10  COM-OPT-PAY-MM          PIC 9(2).
      10  COM-OPT-PAY-DD          PIC 9(2).
    05  COM-OPT-LR-CD         PIC 9.
    05  COM-OPT-REV-IND           PIC X.
    05  COM-OPT-BRR-CD            PIC X.
    05  COM-OPT-TRANS-CD          PIC X.
    05  COM-OPT-PMT-MTD           PIC X.
    05  COM-OPT-PMT-ODR           PIC X.
    05  COM-OPT-TX-BS-CD          PIC X.
01  W-NINES         PIC X(216) VALUE ALL '9'.
PROCEDURE DIVISION.
JOB-START SECTION.
JOB-START-PARA.
* TOP LEVEL ROUTINE

* OPEN PRINT FILE AND DISPLAY HEADER
    PERFORM PRINT-ID.

* OPEN FILES
    PERFORM OPEN-FILES.

* READ THE LABEL RECORD - IF NOT THERE END PROCESSING
    PERFORM READ-LABEL.
    IF FATAL-ERROR GO TO STOP-RUN.

* READ THE DATE RECORD - IF NOT THERE END PROCESSING
    PERFORM LABEL-DT.
    IF FATAL-ERROR GO TO STOP-RUN.

* READ THE BULLETINS FROM THE TRAN FILE, PUTTING THEM INTO
* A SEPARATE FILE - IF ERRORS END PROCESSING
    PERFORM SEP-REC.
    IF FATAL-ERROR GO TO STOP-RUN.

* READ THE BULLETIN FILE UNTIL FIND FIRST VALID BULLETIN RECORD
* READ THE TRAN FILE UNTIL FIND FIRST VALID TRAN RECORD
* - IF ERRORS END PROCESSING
    PERFORM LOAD-BUF.
    IF FATAL-ERROR GO TO STOP-RUN.

* PROCESS THE DATA FILES - IF ERRORS END PROCESSING
    PERFORM MAIN-JOB.
    IF FATAL-ERROR GO TO STOP-RUN.

* PAD OUTPUT 216 FILE WITH 9'S RECORDS TILL END OF BLOCK
    PERFORM END-OF-OUTPUT-NINES.

* PRINT MESSAGE - NORMAL END OF JOB
    MOVE +2 TO M.
    PERFORM WRITE-MESS.

* END OF JOB PROCESSING STARTS HERE
STOP-RUN.

* WRITE OUT SUMMARY INFO TO PRINT FILE
    PERFORM WRITE-SUMMARY.

* CLOSE THE FILES
    PERFORM CLOSE-FILES.
    STOP RUN.
*********************************************************
*********************************************************
*
PRINT-ID SECTION.
PRINT-ID-PARA.
* OPEN PRINT FILE AND DISPLAY HEADER
    OPEN OUTPUT PRINT-FILE.
    WRITE PRINT-REC FROM ID-1 AFTER ADVANCING NEXT-PAGE.
    WRITE PRINT-REC FROM ID-2 AFTER ADVANCING 2 LINES.
    WRITE PRINT-REC FROM DASHES AFTER ADVANCING 1 LINES.
    MOVE SPACES TO PRINT-REC.
    WRITE PRINT-REC AFTER ADVANCING 6 LINES.
*
*********************************************************
*
OPEN-FILES SECTION.
OPEN-FILES-PARA.
    OPEN INPUT OLDTP70-FILE WITH NO REWIND,
       TRANS-FILE.
    OPEN OUTPUT OUTTP70-FILE, UPDATE-FILE.
    MOVE +3 TO M   PERFORM WRITE-MESS.
OF-EXIT.           EXIT.
*
*********************************************************
*
READ-LABEL SECTION.
READ-LABEL-PARA.
    MOVE +0 TO I.
RL-READ.
    READ TRANS-FILE AT END
* EOF BEFORE LABEL RECORD FOUND - WRITE ERROR AND ABEND
   MOVE +4 TO M
   PERFORM WRITE-MESS
   MOVE 1 TO BAD-NEWS
   GO TO RL-EXIT.
    COMPUTE I = I + 1.
    IF I GREATER 3
* 4 READS WITHOUT FINDING LABEL RECORD - WRITE ERROR AND ABEND
   MOVE +5 TO M
   PERFORM WRITE-MESS
   MOVE 1 TO BAD-NEWS
   GO TO RL-EXIT.
    IF T-LABEL NOT EQUAL TO 'TRAN' AND
   DUMMY NOT EQUAL TO 'LABEL'
* IF NOT LABEL RECORD, READ AGAIN
   GO TO RL-READ.

* FOUND LABEL RECORD, SET FLAGS AND WRITE PROGRESS MESSAGE
    IF T-SCALE-FLAG = 'S'
   MOVE 'T' TO OPT-SCALE-FLAG.
    IF T-EXT-ISTYP-FLAG = 'N'
   MOVE 'T' TO ISS-TYPE-FLAG.
    MOVE +23 TO M.
    PERFORM WRITE-MESS.
    MOVE TRANS-REC TO HDR.
RL-EXIT.           EXIT.
*
*********************************************************
*
LABEL-DT   SECTION.
LABEL-DT-PARA.
* READ THE DATE RECORD AND SET CURRENT DATE VARIABLES

    READ TRANS-FILE AT END
* IF EOF WITH NO DATE RECORD, WRITE ERROR AND ABEND
   MOVE +6 TO M
   PERFORM WRITE-MESS
   MOVE 1 TO BAD-NEWS
   GO TO LD-EXIT.

    MOVE T-LABEL-DT TO TODAYS-DT, HDR-DT.
    MOVE TD-YY TO WS-TD-YY.
    MOVE TODAYS-DT-FT TO WS-TD-MD.

* COMPUTE THE CENTURY - DATE LESS THAN 1970 ASSUMES 21ST CENTURY
    COMPUTE TD-YYYY = TD-YY + 1900.
    IF TD-YYYY LESS 1970
   ADD +100 TO TD-YYYY.
    MOVE TD-YYYY TO S-YYYY-1.
    MOVE TODAYS-DT-FT TO S-DT-FT.
    COMPUTE TD-M-CNT = TD-YYYY * 12 + TD-MM.

* WRITE PROGRESS MESSAGE
    MOVE +24 TO M.
    PERFORM WRITE-MESS.
LD-EXIT.           EXIT.
*
*********************************************************
*
SEP-REC SECTION.
SEP-REC-PARA.
* READ THE BULLETINS FROM THE TRAN FILE, PUTTING THEM INTO
* A SEPARATE FILE

SR-READ.
    READ TRANS-FILE INTO UPD-REC AT END
* EOF BEFORE TRAN RECORDS FOUND - WRITE ERROR AND ABEND
   MOVE +7 TO M
   PERFORM WRITE-MESS
   MOVE 1 TO BAD-NEWS
   GO TO SR-EXIT.

    IF U-ISSUER EQUAL TO '999999'
   NEXT SENTENCE
    ELSE
* IF HAVEN'T HIT 9'S SEPARATOR RECORD, WRITE RECORD TO UPDATE
* FILE AND READ NEXT RECORD
   WRITE UPD-REC
   GO TO SR-READ.

* WRITE 9'S SEPARATOR RECORD TO UPDATE FILE
    WRITE UPD-REC.

* CLOSE FILE AND REOPEN FOR INPUT
    CLOSE UPDATE-FILE.
    OPEN INPUT UPDATE-FILE.

* WRITE PROGRESS MESSAGE
    MOVE +8 TO M.
    PERFORM WRITE-MESS.
SR-EXIT.           EXIT.
*
*********************************************************
*
LOAD-BUF SECTION.
LOAD-BUF-PARA.
* PERFORMS THE PRIMING READS OF THE 3 INPUT FILES,
* AND INITIALIZES THE OUTPUT CUSIP TO THE LOWEST OF
* THE INPUT CUSIPS

* READ THE BULLETIN FILE UNTIL FIND FIRST VALID BULLETIN RECORD
    PERFORM UPD-READ.
    IF U-ISSUER EQUAL TO '000000'
   PERFORM UPD-READ UNTIL (ENDUPD) OR
   (U-ISSUER NOT EQUAL TO '000000').

    MOVE U-X-QSIP TO O-X-QSIP.

* READ THE TRAN FILE UNTIL FIND FIRST VALID TRAN RECORD
    PERFORM TRANS-READ.
    IF T-ISSUER EQUAL TO '000000'
   PERFORM TRANS-READ UNTIL (ENDTRANS) OR
   (T-ISSUER NOT EQUAL TO '000000').

    IF T-X-QSIP < O-X-QSIP
   MOVE T-X-QSIP TO O-X-QSIP.

* READ IN THE FIRST OLD 216 RECORD
    PERFORM OLDTP-READ.

    IF I-X-QSIP < O-X-QSIP
   MOVE I-X-QSIP TO O-X-QSIP.

* WRITE PROGRESS OR ERROR MESSAGE
    IF FATAL-ERROR
   MOVE +9 TO M
    ELSE
   MOVE +10 TO M.
    PERFORM WRITE-MESS.
LB-EXIT.           EXIT.
*
**********************************************************
*
CLOSE-FILES SECTION.
CLOSE-FILES-PARA.
* CLOSE THE FILES
    MOVE +11 TO M.
    PERFORM WRITE-MESS.
    CLOSE OLDTP70-FILE, UPDATE-FILE, TRANS-FILE, OUTTP70-FILE,
      PRINT-FILE.
CF-EXIT.           EXIT.
*
*********************************************************
*
MAIN-JOB SECTION.
MAIN-JOB-PARA.
* PROCESS DATA UNTIL END OF ALL FILES OR FATAL ERROR
    MOVE +12 TO M.
    PERFORM WRITE-MESS.
    PERFORM MAIN-RTN UNTIL ENDALL OR FATAL-ERROR.
MAIN-JOB-EXIT.           EXIT.
*
*********************************************************
   *
MAIN-RTN SECTION.
MAIN-RTN-PARA.
* PROCESSES INPUT DATA

    IF NOT ENDOLDTP AND
   I-X-QSIP NOT > U-X-QSIP AND
   I-X-QSIP NOT > T-X-QSIP
   PERFORM OLD-RTN
    ELSE IF NOT ENDUPD AND
   U-X-QSIP NOT > T-X-QSIP
* IF THE UPDATE RECORD HAS THE LOWEST CUSIP, PROCESS IT
   PERFORM UPD-RTN
    ELSE IF NOT ENDTRANS AND
   T-X-QSIP NOT > U-X-QSIP OR
   (T-ISSUE-R = O-ISSUE-R AND T-NOT-PRICE)
* IF THE TRAN RECORD HAS THE LOWEST CUSIP, PROCESS IT
   PERFORM PROC-TRANS.
    IF O-X-QSIP < I-X-QSIP AND
   O-X-QSIP < U-X-QSIP AND
   O-X-QSIP < T-X-QSIP AND
   NOT (T-ISSUE-R = O-ISSUE-R AND T-NOT-PRICE)
   PERFORM WRITE-OUTPUT.
MR-EXIT.           EXIT.

**********************************************************
****      MAIN-RTN SUBROUTINES        ****
**********************************************************
OLD-RTN SECTION.
OLD-RTN-PARA.
    MOVE 'F' TO ADD-A-REC, CAP-7, NOT-OLD-TRD.
    MOVE 'T' TO REC-NOT-TRADED.
    MOVE INTP-REC TO OUTP-REC.
    PERFORM OUTREC-INIT.
    MOVE I-X-QSIP TO HI-X-QSIP.
    MOVE I-TRD-TYPE TO HI-TRD-TYPE.
    MOVE I-CLOSE TO HI-CLOSE.
    MOVE I-CLOSE-ADJ TO HI-CLOSE-ADJ.
    MOVE I-CLOSE-ADJ TO HI-CLOSE-ADJ.
    MOVE I-ADJ-PREV-CLOSE TO HI-ADJ-PREV-CLOSE.
    MOVE I-ADJ-PREV-CLOSE-ADJ TO HI-ADJ-PREV-CLOSE-ADJ.
    MOVE I-LOW TO HI-LOW.
    MOVE I-LOW-ADJ TO HI-LOW-ADJ.
    MOVE I-HIGH TO HI-HIGH.
    MOVE I-HIGH-ADJ TO HI-HIGH-ADJ.
SKIP-C.
    PERFORM OLDTP-READ.
    IF I-ISS-ST = 'C'
   ADD +1 TO P-CNT
   GO TO SKIP-C.
ORTN-EXIT.           EXIT.
*
OLDTP-READ SECTION.
OLDTP-READ-PARA.
* READ IN THE OLD 216 RECORD

    IF ENDOLDTP
   GO TO OR-EXIT
    ELSE
   READ OLDTP70-FILE AT END
      MOVE 1 TO BAD-NEWS
      MOVE  22 TO M
      PERFORM WRITE-MESS
      PERFORM SHOW-QSIP
      MOVE 'T' TO W-ENDOLDTP
      GO TO OR-EXIT.
    IF I-ISSUER EQUAL TO '999999'
   MOVE 'T' TO W-ENDOLDTP
   GO TO OR-EXIT
    ELSE
   ADD 1 TO  W-OLDTP-CNT
* FOR STOCKS RESET THE SPLIT ADJUSTMENT FACTOR TO 1
   MOVE I-ISS-TYPE TO TEST-ISS-TYPE
   IF TEST-IS-STOCK
      MOVE '1' TO I-ADJ-FACT.

OR-EXIT.           EXIT.
*
*********************************************************
*
UPD-RTN SECTION.
UPD-RTN-PARA.
*
*  PROCESS UPDATE - BULLETINS AND CAPITALIZATION CHANGES
*
    IF U-X-QSIP NOT = O-X-QSIP
* THIS CUSIP WAS NOT ON THE INPUT 216 FILE, SO MAKE A
* SKELETON RECORD
   MOVE U-X-QSIP TO O-X-QSIP
   PERFORM NEW-RECRD
   IF U-ISS-ST EQUAL TO '1' OR '3' OR '4' OR 'K' OR 'N'
      ADD +1 TO SC-CNT
   ELSE
      ADD +1 TO NSC-CNT.

* PROCESS THIS UPDATE RECORD
    PERFORM PROC-UPD.

* READ THE NEXT UPDATE RECORD
    PERFORM UPD-READ.

UR-EXIT.           EXIT.
*
**********************************************************
*
****      UPD-RTN SUBROUTINES         ****
**********************************************************
*
NEW-RECRD SECTION.
NEW-RECRD-PARA.
*
* INITIALIZE THE OUTPUT RECORD FIELDS
    MOVE W-NINES TO O-DATA.
    MOVE ZEROS TO O-PRICE-SECT.
    MOVE ZEROS TO O-DIVIDEND-AREAS.
    MOVE 'H' TO O-DIV-RV-IND(1), O-DIV-RV-IND(2).
    MOVE '2' TO O-DIV-ECODE(1), O-DIV-ECODE(2).
    MOVE 'T' TO ADD-A-REC.
    PERFORM OUTREC-INIT.

* IF ADDING FOR A PRICE TRAN RECORD, SET DEFAULT ISSUE TYPE
    IF O-X-QSIP = T-X-QSIP
   MOVE '4' TO O-ISS-ST
   IF T-FUTURE-PRICE
      MOVE 'C' TO O-ISS-TYPE
   ELSE IF T-BOND-PRICE
      MOVE '6' TO O-ISS-TYPE
   ELSE IF T-STOCK-PRICE
      MOVE '0' TO O-ISS-TYPE
   ELSE IF T-OPTION-PRICE
      IF T-EQUITY-OPT
         MOVE 'B' TO O-ISS-TYPE
      ELSE
         MOVE 'D' TO O-ISS-TYPE.

NR-EXIT.           EXIT.
*
**********************************************************
*
WRITE-OUTPUT SECTION.
WRITE-OUTPUT-PARA.
* DOES SOME UPDATES TO OUTPUT 216 RECORD AND WRITES IT

    IF O-ISS-ST = 'E'
* DO NOT KEEP OLD RECORD ON EXCHANGE CHANGES
   GO TO WO-EXIT.

    MOVE O-ISS-TYPE TO TEST-ISS-TYPE.
    IF C5-CHANGE
   MOVE 'F' TO C5-STAT-CHANGE
    ELSE
* IF HAVEN'T HAD 'C' OR '5' STATUS BULLETIN,
* TEST IF SECURITY HAS MATURED
   IF TEST-IS-OPTION OR TEST-IS-BOND OR TEST-IS-FUTURE
      MOVE O-MAT-YYYY TO YYYY-1
      MOVE O-MAT-MM TO MM-1
      MOVE O-MAT-DD TO DD-1
      IF (CHK-DATE  LESS SAVE-DATE)
         IF (O-ISS-ST EQUAL TO '0')
        MOVE '5' TO O-ISS-ST
         ELSE IF (O-ISS-ST EQUAL TO '5')
        MOVE 'C' TO O-ISS-ST
         ELSE
        NEXT SENTENCE
      ELSE
         NEXT SENTENCE
   ELSE
      NEXT SENTENCE.

    IF TEST-IS-EQUITY-OPTION
* FILL IN OPTIONS DIVIDEND SECTION
   PERFORM CHK-COM-DIV VARYING P FROM 1 BY 1 UNTIL P > 2
    ELSE IF TEST-IS-STOCK
* PROCESS DIVIDENDS
   PERFORM CHK-DIV-RECS.

    PERFORM TRD-RT.
    IF O-DESCRIP-CHK EQUAL TO '9999' AND O-X-QSIP NOT EQUAL TO
          '9999999999'
   ADD +1 TO NO-DESCRIP-CNT.
    IF O-X-QSIP NOT EQUAL TO '9999999999'
   ADD +1 TO W-OUTP-CNT.

    IF NON-EXTENDED
* IF THEY DON'T WANT EXTENDED ISSUE TYPE, CONVERT TO NON-EXTENDED
   IF O-ISS-TYPE NOT < 'H' AND O-ISS-TYPE NOT > 'Z'
      AND O-ISS-TYPE NOT = 'Q'
      MOVE '6' TO O-ISS-TYPE.

    IF TEST-IS-EQUITY-OPTION
* IF AN OPTION'S UNDERLYING SECURITY HAD A NEW IAD REPORTED,
* PUT IT IN OPTION RECORD
   IF O-OP-UCUS = HOLD-IAD-CUSIP
      MOVE HOLD-IAD TO O-OP-IAD.

    WRITE OUTP216-REC FROM OUTP-REC.
    MOVE O-X-QSIP TO OUT-QSIP-SAVE.
    MOVE O-X-TIC TO OUT-TIC-SAVE.
    MOVE O-SIC TO OUT-SIC-SAVE.
    MOVE O-ISSUER-DESCRIP TO OUT-DESC-SAVE.
    IF NEW-REC
   MOVE 'F' TO ADD-A-REC.
    MOVE 'T' TO REC-NOT-TRADED.
    MOVE 'F' TO CAP-7, NOT-OLD-TRD.

WO-EXIT.
    EXIT.
*
**********************************************************
*
TRD-RT   SECTION.
TRD-RT-PARA.
*
*  CHECKS TO SEE IF RECORD TRADED.
*  ADJUSTS TRADE TYPE, ISSUE-STATUS, ADJUSTMENT FACTOR,
*  ADJUSTMENT TO PREVIOUS CLOSE WHERE EVER NECESSARY.
*
    MOVE O-ISS-TYPE TO TEST-ISS-TYPE.
    IF NOT-TRADED
   IF O-TRD-TYPE EQUAL TO 'Z' OR NEW-TRD-TYPE
*  IF ACTIVE FOR DIVIDENDS ONLY, OR HAS A NEW TRADE TYPE,
*  DON'T UPDATE TRADE TYPE, PREVIOUS CLOSE OR YIELD
      GO TO TT-EXIT
   ELSE
*  SET UNTRADED SECURITY'S TRADE TYPE TO 9
      MOVE '9' TO O-TRD-TYPE
      IF NOT TEST-IS-STOCK
         GO TO TT-EXIT
      ELSE IF (O-ADJ-FACT NOT EQUAL TO 1 AND 0) AND
          NOT (NEW-REC)
*  IF IS A STOCK AND HAS SPLIT, ADJUST PREVIOUS CLOSE
         COMPUTE O-ADJ-PREV-CLOSE =
        O-ADJ-PREV-CLOSE * O-ADJ-FACT
         GO TO TT-EXIT
      ELSE
         GO TO TT-EXIT
    ELSE
   NEXT SENTENCE.
    IF (O-ISS-ST EQUAL TO '3' OR '4')
   IF NEW-REC
      NEXT SENTENCE
   ELSE
*  IF ISSUE STATUS WAS 3 OR 4, AND RECORD ISN'T NEW, RESET
*  ISSUE STATUS TO 0
      MOVE '0' TO O-ISS-ST
    ELSE IF O-ISS-ST EQUAL TO 'N'
*  IF WAS A NEW RECORD, AND HAS TRADED, SET ISSUE STATUS TO 4
   MOVE '4' TO O-ISS-ST
    ELSE IF O-ISS-ST EQUAL TO '1' OR '2' OR '5' OR '6'
               OR '7' OR '8' OR '9'
*  LEAVE THESE ISSUE STATUS' ALONE
   NEXT SENTENCE
    ELSE
*  RESET ALL REMAINING STATUS' TO 0
   MOVE '0' TO O-ISS-ST.

*  CALCULATE CURRENT YIELD FOR BONDS
    IF (O-UNQ-CD EQUAL TO 'U' OR 'Z')
*  BONDS TRADING FLAT HAVE NO NEW YIELD
   NEXT SENTENCE
    ELSE IF (O-ISS-ST = '0' OR '1' OR '3' OR '4' OR '6' OR
            '8' OR '9')
   IF O-X-CD = 'L' OR 'M'
*  LISTED BONDS WITH ACTIVE STATUS - YIELD IS AS FOLLOWS
      IF O-TRD-TYPE EQUAL TO '8' OR 'A' AND O-LOW
*  BID/ASK OR MODEL PRICES USE BID (O-LOW)
         NOT EQUAL TO 0
         COMPUTE O-BD-CUR-YLD =
         ((O-COUP-RATE * 100.          0) / O-LOW)
      ELSE IF O-TRD-TYPE EQUAL TO '9'
         IF O-VOL EQUAL TO 0 AND O-LOW NOT EQUAL TO 0
*  UNTRADED WITH BID/ASK USE OLD BID (O-LOW)
        COMPUTE O-BD-CUR-YLD ROUNDED =
            ((O-COUP-RATE * 100.          0) / O-LOW)
         ELSE IF O-CLOSE NOT EQUAL TO 0
*  OTHER UNTRADED USE CLOSE
        COMPUTE O-BD-CUR-YLD ROUNDED =
            ((O-COUP-RATE * 100.          0) / O-CLOSE)
         ELSE
*  IF NO BID OR CLOSE, WRITE ERROR
        MOVE 0 TO O-BD-CUR-YLD
        MOVE +26 TO M
        PERFORM WRITE-MESS
        MOVE O-X-QSIP TO O-ERR-QSIP
        MOVE ERR-LINE-2 TO PRINT-LINE
        WRITE PRINT-REC AFTER ADVANCING 1 LINES
      ELSE IF O-VOL EQUAL TO 0 AND
* IF TRADE TYPE NO '8', 'A', '9', AND NOT '0', USE BID (O-LOW)
          O-TRD-TYPE NOT EQUAL TO '0' AND
          O-LOW NOT EQUAL TO 0
          COMPUTE O-BD-CUR-YLD ROUNDED =
             ((O-COUP-RATE * 100.          0) / O-LOW)
      ELSE IF O-CLOSE NOT EQUAL TO 0
* IF TRADE TYPE '0' AND HAVE CLOSE, USE CLOSE
         COMPUTE O-BD-CUR-YLD ROUNDED =
         ((O-COUP-RATE * 100.          0) / O-CLOSE)
      ELSE
* IF TRADE TYPE '0' AND NO CLOSE, PRINT ERROR
         MOVE 0 TO O-BD-CUR-YLD
         MOVE +26 TO M
         PERFORM WRITE-MESS
         MOVE O-X-QSIP TO O-ERR-QSIP
         MOVE ERR-LINE-2 TO PRINT-LINE
         WRITE PRINT-REC AFTER ADVANCING 1 LINES
   ELSE IF O-X-CD EQUAL TO 'N' OR 'O' OR 'P' OR
               'S' OR 'X'
*  NON-LISTED BONDS GET YIELD AS FOLLOWS
      IF O-TRD-TYPE EQUAL TO '9' AND
         O-ADJ-PREV-CLOSE NOT EQUAL TO 0
*  IF UNTRADED, USE ADJUSTED PREVIOUS CLOSE
         COMPUTE O-BD-CUR-YLD =
       ((O-COUP-RATE * 100.          0) / O-ADJ-PREV-CLOSE)
      ELSE IF O-LOW NOT EQUAL TO 0
*  IF TRADED AND HAVE BID (O-LOW), USE BID
        COMPUTE O-BD-CUR-YLD = ((O-COUP-RATE
* 100.          0) / O-LOW)
      ELSE IF O-CLOSE NOT EQUAL TO 0
*  NO BID, SO USE CLOSE
        COMPUTE O-BD-CUR-YLD =
           ((O-COUP-RATE * 100.          0) / O-CLOSE)
      ELSE
*  NO BID, NO CLOSE, PRINT ERROR
         MOVE 0 TO O-BD-CUR-YLD
         MOVE +26 TO M
         PERFORM WRITE-MESS
         MOVE O-X-QSIP TO O-ERR-QSIP
         MOVE ERR-LINE-2 TO PRINT-LINE
         WRITE PRINT-REC AFTER ADVANCING 1 LINES
      ELSE
         NEXT SENTENCE
   ELSE
      NEXT SENTENCE.

    IF TEST-IS-STOCK
   IF NEW-REC
      IF CAP-7-REC
         NEXT SENTENCE
      ELSE
*  NEW STOCKS WITHOUT CAP CHANGE 7, SET ADJUSTMENT FACTOR TO 1
         MOVE '1' TO O-ADJ-FACT
   ELSE IF O-TRD-TYPE EQUAL TO '1' OR CAP-7-REC
*  IF EXISTING STOCK HAS A SPLIT, ADJUST PREVIOUS CLOSE
      COMPUTE O-ADJ-PREV-CLOSE = O-ADJ-PREV-CLOSE *
                   O-ADJ-FACT
   ELSE
      NEXT SENTENCE.
TT-EXIT.           EXIT.
*
**********************************************************
****       TRANS-UPD-OLD SUBROUTINES      ****
**********************************************************
*
UPD-READ    SECTION.
UPD-READ-PARA.
    IF ENDUPD
   GO TO UR-EXIT
    ELSE
   READ UPDATE-FILE AT END
      MOVE 1 TO BAD-NEWS
      MOVE +20 TO M
      PERFORM WRITE-MESS
      PERFORM SHOW-QSIP
      MOVE 'T' TO W-ENDUPD
      GO TO UR-EXIT.
    IF U-ISSUER EQUAL TO '999999'
   MOVE 'T' TO W-ENDUPD
    ELSE
   MOVE UPD-REC TO LT-U-REC
   ADD  1 TO W-UPD-CNT.
UR-EXIT.           EXIT.
*
*********************************************************
*
PROC-UPD   SECTION.
PROC-UPD-PARA.
*
*  CHECKS TO SEE IF BULLETIN RECORD IS A STATUS CHANGE,STATUS
*  CAPITALIZATION CHANGE OR STATUS CONTINUATION RECORD
*
    IF U-IS-CAP
   PERFORM CAP-REC
    ELSE IF U-IS-BULL
   IF UC-CON-IND GREATER 0
      PERFORM STAT-CON
   ELSE
      PERFORM STAT-CHNG.
*
*********************************************************
****        PROC-UPD SUBROUTINES         ****
*********************************************************
*
CAP-REC SECTION.
CAP-REC-PARA.
    ADD 1 TO CR-CNT.
    IF C-CAP-SECT EQUAL TO 6
   PERFORM IAD-6
    ELSE IF C-CAP-SECT EQUAL TO 7
   PERFORM RECAP-7
    ELSE IF C-CAP-SECT EQUAL TO 9
   PERFORM TRD-REV-9
    ELSE
   PERFORM CAP-SEC-ERROR.
*::::::::::::::::::::::::::::::::::::::::::::::::::::
IAD-6 SECTION.
IAD-6-PARA.
    MOVE C-ISS-TYPE TO TEST-ISS-TYPE.
    IF TEST-IS-BOND
   MOVE C-BOND-SUPP-TYPE TO O-SUPP-TYPE
   GO TO I-6-EXIT.
    MOVE C-IAD-FOOT TO W-IAD-FOOT.
    MOVE C-IAD TO W-IAD.
    IF W-IAD-FOOT EQUAL TO '9'
   MOVE 0 TO W-IAD
    ELSE IF W-IAD-FOOT EQUAL TO '7'
   MOVE '8' TO W-IAD-FOOT
   COMPUTE W-IAD = (W-IAD / 10)
    ELSE IF W-IAD LESS 10
   NEXT SENTENCE
    ELSE IF W-IAD LESS 20
   MOVE '1' TO W-IAD-FOOT
   COMPUTE W-IAD = (W-IAD - 10)
    ELSE IF W-IAD LESS 30
   MOVE '2' TO W-IAD-FOOT
   COMPUTE W-IAD = (W-IAD - 20)
    ELSE
   MOVE '7' TO W-IAD-FOOT
   COMPUTE W-IAD = (W-IAD / 10).
    MOVE W-IAD-FOOT TO O-IAD-FOOT.
    MOVE W-IAD      TO O-IAD.
    MOVE C-EPS TO O-ERN-PER-SHR.
    MOVE C-EPS-IND TO O-NEG-IND.
    MOVE C-12M-END-DT TO O-END12-DT.
* SAVE THE LATEST IAD UPDATE TO PUT IN OPTIONS RECORDS
    MOVE W-IAD TO HOLD-IAD.
    MOVE U-ISSUE-R TO HOLD-IAD-CUSIP.
I-6-EXIT.           EXIT.
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
RECAP-7 SECTION.
RECAP-7-PARA.
    IF O-ISS-TYPE EQUAL TO 'B' OR 'C' OR 'D'
*  OPTIONS AND FUTURES DON'T HAVE CAP 7'S, SO SKIP ANY U GET
   GO TO R-7-EXIT.
    MOVE C-SHRS-OUT   TO O-SHRS-OUT.
    MOVE C-ADJ-FAC    TO O-ADJ-FACT.
    IF C-ADJ-FAC NOT EQUAL TO 1.          0
   MOVE 'T' TO CAP-7
    ELSE
   NEXT SENTENCE.
R-7-EXIT.           EXIT.
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
TRD-REV-9   SECTION.
TRD-REV-9-PARA.
    MOVE C-ISS-TYPE TO TEST-ISS-TYPE.
    IF TEST-IS-BOND
   MOVE C-SNP-BD-RATE TO O-SNP-BD
   MOVE C-MOODY-FIN-QL TO O-MOODY-RATE
   MOVE C-TRANS-CD TO O-BD-TRANS-CD
   IF C-BOND-FORM EQUAL '9'
      NEXT SENTENCE
   ELSE
      MOVE C-BOND-FORM TO O-BOND-FORM
    ELSE IF TEST-IS-STOCK
   MOVE C-SNP-BD-RATE  TO O-SNP-RATE
   MOVE C-MARG-IND TO O-MRG-IND
   MOVE C-TRANS-CD TO O-DIV2-TRANS-CD(2)
    ELSE IF TEST-IS-OPTION
   MOVE C-TRD-UNT-ST  TO O-TRD-UN-ST
   MOVE C-X-PR-ST    TO O-XPRICE-ST
   MOVE C-TRD-UNT-ADJ TO O-TRD-ADJ.
T-R-EXIT.           EXIT.
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
CAP-SEC-ERROR SECTION.
CAP-SEC-ERROR-PARA.
    MOVE +14 TO M.
    PERFORM WRITE-MESS.
    PERFORM SHOW-QSIP.
    MOVE 2 TO BAD-NEWS.
*
*********************************************************
*
STAT-CON SECTION.
STAT-CON-PARA.
    ADD +1 TO STAT-CON-CNT.
    ADD +1 TO CONT-CNT.
    IF STAT-CON-CNT GREATER 5
   MOVE 0 TO STAT-CON-CNT
   MOVE +15 TO M   PERFORM WRITE-MESS
   PERFORM SHOW-QSIP
   MOVE 2   TO BAD-NEWS
   GO TO STAT-CON-EXIT
    ELSE IF UC-CON-IND = 1
   MOVE UC-ISSUER-1 TO O-ISSUER-1
    ELSE IF UC-CON-IND = 2
   MOVE UC-ISSUER-2 TO O-ISSUER-2
   MOVE UC-ISSUE-1 TO O-ISSUE-1
    ELSE IF UC-CON-IND = 3
   MOVE UC-ISSUE-2 TO O-ISSUE-2
    ELSE IF UC-CON-IND = 4
   MOVE UC-TICK     TO O-X-TIC
   MOVE UC-SIC      TO O-SIC
   MOVE UC-ISS-TYPE TO O-ISS-TYPE
   MOVE UC-TRD-TYPE TO O-TRD-TYPE
   MOVE UC-ISS-ST   TO O-ISS-ST
    ELSE IF UC-CON-IND = 5
   MOVE UC-EXP-DT   TO O-EXP-DT
   MOVE UC-ADJ-FACT TO O-ADJ-FACT
   MOVE O-ISS-TYPE TO TEST-ISS-TYPE
   IF TEST-IS-STOCK
      MOVE UC-BOND-FORM TO O-SNP-RATE
   ELSE
      MOVE UC-BOND-FORM TO O-BOND-FORM
    ELSE
   MOVE +13 TO M  PERFORM WRITE-MESS
   MOVE 2   TO BAD-NEWS
   PERFORM SHOW-QSIP.
STAT-CON-EXIT.           EXIT.
*
*********************************************************
*
STAT-CHNG SECTION.
STAT-CHNG-PARA.
    MOVE O-ISS-TYPE TO TEST-ISS-TYPE.
    ADD 1 TO SCNG-CNT.
    IF U-ISS-ST NOT EQUAL TO 'G'
   MOVE U-ISS-ST TO O-ISS-ST.
    IF U-HAS-CONT
   MOVE +0 TO STAT-CON-CNT.
    IF U-ISS-ST EQUAL TO 'A' OR 'D' OR 'S' OR
             'T' OR 'X' OR 'Y'
   MOVE 0 TO O-HIGH, O-LOW, O-CLOSE, O-VOL
   MOVE TODAYS-DT-FT TO O-PRICE-DT
   IF TEST-IS-OPTION AND O-P-ADJ-IND EQUAL TO '1'
      MOVE '0' TO O-HIGH-ADJ
      MOVE '0' TO O-LOW-ADJ
      MOVE '0' TO O-CLOSE-ADJ
   ELSE
      NEXT SENTENCE
    ELSE
   NEXT SENTENCE.
    IF U-HAS-XREF
   MOVE U-XREF-ISSUER TO O-XREFQSIP
    ELSE IF U-HAS-UCUS
   MOVE U-XREF-QSIP TO O-OP-UCUS.
    IF U-ISS-ST   EQUAL TO '8' OR '9'
   MOVE U-EFCT-MM  TO WS-D-MM, O-MAT-MM
   MOVE U-EFCT-DD  TO WS-D-DD, O-MAT-DD
   MOVE U-EFCT-Y   TO WS-D-YY
   PERFORM TRANS-DT-FIX
   MOVE WS-D-YYYY TO O-MAT-YYYY.
    IF (U-ISS-ST EQUAL TO '5' OR 'C')
   MOVE 'T' TO C5-STAT-CHANGE.

    IF TEST-IS-BOND AND U-ISS-ST = '5'
* TEST IF BOND WAS CALLED OR MATURED AND SET CALL ENTRY CODE
      MOVE O-MAT-YYYY TO YYYY-1
      MOVE O-MAT-MM TO MM-1
      MOVE O-MAT-DD TO DD-1
      IF CHK-DATE  NOT > SAVE-DATE
         MOVE '5' TO O-ENTRY-CODE.

STAT-CHNG-EXIT.           EXIT.
*
*********************************************************
****      COMMON AND MISCELLANEOUS SUBROUTINES   ****
*********************************************************
*
PROC-TRANS    SECTION.
PROC-TRANS-PARA.

    IF T-X-QSIP NOT = O-X-QSIP AND
   NOT (T-ISSUE-R = O-ISSUE-R AND T-NOT-PRICE)
* THIS CUSIP WAS NOT ON THE INPUT 216 FILE, SO MAKE A
* SKELETON RECORD
   MOVE T-X-QSIP TO O-X-QSIP
   PERFORM NEW-RECRD
   ADD +1 TO NSC-CNT.
*
*  PROCESSES THE VARIOUS KINDS OF TRANSMISSION RECORDS
*
    IF T-FUTURE-PRICE
   PERFORM FUTURE-REC-PROC
   IF NOT-TRADED
      NEXT SENTENCE
   ELSE
      PERFORM NEW-TRDNDT
    ELSE IF T-DIV
   ADD 1 TO DR-CNT
   IF T-ISSUE-R NOT EQUAL TO D-ISSUE-R (1)
      MOVE SPACES TO DIV-REC (2)
      MOVE TRANS-REC TO DIV-REC (1)
      IF R-ISS-TYPE EQUAL TO '0'
         IF T-ISSUE-R NOT EQUAL TO
          COM-OPT-ISSUE-R (1)
        MOVE SPACES TO COM-OPT-DIV-REC (2)
        MOVE TRANS-REC TO COM-OPT-DIV-REC (1)
         ELSE
        MOVE TRANS-REC TO COM-OPT-DIV-REC (2)
      ELSE
         NEXT SENTENCE
   ELSE
      MOVE TRANS-REC TO DIV-REC (2)
      IF R-ISS-TYPE EQUAL TO '0'
         IF T-ISSUE-R NOT EQUAL TO
         COM-OPT-ISSUE-R (1)
        MOVE SPACES TO COM-OPT-DIV-REC (2)
        MOVE TRANS-REC TO COM-OPT-DIV-REC (1)
         ELSE
        MOVE TRANS-REC TO COM-OPT-DIV-REC (2)
      ELSE
         NEXT SENTENCE
    ELSE IF T-STOCK-PRICE OR T-BOND-PRICE OR T-OPTION-PRICE
   PERFORM PRICE-REC-PROC
   IF NOT-TRADED
      NEXT SENTENCE
   ELSE
      PERFORM NEW-TRDNDT
    ELSE IF T-OPTION-BA
   ADD 1 TO BA-CNT
   PERFORM BA-REC-PROC
   IF NOT-TRADED
      NEXT SENTENCE
   ELSE
      PERFORM NEW-TRDNDT
    ELSE IF T-FUTURE-OI
   ADD 1 TO OI-CNT
   PERFORM FUT-OI-PROC
    ELSE IF T-RBI
   IF T-ISSUE-R EQUAL TO O-ISSUE-R
      PERFORM REG-BOND-PROC
   ELSE
      NEXT SENTENCE
    ELSE IF T-CALL
   ADD 1 TO CL-CNT
   IF T-ISSUE-R EQUAL TO O-ISSUE-R
      PERFORM CALL-BOND-PROC
   ELSE
      NEXT SENTENCE
    ELSE
   NEXT SENTENCE.
    PERFORM TRANS-READ.
*
*******************************************************
****     PROC-TRANS SUBROUTINES        ****
*******************************************************
*
TRANS-READ SECTION.
TRANS-READ-PARA.
    IF ENDTRANS
   GO TO TR-EXIT
    ELSE
   READ TRANS-FILE AT END
      MOVE 1 TO BAD-NEWS
      MOVE +21 TO M
      PERFORM WRITE-MESS
      PERFORM SHOW-QSIP
      MOVE 'T' TO W-ENDTRANS
      GO TO TR-EXIT.
    IF T-ISSUER EQUAL TO '999999'
   MOVE 'T' TO W-ENDTRANS
    ELSE
   MOVE TRANS-REC TO LT-REC
   ADD 1 TO W-TRANS-CNT
   IF T-STOCK-PRICE AND T-UNQ-CD = 'W'
      MOVE '1' TO T-UNQ-CD
   ELSE
      NEXT SENTENCE.
TR-EXIT.           EXIT.
*
*********************************************************
*
NEW-TRDNDT SECTION.
NEW-TRDNDT-PARA.
    IF O-TRD-TYPE EQUAL TO 'Z'
      GO TO NEW-TD-EXIT.
    IF (O-TRD-TYPE EQUAL TO 'A')
   OR ((O-TRD-TYPE EQUAL TO '8')  AND
       NOT (O-X-CD EQUAL TO 'G' OR 'H' OR 'I' OR 'N' OR
                'O' OR 'P' OR 'S' OR 'U' OR
                'W' OR 'X' OR 'Y'))
*  IF HAVE MODEL PRICE, OR BID/ASK PRICING AND EXCH IS NOT
*  A BID/ASK EXCH, SET PRICING DATE, BUT DON'T RESET LAST
*  TRADE DATE
   MOVE T-PRICING-DT TO O-PRICE-DT
    ELSE
* HAVE NEW TRADE DATA, SET PRICE DATE AND LAST TRADE DATE
   MOVE T-PRICING-DT TO O-PRICE-DT
   MOVE HOLD-LAST-TRDNDT TO O-LAST-TDT.
NEW-TD-EXIT.           EXIT.
*
*********************************************************
*
FUTURE-REC-PROC SECTION.
FUTURE-REC-PROC-PARA.
    IF T-CLOSE EQUAL TO 0 AND T-LOW EQUAL TO 0
               AND T-HIGH EQUAL TO 0
   ADD +1 TO BADPR-CNT
   GO TO FRP-EXIT
    ELSE
   NEXT SENTENCE.
    ADD 1 TO PR-CNT.
    MOVE 'F' TO REC-NOT-TRADED.
    MOVE T-TRD-TYPE TO O-TRD-TYPE.
    MOVE T-VOL TO O-VOL.
    IF (NEW-REC OR O-TRD-TYPE EQUAL TO  '8' OR '9')
   NEXT SENTENCE
    ELSE IF O-FUT-DP4-CD EQUAL TO '4'
   MOVE O-CLOSE4 TO O-ADJ-PREV-CLOSE4
    ELSE
   MOVE O-CLOSE TO O-ADJ-PREV-CLOSE.
    IF T-FUT-DIVISOR EQUAL TO '1'
   MOVE 10 TO DIV
    ELSE IF T-FUT-DIVISOR EQUAL TO '2' OR 'F'
   MOVE 100 TO DIV
    ELSE IF T-FUT-DIVISOR EQUAL TO '3'
   MOVE 1000 TO DIV
    ELSE IF T-FUT-DIVISOR EQUAL TO '4'
   MOVE 10000 TO DIV.
    COMPUTE TEMP-PRCS =    (T-CLOSE / DIV).
    COMPUTE TEMP-HIGH =    ((T-CLOSE + T-HIGH) / DIV).
    COMPUTE TEMP-LOW  =    ((T-CLOSE - T-LOW ) / DIV).
    IF O-FUT-DP4-CD EQUAL TO '4'
   MOVE TEMP-PRCS TO O-CLOSE4
   MOVE TEMP-HIGH TO O-HIGH4
   MOVE TEMP-LOW TO O-LOW4
    ELSE
   MOVE TEMP-PRCS TO O-CLOSE
   MOVE TEMP-HIGH TO O-HIGH
   MOVE TEMP-LOW  TO O-LOW.
    IF (T-FUT-DIVISOR EQUAL TO 'F') AND
    (T-YLD-2-MAT GREATER 0 AND LESS 99)
   MOVE T-YLD-2-MAT TO O-YIELD-2-MAT.
    COMPUTE FUT-TEMP-PRICE =    (T-FUT-CASH-PRICE / DIV).
    MOVE FUT-TEMP-PRICE TO O-FUT-CASH-PRICE.
FRP-EXIT.           EXIT.
*
*********************************************************
*
PRICE-REC-PROC SECTION.
PRICE-REC-PROC-PARA.
*
* ADJUSTED PREVIOUS CLOSE CASH EX-DISTRIBUTIONS ARE NOT
* REFLECTED IN ADJUSTED PREVIOUS CLOSE.
* FURTHER TESTING FOR CONDITIONS OF TRD -TYPE GOING FROM
* 0-9 OR 8-9 ALSO CHECK FUTURE-REC-PROC
*
    IF ((T-CLOSE EQUAL TO 0) AND (T-LOW EQUAL TO 0) AND
          (T-HIGH EQUAL TO 0))
   MOVE T-TRD-TYPE TO O-TRD-TYPE
   MOVE 'T' TO NOT-OLD-TRD
   ADD +1 TO BADPR-CNT
    ELSE
   MOVE 'F' TO REC-NOT-TRADED
   IF (NEW-REC AND T-TRD-TYPE EQUAL TO '1') OR
          (T-TRD-TYPE EQUAL TO '9')
      PERFORM PRICE-CAL
      PERFORM TRD-APC
   ELSE
      PERFORM TRD-APC
      PERFORM PRICE-CAL.
    ADD 1 TO PR-CNT.
PRP-EXIT.           EXIT.
*
**********************************************************
*
TRD-APC SECTION.
TRD-APC-PARA.

* UPDATE THE TRADE TYPE FROM THE TRAN RECORD
    MOVE T-TRD-TYPE TO O-TRD-TYPE.

* IF THERE IS NO INPUT 216, SET APC TO 0'S
    IF HI-X-QSIP NOT = O-X-QSIP
   MOVE 0 TO O-ADJ-PREV-CLOSE
   MOVE 0 TO O-ADJ-PREV-CLOSE-ADJ
   GO TO TRD-APC-EXIT.

* UPDATE THE ADJUSTED PREVIOUS CLOSE, IF NECESSARY
    IF NOT (O-X-CD EQUAL TO  'G' OR 'H' OR 'I' OR
            'N' OR 'O' OR 'P' OR 'S' OR
            'U' OR 'W' OR 'X' OR 'Y')
* IF NOT A BID/ASK EXCHANGE.          .          .
   IF HI-TRD-TYPE NOT < '0' AND HI-TRD-TYPE < '8'
* AND IT DID TRADE YESTERDAY, THEN USE YESTERDAY'S CLOSE
      MOVE HI-CLOSE TO O-ADJ-PREV-CLOSE
      MOVE HI-CLOSE-ADJ TO O-ADJ-PREV-CLOSE-ADJ
   ELSE
* NO TRADE SO KEEP OLD ADJUSTED PREVIOUS CLOSE
      MOVE HI-ADJ-PREV-CLOSE TO O-ADJ-PREV-CLOSE
      MOVE HI-ADJ-PREV-CLOSE-ADJ TO O-ADJ-PREV-CLOSE-ADJ
    ELSE
* IS A BID/ASK EXCHANGE
   IF HI-TRD-TYPE NOT < '0' AND HI-TRD-TYPE < '9'
* DID PRICE YESTERDAY
      IF HI-LOW NOT = 0
* USE THE LOW (BID) IF WE HAVE IT
         MOVE HI-LOW TO O-ADJ-PREV-CLOSE
         MOVE HI-LOW-ADJ TO O-ADJ-PREV-CLOSE-ADJ
      ELSE
* YESTERDAY'S BID NOT AVAILABLE, USE YESTERDAY'S ASK
         MOVE HI-HIGH TO O-ADJ-PREV-CLOSE
         MOVE HI-HIGH-ADJ TO O-ADJ-PREV-CLOSE-ADJ
   ELSE
* NO BID/ASK SO KEEP OLD ADJUSTED PREVIOUS CLOSE
      MOVE HI-ADJ-PREV-CLOSE TO O-ADJ-PREV-CLOSE
      MOVE HI-ADJ-PREV-CLOSE-ADJ TO O-ADJ-PREV-CLOSE-ADJ.
TRD-APC-EXIT.           EXIT.
*
***********************************************************
*
PRICE-CAL SECTION.
PRICE-CAL-PARA.
* CONVERT PRICES TO PROPER REPRESENTATION

* CONVERT VOLUMES
* BONDS REPORT VOLUME AS IS
* IF OPTION OVERFLOW, ADD HIGH ORDER DIGIT, ELSE OPTIONS
* REPORT VOLUME AS IS
* STOCKS ADD HIGH ORDER DIGIT AND MULTIPLY BY 100
    IF T-BOND-PRICE
   MOVE T-VOL TO O-VOL
    ELSE IF T-OPTION-PRICE
   IF T-OPT-OFLOW
      COMPUTE VOL-TEMP = (T-VOL-HO * 100000) + T-VOL
      MOVE VOL-TEMP TO O-VOL
   ELSE
      MOVE T-VOL TO O-VOL
    ELSE
    COMPUTE VOL-TEMP = (T-VOL-HO * 100000) + T-VOL
    COMPUTE VOL-TEMP = VOL-TEMP * 100
    MOVE VOL-TEMP TO O-VOL.

* MOVE PRICE DATA TO WORKING STORAGE, CONVERTING OFFSETS TO
* REAL PRICES
    IF T-STOCK-PRICE OR (T-OPTION-PRICE AND T-OPT-OFLOW)
* ADD HIGH ORDER DIGITS FOR STOCKS OR OVERFLOWED OPTIONS
   COMPUTE WS-CLOSE-INT = (T-CLOSE-HO * 100000) + T-CLOSE
   COMPUTE WS-HIGH-INT =  WS-CLOSE-INT +
              ((T-HIGH-HO * 10000) + T-HIGH)
   COMPUTE WS-LOW-INT =   WS-CLOSE-INT -
              ((T-LOW-HO * 10000) + T-LOW)
    ELSE
   MOVE T-CLOSE TO WS-CLOSE-INT
   COMPUTE WS-HIGH-INT =  WS-CLOSE-INT + T-HIGH
   COMPUTE WS-LOW-INT =   WS-CLOSE-INT - T-LOW.

* FIGURE SCALING FACTOR
    IF T-STOCK-PRICE OR T-BOND-PRICE
* STOCKS AND BONDS USE SCALING INDICATOR
   IF IN-THOU
      MOVE 1000 TO DIV
   ELSE IF IN-HUND OR WITH-32NDS
      MOVE 100 TO DIV
   ELSE IF IN-8THS
      MOVE 8 TO DIV
   ELSE IF IN-16THS
      MOVE 16 TO DIV
   ELSE IF IN-32NDS
      MOVE 32 TO DIV
   ELSE IF IN-64THS
      MOVE 64 TO DIV
   ELSE
      NEXT SENTENCE
    ELSE IF T-CURRENCY-OPT
* CURRENCY OPTIONS ARE IN DOLLARS AND CENTS
   MOVE 100 TO DIV
    ELSE IF T-INTEREST-OPT OR T-DEBT-OPT
* INTEREST RATE AND DEBT OPTIONS ARE IN 64THS
   MOVE 64 TO DIV
    ELSE
* ALL OTHER OPTIONS ARE IN 8THS
   MOVE 8 TO DIV.

* APPLY SCALING FACTORS
    COMPUTE TEMP-CLOSE ROUNDED = (WS-CLOSE-INT / DIV).
    COMPUTE TEMP-HIGH ROUNDED = (WS-HIGH-INT / DIV).
    COMPUTE TEMP-LOW ROUNDED = (WS-LOW-INT / DIV).

    IF T-X-CD = 'P' AND TNOTE-TBOND
* TREASURY NOTES AND BONDS NEED LOW ORDER DIGITS ADDED,
* TO MAKE 32NDS IN DECIMAL REPRESENTATION
   COMPUTE WHOLE-32NDS ROUNDED = TEMP-CLOSE * 32
   COMPUTE FRAC-32NDS ROUNDED = WHOLE-32NDS / 32
   MOVE FRAC-32NDS TO TEMP-CLOSE
   COMPUTE WHOLE-32NDS ROUNDED = TEMP-HIGH * 32
   COMPUTE FRAC-32NDS ROUNDED = WHOLE-32NDS / 32
   MOVE FRAC-32NDS TO TEMP-HIGH
   COMPUTE WHOLE-32NDS ROUNDED = TEMP-LOW * 32
   COMPUTE FRAC-32NDS ROUNDED = WHOLE-32NDS / 32
   MOVE FRAC-32NDS TO TEMP-LOW
    ELSE
   NEXT SENTENCE.

    IF T-OPTION-PRICE
* SET OPTION PRICE CODE AND RESCALE PRICES IF NECESSARY
   IF PRICE-PER-SHARE
      IF T-EQUITY-OPT OR T-INDEX-OPT
         MOVE '0' TO O-DIV-LR-CD (1)
         COMPUTE TEMP-CLOSE ROUNDED = TEMP-CLOSE / 100
         COMPUTE TEMP-HIGH ROUNDED = TEMP-HIGH / 100
         COMPUTE TEMP-LOW ROUNDED = TEMP-LOW / 100
      ELSE IF T-CURRENCY-OPT
         MOVE '0' TO O-DIV-LR-CD (1)
      ELSE IF T-INTEREST-OPT OR T-DEBT-OPT
         MOVE '1' TO O-DIV-LR-CD (1)
         IF O-TRD-UN-ST = '7'
        COMPUTE TEMP-CLOSE ROUNDED = TEMP-CLOSE /
            (100 * O-TRD-ADJ)
        COMPUTE TEMP-HIGH ROUNDED = TEMP-HIGH /
            (100 * O-TRD-ADJ)
        COMPUTE TEMP-LOW ROUNDED = TEMP-LOW /
            (100 * O-TRD-ADJ)
         ELSE
        COMPUTE TEMP-CLOSE ROUNDED = TEMP-CLOSE /
            (10 * O-TRD-ADJ)
        COMPUTE TEMP-HIGH ROUNDED = TEMP-HIGH /
            (10 * O-TRD-ADJ)
        COMPUTE TEMP-LOW ROUNDED = TEMP-LOW /
            (10 * O-TRD-ADJ)
      ELSE
         NEXT SENTENCE
   ELSE IF T-EQUITY-OPT OR T-INDEX-OPT
      MOVE '2' TO O-DIV-LR-CD (1)
   ELSE IF T-CURRENCY-OPT
      MOVE '0' TO O-DIV-LR-CD (1)
   ELSE IF T-INTEREST-OPT OR T-DEBT-OPT
      MOVE '3' TO O-DIV-LR-CD (1)
   ELSE
      NEXT SENTENCE
    ELSE
   NEXT SENTENCE.

    MOVE TEMP-CLOSE   TO O-CLOSE.
    MOVE TEMP-HIGH    TO O-HIGH.
    MOVE TEMP-LOW     TO O-LOW.

    IF T-BOND-PRICE
* MOVE YIELD TO MATURITY FOR BONDS
   MOVE T-YLD-2-MAT TO O-YIELD-2-MAT
    ELSE IF T-OPTION-PRICE AND NOT T-OPT-OFLOW
* OPTIONS NOT OVERFLOWED HAVE UNDERLYING PRICE IN 64THS
   COMPUTE O-UND-STK-PRICE ROUNDED =
          T-UND-STK-PR / 64
    ELSE IF T-OPTION-PRICE
* MOVE PRICE OVERFLOW FIELDS AND SET PRICE OVERFLOW FLAG
   MOVE TEMP-HIGH-OFLOW TO O-HIGH-ADJ
   MOVE TEMP-LOW-OFLOW TO O-LOW-ADJ
   MOVE TEMP-CLOSE-OFLOW TO O-CLOSE-ADJ
   IF (TEMP-HIGH-OFLOW NOT = 0) OR
      (TEMP-LOW-OFLOW NOT = 0) OR
      (TEMP-CLOSE-OFLOW NOT = 0)
      MOVE 0 TO O-P-ADJ-IND
   ELSE
      MOVE 1 TO O-P-ADJ-IND
    ELSE
   NEXT SENTENCE.

PCS-EXIT.           EXIT.
*
*********************************************************
*
BA-REC-PROC SECTION.
BA-REC-PROC-PARA.
* PROCESS OPTION BID-ASK RECORDS

* IF BID/ASK IS UPDATED, SET BID AND ASK
    IF BA-UPD-BA
   IF PRICE-PER-SHARE
      IF O-EQUITY-OPT OR O-INDEX-OPT
         COMPUTE TEMP-BID ROUNDED = BA-BID / 100
         COMPUTE TEMP-ASK ROUNDED = BA-ASK / 100
      ELSE IF O-INTEREST-OPT OR O-DEBT-OPT
         IF O-TRD-UN-ST = '7'
        COMPUTE TEMP-BID ROUNDED = BA-BID /
            (100 * O-TRD-ADJ)
        COMPUTE TEMP-ASK ROUNDED = BA-ASK /
            (100 * O-TRD-ADJ)
         ELSE
        COMPUTE TEMP-BID ROUNDED = BA-BID /
            (10 * O-TRD-ADJ)
        COMPUTE TEMP-ASK ROUNDED = BA-ASK /
            (10 * O-TRD-ADJ)
      ELSE
         NEXT SENTENCE
   ELSE
      MOVE BA-BID TO TEMP-BID
      MOVE BA-ASK TO TEMP-ASK.
    IF BA-UPD-BA
   MOVE TEMP-OP-ASK TO O-OP-ASK
   MOVE TEMP-OP-BID TO O-OP-BID
   MOVE TEMP-ASK-ADJ TO O-OP-ASK-ADJ
   MOVE TEMP-BID-ADJ TO O-OP-BID-ADJ
   MOVE BA-DT-Y      TO WS-D-YY
   MOVE BA-DT-MM     TO WS-D-MM
   MOVE BA-DT-DD     TO WS-D-DD
   PERFORM TRANS-DT-FIX
   MOVE WS-D-YY TO WS-YMD-YY
   MOVE WS-D-MM TO WS-YMD-MM
   MOVE WS-D-DD TO WS-YMD-DD
   MOVE WS-YMD-N TO O-OP-BA-DATE.

* IF OPEN INTEREST IS UPDATE, SET OPEN INTEREST
    IF BA-UPD-OPEN-INT
   MOVE BA-OPEN-INT TO O-OPEN-INT
   IF O-TRD-TYPE = '9'
      MOVE 'I' TO O-TRD-TYPE.

* IF OPEN INTEREST HAS BEEN ADJUSTED, SET INDICATOR
    IF BA-ADJ-OPEN-INT
   MOVE 1 TO O-OPEN-INT-ADJ-IND
    ELSE
   MOVE 0 TO O-OPEN-INT-ADJ-IND.
BA-REC-PROC-EXIT.           EXIT.
*
*********************************************************
*
CALL-BOND-PROC  SECTION.
CALL-BOND-PROC-PARA.
* UPDATE THE CALL BOND DATA FROM A CALL BOND RECORD
    MOVE C-ENTRY-CODE      TO O-ENTRY-CODE.
    MOVE C-REV-CODE    TO O-REV-CODE.
    MOVE C-SAP-PG-NO   TO O-SAP-PG-NO.
    MOVE C-CALL-Y      TO WS-D-YY.
    MOVE C-CALL-MM     TO WS-D-MM.
    MOVE C-CALL-DD     TO WS-D-DD.
    PERFORM TRANS-DT-FIX.
    MOVE WS-DT-N     TO O-CALL-DATE.
    MOVE C-CALL-ISS-TYPE    TO O-CALL-ISS-TYPE.
    MOVE C-SF-CODE     TO O-SF-CODE.
    MOVE C-CALL-TYPE   TO O-CALL-TYPE.
    MOVE C-REDEMPTION      TO O-REDEMPTION.
    MOVE C-PREMIUM-CODE    TO O-PREMIUM-CODE.
    MOVE C-CALL-AMT    TO O-CALL-AMT.
    MOVE C-CALL-PRICE      TO O-CALL-PRICE.

CALL-BOND-PROC-EXIT.           EXIT.
*
*********************************************************
*
FUT-OI-PROC SECTION.
FUT-OI-PROC-PARA.
    MOVE BA-OPEN-INT TO O-FUT-OPEN-INTEREST.
FUT-OI-REC-PROC-EXIT.           EXIT.
*
*
*********************************************************
*
REG-BOND-PROC SECTION.
REG-BOND-PROC-PARA.
    MOVE '00000' TO REV-WORD.
    ADD 1 TO RB-CNT.
    IF  (R-PMT-MTD NOT EQUAL TO O-BD-PMT-MTD) OR
    (R-PMT-CD  NOT EQUAL TO O-BD-PMT-CD ) OR
    (R-TX-BS-CD NOT EQUAL TO O-BD-TX-BS-CD)
    MOVE '1' TO REV-IND (1).
    IF  (R-INT-PMT  NOT EQUAL TO O-BD-INT-PMT)
    MOVE '1' TO REV-IND (2).
    IF  (R-TRANS-CD NOT EQUAL TO O-BD-TRANS-CD)
    MOVE '1' TO REV-IND (3).
    MOVE R-ECODE    TO O-BD-ECODE.
    MOVE R-ISS-TYPE TO O-ISS-TYPE.
    MOVE R-INT-PMT  TO O-BD-INT-PMT.
    MOVE R-PMT-FQ   TO O-BD-PMT-FQ.
    MOVE R-LR-CD    TO O-BD-LR-CD.
    MOVE R-TRANS-CD TO O-BD-TRANS-CD.
    MOVE R-PMT-MTD  TO O-BD-PMT-MTD.
    MOVE R-PMT-CD   TO O-BD-PMT-CD.
    MOVE R-TX-BS-CD TO O-BD-TX-BS-CD.
    MOVE R-REC-MM TO WS-D-MM.
    MOVE R-REC-DD TO WS-D-DD.
    MOVE R-REC-Y  TO WS-D-YY.
    PERFORM TRANS-DT-FIX.
    IF  (WS-DT-N NOT EQUAL TO O-BD-REC-DT)
         MOVE '1' TO REV-IND (3).
    MOVE WS-DT-N TO O-BD-REC-DT.
    MOVE R-PMT-MM TO WS-D-MM.
    MOVE R-PMT-DD TO WS-D-DD.
    MOVE R-PMT-Y  TO WS-D-YY.
    PERFORM TRANS-DT-FIX.
    IF  (WS-DT-N NOT EQUAL TO O-BD-PMT-DT)
         MOVE '1' TO REV-IND (4).
    MOVE WS-DT-N TO O-BD-PMT-DT.
    IF R-ECODE EQUAL TO '3' PERFORM FIND-REV-CODE
   ELSE      MOVE 'H' TO O-BD-RV-IND.
RBP-EXIT.           EXIT.
*
*********************************************************
*
FIND-REV-CODE SECTION.
FIND-REV-CODE-PARA.
    MOVE 1 TO J.
FRC-LOOP.
    IF REV-WORD EQUAL TO RW-TABLE (J)
    MOVE REV-CODE-TABLE (J) TO O-BD-RV-IND
    GO TO FRC-EXIT
      ELSE
    ADD 1 TO J.
    IF J LESS 38 GO TO FRC-LOOP.
FRC-EXIT.           EXIT.
*
*********************************************************
*
CHK-COM-DIV SECTION.
CHK-COM-DIV-PARA.
*
*  CHECK COMMON STOCK DIVIDEND AREA FOR OPTION
*  AND MOVE LATEST CASH DIVIDEND TO FIRST DIVIDEND AREA
*
    IF (COM-OPT-ISSUER (P) = O-ISSUER) AND
   (COM-OPT-PMT-MTD (P) = '1' OR '2' OR '3' OR '4')
* THE CUSIP MATCHES AND THIS IS A CASH DIVIDEND
   IF COM-OPT-DIV-SECT-NUM (P) NOT EQUAL TO '1' AND '2'
* BAD DIV SECTION NUMBER, REPORT ERROR
       MOVE 2 TO BAD-NEWS
       MOVE +16 TO M   PERFORM WRITE-MESS
       PERFORM SHOW-QSIP
   ELSE
* CHECK THE EX-DIS DATE TO SEE IF ANY NEW CASH DIV IS LATER
* THAN ONE ALREADY IN BUCKET
      MOVE O-DIV-XDIS-DT (1) TO WS-DT-N
      MOVE WS-D-YY TO WS-XD-YY
      MOVE WS-D-MM TO WS-XD-MM
      MOVE WS-D-DD TO WS-XD-DD
      MOVE COM-OPT-XDIS-MM (P) TO WS-D-MM
      MOVE COM-OPT-XDIS-DD (P) TO WS-D-DD
      MOVE COM-OPT-XDIS-Y (P) TO WS-D-YY
      PERFORM TRANS-DT-FIX
      MOVE WS-D-YY TO WS-YMD-YY
      MOVE WS-D-MM TO WS-YMD-MM
      MOVE WS-D-DD TO WS-YMD-DD
      IF WS-YMD-N > WS-XD-N
         MOVE WS-DT-N TO O-DIV-XDIS-DT (1)
         MOVE COM-OPT-AMT-RATE (P) TO O-DIV-AMT (1)
         MOVE COM-OPT-LR-CD (P) TO O-DIV-LR-CD (1)
         MOVE COM-OPT-REV-IND (P) TO O-DIV-RV-IND (1)
         MOVE COM-OPT-PMT-MTD (P)  TO O-DIV-PAY-MTD (1)
         MOVE COM-OPT-PMT-ODR (P) TO O-DIV-PAY-ODR (1)
         MOVE COM-OPT-TX-BS-CD (P) TO O-DIV-TX-BS (1)
         MOVE COM-OPT-REC-MM (P) TO WS-D-MM
         MOVE COM-OPT-REC-DD (P) TO WS-D-DD
         MOVE COM-OPT-REC-Y (P) TO WS-D-YY
         PERFORM TRANS-DT-FIX
         MOVE WS-DT-N TO O-DIV-REC-DT (1)
         MOVE COM-OPT-PAY-MM (P) TO WS-D-MM
         MOVE COM-OPT-PAY-DD (P) TO WS-D-DD
         MOVE COM-OPT-PAY-Y (P) TO WS-D-YY
         PERFORM TRANS-DT-FIX
         MOVE WS-DT-N TO O-DIV-PAY-DT (1).
CCD-EXIT.           EXIT.
*
*********************************************************
*
CHK-DIV-RECS SECTION.
CHK-DIV-RECS-PARA.
    MOVE +1 TO P.
CD-LOOP.
    IF P GREATER 2 OR D-ISSUE-R (P) EQUAL TO SPACES
                 GO TO CD-EXIT.
    IF (D-ISSUE-R (P) EQUAL TO O-ISSUE-R)
    PERFORM DIV-REC-PROC
    ADD +1 TO P
    GO TO CD-LOOP.
CD-EXIT.           EXIT.
*
*********************************************************
*
DIV-REC-PROC SECTION.
DIV-REC-PROC-PARA.
    IF D-DIV-SECT-NUM (P) NOT EQUAL TO '1' AND '2'
    MOVE 2 TO BAD-NEWS
    MOVE +16 TO M   PERFORM WRITE-MESS
    PERFORM SHOW-QSIP
    GO TO DRP-EXIT.
    MOVE D-DIV-SECT-NUM (P) TO I.
    MOVE D-ECODE (P)     TO O-DIV-ECODE (I).
    MOVE D-ISS-TYPE (P) TO O-ISS-TYPE.
    MOVE D-AMT-RATE (P) TO O-DIV-AMT (I).
    MOVE D-LR-CD (P)     TO O-DIV-LR-CD (I).
    MOVE D-REV-IND (P)   TO O-DIV-RV-IND (I).
    MOVE D-BRR-CD (P) TO O-DIV2-BRR-CD (2)
    MOVE D-TRANS-CD (P) TO O-DIV2-TRANS-CD (2).
    MOVE D-PMT-MTD (P)    TO O-DIV-PAY-MTD (I).
    MOVE D-PMT-ODR (P)   TO O-DIV-PAY-ODR (I).
    MOVE D-TX-BS-CD (P)  TO O-DIV-TX-BS (I).
    MOVE D-XDIS-MM (P)   TO WS-D-MM.
    MOVE D-XDIS-DD (P)   TO WS-D-DD.
    MOVE D-XDIS-Y (P)    TO WS-D-YY.
    PERFORM TRANS-DT-FIX.
    MOVE WS-DT-N     TO O-DIV-XDIS-DT (I).
    MOVE D-REC-MM (P)    TO WS-D-MM.
    MOVE D-REC-DD (P)    TO WS-D-DD.
    MOVE D-REC-Y (P)     TO WS-D-YY.
    PERFORM TRANS-DT-FIX.
    MOVE WS-DT-N     TO O-DIV-REC-DT (I).
    MOVE D-PAY-MM (P)    TO WS-D-MM.
    MOVE D-PAY-DD (P)    TO WS-D-DD.
    MOVE D-PAY-Y (P)     TO WS-D-YY.
    PERFORM TRANS-DT-FIX.
    MOVE WS-DT-N     TO O-DIV-PAY-DT (I).
DRP-EXIT.           EXIT.
*
*********************************************************
*
TRANS-DT-FIX   SECTION.
TRANS-DT-FIX-PARA.
    IF WS-D-MM EQUAL TO  99    MOVE  9999  TO WS-D-YYYY
               ADD 90 TO WS-D-YY
               GO TO TDF-EXIT.
    ADD 70 TO WS-D-YY.
    MOVE 0 TO ITN-CNT.
ONEDIG-DT-ADJ.
    IF ITN-CNT GREATER 6 MOVE +19 TO M PERFORM WRITE-MESS
           PERFORM CLOSE-FILES
           STOP RUN.
    COMPUTE WS-D-YYYY =    (WS-D-YY + 1900).
    IF WS-D-YYYY LESS 1970 ADD 100 TO WS-D-YYYY.
    COMPUTE WS-M-CNT =    (WS-D-YYYY * 12 + WS-D-MM).
    IF (TD-M-CNT - WS-M-CNT) LESS 12 NEXT SENTENCE
    ELSE            ADD 10 TO WS-D-YY
            ADD 1  TO ITN-CNT
            GO TO ONEDIG-DT-ADJ.
TDF-EXIT.           EXIT.
*
*********************************************************
*
END-OF-OUTPUT-NINES SECTION.
END-OF-OUTPUT-NINES-PARA.
    DIVIDE BLK-SZ INTO W-OUTP-CNT GIVING FT-BLK REMAINDER PT-BLK
    ON SIZE ERROR DISPLAY W-OUTP-CNT.
    SUBTRACT PT-BLK FROM BLK-SZ GIVING NO-9BLKS
     ON SIZE ERROR DISPLAY NO-9BLKS.
    PERFORM W-NINES-OUT NO-9BLKS TIMES.
*
*********************************************************
*
W-NINES-OUT SECTION.
W-NINES-OUT-PARA.
    WRITE OUTP216-REC FROM W-NINES.
*
*********************************************************
*
OUTREC-INIT SECTION.
OUTREC-INIT-PARA.
*  SETS PREVIOUS TRADE DATE,
*  INITIALIZES DIVIDEND OR BOND ENTRY CODES.
*
    IF O-TRD-TYPE NOT < '0' AND O-TRD-TYPE < '8' OR
   ((O-TRD-TYPE EQUAL TO '8')  AND
       (O-X-CD EQUAL TO 'G' OR 'H' OR 'I' OR 'N' OR
                'O' OR 'P' OR 'S' OR 'U' OR
                'W' OR 'X' OR 'Y'))
* IF THE PREVIOUS PRICE WAS NOT A MODEL PRICE OR BID ASK QUOTE,
* OR IT WAS A BID ASK QUOTE FROM A BID/ASK TYPE EXCHANGE,
* THEN SAVE THE PRICE DATE FOR LATER USE AS THE
* PREVIOUS TRADE DATE
   MOVE O-PRICE-DT TO FIX-FST
   IF TD-MM EQUAL TO 1 AND T-PR-MM EQUAL TO 12
      COMPUTE D-YY =    TD-YY - 1
      MOVE FIX-DT   TO HOLD-LAST-TRDNDT
   ELSE
      MOVE TD-YY TO D-YY
      MOVE FIX-DT TO HOLD-LAST-TRDNDT
    ELSE
* PREVIOUS PRICING DATA WAS NOT TRADE, SO KEEP OLD LAST TRADE DATE
   MOVE O-LAST-TDT TO HOLD-LAST-TRDNDT.

    IF (O-X-CD EQUAL TO 'G') GO TO OI-EXIT.
    IF (O-X-CD LESS 'L') OR (O-X-CD EQUAL TO 'U' OR 'V' OR 'R')
      IF O-DIV-ECODE (1) NOT EQUAL TO '0'
           MOVE '2' TO O-DIV-ECODE (1)
           IF O-DIV-ECODE (2) NOT EQUAL TO '0'
            MOVE '2' TO O-DIV-ECODE (2)
            GO TO OI-EXIT
            ELSE GO TO OI-EXIT
     ELSE IF O-DIV-ECODE (2) NOT EQUAL TO '0'
           MOVE '2' TO O-DIV-ECODE (2)
            GO TO OI-EXIT
         ELSE GO TO OI-EXIT
    ELSE NEXT SENTENCE.
    IF O-X-CD EQUAL TO 'Q' GO TO OI-EXIT.
    IF O-BD-ECODE NOT EQUAL TO '4' AND '9' MOVE 2 TO O-BD-ECODE.
OI-EXIT.           EXIT.
*
*********************************************************
*
SHOW-QSIP SECTION.
SHOW-QSIP-PARA.
    MOVE SPACES TO PRINT-REC.
    MOVE ERR-LINE TO PRINT-MESS.
    WRITE PRINT-REC AFTER ADVANCING 2 LINES.
    MOVE O-X-QSIP TO ERR-O-QSIP.
    MOVE U-X-QSIP TO ERR-U-QSIP.
    MOVE T-X-QSIP TO ERR-T-QSIP.
    MOVE D-ISSUE-R (P) TO ERR-D-QSIP.
    MOVE ERR-LINE-1 TO PRINT-MESS.
    WRITE PRINT-REC AFTER ADVANCING 1 LINES.
    MOVE SPACES TO PRINT-REC.
    WRITE PRINT-REC AFTER ADVANCING 2 LINES.
S-Q-EXIT.           EXIT.
*
*********************************************************
*
WRITE-MESS SECTION.
WRITE-MESS-PARA.
    MOVE MESS (M) TO MESG.
    WRITE PRINT-REC FROM MESS-REC AFTER ADVANCING 1 LINES.
*
*********************************************************
*
WRITE-SUMMARY SECTION.
WRITE-SUMMARY-PARA.
    MOVE SPACES TO PRINT-REC.
    MOVE HEAD-1 TO PRINT-LINE.
    WRITE PRINT-REC AFTER ADVANCING NEXT-PAGE.
    MOVE DASHES TO PRINT-LINE.
    WRITE PRINT-REC AFTER ADVANCING 1 LINES.
    MOVE SPACES TO PRINT-LINE.
    MOVE HDR-1 TO PRINT-MESS.
    WRITE PRINT-REC AFTER ADVANCING 7 LINES.
    MOVE HDR-REC TO PRINT-MESS.
    WRITE PRINT-REC AFTER ADVANCING 2 LINES.
    MOVE LAST-T-REC TO PRINT-MESS.
    WRITE PRINT-REC AFTER ADVANCING 2 LINES.
    MOVE LAST-U-REC TO PRINT-MESS.
    WRITE PRINT-REC AFTER ADVANCING 2 LINES.
    MOVE LAST-O-REC TO PRINT-MESS.
    WRITE PRINT-REC AFTER ADVANCING 2 LINES.
    MOVE LINE-2 TO PRINT-LINE.
    WRITE PRINT-REC AFTER ADVANCING 5 LINES.
    MOVE W-OLDTP-CNT TO TOT-OLD.
    MOVE W-UPD-CNT   TO TOT-UPD.
    MOVE W-TRANS-CNT TO TOT-TRANS.
    MOVE W-OUTP-CNT  TO TOT-OUT.
    MOVE TOT-LINE TO PRINT-LINE.
    WRITE PRINT-REC AFTER ADVANCING 2 LINES.
    MOVE SPACES TO PRINT-LINE.
    MOVE LINE-3 TO PRINT-LINE.
    WRITE PRINT-REC AFTER ADVANCING 5 LINES.
    MOVE 'ADDITIONS      ' TO PT-TYPE.
    MOVE SC-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE 'CAP CHANGES    ' TO PT-TYPE.
    MOVE CR-CNT   TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE 'STATUS CHANGES ' TO PT-TYPE.
    MOVE SCNG-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE 'STAT CONTINUES ' TO PT-TYPE.
    MOVE CONT-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE SUB-HEAD TO PRINT-MESS.
    WRITE PRINT-REC AFTER ADVANCING 4 LINES.
    MOVE 'ADDS-NO DESC' TO PT-TYPE.
    MOVE NSC-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE 'PRICES     ' TO PT-TYPE.
    MOVE PR-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE 'OPTION BID/ASKS' TO PT-TYPE.
    MOVE BA-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE 'FUTURES OPENINT' TO PT-TYPE.
    MOVE OI-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE 'REG BONDS      ' TO PT-TYPE.
    MOVE RB-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE 'DIVIDENDS      '  TO PT-TYPE.
    MOVE DR-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE 'BOND CALLS    '  TO PT-TYPE.
    MOVE CL-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE 'ZERO PRICE DATA'  TO PT-TYPE.
    MOVE BADPR-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE SPACES TO PRINT-REC.
    MOVE SUB-HEAD-1 TO PRINT-MESS.
    WRITE PRINT-REC AFTER ADVANCING 4 LINES.
    MOVE 'PURGES     '  TO PT-TYPE.
    MOVE P-CNT TO OUT-PT.
    PERFORM PT-PRINT.
    MOVE SPACES TO PRINT-REC.
    MOVE SUB-HEAD-2 TO PRINT-MESS.
    WRITE PRINT-REC AFTER ADVANCING 4 LINES.
    MOVE PART-TOT-1 TO PRINT-LINE.
    WRITE PRINT-REC AFTER ADVANCING 1 LINES.
    MOVE 'WITHOUT DESCRP ' TO PT-TYPE.
    MOVE NO-DESCRIP-CNT TO OUT-PT.
    PERFORM PT-PRINT.
*
*********************************************************
*
PT-PRINT SECTION.
PT-PRINT-PARA.
    MOVE PART-TOT TO PRINT-MESS.
    WRITE PRINT-REC AFTER ADVANCING 1 LINES.
*********************************************************
******************* END OF PROGRAM **********************
*********************************************************
    z g}#