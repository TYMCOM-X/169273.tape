(*SCDON*)
   SYMBOL = (NUL, PLUS, MINUS, TIMES, SLASH, EQUAL, NOTEQUAL, LESSTHAN,
             GREATERTHAN, LESSOREQUAL, GREATEROREQUAL, LPAREN, RPAREN,
             PERCENT, DOUBLESLASH, TRIPLESLASH, ENDOFFILE, BACKSLASH,
             COMMA, SEMICOLON, COLON, LEFTSQUARE, RIGHTSQUARE, ASSIGN, 
             DOUBLEQUOTE, SINGLEQUOTE, UNSIGNEDINT, UNSIGNEDREAL,
             PROCIDENT, CONDNUM,  ORSY, ANDSY, EXITSY, SYMCOLSY,
             TITLESY, PROCEDURESY, GENERICSY, FUNCDES, RANGESSY,
             PERMITTEDSY, REASONABLESY, VARIABLESSY, INPUTSSY, OTHERSY,
             CONDITIONSSY, CALCULATIONSSY, DESTINATIONSY, YESORNOSY,
             MODESY, NUMBERSY, RANGESY, REQUIREDSY, CHOOSESY, TEOSY,
              FORMULASSY, NEQSY, SELONSY, SELOFFSY,
             BEGINSY, ENDSY, IFSY, THENSY, ELSESY, REPEATSY, SYMBOLSY,
             HOSTSY, RSMSY, INPUTSYMBOLSY, NOTSY, IDENTIFIER, LOCALID,
             AANDMSY, EQUIPSY, WHOLESY, EQCOLSY,
             ALLSY, CHARACTERSY, CODESY, DATESY, DECIMALSY,
             EQSY, FALSESY, LISTSY, MDSY, MPSSY, NOSY, NONESY,
             PRICESY, PROVSY, STDSY, SUMSY, PDVARIABLESSY,
             TRAFFICSY, TRUESY, USEDSY, VALUESY, WECOSY, YESSY,
             CHOICESY, COINCIDENCESY, NUMCOLSY, TEOFSSY,
             DEFAULTSY, DEFAULTABLESY, EDITABLESY, FORMSY,
             GEQSY, GTSY, ITEMSY, LEQSY, LINESY, LTSY, MULTIPLESY,
             OVERRIDEABLESY, PRODUCTSY, RATINGSY, RELATIONALSY,
             REPORTSSY,REPORTSY, RESPONSIBILITYSY, TYPESY, STANDARDSY,
             CARRYSY, FORWARDSY, EXTRAPOLATIONSY, EXITIFSY, LOOPSY,
             ENDLOOPSY,ORDERINGSY, PLANNINGSY, EDITABLEBYSY,TERSY,ERSY,
             OVERRIDEABLEBYSY, DATASTSY,BYSY,EESY, TESY, CAPSY,TPRICESY,
             DRAINSY,JDSSY,CURRENTSY,DRAWINGSY, INTERNALSY,EPRICESY,
             SSY,LIMITINGSY, DEMANDSY, CAPACITYSY,PCTUTILSY,QUANTITYSY,
             MINARGSY,DCSY,CLEISY,DEFERSY);
(*SCDOFF*)
   SYMSET = SET OF SYMBOL;
   TYPLINES = (STDLINE, FWDLINE, XTRLINE);
   IDENTYP = (GLO, LOC, INP);
   DMOD = (INT, REL, LOG, CHA, PCK, DTE);
   PURGERELATIONS = (MULTIPLE, LESS, GREATER, GREATEREQ, LESSEQ, EQU,
                     NOTEQU, COINCIDENT, NOTCOINCIDENT);
   TYPRATING = (NORATING, AANDM, STDRATING, PROV, MD);
   CODETYP = (SYMCODE, CAPCODE, ITLCODE,  RELCODE, EDITCODE, LENCKCODE,INPRANCODE,INRRANCODE);
   ENGINEERS = (ALLENG, NOENG, TRAFFICENG, EQUIPMENTENG, WECOENG);
   LOGICOPER = (LT, LE, GT, GE, EQ, NE);
   SYMTYPE = PACKED ARRAY [1..SYMLEN] OF CHAR;
   QTSYMPA = PACKED ARRAY[1..SYMLEN+2] OF CHAR;
   PROMPTMSGPA = PACKED ARRAY[1..PROMPTLEN] OF CHAR;
   STUFFPA = PACKED ARRAY[1..STUFFLEN] OF CHAR;
   MSGPA = PACKED ARRAY[1..MSGSIZE] OF CHAR;
   RELOPPA = PACKED ARRAY[1..5] OF CHAR;
(*SCDON*)

   DESPA = PACKED ARRAY[1..DESLEN] OF CHAR;
   DATEPA = PACKED ARRAY[1..DATELEN] OF CHAR;
   REFLISTPA = PACKED ARRAY[1..40] OF CHAR;
   REFLISTTYPE = ARRAY[1..MAXREF] OF  REFLISTPA;
   TEXTSTR = PACKED ARRAY [1..TEXTLEN] OF CHAR;
   ALPHA = PACKED ARRAY [1..IDLEN] OF CHAR;
   XDMOD = RECORD
             CASE XMOD : DMOD OF
                  CHA : ( CSTART : INTEGER;
                          CEND   : INTEGER);
                  INT,REL,LOG,DTE : ();
           END;
   SELDATUM = RECORD
              CASE DMOD OF
                 INT : (IVALUE : INTEGER);
                 REL : (RVALUE : REAL);
                 LOG : (LVALUE : BOOLEAN);
                 CHA : (CVALUE : DESPA);
                 DTE : (DVALUE : DATEPA);
           END;

   GENETYP = (ALL,SPCF,NOGENE);
   FCTYP = (HDEQUIPMENT, HDTRAFFIC, HDINTERNAL, HDCAPACITY);
   GENEMODE = RECORD (* GENERIC *)
                CASE TYP : GENETYP OF
                  ALL : ();
                  SPCF: (NUM : REAL );
                  NOGENE : ();
               END;

(*SCDOFF*)
   REPMODE = (EQLIST, EQSUM, MPS, PRICE, TEO,CAPREP,TPRICE,
              EPRICE,JDS,TEOFS,EQREP,TRFREP,DCREP,NOREP);
(*SCDON*)
   REPORTSET = SET OF REPMODE;
   COLUMNS = (DEMAND, CAPACITY, PCTUTIL, QUANTITY,NOCOLUMN);
       ITEMRECD = RECORD (* ITEM NUMBER *)
                    STARTITEM : INTEGER;
                    ENDITEM   : INTEGER;
                   END;
   DEFERRECD = RECORD
                  CASE DFERABLE : BOOLEAN OF
		    TRUE : (CLEI: PACKED ARRAY[1..8] OF CHAR);
		    FALSE : ();
	       END;   

   REPORTRECD = RECORD (* TWO DIFFERENT KIND  OF REPORTS *)
                 CASE CAPBOOL : BOOLEAN OF
                    TRUE : ( CAPID : SYMTYPE;
                             JDSID : SYMTYPE;
                             MODE: DMOD;
                             TITLE: TEXTSTR;
                             COL: COLUMNS;
                             CAPREP: REPORTSET);
                    FALSE: ( REP: REPORTSET;
                             REF: ALPHA)
                END;

(*SCDOFF*)


   MSGPTR = ^BIGSTRING; (* PTR TO MSG LOCATION *)
   LONGSTRTYP = PACKED ARRAY[1..MSGSIZE] OF CHAR;
   BIGSTRING = RECORD
                 STR : LONGSTRTYP
               END;

   (* SYNTAX TREE NODE *)
   SYNTAXPTR = ^PNODE;
   PNODE = RECORD
             NAME  : ALPHA;
             MODE  : DMOD;
             TYP   : SYMBOL;
             LL,RL : SYNTAXPTR;
             VARFLAG : BOOLEAN;
             MSGL  : MSGPTR
           END;

   (* SYMBOL TYPE FOR VARIABLE DECLARATION IN CALCULATION ALGORITHM *)
   SYMBOLS = (INPUTSYMBOL, LOCALSYMBOL, CALCSYMBOL);
   VARTABLE = RECORD (* VARIABLES IN PROCEDURE HEADING *)
                NAME  : ALPHA;
                TYP   : SYMBOLS;
                MODE   : DMOD
              END;

   (* DATA STRUC FOR RELATIONAL CHECK *)
   RELLINK = ^RELAT;
   RELAT = RECORD
             EXPR : SYNTAXPTR;
             LINK : RELLINK
           END;

   RELHEAD = RECORD
               PURNAME : ALPHA;
               PURELA  : PURGERELATIONS;
               LINK    : RELLINK
             END;


  (* GLOBAL SYMBOL TABLE *)

  GLOSYMPTR = ^GLOSYMNODE;
  GLOSYMNODE = RECORD
                 NAME     : SYMTYPE;
                 TITLE    : TEXTSTR;
                 VALUE    : SELDATUM;
                 MODE     : XDMOD;
                 PROCNO   : INTEGER;
                 OVRD     : BOOLEAN;
                 LLINK    : GLOSYMPTR;
                 RLINK    : GLOSYMPTR;
                 FWLIST   : ^LINK;
                 BWLIST   : ^LINK;
                 INPLIST  : ^INPLINK;
               END;

  LINK = RECORD
            FUNCPTR  : GLOSYMPTR;
            NEXTNODE   : ^LINK
         END;

  LOCSYMPTR = ^LOCSYMNODE;
  LOCSYMNODE = RECORD
                      NAME   : SYMTYPE;
                      VALUE  : SELDATUM;
                      MODE   : XDMOD;
                      LLINK  : LOCSYMPTR;
                      RLINK  : LOCSYMPTR
                   END;

   INPSYMPTR = ^INPSYMNODE;
   INPLINK = RECORD
               INPTR   : INPSYMPTR;
               NEXTNODE  : ^INPLINK;
             END;

(*SCDON*)
   INPDEF = RECORD
               DEFABLE : BOOLEAN;
               DEFVAL  : SELDATUM;
            END;
(*SCDOFF*)

  CLEITYP = RECORD
            NAME : SYMTYPE;
	    CLEI : PACKED ARRAY[1..8] OF CHAR;
	    END;

   CLEIFILTYP = FILE OF CLEITYP;

  
   INPSYMNODE = RECORD
                  NAME   : SYMTYPE;
                  TITLE  : TEXTSTR;
                  MODE  : XDMOD;
                  VALUE  : SELDATUM;
                  DEFAULT  : INPDEF;
                  LLINK  : INPSYMPTR;
                  RLINK  : INPSYMPTR;
                  DESTIN : ^LINK
                END;



(*SCDON*)
   LIMIT = RECORD (* RANGE BOUNDARY WITH ITS OPERATOR *)
             BDOPTR : SYMBOL;
             BOUND  : ALPHA;
           END;

   RESTRICT = RECORD (* LOWER AND UPPER BOUNDS *)
                 MINVAL : LIMIT;
                 MAXVAL : LIMIT;
              END;
   
   RANSET = RECORD (* RANGE  *)
             PMTRANGE : RESTRICT; (* PERMITTED RANGE *)
             RSNRANGE : RESTRICT; (* REASONABLE RANGE *)
           END;











   LISTYP = RECORD
                LISTEXT : REFLISTPA;
                PROCNO  : INTEGER
               END;

    LISTPROC = RECORD
                 LISTCOUNT : INTEGER;
                 REFLIST   : ARRAY[1..MAXREF] OF LISTYP
                 END;



   DRAWRECD = RECORD
                TOTLIST: INTEGER;
                DRAWNO  : ALPHA;
                LIST    : ARRAY[1..10] OF PACKED ARRAY[1..2] OF CHAR;
              END;


   FILETYP = RECORD
               CATEG : (HOST, RSM);
               KIND  : (PLANNING, ORDERING);
               FTYP  : FCTYP;
             END;



   DATARECD = RECORD
              CASE STOREDDATA : BOOLEAN OF
                TRUE : (FORMNUM : INTEGER;
                        LINENUM : INTEGER;
                        ITEMNUM : ITEMRECD);
                FALSE : ( )
             END;






   REFERLIST = RECORD
                 LISTCOUNT : INTEGER;
                 REFLIST   : REFLISTTYPE;
               END;

(*********** INPUT SYMBOLS FILE STRUCTURE ******************)

   INPNODE = RECORD
                NAME         : SYMTYPE;
                CLASS        : FILETYP;
                TITLE        : TEXTSTR;
                REQ          : BOOLEAN;
                PURGECHK     : BOOLEAN;
                MODE         : XDMOD;
                DEFAULT      : INPDEF;
                RANGE        : RANSET;
                RATING       : TYPRATING;
                GENERIC      : GENEMODE;
                REPORT       : REPORTRECD;
                SVCHECK      : BOOLEAN;
                FORMNBR      : INTEGER;
                LINETYP      : TYPLINES;
                LINENBR      : INTEGER;
                ITEMNBR      : ITEMRECD;
                CARRYFWD     : BOOLEAN;
                DESTPOS      : INTEGER;
                DESTLIST     : LISTPROC;
             END;

(********** CALCULATED SYMBOLS FILE STRUCTURE ***********)

   GLONODE = RECORD
                NAME          : SYMTYPE;
                CLASS         : FILETYP;
                TITLE         : TEXTSTR;
                MODE          : XDMOD;
                PROCNO        : INTEGER;
                SVCHECK       : BOOLEAN;
                RATING        : TYPRATING;
                GENERIC       : GENEMODE;
                DRAWNUM       : TEXTSTR;
                CURNTDRAIN    : REAL;
                PRODCODE      : INTEGER;
                CHOICODE      : INTEGER;
                DEFER         : DEFERRECD;
                EDITABLE      : BOOLEAN;
                EDITABLEBY    : SET OF ENGINEERS;
                OVERRIDE      : BOOLEAN;
                REPORT        : REPORTRECD;
                PROCPOS       : INTEGER;
                CMPVARPOS     : INTEGER;
                DESTPOS       : INTEGER;
                INPLIST       : REFERLIST;
                DESTLIST      : LISTPROC;   (* FORWARD X-REFERENCE *)
                CMPVARLIST    : LISTPROC;   (* BACKWARD X-REFERENCE *)
             END;
(*SCDOFF*)

