(*$E+*)
(*******************************************************************
: **                  PROPRIETARY INFORMATION                     **
: **                                                              **
: **  This  source code listing constitutes the proprietary pro-  **
: **  perty of TYMNET.  The recipient, by receiving this program  **
: **  listing, agrees that neither this listing nor the informa-  **
: **  tion disclosed herein nor any part thereof shall be repro-  **
: **  duced or transferred to other documents or used or dis-     **
: **  closed to others for manufacturing or for any other purpose **
: **  except as specifically authorized in writing by TYMNET.     **
: ******************************************************************
: **                   PROGRAM IDENTIFICATION                     **
: **                                                              **
: **  Version Number     : 01.02         Release Date : 12/15/86  **
: **                                                              **
: **  File Name          : cp2101.pas                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     Cp2101.pas contains the code for the 2nd pass of the     **
: **     concurrent pascal compiler.  This pass is responsible    **
: **     syntax analysis.                                         **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.01 12/07/84 PJH  1162     Increased spelling_max to 5000  **
: ** 01.02 12/09/85 PJH  1162     Increased max_index to 7013     **
: ** 01.02 12/09/85 PJH  1162     Increased max_index to 8000     **
: ** 01.02 01/20/86 PJH  1162     Increased spelling_max to 8009  **
: ** 01.02 12/15/86 PJH  1162     Increased max_index to 8512     **
: ** 01.02 12/15/86 PJH  1162     Increased spelling_max to 8310  **
: ** 01.02 12/15/86 PJH  1162     ADDED PROPRIETARY BANNER        **
: **                                                              **
: *****************************************************************)

program cp2, cpass2;

(*###########
#  PREFIX  #
###########*)

CONST
LISTOPTION = 0;    SUMMARYOPTION = 1;  TESTOPTION = 2;     CHECKOPTION = 3;
CODEOPTION = 4;    NUMBEROPTION = 5;   xrefoption = 6;     bstackoption = 7;
dumpoption = 9;    lgxrefoption = 8;

IDLENGTH = 12;


(* hash table constants *)

id_piece_length = 9;    (* piece upper bound *)
max_pieces = 13;        (* 14 pieces => 140 chars *)
hash_max = 8512;        (* hash table upper bound *)
hash_max1 = 8513;       (* prime length of hash table *)
max_index = 8512;       (* max hash table loading *)
null = 32767;           (* the null spelling index *)

(* hash table types *)

type

hash_index = 0 .. hash_max;
spelling_index = integer;
piece = packed array[0..id_piece_length] of char;
piece_ptr = ^ id_piece;
id_piece = record
             part: piece;
             next: piece_ptr
           end;
hash_entry = record
               spix: spelling_index;
               name: id_piece
             end;
hash_table = array[hash_index] of hash_entry;
hash_pointer = ^ hash_table;

spell_table = record
		spells: array[1..max_index] of hash_index;
		hash_ptr: hash_pointer
	      end;
	      
spell_pointer = ^ spell_table;

POINTER = ^ INTEGER;
OPTION = LISTOPTION..dumpOPTION;
PASSPTR = ^PASSLINK;
PASSLINK =
  RECORD
    OPTIONS: SET OF OPTION;
    LABELS, BLOCKS, CONSTANTS: INTEGER;
    resetpoint: pointer;
    TABLES: spell_POINTER
  END;

    int_file = file of integer;
    pass_range = 0 .. 7;
(*************)
(* externals *)
(*************)

procedure initpass(p: pass_range);  extern;

procedure nextpass(p: pass_range);  extern;

procedure giverr(line, pass, number: integer); extern;

procedure printarg(var f: text;  arg: integer);  extern;

procedure printff(var f: text;  p: pass_range);  extern;

procedure printop(var f: text;  op: integer);  extern;
procedure cpass2(inter_pass_ptr: passptr;
                 var ilin, ilout: int_file;
                 var list: text);

CONST

(*INPUT OPERATORS*)

EOM1=0;            BEGIN1=1;           IF1=2;              CASE1=3;
WHILE1=4;          REPEAT1=5;          FOR1=6;             CYCLE1=7;
WITH1=8;           INIT1=9;            ID1=10;             REAL1=11;
STRING1=12;        INTEGER1=13;        CHAR1=14;           OPEN1=15;
NOT1=16;           SUB1=17;            SET1=18;            ARRAY1=19;
RECORD1=20;        CLASS1=21;          MONITOR1=22;        PROCESS1=23;
PERIOD1=24;        STAR1=25;           SLASH1=26;          DIV1=27;
MOD1=28;           AND1=29;            PLUS1=30;           MINUS1=31;
OR1=32;            EQ1=33;             NE1=34;             LE1=35;
GE1=36;            LT1=37;             GT1=38;             IN1=39;
CONST1=40;         TYPE1=41;           VAR1=42;            PROCEDURE1=43;
FUNCTION1=44;      PROGRAM1=45;        SEMICOLON1=46;      CLOSE1=47;
UP_TO1=48;         OF1=49;             COMMA1=50;          BUS1=51;
COLON1=52;         END1=53;            ENTRY1=54;          UNIV1=55;
BECOMES1=56;       THEN1=57;           ELSE1=58;           DO1=59;
UNTIL1=60;         TO1=61;             DOWNTO1=62;         LCONST1=63;
MESSAGE1=64;       NEW_LINE1=65;

(*OUTPUT OPERATORS*)

EOM2=1;            CONST_ID2=2;        CONST_DEF2=3;       TYPE_ID2=4;
TYPE_DEF2=5;       VAR_ID2=6;          VAR_LIST2=7;        VARE_LIST2=8;
INITS_DEF2=9;      INITS_END2=10;      PROC_ID2=11;        PROC_DEF2=12;
PROCE_DEF2=13;     PROC_END2=14;       PROCE_END2=15;      FUNC_ID2=16;
FUNC_DEF2=17;      FUNCE_DEF2=18;      FUNC_END2=19;       FUNCE_END2=20;
PROG_ID2=21;       PROG_DEF2=22;       INTF_ID2=23;        TYPE2=24;
ENUM2=25;          ENUM_ID2=26;        ENUM_DEF2=27;       SUBR_DEF2=28;
SET_DEF2=29;       ARRAY_DEF2=30;      REC2=31;            FIELD_ID2=32;
FIELDLIST2=33;     REC_DEF2=34;        CLASS2=35;          MONITOR2=36;
PROCESS2=37;       STACK2=38;          PSTART2=39;         PARM_ID2=40;
PARM_TYPE2=41;     UNIV_TYPE2=42;      CPARMLIST2=43;      VPARMLIST2=44;
BODY2=45;          BODY_END2=46;       ANAME2=47;          STORE2=48;
CALL_NAME2=49;     CALL2=50;           ARG_LIST2=51;       ARG2=52;
FALSEJUMP2=53;     DEF_LABEL2=54;      JUMP_DEF2=55;       INTF2=56;
DEF_CASE2=57;      CASE2=58;           JUMP2=59;           END_CASE2=60;
ADDRESS2=61;       FOR_STORE2=62;      FOR_LIM2=63;        FOR_UP2=64;
FOR_DOWN2=65;      WITH_VAR2=66;       WITH_TEMP2=67;      WITH2=68;
INIT_NAME2=69;     INIT2=70;           VALUE2=71;          LT2=72;
EQ2=73;            GT2=74;             LE2=75;             NE2=76;
GE2=77;            IN2=78;             UPLUS2=79;          UMINUS2=80;
PLUS2=81;          MINUS2=82;          OR2=83;             STAR2=84;
SLASH2=85;         DIV2=86;            MOD2=87;            AND2=88;
FNAME2=89;         NOT2=90;            EMPTY_SET2=91;      INCLUDE2=92;
FUNCTION2=93;      CALL_FUNC2=94;      NAME2=95;           COMP2=96;
SUB2=97;           ARROW2=98;          CONSTANT2=99;       REAL2=100;
FREAL2=101;        INTEGER2=102;       FINTEGER2=103;      CHAR2=104;
FCHAR2=105;        STRING2=106;        FSTRING2=107;       NEW_LINE2=108;
LCONST2=109;       MESSAGE2=110;       PROCE_ID2=111;      FUNCE_ID2=112;
PEND2=113;         CASE_JUMP2=114;     blockname2=115;

(*OTHER CONSTANTS*)
NONE=0;    MINUS=1;      PLUS=2;

THIS_PASS = 2;     SPELLING_MAX = 8310;
COMP_BLOCK=TRUE;   ROUTINE_BLOCK=FALSE;

(*MODES*)

CLASS_MODE=1;      MONITOR_MODE=2;     PROCESS_MODE=3;     PROC_MODE=4;
PROCE_MODE=5;      FUNC_MODE=6;        FUNCE_MODE=7;       PROGRAM_MODE=8;

(*ERRORS*)

PROG_ERROR=1;      DEC_ERROR=2;        CONSTDEF_ERROR=3;   TYPEDEF_ERROR=4;
TYPE_ERROR=5;      ENUM_ERROR=6;       SUBR_ERROR=7;       SET_ERROR=8;
ARRAY_ERROR=9;     RECORD_ERROR=10;    STACK_ERROR=11;     VAR_ERROR=12;
ROUTINE_ERROR=13;  PROC_ERROR=14;      FUNC_ERROR=15;      WITH_ERROR=16;
PARM_ERROR=17;     BODY_ERROR=18;      STATS_ERROR=19;     STAT_ERROR=20;
IDSTAT_ERROR=21;   ARG_ERROR=22;       COMP_ERROR=23;      IF_ERROR=24;
CASE_ERROR=25;     LABEL_ERROR=26;     WHILE_ERROR=27;     REPEAT_ERROR=28;
FOR_ERROR=29;      CYCLE_ERROR=30;     EXPR_ERROR=31;      VARIABLE_ERROR=32;
CONSTANT_ERROR=33; INIT_ERROR=34;      MPROG_ERROR=35;

(*STANDARD SPELLING/NOUN INDICES*)

XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XQUEUE=6;           XABS=7;
XATTRIBUTE=8;      XCHR=9;             XCONTINUE=10;       XCONV=11;
XDELAY=12;         XEMPTY=13;          XIO=14;             XORD=15;
XPRED=16;          XSTOP=17;           XREALTIME=18;       XSETHEAP=19;
XSUCC=20;          XTRUNC=21;          XSTART=22;          XWAIT=23;
XREAL=24;

TYPE

  SPELLING_INDEX = 0..SPELLING_MAX; 

  LAB=INTEGER;

  SYMBOL=EOM1..NEW_LINE1;

  SETS=SET OF SYMBOL;

VAR

  SY:SYMBOL;

  ARG:INTEGER;

  CURRENT_LABEL:LAB;

  line_no: integer;
  
  TEST:BOOLEAN;

(*KEY SETS*)

QIGNORE,           QOPEN,              QCLOSE,             QEOM,
QEND,              QSEMICOLON,         QBODY,              QID,
QDEFINITIONS,      QROUTINES,          QDECLARATIONS,      QDEF,
QDEC,              QCONSTANT,          QCONST_DEF,         QTYPE,
QTYPE_DEF,         QSUBR_LIMIT,        QDIMENSION,         QOF_TYPE,
QVAR_DEF,          QBLOCK,             QPARM_END,          QID_LIST,
QPROC_END,         QPROC_PARMS,        QFUNC_END,          QFUNC_TYPE,
QPROG_END,         QINTERFACE,         QPARM_LIST,         QSTAT,
QBODY_END,         QENTRY,             QSTAT_LIST,         QID_END,
QARGUMENT,         QARG_END,           QIF_END,            QTHEN_END,
QCASES,            QCASE_END,          QLABEL_LIST,        QDO_TAIL,
QUNARY,            QFACTOR,            QEXPR,              QUNTIL_TAIL,
QFOR_END,          QFORB_END,          QEXPR_OP,           QSEXPR_OP,
QTERM_OP,          QTERM_LIST,         QFACTOR_LIST,       QSET_EXPR,
QSELECT,           QSUB_END,           QARG,               QCOMMA,
QVARE_DEF,         QTYPE_LIST,         QWITH_LIST,         QINIT_LIST,
QTO_TAIL,          QSTACK,             QLABEL_TAIL:        SETS;
(******************)
(* i/o procedures *)
(******************)

procedure read_ifl(var i: integer);
begin
  i := ilin ^;  get(ilin);
end;

procedure write_ifl(i: integer);
begin 
  ilout ^ := i;  put(ilout)
end;

  PROCEDURE PUT_ARG(ARG:INTEGER);
  BEGIN
    WRITE_IFL(ARG);
    IF TEST THEN PRINTARG(list, ARG)
  END;

  PROCEDURE PUT0(OP:INTEGER);
  BEGIN
    WRITE_IFL(OP);
    IF TEST THEN PRINTOP(list, OP)
  END;

  PROCEDURE PUT1(OP,ARG:INTEGER);
  BEGIN
    WRITE_IFL(OP); WRITE_IFL(ARG);
    IF TEST THEN BEGIN
      PRINTOP(list, OP); PRINTARG(list, ARG)
    END
  END;

  PROCEDURE PUT2(OP,ARG1,ARG2:INTEGER);
  BEGIN
    WRITE_IFL(OP); WRITE_IFL(ARG1); WRITE_IFL(ARG2);
    IF TEST THEN BEGIN
      PRINTOP(list, OP);
      PRINTARG(list, ARG1); PRINTARG(list, ARG2)
    END
  END;

  PROCEDURE PUT3(OP,ARG1,ARG2,ARG3:INTEGER);
  BEGIN
    PUT2(OP,ARG1,ARG2);
    PUT_ARG(ARG3)
  END;
(*#############*)
(*PASS ROUTINES*)
(*#############*)

  (*PARSING ROUTINES*)

  PROCEDURE PROGRAM_; FORWARD;
  PROCEDURE BLOCK(KEYS:SETS; IN_COMPONENT:BOOLEAN; id: integer); FORWARD;
  PROCEDURE DECLARATIONS(KEYS:SETS); FORWARD;
  PROCEDURE CONST_DEC(KEYS:SETS); FORWARD;
  PROCEDURE TYPE_DEC(KEYS:SETS); FORWARD;
  PROCEDURE TYPE_(KEYS:SETS; id:integer); FORWARD;
  PROCEDURE ENUM_TYPE(KEYS:SETS); FORWARD;
  PROCEDURE SUBR_TYPE(KEYS:SETS); FORWARD;
  PROCEDURE SET_TYPE(KEYS:SETS);  FORWARD;
  PROCEDURE ARRAY_TYPE(KEYS:SETS); FORWARD;
  PROCEDURE RECORD_TYPE(KEYS:SETS);  FORWARD;
  PROCEDURE COMP_TYPE(KEYS:SETS; id:integer);  FORWARD;
  PROCEDURE VAR_DEC(KEYS:SETS);  FORWARD;
  PROCEDURE ID_LIST(KEYS:SETS; OP,ERROR_NUM:INTEGER; VAR ID_COUNT:INTEGER);
  FORWARD;
  PROCEDURE IDENTIFIER(KEYS:SETS; OP,ERROR_NUM:INTEGER);  FORWARD;
  function get_id(KEYS:SETS; OP,ERROR_NUM:INTEGER): integer;  FORWARD;
  PROCEDURE ROUTINE_DEC(KEYS:SETS);  FORWARD;
  PROCEDURE PROC_DEC(KEYS:SETS); FORWARD;
  PROCEDURE FUNC_DEC(KEYS:SETS);  FORWARD;
  PROCEDURE PROG_DEC(KEYS:SETS);  FORWARD;
  PROCEDURE PARM_LIST(KEYS:SETS; MODE:INTEGER);  FORWARD;
  PROCEDURE BODY(KEYS:SETS; id:integer);  FORWARD;
  PROCEDURE STAT_LIST (KEYS:SETS);           FORWARD;
  PROCEDURE STAT(KEYS:SETS);  FORWARD;
  PROCEDURE ID_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE ARG_LIST(KEYS:SETS);  FORWARD;
  PROCEDURE COMPOUND_STAT(KEYS:SETS); FORWARD;
  PROCEDURE IF_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE CASE_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE LABEL_LIST(KEYS:SETS);  FORWARD;
  PROCEDURE WHILE_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE REPEAT_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE FOR_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE CYCLE_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE WITH_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE INIT_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE EXPR(KEYS:SETS);  FORWARD;
  PROCEDURE SEXPR(KEYS:SETS);  FORWARD;
  PROCEDURE TERM(KEYS:SETS); FORWARD;
  PROCEDURE FACTOR(KEYS:SETS); FORWARD;
  PROCEDURE FACTOR_ID(KEYS:SETS);  FORWARD;
  PROCEDURE VARIABLE(KEYS:SETS);  FORWARD;
  PROCEDURE CONSTANT(KEYS:SETS);  FORWARD;
(*##########*)
(*INITIALIZE*)
(*##########*)

  PROCEDURE GET;
  VAR LENGTH,I,VAL,PASS_NO,MESSAGE_NO:INTEGER;
    DONE:BOOLEAN;
  BEGIN
    DONE:=FALSE;
    REPEAT
      READ_IFL(SY);
      IF SY IN QIGNORE THEN
        CASE SY OF
          LCONST1: BEGIN
            READ_IFL(LENGTH); PUT1(LCONST2,LENGTH);
            FOR I:=1 TO LENGTH DIV 2 DO BEGIN
              READ_IFL(VAL); PUT_ARG(VAL)
            END
          END;
          MESSAGE1: BEGIN
            READ_IFL(PASS_NO); READ_IFL(MESSAGE_NO);
            giverr(line_no, pass_no, message_no);
            PUT2(MESSAGE2,PASS_NO,MESSAGE_NO)
          END;
          NEW_LINE1: BEGIN
          READ_IFL(LINE_NO); PUT1(NEW_LINE2,LINE_NO)
          END
        END
      ELSE DONE:=TRUE
    UNTIL DONE;
    IF SY IN QARG THEN READ_IFL(ARG)
  END;

  PROCEDURE INITIALIZE;
  BEGIN
    rewrite(ilout);
    reset(ilin);
    INITPASS(this_pass);
    WITH INTER_PASS_PTR^ DO BEGIN
      TEST:=TESTOPTION IN OPTIONS
    END;
    IF TEST THEN PRINTFF(list, this_pass);
    CURRENT_LABEL:=1; (*LABEL 1 DENOTES THE BLOCK OF THE INITIAL PROCESS;
      IT IS ONLY REFERENCED BY THE FIRST JUMP INSTRUCTION IN THE PROGRAM*)
    QIGNORE:=[LCONST1,MESSAGE1,NEW_LINE1];
    QCOMMA:=[COMMA1];
    QOPEN:=[OPEN1]; QCLOSE:=[CLOSE1];
    QEOM:=[EOM1]; QEND:=[END1];
    QSEMICOLON:=[SEMICOLON1];
    QBODY:=[BEGIN1]; QID:=[ID1];
    QDEFINITIONS:=[CONST1,TYPE1];
    QROUTINES:=[PROCEDURE1,FUNCTION1,PROGRAM1];
    QSTACK:=[PLUS1];
    QENTRY:=[ENTRY1];
    QDECLARATIONS:=QDEFINITIONS + [VAR1] + QROUTINES;
    QDEF:=[ID1,SEMICOLON1,EQ1];
    QDEC:=[ID1,SEMICOLON1,COLON1];
    QUNARY:=[PLUS1,MINUS1];
    QCONSTANT:=[ID1,INTEGER1,REAL1,CHAR1,STRING1] + QUNARY;
    QCONST_DEF:=QDEF + QCONSTANT;
    QTYPE:=[OPEN1,SET1,ARRAY1,RECORD1,CLASS1,MONITOR1,PROCESS1]
      + QCONSTANT;
    QTYPE_DEF:=QDEF + QTYPE;
    QTYPE_LIST:=QTYPE + QCOMMA;
    QSUBR_LIMIT:=[UP_TO1] + QCONSTANT;
    QDIMENSION:=QTYPE + [COMMA1,BUS1,OF1];
    QOF_TYPE:=QTYPE + [OF1];
    QVAR_DEF:=QDEC + QTYPE; QVARE_DEF:=QVAR_DEF + [ENTRY1];
    QBLOCK:=QDECLARATIONS + QBODY;
    QPARM_END:=QSEMICOLON + QBLOCK;
    QID_LIST:=[ID1,COMMA1];
    QPROC_END:=[ENTRY1,ID1,OPEN1] + QPARM_END;
    QARG:=[ID1,INTEGER1,CHAR1,STRING1];
    QPROC_PARMS:=QPROC_END-QID;
    QFUNC_END:=QPROC_END + [COLON1];
    QFUNC_TYPE:=QPARM_END + QID;
    QPROG_END:=QPROC_END-QBLOCK;
    QINTERFACE:=[ENTRY1,ID1,COMMA1,SEMICOLON1];
    QPARM_LIST:=QDEC + [UNIV1,VAR1];
    QSTAT:=[ID1,BEGIN1,IF1,CASE1,WHILE1,REPEAT1,FOR1,
      CYCLE1,WITH1,INIT1];
    QBODY_END:=QSTAT + QEND;
    QSTAT_LIST :=QSTAT + QSEMICOLON;
    QID_END:=[BECOMES1,OPEN1];
    QINIT_LIST:=[ID1,OPEN1,COMMA1];
    QIF_END:=[THEN1,ELSE1] + QSTAT;
    QTHEN_END:=QIF_END-[THEN1];
    QCASES:=QCONSTANT-QUNARY + QSTAT + [COLON1,COMMA1,SEMICOLON1];
    QCASE_END:=QCASES + [OF1,END1];
    QLABEL_LIST:=QCONSTANT-QUNARY + QCOMMA;
    QLABEL_TAIL:=QLABEL_LIST + [COLON1];
    QDO_TAIL:=QSTAT + [DO1];
    QFACTOR:=QCONSTANT-QUNARY + [OPEN1,NOT1,SUB1];
    QEXPR:=QUNARY + QFACTOR;
    QARGUMENT:=QEXPR + QCOMMA;
    QARG_END:=QARGUMENT + QCLOSE;
    QUNTIL_TAIL:=QEXPR + [UNTIL1];
    QFOR_END:=QEXPR + QSTAT + [BECOMES1,TO1,DOWNTO1,DO1];
    QFORB_END:=QFOR_END-[BECOMES1];
    QEXPR_OP:=[EQ1,NE1,LE1,GE1,LT1,GT1,IN1];
    QSEXPR_OP:=[PLUS1,MINUS1,OR1];
    QTERM_OP:=[STAR1,SLASH1,DIV1,MOD1,AND1];
    QTERM_LIST:=QFACTOR + QSEXPR_OP;
    QFACTOR_LIST:=QFACTOR + QTERM_OP;
    QSET_EXPR:=QARGUMENT + [BUS1];
    QSELECT:=[PERIOD1,SUB1];
    QSUB_END:=QARGUMENT + [BUS1];
    QWITH_LIST:=QDO_TAIL + QCOMMA;
    QTO_TAIL:=QDO_TAIL + QEXPR;
    GET
  END;

  PROCEDURE ERROR(NUMBER:INTEGER; KEYS:SETS);
  BEGIN
    giverr(line_no, this_pass, number);
    PUT2(MESSAGE2,THIS_PASS,NUMBER);
    WHILE NOT (SY IN KEYS) DO GET
  END;

  PROCEDURE CHECK(NUMBER:INTEGER; KEYS:SETS);
  BEGIN
    IF NOT (SY IN KEYS) THEN ERROR(NUMBER,KEYS)
  END;

  PROCEDURE NEW_LABEL(VAR L:LAB);
  BEGIN
    CURRENT_LABEL:=CURRENT_LABEL+1;
    L:=CURRENT_LABEL
  END;
(*#######*)
(*PROGRAM*)
(*#######*)

  PROCEDURE PROGRAM_;
  BEGIN
    PUT1(PSTART2,PROCESS_MODE); PUT0(PROCESS2);
    BLOCK(QEOM, COMP_BLOCK, xundef);
    IF SY=PERIOD1 THEN GET ELSE ERROR(MERROR,QEOM);
    IF SY<>EOM1 THEN ERROR(MPROG_ERROR,QEOM);
    PUT0(EOM2)
  END;
(*#####*)
(*BLOCK*)
(*#####*)

  PROCEDURE BLOCK(* keys: sets;  in_component: boolean;  id: integer *);
  BEGIN
    DECLARATIONS(KEYS + QBODY);
    IF IN_COMPONENT THEN PUT0(INITS_DEF2);
    BODY(KEYS, id);
    IF IN_COMPONENT THEN PUT0(INITS_END2)
  END;
(*############*)
(*DECLARATIONS*)
(*############*)

  PROCEDURE DECLARATIONS(* keys: sets *);
  VAR LKEYS1,LKEYS2:SETS;
  BEGIN
    LKEYS1:=KEYS + QDECLARATIONS;
    LKEYS2:=KEYS + QROUTINES;
    CHECK(DEC_ERROR,LKEYS1);
    WHILE SY IN QDEFINITIONS DO BEGIN
      IF SY=CONST1 THEN CONST_DEC(LKEYS1) ELSE TYPE_DEC(LKEYS1);
      CHECK(DEC_ERROR,LKEYS1)
    END;
    IF SY=VAR1 THEN VAR_DEC(LKEYS2);
    CHECK(DEC_ERROR,LKEYS2);
    IF SY IN QROUTINES THEN ROUTINE_DEC(KEYS)
  END;

  PROCEDURE CONST_DEC(* keys: sets *);
  VAR LKEYS1,LKEYS2:SETS;
  BEGIN
    LKEYS1:=KEYS + QCONST_DEF;
    LKEYS2:=KEYS-QCONST_DEF;
    GET;
    REPEAT
      IDENTIFIER(LKEYS1,CONST_ID2,CONSTDEF_ERROR);
      IF SY=EQ1 THEN GET ELSE ERROR(CONSTDEF_ERROR,LKEYS1);
      EXPR(LKEYS1);
      PUT0(CONST_DEF2);
      IF SY=SEMICOLON1 THEN GET ELSE ERROR(CONSTDEF_ERROR,LKEYS1);
      CHECK(CONSTDEF_ERROR,LKEYS1)
    UNTIL SY IN LKEYS2
  END;

  PROCEDURE TYPE_DEC(* keys: sets *);
  VAR LKEYS1,LKEYS2:SETS;
    id: integer;
  BEGIN
    LKEYS1:=KEYS + QTYPE_DEF;
    LKEYS2:=KEYS-QTYPE_DEF;
    GET;
    REPEAT
      id := get_id(LKEYS1,TYPE_ID2,TYPEDEF_ERROR);
      IF SY=EQ1 THEN GET ELSE ERROR(TYPEDEF_ERROR,LKEYS1);
      TYPE_(LKEYS1, id);
      PUT0(TYPE_DEF2);
      IF SY=SEMICOLON1 THEN GET ELSE ERROR(TYPEDEF_ERROR,LKEYS1);
      CHECK(TYPEDEF_ERROR,LKEYS1)
    UNTIL SY IN LKEYS2
  END;
(*####*)
(*TYPE*)
(*####*)

  PROCEDURE TYPE_(* keys: sets; id: integer *);
  BEGIN
    CHECK(TYPE_ERROR,KEYS + QTYPE);
    IF SY IN QTYPE THEN
      CASE SY OF
        OPEN1: ENUM_TYPE(KEYS);
        ID1,INTEGER1,REAL1,CHAR1,STRING1,MINUS1,PLUS1: SUBR_TYPE(KEYS);
        SET1: SET_TYPE(KEYS);
        ARRAY1: ARRAY_TYPE(KEYS);
        RECORD1: RECORD_TYPE(KEYS);
        CLASS1,MONITOR1,PROCESS1: COMP_TYPE(KEYS, id)
      END
    ELSE BEGIN
      ERROR(TYPE_ERROR,KEYS);
      PUT1(TYPE2,XUNDEF)
    END
  END;

  PROCEDURE ENUM_TYPE(* keys: sets *);
  VAR NUMBER:INTEGER;
  BEGIN
    PUT0(ENUM2); GET;
    ID_LIST(KEYS + QCLOSE,ENUM_ID2,ENUM_ERROR,NUMBER);
    PUT0(ENUM_DEF2);
    IF SY=CLOSE1 THEN GET ELSE ERROR(ENUM_ERROR,KEYS);
  END;

  PROCEDURE SUBR_TYPE(* keys: sets *);
  VAR SPIX:SPELLING_INDEX;
  BEGIN
    IF SY=ID1 THEN BEGIN
      SPIX:=ARG; GET;
      CHECK(SUBR_ERROR,KEYS + QSUBR_LIMIT);
      IF SY=UP_TO1 THEN BEGIN
        PUT1(CONSTANT2,SPIX);
        GET;
        CONSTANT(KEYS);
        PUT0(SUBR_DEF2)
      END ELSE PUT1(TYPE2,SPIX)
    END ELSE BEGIN
      CONSTANT(KEYS + QSUBR_LIMIT);
      IF SY=UP_TO1 THEN GET ELSE ERROR(SUBR_ERROR,KEYS + QCONSTANT);
      CONSTANT(KEYS);
      PUT0(SUBR_DEF2)
    END
  END;

  PROCEDURE SET_TYPE(* keys: sets *);
  BEGIN
    GET;
    IF SY=OF1 THEN GET ELSE ERROR(SET_ERROR,KEYS + QTYPE);
    TYPE_(KEYS, xundef);
    PUT0(SET_DEF2)
  END;

  PROCEDURE ARRAY_TYPE(* keys: sets *);
  VAR LKEYS1:SETS; I,DIMENSIONS:INTEGER; DONE:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QDIMENSION;
    GET;
    IF SY=SUB1 THEN GET ELSE ERROR(ARRAY_ERROR,LKEYS1);
    DIMENSIONS:=0; DONE:=FALSE;
    REPEAT
      (*INDEX*)TYPE_(LKEYS1, xundef); DIMENSIONS:=DIMENSIONS+1;
      CHECK(ARRAY_ERROR,LKEYS1);
      IF SY IN QTYPE_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(ARRAY_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE;
    IF SY=BUS1 THEN GET ELSE ERROR(ARRAY_ERROR,KEYS + QOF_TYPE);
    IF SY=OF1 THEN GET ELSE ERROR(ARRAY_ERROR,KEYS + QTYPE);
    (*ELEMENT*)TYPE_(KEYS, xundef);
    FOR I:=1 TO DIMENSIONS DO PUT0(ARRAY_DEF2)
  END;

  PROCEDURE RECORD_TYPE(* keys: sets *);
  VAR NUMBER:INTEGER; DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QVAR_DEF + QEND;
    PUT0(REC2); GET; DONE:= FALSE;
    REPEAT
      ID_LIST(LKEYS1,FIELD_ID2,RECORD_ERROR,NUMBER);
      IF SY=COLON1 THEN GET ELSE ERROR(RECORD_ERROR,LKEYS1);
      (*FIELD*)TYPE_(LKEYS1, xundef);
      PUT1(FIELDLIST2,NUMBER);
      CHECK(RECORD_ERROR,LKEYS1);
      IF SY IN QVAR_DEF THEN
        IF SY=SEMICOLON1 THEN GET ELSE ERROR(RECORD_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE;
    PUT0(REC_DEF2);
    IF SY=END1 THEN GET ELSE ERROR(RECORD_ERROR,KEYS);
  END;

  PROCEDURE COMP_TYPE(* keys: sets; id: integer *);
  VAR MODE,OP:INTEGER;
  BEGIN
    CASE SY OF
      CLASS1: BEGIN MODE:=CLASS_MODE; OP:=CLASS2 END;
      MONITOR1: BEGIN MODE:=MONITOR_MODE; OP:=MONITOR2 END;
      PROCESS1: BEGIN MODE:=PROCESS_MODE; OP:=PROCESS2 END
    END;
    GET;
    PARM_LIST(KEYS + QPARM_END + QSTACK,MODE);
    PUT0(OP);
    IF SY=SEMICOLON1 THEN GET;
    (*NO CHECK SINCE AD HOC EXTENSION*)
    IF SY=PLUS1 THEN BEGIN
      GET;
      IF SY=INTEGER1 THEN BEGIN
        PUT1(STACK2,ARG);
        GET
      END ELSE ERROR(STACK_ERROR,KEYS + QBLOCK)
    END;
    BLOCK(KEYS,COMP_BLOCK,id)
  END;

  PROCEDURE LABEL_LIST(* keys: sets *);
  VAR DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QLABEL_LIST;
    DONE:=FALSE;
    REPEAT
      CONSTANT(LKEYS1);
      PUT0(CASE2);
      CHECK(LABEL_ERROR,LKEYS1);
      IF SY IN QLABEL_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(LABEL_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE;
    IF SY=COLON1 THEN GET ELSE ERROR(CASE_ERROR,LKEYS1);
  END;

(*#########*)
(*VARIABLES*)
(*#########*)

  PROCEDURE VAR_DEC(* keys: sets *);
  VAR OP,NUMBER:INTEGER; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QVARE_DEF;
    GET;
    REPEAT
      CHECK(VAR_ERROR,LKEYS1);
      IF SY=ENTRY1 THEN BEGIN OP:=VARE_LIST2; GET END ELSE OP:=VAR_LIST2;
      ID_LIST(LKEYS1,VAR_ID2,VAR_ERROR,NUMBER);
      IF SY=COLON1 THEN GET ELSE ERROR(VAR_ERROR,LKEYS1);
      (*VAR*)TYPE_(LKEYS1, xundef);
      PUT1(OP,NUMBER);
      IF SY=SEMICOLON1 THEN GET ELSE ERROR(VAR_ERROR,LKEYS1);
      CHECK(VAR_ERROR,LKEYS1)
    UNTIL NOT(SY IN QVARE_DEF);
  END;

  PROCEDURE ID_LIST(* keys: sets;
                       op, error_num: integer;
                       var id_count: integer *);
  VAR LKEYS1:SETS; DONE:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QID_LIST;
    ID_COUNT:=0; DONE:=FALSE;
    REPEAT
      IDENTIFIER(LKEYS1,OP,ERROR_NUM);
      ID_COUNT:=ID_COUNT+1;
      CHECK(ERROR_NUM,LKEYS1);
      IF SY IN QID_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(ERROR_NUM,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE
  END;

  PROCEDURE IDENTIFIER(* keys: sets;  op, error_num: integer *);
  var
    trash: integer;
  BEGIN
    trash := get_id(keys, op, error_num)
  END;

  FUNCTION get_id(* keys: sets;  op, error_num: integer returns integer *);
  BEGIN
    IF SY=ID1 
    THEN BEGIN 
      get_id := arg;
      PUT1(OP,ARG); 
      GET 
    END
    ELSE BEGIN
      get_id := xundef;
      ERROR(ERROR_NUM,KEYS);
      PUT1(OP,XUNDEF)
    END
  END;
(*########*)
(*ROUTINES*)
(*########*)

  PROCEDURE ROUTINE_DEC(* keys: sets *);
  VAR LKEYS1:SETS; SEMI_EXPECTED:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QROUTINES;
    REPEAT
      SEMI_EXPECTED:=TRUE;
      CASE SY OF
        PROCEDURE1: PROC_DEC(LKEYS1);
        FUNCTION1: FUNC_DEC(LKEYS1);
        PROGRAM1: BEGIN SEMI_EXPECTED:=FALSE; PROG_DEC(LKEYS1) END
      END;
      IF SEMI_EXPECTED THEN
        IF SY=SEMICOLON1 THEN GET ELSE ERROR(ROUTINE_ERROR,LKEYS1);
      CHECK(ROUTINE_ERROR,LKEYS1);
    UNTIL NOT(SY IN QROUTINES)
  END;

  PROCEDURE PROC_DEC(* keys: sets *);
  VAR MODE,ID_OP,DEF_OP,END_OP,id:INTEGER;
  BEGIN
    GET;
    CHECK(PROC_ERROR,KEYS + QPROC_END);
    IF SY=ENTRY1 THEN BEGIN
      GET;
      MODE:=PROCE_MODE;
      ID_OP:=PROCE_ID2;
      DEF_OP:=PROCE_DEF2; END_OP:=PROCE_END2
    END ELSE BEGIN
      MODE:=PROC_MODE;
      ID_OP:=PROC_ID2;
      DEF_OP:=PROC_DEF2; END_OP:=PROC_END2
    END;
    id := get_id(KEYS + QPROC_PARMS,ID_OP,PROC_ERROR);
    PARM_LIST(KEYS + QPARM_END,MODE);
    PUT0(DEF_OP);
    IF SY=SEMICOLON1 THEN GET ELSE ERROR(PROC_ERROR,KEYS + QBLOCK);
    BLOCK(KEYS,ROUTINE_BLOCK,id);
    PUT0(END_OP)
  END;

  PROCEDURE FUNC_DEC(* keys: sets *);
  VAR MODE,ID_OP,DEF_OP,END_OP,id:INTEGER; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QFUNC_END;
    GET;
    CHECK(FUNC_ERROR,LKEYS1);
    IF SY=ENTRY1 THEN BEGIN
      GET;
      MODE:=FUNCE_MODE; ID_OP:=FUNCE_ID2;
      DEF_OP:=FUNCE_DEF2; END_OP:=FUNCE_END2
    END ELSE BEGIN
      MODE:=FUNC_MODE; ID_OP:=FUNC_ID2;
      DEF_OP:=FUNC_DEF2; END_OP:=FUNC_END2
    END;
    id := get_id(LKEYS1,ID_OP,FUNC_ERROR);
    PARM_LIST(LKEYS1-QOPEN,MODE);
    IF SY=COLON1 THEN GET ELSE ERROR(FUNC_ERROR,KEYS + QFUNC_TYPE);
    IDENTIFIER(KEYS + QPARM_END,DEF_OP,FUNC_ERROR);
    IF SY=SEMICOLON1 THEN GET ELSE ERROR(FUNC_ERROR,KEYS + QBLOCK);
    BLOCK(KEYS,ROUTINE_BLOCK,id);
    PUT0(END_OP)
  END;

  PROCEDURE PROG_DEC(* keys: sets *);
  VAR DUMMY:INTEGER;
  BEGIN
    GET;
    IDENTIFIER(KEYS + QPROG_END,PROG_ID2,PROG_ERROR);
    PARM_LIST(KEYS + QINTERFACE,PROGRAM_MODE);
    PUT0(INTF2);
    IF SY=SEMICOLON1 THEN GET ELSE ERROR(PROG_ERROR,KEYS + QINTERFACE);
    CHECK(PROG_ERROR,KEYS + QENTRY);
    IF SY=ENTRY1 THEN BEGIN
      GET;
      ID_LIST(KEYS + QSEMICOLON,INTF_ID2,PROG_ERROR,DUMMY);
      IF SY=SEMICOLON1 THEN GET ELSE ERROR(PROG_ERROR,KEYS)
    END;
    PUT0(PROG_DEF2)
  END;

  PROCEDURE PARM_LIST(* keys: sets;  mode: integer *);
  VAR LIST_OP,TYPE_OP,NUMBER:INTEGER; DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    PUT1(PSTART2,MODE);
    CHECK(PARM_ERROR,KEYS + QOPEN);
    IF SY=OPEN1 THEN BEGIN
      LKEYS1:=KEYS + QPARM_LIST + QCLOSE;
      GET; DONE:=FALSE;
      REPEAT
        CHECK(PARM_ERROR,LKEYS1);
        IF SY=VAR1 THEN BEGIN
          GET; LIST_OP:=VPARMLIST2
        END ELSE LIST_OP:=CPARMLIST2;
        ID_LIST(LKEYS1,PARM_ID2,PARM_ERROR,NUMBER);
        IF SY=COLON1 THEN GET ELSE ERROR(PARM_ERROR,LKEYS1);
        CHECK(PARM_ERROR,LKEYS1);
        IF SY=UNIV1 THEN BEGIN
          GET; TYPE_OP:=UNIV_TYPE2
        END ELSE TYPE_OP:=PARM_TYPE2;
        (*TYPE*)IDENTIFIER(LKEYS1,TYPE_OP,PARM_ERROR);
        PUT1(LIST_OP,NUMBER);
        CHECK(PARM_ERROR,LKEYS1);
        IF SY IN QPARM_LIST THEN
          IF SY=SEMICOLON1 THEN GET ELSE ERROR(PARM_ERROR,LKEYS1)
        ELSE DONE:=TRUE
      UNTIL DONE;
      IF SY=CLOSE1 THEN GET ELSE ERROR(PARM_ERROR,KEYS);
      PUT0(PEND2)
    END
  END;
(*####*)
(*BODY*)
(*####*)

  procedure putname(id: integer);
  var
    fpiece: id_piece; (* the first piece in the name *)
    piecep: piece_ptr;
    charcount: integer;
    i: integer;

  function chars(p: piece): integer;
  var
    i: integer;
  begin (* chars *)
    chars := 0;
    for i := 0 to id_piece_length do
      if p[i] <> ' '
      then chars := i+1;
  end;
  
  begin (* putname *)
    if id = xundef
    then begin
      put1(blockname2, 12);
      put_arg(ord('m')*256+ord('a')); (*'main process'*)
      put_arg(ord('i')*256+ord('n'));
      put_arg(ord(' ')*256+ord('p'));
      put_arg(ord('r')*256+ord('o'));
      put_arg(ord('c')*256+ord('e'));
      put_arg(ord('s')*256+ord('s'))
    end
    else begin
      with inter_pass_ptr^.tables^ do
	fpiece := hash_ptr^[spells[id]].name;
      charcount := chars(fpiece.part);
      piecep := fpiece.next;
      while piecep <> nil do begin
	charcount := charcount + chars(piecep^.part);
	piecep := piecep^.next
      end;
      charcount := charcount + charcount mod 2; (* force it even *)
      put1(blockname2, charcount);
      for i := 0 to charcount div 2-1 do begin
	with fpiece do
	  put_arg(ord(part[2*(i mod 5)])*256+ord(part[2*(i mod 5)+1]));
	if (i mod 5 = 4) and (fpiece.next<>nil)
	then fpiece := fpiece.next^
      end;
    end;
  end;
  
  PROCEDURE BODY(* keys: sets; id: integer *);
  begin
    PUT0(BODY2);
    putname(id);
    IF SY=BEGIN1 THEN GET ELSE ERROR(BODY_ERROR,KEYS + QBODY_END);
    STAT_LIST (KEYS + QEND);
    PUT0(BODY_END2);
    IF SY=END1 THEN GET ELSE ERROR(BODY_ERROR,KEYS)
  END;

  PROCEDURE STAT_LIST(* keys: sets *);
  VAR DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QSTAT_LIST;
    DONE:=FALSE;
    REPEAT
      STAT(LKEYS1);
      CHECK(STATS_ERROR,LKEYS1);
      IF SY IN QSTAT_LIST  THEN
        IF SY=SEMICOLON1 THEN GET ELSE ERROR(STATS_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE
  END;

  PROCEDURE STAT(* keys: sets *);
  BEGIN
    CHECK(STAT_ERROR,KEYS + QSTAT);
    IF SY IN QSTAT THEN
      CASE SY OF
        ID1: ID_STAT(KEYS);
        BEGIN1: COMPOUND_STAT(KEYS);
        IF1: IF_STAT(KEYS);
        CASE1: CASE_STAT(KEYS);
        WHILE1: WHILE_STAT(KEYS);
        REPEAT1: REPEAT_STAT(KEYS);
        FOR1: FOR_STAT(KEYS);
        CYCLE1: CYCLE_STAT(KEYS);
        WITH1: WITH_STAT(KEYS);
        INIT1: INIT_STAT(KEYS)
      END
  END;

  PROCEDURE ID_STAT(* keys: sets *);
  VAR LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QID_END;
    VARIABLE(LKEYS1);
    CHECK(IDSTAT_ERROR,LKEYS1);
    IF SY=BECOMES1 THEN BEGIN
      PUT0(ANAME2); GET;
      EXPR(KEYS); PUT0(STORE2)
    END ELSE BEGIN
      PUT0(CALL_NAME2);
      ARG_LIST(KEYS);
      PUT0(CALL2)
    END
  END;

  PROCEDURE ARG_LIST(* keys: sets *);
  VAR DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    CHECK(ARG_ERROR,KEYS + QOPEN);
    IF SY=OPEN1 THEN BEGIN
      PUT0(ARG_LIST2); GET; DONE:=FALSE; LKEYS1:=KEYS + QARG_END;
      REPEAT
        EXPR(LKEYS1); PUT0(ARG2);
        CHECK(ARG_ERROR,LKEYS1);
        IF SY IN QARGUMENT THEN
          IF SY=COMMA1 THEN GET ELSE ERROR(ARG_ERROR,LKEYS1)
        ELSE DONE:=TRUE
      UNTIL DONE;
      IF SY=CLOSE1 THEN GET ELSE ERROR(ARG_ERROR,KEYS)
    END
  END;

  PROCEDURE COMPOUND_STAT(* keys: sets *);
  BEGIN
    GET;
    STAT_LIST (KEYS);
    IF SY=END1 THEN GET ELSE ERROR(COMP_ERROR,KEYS)
  END;

  PROCEDURE IF_STAT(* keys: sets *);
  VAR L1,L2:LAB; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QTHEN_END;
    GET;
    EXPR(KEYS + QIF_END);
    NEW_LABEL(L1); PUT1(FALSEJUMP2,L1);
    IF SY=THEN1 THEN GET ELSE ERROR(IF_ERROR,LKEYS1);
    STAT(LKEYS1);
    CHECK(IF_ERROR,LKEYS1);
    IF SY=ELSE1 THEN BEGIN
      NEW_LABEL(L2); PUT2(JUMP_DEF2,L2,L1);
      GET;
      STAT(KEYS);
      PUT1(DEF_LABEL2,L2)
    END ELSE PUT1(DEF_LABEL2,L1)
  END;

  PROCEDURE CASE_STAT(* keys: sets *);
  VAR L0,LI,LN:LAB; DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QCASES;
    GET; NEW_LABEL(L0); NEW_LABEL(LN);
    EXPR(KEYS + QCASE_END);
    PUT1(CASE_JUMP2,L0); DONE:=FALSE;
    IF SY=OF1 THEN GET ELSE ERROR(CASE_ERROR,LKEYS1);
    REPEAT
      NEW_LABEL(LI); PUT1(DEF_CASE2,LI);
      LABEL_LIST(LKEYS1);
      STAT(LKEYS1); PUT1(JUMP2,LN);
      CHECK(CASE_ERROR,LKEYS1);
      IF SY IN QCASES THEN
        IF SY=SEMICOLON1 THEN GET ELSE ERROR(CASE_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE;
    PUT2(END_CASE2,L0,LN);
    IF SY=END1 THEN GET ELSE ERROR(CASE_ERROR,KEYS);
  END;

  PROCEDURE WHILE_STAT(* keys: sets *);
  VAR L1,L2:LAB;
  BEGIN
    NEW_LABEL(L1); NEW_LABEL(L2);
    PUT1(DEF_LABEL2,L1);
    GET;
    EXPR(KEYS + QDO_TAIL);
    PUT1(FALSEJUMP2,L2);
    IF SY=DO1 THEN GET ELSE ERROR(WHILE_ERROR,KEYS + QSTAT);
    STAT(KEYS);
    PUT2(JUMP_DEF2,L1,L2)
  END;

  PROCEDURE REPEAT_STAT(* keys: sets *);
  VAR L:LAB;
  BEGIN
    NEW_LABEL(L);
    PUT1(DEF_LABEL2,L);
    GET;
    STAT_LIST (KEYS + QUNTIL_TAIL);
    IF SY=UNTIL1 THEN GET ELSE ERROR(REPEAT_ERROR,KEYS + QEXPR);
    EXPR(KEYS);
    PUT1(FALSEJUMP2,L)
  END;

  PROCEDURE FOR_STAT(* keys: sets *);
  CONST UP=5; DOWN=3;
  VAR L1,L2:LAB; LKEYS1:SETS; OP,DIRECTION:INTEGER;
  BEGIN
    LKEYS1:=KEYS + QFORB_END;
    GET; NEW_LABEL(L1); NEW_LABEL(L2);
    IDENTIFIER(KEYS + QFOR_END,NAME2,FOR_ERROR); PUT0(ADDRESS2);
    IF SY=BECOMES1 THEN GET ELSE ERROR(FOR_ERROR,LKEYS1);
    EXPR(LKEYS1); PUT0(FOR_STORE2);
    CHECK(FOR_ERROR,LKEYS1); DIRECTION:=UP; OP:=FOR_UP2;
    IF SY=TO1 THEN GET
    ELSE IF SY=DOWNTO1 THEN BEGIN
      GET; DIRECTION:=DOWN; OP:=FOR_DOWN2
    END ELSE ERROR(FOR_ERROR,QTO_TAIL);
    EXPR(KEYS + QDO_TAIL);
    PUT3(FOR_LIM2,L1,DIRECTION,L2);
    IF SY=DO1 THEN GET ELSE ERROR(FOR_ERROR,KEYS);
    STAT(KEYS);
    PUT2(OP,L1,L2)
  END;

  PROCEDURE CYCLE_STAT(* keys: sets *);
  VAR L:LAB;
  BEGIN
    GET; NEW_LABEL(L);
    PUT1(DEF_LABEL2,L);
    STAT_LIST (KEYS);
    IF SY=END1 THEN GET ELSE ERROR(CYCLE_ERROR,KEYS);
    PUT1(JUMP2,L)
  END;

  PROCEDURE WITH_STAT(* keys: sets *);
  VAR WITH_COUNT,I:INTEGER; LKEYS1:SETS; DONE:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QWITH_LIST;
    WITH_COUNT:=0; GET; DONE:=FALSE;
    REPEAT
      PUT0(WITH_VAR2);
      VARIABLE(LKEYS1);
      PUT0(WITH_TEMP2);
      WITH_COUNT:=WITH_COUNT+1;
      CHECK(WITH_ERROR,LKEYS1);
      IF SY IN QID_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(WITH_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE;
    IF SY=DO1 THEN GET ELSE ERROR(WITH_ERROR,KEYS + QSTAT);
    STAT(KEYS);
    FOR I:=1 TO WITH_COUNT DO PUT0(WITH2)
  END;

  PROCEDURE INIT_STAT(* keys: sets *);
  VAR DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    GET;
    LKEYS1:=KEYS + QINIT_LIST;
    DONE:=FALSE;
    REPEAT
      VARIABLE(LKEYS1); PUT0(INIT_NAME2);
      ARG_LIST(LKEYS1); PUT0(INIT2);
      CHECK(INIT_ERROR,LKEYS1);
      IF SY IN QINIT_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(INIT_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE
  END;
(*##########*)
(*EXPRESSION*)
(*##########*)

  PROCEDURE EXPR(* keys: sets *);
  VAR OP:INTEGER;
  BEGIN
    SEXPR(KEYS + QEXPR_OP);
    CHECK(EXPR_ERROR,KEYS + QEXPR_OP);
    IF SY IN QEXPR_OP THEN BEGIN
      CASE SY OF
        EQ1: OP:=EQ2;
        NE1: OP:=NE2;
        LE1: OP:=LE2;
        GE1: OP:=GE2;
        LT1: OP:=LT2;
        GT1: OP:=GT2;
        IN1: OP:=IN2
      END;
      PUT0(VALUE2); GET;
      SEXPR(KEYS);
      PUT0(OP)
    END
  END;

  PROCEDURE SEXPR(* keys: sets *);
  VAR UNARY:BOOLEAN; LKEYS1:SETS; OP:INTEGER;
  BEGIN
    LKEYS1:=KEYS + QTERM_LIST;
    CHECK(EXPR_ERROR,LKEYS1);
    IF SY IN QUNARY THEN BEGIN
      UNARY:=TRUE;
      IF SY=PLUS1 THEN OP:=UPLUS2 ELSE OP:=UMINUS2;
      GET
    END ELSE UNARY:=FALSE;
    TERM(LKEYS1);
    IF UNARY THEN PUT0(OP);
    CHECK(EXPR_ERROR,LKEYS1);
    IF SY IN QTERM_LIST THEN BEGIN
      PUT0(VALUE2);
      REPEAT
        IF SY IN QSEXPR_OP THEN BEGIN
          CASE SY OF
            PLUS1: OP:=PLUS2;
            MINUS1: OP:=MINUS2;
            OR1: OP:=OR2
          END; GET
        END ELSE BEGIN
          ERROR(EXPR_ERROR,LKEYS1);
          OP:=PLUS2
        END;
        TERM(LKEYS1); PUT0(OP);
        CHECK(EXPR_ERROR,LKEYS1);
      UNTIL NOT(SY IN QTERM_LIST)
    END
  END;

  PROCEDURE TERM(* keys: sets *);
  VAR OP:INTEGER; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QFACTOR_LIST;
    FACTOR(LKEYS1);
    CHECK(EXPR_ERROR,LKEYS1);
    IF SY IN QFACTOR_LIST THEN BEGIN
      PUT0(VALUE2);
      REPEAT
        IF SY IN QTERM_OP THEN BEGIN
          CASE SY OF
            STAR1: OP:=STAR2;
            SLASH1: OP:=SLASH2;
            DIV1: OP:=DIV2;
            MOD1: OP:=MOD2;
            AND1: OP:=AND2
          END;
          GET
        END ELSE BEGIN
          ERROR(EXPR_ERROR,LKEYS1);
          OP:=STAR2
        END;
        FACTOR(LKEYS1);
        PUT0(OP);
        CHECK(EXPR_ERROR,LKEYS1)
      UNTIL NOT(SY IN QFACTOR_LIST)
    END
  END;

  PROCEDURE FACTOR(* keys: sets *);
  VAR LKEYS1:SETS;
  BEGIN
    CHECK(EXPR_ERROR,KEYS + QFACTOR);
    IF SY IN QFACTOR THEN
      CASE SY OF
        REAL1: BEGIN PUT0(FREAL2); GET END;
        STRING1: BEGIN PUT1(FSTRING2,ARG); GET END;
        INTEGER1: BEGIN PUT1(FINTEGER2,ARG); GET END;
        CHAR1: BEGIN PUT1(FCHAR2,ARG); GET END;
        ID1: FACTOR_ID(KEYS);
        OPEN1: BEGIN
          GET; EXPR(KEYS + QCLOSE);
          IF SY=CLOSE1 THEN GET ELSE ERROR(EXPR_ERROR,KEYS)
        END;
        NOT1: BEGIN
          GET; FACTOR(KEYS); PUT0(NOT2)
        END;
        SUB1: BEGIN
          GET; PUT0(EMPTY_SET2);
          LKEYS1:=KEYS + QSET_EXPR;
          CHECK(EXPR_ERROR,LKEYS1);
          WHILE SY IN QARGUMENT DO BEGIN
            EXPR(LKEYS1); PUT0(INCLUDE2);
            CHECK(EXPR_ERROR,LKEYS1);
            IF SY IN QARGUMENT THEN
              IF SY=COMMA1 THEN GET ELSE ERROR(EXPR_ERROR,LKEYS1);
            CHECK(EXPR_ERROR,LKEYS1)
          END;
          IF SY=BUS1 THEN GET ELSE ERROR(EXPR_ERROR,KEYS)
        END
      END
    ELSE PUT1(NAME2,XUNDEF)
  END;

  PROCEDURE FACTOR_ID(* keys: sets *);
  BEGIN
    VARIABLE(KEYS + QOPEN);
    CHECK(EXPR_ERROR,KEYS + QOPEN);
    IF SY=OPEN1 THEN BEGIN
      PUT0(FUNCTION2);
      ARG_LIST(KEYS);
      PUT0(CALL_FUNC2)
    END ELSE PUT0(FNAME2)
  END;
(*########*)
(*VARIABLE*)
(*########*)

  PROCEDURE VARIABLE(* keys: sets *);
  VAR LKEYS1,LKEYS2:SETS; DONE:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QSELECT;
    IDENTIFIER(LKEYS1,NAME2,VARIABLE_ERROR);
    CHECK(VARIABLE_ERROR,LKEYS1);
    WHILE SY IN QSELECT DO BEGIN
      CASE SY OF
      PERIOD1:
        BEGIN
        PUT0(ADDRESS2);
        GET;
        IDENTIFIER(LKEYS1,COMP2,VARIABLE_ERROR)
        END;
      SUB1:
        BEGIN
        PUT0(ADDRESS2); GET;
        LKEYS2:=LKEYS1 + QSUB_END; DONE:=FALSE;
        REPEAT
          EXPR(LKEYS2); PUT0(SUB2);
          CHECK(VARIABLE_ERROR,LKEYS2);
          IF SY IN QARGUMENT THEN
            IF SY=COMMA1 THEN GET ELSE ERROR(VARIABLE_ERROR,LKEYS2)
          ELSE DONE:=TRUE
        UNTIL DONE;
        IF SY=BUS1 THEN GET ELSE ERROR(VARIABLE_ERROR,LKEYS1)
        END
      END;
      CHECK(VARIABLE_ERROR,LKEYS1)
    END
  END;

  PROCEDURE NEGREAL;
  VAR VAL,LTH,I:INTEGER;
  BEGIN
    READ_IFL(VAL);  READ_IFL(LTH);  PUT1(LCONST2,LTH);
    READ_IFL(VAL);  VAL:=VAL+(-32767);
    PUT_ARG(VAL);
    FOR I:=2 TO LTH DIV 2 DO BEGIN
      READ_IFL(VAL);  PUT_ARG(VAL);
    END;
  END;

  PROCEDURE CONSTANT(* keys: sets *);
  VAR SIGN:INTEGER;
  BEGIN
    CHECK(CONSTANT_ERROR,KEYS + QCONSTANT);
    IF SY=PLUS1 THEN BEGIN SIGN:=PLUS;  GET;  END
    ELSE IF SY=MINUS1 THEN BEGIN SIGN:=MINUS; GET; END
    ELSE SIGN:=NONE;
    IF(SY=ID1)AND(SIGN=NONE) THEN BEGIN
      PUT1(CONSTANT2,ARG); GET;
    END ELSE IF(SY=CHAR1)AND(SIGN=NONE) THEN BEGIN
      PUT1(CHAR2,ARG);  GET;
    END ELSE IF(SY=STRING1)AND(SIGN=NONE) THEN BEGIN
      PUT1(STRING2,ARG);  GET;
    END ELSE IF SY=INTEGER1 THEN BEGIN
      IF SIGN=MINUS THEN ARG:=-ARG;
      PUT1(INTEGER2,ARG);  GET;
    END ELSE IF SY=REAL1 THEN BEGIN
      PUT0(REAL2);
      IF SIGN=MINUS THEN NEGREAL;
      GET;
    END ELSE BEGIN
      ERROR(CONSTANT_ERROR,KEYS);
      PUT1(CONSTANT2,XUNDEF)
    END
  END;
(*############*)
(*MAIN PROGRAM*)
(*############*)

BEGIN (* cpass2 *)
  INITIALIZE;
    PROGRAM_;
    INTER_PASS_PTR^.LABELS:=CURRENT_LABEL;
    break(list);
    NEXTPASS(this_pass)
END   (* cpass2 *);

begin end.
 ZWH+A