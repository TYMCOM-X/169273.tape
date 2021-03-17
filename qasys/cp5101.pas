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
: **  File Name          : cp5101.pas                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     Cp5101.pas contains the code for the 5th pass of the     **
: **     concurrent pascal compiler.  This pass is responsible    **
: **     for the concurrent body semantic analysis for the        **
: **     tymnet engine.                                           **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.01 04/13/79 DEG  1162     Distinguish between hw and fw   **
: **                              objects                         **
: ** 01.01 04/13/79 DEG  1162     Byte objects not automatically  **
: **                              widened to hw on stack          **
: ** 01.01 04/13/79 DEG  1162     System component addresses no   **
: **                              longer adjusted by offset       **
: ** 01.01 04/13/79 DEG  1162     Function result addresses       **
: **                              adjusted to negative            **
: **                              displacements                   **
: ** 01.01 05/21/79 DEG  1162     Integers are now fullword type  **
: ** 01.01 05/21/79 DEG  1162     Increment and decrement output  **
: **                              il operators now take a typ     **
: **                              argument                        **
: ** 01.01 06/18/79 DEG  1162     Output il change: retain        **
: **                              distinction between casejump    **
: **                              and normal jump                 **
: ** 01.01 08/03/79 DEG  1162     Allocate a halfword for         **
: **                              booleans, so level 1 code       **
: **                              generator can generate rx       **
: **                              instructions                    **
: ** 01.01 08/09/79 DEG  1162     Output il change: added for_lim **
: **                              to mark for-loop limit          **
: **                              expression                      **
: ** 01.01 09/14/79 DEG  1162     Increased setlength to 32 bytes **
: ** 01.01 10/20/80 MDS  1162     Fixed 'for_lim' to recognize    **
: **                              that not all loop control       **
: **                              variables are fullword          **
: ** 01.02 12/15/86 PJH  1162     ADDED PROPRIETARY BANNER        **
: **                                                              **
: *****************************************************************)

program cp5e, cpas5e;

(*###########
#  PREFIX  #
###########*)

CONST
byte_size = 1;
hw_size = 2 (* bytes *) ;
fw_size = 4 (* bytes *) ;
boollength = hw_size;
intlength = fw_size;
REALLENGTH = 8 (*BYTES*);
SETLENGTH = 32 (*BYTES*);
LISTOPTION = 0;    SUMMARYOPTION = 1;
TESTOPTION = 2;    CHECKOPTION = 3;
CODEOPTION = 4;    NUMBEROPTION = 5;
xrefoption = 6;    bstackoption = 7;
dumpoption = 9;    lgxrefoption = 8;

TYPE
       POINTER = ^ INTEGER;
OPTION = LISTOPTION..dumpOPTION;
PASSPTR = ^PASSLINK;
PASSLINK =
  RECORD
    OPTIONS: SET OF OPTION;
    LABELS, BLOCKS, CONSTANTS: INTEGER;
    resetpoint: pointer;
    TABLES: POINTER
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
procedure cpas5e(inter_pass_ptr: passptr;
		 var ilin, ilout: int_file;
		 var list: text);

CONST

(*INPUT OPERATORS*)

EOM1=1;            BODY1=2;            BODY_END1=3;        ADDRESS1=4;
RESULT1=5;         STORE1=6;           CALL_PROC1=7;       CONSTPARM1=8;
VARPARM1=9;        FALSEJUMP1=10;      DEF_LABEL1=11;      JUMP_DEF1=12;
CASE_JUMP1=13;     JUMP1=14;           CASE_LIST1=15;      FOR_STORE1=16;
FOR_LIM1=17;       FOR_UP1=18;         FOR_DOWN1=19;       WITH1=20;
INIT1=21;          PROG_CALL1=22;      INTF_LBL1=23;       VALUE1=24;
LT1=25;            EQ1=26;             GT1=27;             LE1=28;
NE1=29;            GE1=30;             IN1=31;             UPLUS1=32;
UMINUS1=33;        PLUS1=34;           MINUS1=35;          OR1=36;
STAR1=37;          SLASH1=38;          DIV1=39;            MOD1=40;
AND1=41;           EMPTY_SET1=42;      INCLUDE1=43;        FUNCTION1=44;
CALL_FUNC1=45;     ROUTINE1=46;        VAR1=47;            ARROW1=48;
VCOMP1=49;         RCOMP1=50;          SUB1=51;            LCONST1=52;
MESSAGE1=53;       NEW_LINE1=54;       CHK_TYPE1=55;       SAVEPARM1=56;
CALL_GEN1=57;      NOT1=58;            UNDEF1=59;          RANGE1=60;
subnc1=61;

(*OUTPUT OPERATORS*)

PUSHCONST2=0;      PUSHVAR2=1;         PUSHIND2=2;         PUSHADDR2=3;
FIELD2=4;          INDEX2=5;           POINTER2=6;         VARIANT2=7;
RANGE2=8;          ASSIGN2=9;          ASSIGNTAG2=10;      COPY2=11;
NEW2=12;           NOT2=13;            AND2=14;            OR2=15;
NEG2=16;           ADD2=17;            SUB2=18;            MUL2=19;
DIV2=20;           MOD2=21;            caselist2=22;       for_lim2=23;
FUNCTION2=24;      BUILDSET2=25;       COMPARE2=26;        COMPSTRCT2=27;
FUNCVALUE2=28;     DEFLABEL2=29;       JUMP2=30;           FALSEJUMP2=31;
CASEJUMP2=32;      INITVAR2=33;        CALL2=34;           ENTER2=35;
RETURN2=36;        POP2=37;            NEWLINE2=38;        ERR2=39;
LCONST2=40;        MESSAGE2=41;        INCREMENT2=42;      DECREMENT2=43;
PROCEDURE2=44;     INIT2=45;           PUSHLABEL2=46;      CALLPROG2=47;
EOM2=48;           noop2=49;           indexnc2=50;

(*CONTEXT*)

FUNC_RESULT=1;     ENTRY_VAR=2;        VARIABLE=3;         VAR_PARM=4;
UNIV_VAR=5;        CONST_PARM=6;       UNIV_CONST=7;       FIELD=8;
EXPR=10;           CONSTANT=11;        SAVE_PARM=12;       WITH_CONST = 13;
WITH_VAR = 14;

(*TYPE KIND*)

INT_KIND=0;        REAL_KIND=1;        BOOL_KIND=2;        CHAR_KIND=3;
ENUM_KIND=4;       SET_KIND=5;         STRING_KIND=6;      PASSIVE_KIND=7;
POINTER_KIND=8;    QUEUE_KIND= 9;      GENERIC_KIND=10;    UNDEF_KIND=11;
SYSCOMP_KIND=12;   ROUTINE_KIND=13;    ACTIVE_KIND=14;

(*STANDARD SPELLING/NOUN INDICES*)

XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XQUEUE=6;           XABS=7;
XATTRIBUTE=8;      XCHR=9 ;            XCONTINUE=10;       XCONV=11;
XDELAY=12;         XEMPTY=13;          XIO=14;             XORD=15;
XPRED=16;          XSTOP=17;           XREALTIME=18;       XSETHEAP=19;
XSUCC=20;          XTRUNC=21;          XSTART=22;          XWAIT=23;
XREAL=24;

(*STANDARD NOUN INDICES*)

ZARITHMETIC=25;    ZINDEX=26;          ZPASSIVE=27;        ZVPARM=28;
ZCPARM=29;         ZSPARM=30;          ZWITH=31;

(*DATA TYPS*)

byte_typ   = 0;
hw_typ     = 1;
fw_typ     = 2;
real_typ   = 3;
set_typ    = 4;
struct_typ = 5;

(*ADDRESS MODES*)

SCONST_MODE=11;    LCONST_MODE=0;      PROC_MODE=1;        PROG_MODE=2;
PE_MODE=3;         CE_MODE=4;          ME_MODE=5;          PROCESS_MODE=6;
CLASS_MODE=7;      MONITOR_MODE=8;     STD_MODE=9;         UNDEF_MODE=10;
TEMP_MODE=PROC_MODE;

(*COMPARISONS*)

LESS=0;            EQUAL=1;            GREATER=2;          NOTLESS=3;
NOTEQUAL=4;        NOTGREATER=5;       INSET=6;

(*ERRORS*)

COMPILER_ERROR=1;  TYPE_ERROR=2;       ADDRESS_ERROR=3;    ASSIGN_ERROR=4;
init_error=5; trunc_error=6;

THIS_PASS=5;

TYPE

  ADDR_STATE=(DIRECT,INDIRECT,ADDR,EXPRESSION);

  ADDR_MODE=LCONST_MODE..SCONST_MODE;

  ADDR_MODES=SET OF ADDR_MODE;

  DISPLACEMENT=INTEGER;

  TYPE_KIND=INT_KIND..ROUTINE_KIND;

  TYPE_KINDS=SET OF TYPE_KIND;

  CONTEXT_KIND=FUNC_RESULT..WITH_VAR;

  CONTEXTS=SET OF CONTEXT_KIND;

  OPERAND_CLASS=(UNDEFINED,VALUE,ROUTINE);

  OPERAND=
    RECORD
      KIND:TYPE_KIND; NOUN:INTEGER;
      MODE:ADDR_MODE; DISP:DISPLACEMENT; LENGTH:DISPLACEMENT;
      CASE CLASS:OPERAND_CLASS OF
        VALUE:(CONTEXT:CONTEXT_KIND; STATE:ADDR_STATE);
        ROUTINE:(PARM_SIZE,VAR_SIZE,STACK_SIZE:DISPLACEMENT)
    END;

  OPERAND_PTR=^OPERAND;

  STACK_LINK=^STACK_ENTRY;

  STACK_ENTRY=RECORD
                OPND:OPERAND_PTR;
                RESET_POINT:pointer;
                NEXT_ENTRY:STACK_LINK
              END;

VAR

  INT_EXPR,REAL_EXPR,BOOL_EXPR,SET_EXPR,UNDEF_EXPR: OPERAND;

  SY: INTEGER;

  S,T: OPERAND_PTR;

  CURRENT_MODE: ADDR_MODE;

  ROUTINE_MODES, INIT_MODES: ADDR_MODES;

  TOP_STACK,THIS_STACK,EMPTY_STACK:STACK_LINK;

  DEBUG,DONE: BOOLEAN;

  PASSIVES,INDEXS,LARGES,ARITHMETIC,INDIRECTS,SMALLS: TYPE_KINDS;

  UNIVERSAL,ASSIGNS,VAR_PARMS,CNST_PARMS, PARMS: CONTEXTS;
  
  line_no: integer;
  
  prev_op: integer;
  
(****************)
(* i/o routines *)
(****************)

PROCEDURE READ_IFL (VAR I: INTEGER);
BEGIN
  i := ilin^;  get(ilin)
END;

PROCEDURE WRITE_IFL (I: INTEGER);
BEGIN
  ilout^ := i;  put(ilout)
END;

  PROCEDURE PUT_ARG(ARG:INTEGER);
  BEGIN
    WRITE_IFL(ARG);
    IF DEBUG THEN PRINTARG(list, ARG)
  END;

  PROCEDURE PUT0(OP:INTEGER);
  BEGIN
    prev_op := op;
    WRITE_IFL(OP);
    IF DEBUG THEN PRINTOP(list, OP)
  END;

  PROCEDURE PUT1(OP,ARG1:INTEGER);
  BEGIN
    put0(op); put_arg(arg1)
  END;

  PROCEDURE PUT2(OP,ARG1,ARG2:INTEGER);
  BEGIN
    put0(op); put_arg(arg1); put_arg(arg2)
  END;

  PROCEDURE PUT3(OP,ARG1,ARG2,ARG3:INTEGER);
  BEGIN
    put0(op); put_arg(arg1); put_arg(arg2); put_arg(arg3)
  END;

  PROCEDURE PUT4(OP,ARG1,ARG2,ARG3,ARG4:INTEGER);
  BEGIN
    put0(op); put_arg(arg1); put_arg(arg2); put_arg(arg3); put_arg(arg4)
  END;

  PROCEDURE PUT5(OP,ARG1,ARG2,ARG3,ARG4,ARG5:INTEGER);
  BEGIN
    put0(op); put_arg(arg1); put_arg(arg2); put_arg(arg3); 
              put_arg(arg4); put_arg(arg5)
  END;

(*NOTE: A PASS RUNNING WITH TEST OUTPUT SHOULD START WITH PRINTFF*)
(*##########################*)
(*OPERAND STACK MANIPULATION*)
(*##########################*)

  PROCEDURE POP;
  BEGIN
    T:=S; TOP_STACK:=TOP_STACK^.NEXT_ENTRY;
    dispose(TOP_STACK^.RESET_POINT);
    IF TOP_STACK=EMPTY_STACK THEN S:=NIL ELSE S:=TOP_STACK^.NEXT_ENTRY^.OPND;
  END;

  PROCEDURE PUSH;
  BEGIN
    S:=T; NEW(THIS_STACK);
    WITH THIS_STACK^ DO BEGIN
      NEW(OPND); T:=OPND;
      NEXT_ENTRY:=TOP_STACK; new(RESET_POINT)
    END;
    TOP_STACK:=THIS_STACK
  END;
(*##########*)
(*INITIALIZE*)
(*##########*)

  PROCEDURE INITIALIZE;
  BEGIN
    DONE:=FALSE;
    prev_op := eom2;
    rewrite(ilout);  reset(ilin);
    INITPASS(this_pass);
    WITH INTER_PASS_PTR^ DO BEGIN
      DEBUG:=TESTOPTION IN OPTIONS;
      IF DEBUG THEN PRINTFF(list, this_pass)
    END;
    ARITHMETIC:=[INT_KIND,REAL_KIND];
    INDEXS:=[INT_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND];
    SMALLS:=INDEXS + [REAL_KIND,SET_KIND,QUEUE_KIND,POINTER_KIND];
    PASSIVES:=INDEXS + [REAL_KIND,SET_KIND,POINTER_KIND,STRING_KIND,
      PASSIVE_KIND];
    LARGES:=[STRING_KIND,PASSIVE_KIND,ACTIVE_KIND,SYSCOMP_KIND];
    INDIRECTS:=LARGES;
    INIT_MODES:= [CLASS_MODE, MONITOR_MODE, PROCESS_MODE];
    ROUTINE_MODES:= [PROC_MODE,PE_MODE,CE_MODE,ME_MODE];
    UNIVERSAL:=[UNIV_VAR,UNIV_CONST];
    ASSIGNS:=[FUNC_RESULT,VARIABLE,VAR_PARM,UNIV_VAR, WITH_VAR];
    VAR_PARMS:=[VAR_PARM,UNIV_VAR];
    CNST_PARMS:=[CONST_PARM,UNIV_CONST];
    PARMS:= VAR_PARMS + CNST_PARMS;
    S:=NIL; T:=NIL; NEW(EMPTY_STACK); TOP_STACK:=EMPTY_STACK;
    WITH EMPTY_STACK^ DO BEGIN
      NEXT_ENTRY:=NIL; OPND:=NIL; new(RESET_POINT)
    END;
    WITH INT_EXPR DO BEGIN
      KIND:=INT_KIND; NOUN:=XINTEGER; LENGTH:=intlength;
      MODE:=UNDEF_MODE;
      CLASS:=VALUE; CONTEXT:=EXPR; STATE:=EXPRESSION
    END;
    REAL_EXPR:=INT_EXPR;
    WITH REAL_EXPR DO BEGIN
      KIND:=REAL_KIND; NOUN:=XREAL;     LENGTH:=REALLENGTH
    END;
    BOOL_EXPR:=INT_EXPR;
    WITH BOOL_EXPR DO BEGIN
      KIND:=BOOL_KIND; NOUN:=XBOOLEAN; length:=boollength;
    END;
    SET_EXPR:=INT_EXPR;
    WITH SET_EXPR DO BEGIN
      KIND:=SET_KIND; NOUN:=XUNDEF;     LENGTH:=SETLENGTH
    END;
    UNDEF_EXPR:=INT_EXPR;
    WITH UNDEF_EXPR DO BEGIN
      KIND:=UNDEF_KIND; NOUN:=XUNDEF;  length := hw_size
    END;
    PUT1(JUMP2,1) (*JUMP TO BLOCK LABEL 1, THE INITIAL PROCESS*)
  END;
(*######*)
(*ERRORS*)
(*######*)

  procedure messpass;
  var
    mess, pass: integer;
  begin
    read_ifl(pass); read_ifl(mess);
    put2(message2, pass, mess);
    giverr(line_no, pass, mess)
  end;

  PROCEDURE ERROR1(ERROR: INTEGER);
  BEGIN
    WITH T^ DO
      IF KIND=UNDEF_KIND THEN (*SUPPRESS MESSAGE*)
      ELSE begin
        PUT2(MESSAGE2,THIS_PASS,ERROR);
        giverr(line_no, this_pass, error);
      end;
    T^:=UNDEF_EXPR
  END;

  PROCEDURE ERROR2(ERROR:INTEGER);
  BEGIN
    IF (T^.KIND=UNDEF_KIND) OR (S^.KIND=UNDEF_KIND) THEN (*SUPPRESS MESSAGE*)
    ELSE begin
      giverr(line_no, this_pass, error);
      PUT2(MESSAGE2,THIS_PASS,ERROR)
    end;
    S^:=UNDEF_EXPR
  END;

  PROCEDURE ERROR2P(ERROR:INTEGER);
  BEGIN
    ERROR2(ERROR); POP
  END;

  PROCEDURE EOM;
  VAR VAR_LENGTH:DISPLACEMENT;
  BEGIN
    WITH INTER_PASS_PTR^ DO
      begin dispose(RESETPOINT);  new(resetpoint) end;
    READ_IFL(VAR_LENGTH); PUT1(EOM2,VAR_LENGTH);
    DONE:=TRUE
  END;
(*#############*)
(*TYPE CHECKING*)
(*#############*)

  FUNCTION TTYP:INTEGER (*TYPE CODE*);
  BEGIN
    WITH T^ DO
      CASE KIND OF
	char_kind: ttyp:=byte_typ;
	bool_kind,enum_kind,undef_kind: ttyp:=hw_typ;
	int_kind,pointer_kind,queue_kind: ttyp:=fw_typ;
        REAL_KIND: TTYP:=REAL_TYP;
        SET_KIND: TTYP:=SET_TYP;
        STRING_KIND,PASSIVE_KIND: TTYP:=STRUCT_TYP;
        ACTIVE_KIND,GENERIC_KIND,SYSCOMP_KIND,ROUTINE_KIND: BEGIN
          ERROR1(TYPE_ERROR); TTYP:=hw_typ END
      END
  END;

  FUNCTION COMPATIBLE:BOOLEAN;
  VAR RESULT:BOOLEAN;
  BEGIN
    IF (T^.CLASS <> VALUE) OR (S^.CLASS <> VALUE) THEN RESULT:= FALSE ELSE
    IF T^.CONTEXT IN UNIVERSAL THEN
      RESULT:=(S^.KIND IN PASSIVES) AND (T^.LENGTH=S^.LENGTH)
    ELSE
    IF T^.KIND=S^.KIND THEN
      CASE T^.KIND OF
        INT_KIND,REAL_KIND,BOOL_KIND,CHAR_KIND,
        QUEUE_KIND: RESULT:=TRUE;
        ENUM_KIND,PASSIVE_KIND,
        ACTIVE_KIND,SYSCOMP_KIND:
          RESULT:=T^.NOUN=S^.NOUN;
        STRING_KIND:
	begin
          if (t^.context in cnst_parms) and(t^.length<s^.length)
          then error2(trunc_error);
          RESULT:=(T^.LENGTH=S^.LENGTH) OR (T^.CONTEXT IN CNST_PARMS);
        end;
        SET_KIND,POINTER_KIND:
          RESULT:=(T^.NOUN=S^.NOUN) OR (T^.NOUN=XUNDEF)
            OR (S^.NOUN=XUNDEF);
        UNDEF_KIND,ROUTINE_KIND: RESULT:=FALSE
      END
    ELSE IF T^.KIND=GENERIC_KIND THEN
      CASE T^.NOUN OF
        ZARITHMETIC: RESULT:=S^.KIND IN ARITHMETIC;
        ZINDEX: RESULT:=S^.KIND IN INDEXS;
        ZPASSIVE: RESULT:=S^.KIND IN PASSIVES
      END
    ELSE RESULT:=FALSE;
    IF NOT RESULT THEN ERROR2(TYPE_ERROR);
    COMPATIBLE:=RESULT
  END;
(*######*)
(*IGNORE*)
(*######*)

  PROCEDURE LCONST;
  VAR LENGTH,I,ARG:INTEGER;
  BEGIN
    READ_IFL(LENGTH); PUT1(LCONST2,LENGTH);
    FOR I:=1 TO LENGTH DIV hw_size DO BEGIN
      READ_IFL(ARG); PUT_ARG(ARG)
    END
  END;

  PROCEDURE IGNORE1(OP:INTEGER);
  VAR ARG:INTEGER;
  BEGIN
    READ_IFL(ARG); PUT1(OP,ARG)
  END;

  PROCEDURE IGNORE2(OP:INTEGER);
  VAR ARG1,ARG2:INTEGER;
  BEGIN
    READ_IFL(ARG1); READ_IFL(ARG2);
    PUT2(OP,ARG1,ARG2)
  END;
(*####*)
(*BODY*)
(*####*)

  PROCEDURE ROUTINE_;
  BEGIN
    PUSH;
    WITH T^ DO BEGIN
      READ_IFL(MODE); READ_IFL(DISP);
      CLASS:=ROUTINE;
      READ_IFL(PARM_SIZE); READ_IFL(VAR_SIZE); READ_IFL(STACK_SIZE)
    END
  END;

  PROCEDURE BODY;
  BEGIN
    ROUTINE_;
    WITH T^ DO BEGIN
      PUT5(ENTER2,MODE,DISP,PARM_SIZE,VAR_SIZE,STACK_SIZE);
      CURRENT_MODE:=MODE
    END
  END;

  PROCEDURE BODY_END;
  BEGIN
    PUT1(RETURN2,CURRENT_MODE);
    POP
  END;
(*#######*)
(*LOADING*)
(*#######*)

  PROCEDURE ADDR_ERROR;
  BEGIN
    ERROR1(ADDRESS_ERROR);
    PUT1(PUSHCONST2,0)
  END;

  PROCEDURE ADDRESS;
  BEGIN
    WITH T^ DO
      IF CLASS=VALUE THEN BEGIN
        CASE STATE OF
          DIRECT: BEGIN
            IF MODE=SCONST_MODE THEN ADDR_ERROR
          E PUT2(PUSHADDR2,MODE,DISP)
          END;
          INDIRECT: PUT3(PUSHVAR2,fw_typ,MODE,DISP);
          ADDR: ;
          EXPRESSION: ADDR_ERROR
        END;
        STATE:=ADDR
      END ELSE ADDR_ERROR
  END;

  PROCEDURE TYPE_;
  BEGIN
    WITH T^ DO BEGIN
      READ_IFL(KIND); READ_IFL(NOUN); READ_IFL(LENGTH)
    END
  END;

  PROCEDURE RESULT;
  BEGIN
    WITH T^ DO BEGIN
      CLASS:=VALUE;
      READ_IFL(DISP);
      PUT2(PUSHADDR2,MODE,DISP);
      CONTEXT:=FUNC_RESULT; STATE:=ADDR;
      (*RESULT*) TYPE_;
      put1(field2,-length)  (* function result has neg. offset *)
    END
  END;

  PROCEDURE VALUE_;
  BEGIN
    WITH T^ DO BEGIN
      IF KIND IN SMALLS THEN BEGIN (*LOAD VALUE*)
        CASE STATE OF
          DIRECT: IF MODE=SCONST_MODE THEN PUT1(PUSHCONST2,DISP) ELSE
            PUT3(PUSHVAR2,TTYP,MODE,DISP);
          INDIRECT: BEGIN
            PUT3(PUSHVAR2,fw_typ,MODE,DISP);
            PUT1(PUSHIND2,TTYP)
          END;
          ADDR: PUT1(PUSHIND2,TTYP);
          EXPRESSION:
        END;
        STATE:=EXPRESSION
      END ELSE IF KIND IN INDIRECTS THEN ADDRESS
      ELSE (*ERROR*) PUT1(PUSHCONST2,0);
      CONTEXT:=EXPR
    END
  END;

  PROCEDURE STORE(POPVAR:BOOLEAN);
  VAR TYP:INTEGER; SIMILAR:BOOLEAN;
  BEGIN
    (*EXPRESSION*) VALUE_;
    SIMILAR:=COMPATIBLE;
    POP (*EXPRESSION*);
    IF SIMILAR THEN WITH T^ DO
      IF CONTEXT IN ASSIGNS THEN BEGIN
        TYP:=TTYP;
        IF TYP=STRUCT_TYP THEN PUT1(COPY2,LENGTH)
        ELSE PUT1(ASSIGN2,TYP)
      END ELSE ERROR1(ASSIGN_ERROR);
    IF POPVAR THEN POP (*VARIABLE*)
  END;
(*##########*)
(*STATEMENTS*)
(*##########*)

  PROCEDURE VAR_REF;
  BEGIN
    WITH T^ DO BEGIN
      CLASS:=VALUE;
      READ_IFL(MODE); READ_IFL(DISP); READ_IFL(CONTEXT)
    END
  END;

  procedure setstate;
  begin
    WITH T^ DO
      IF(CONTEXT IN VAR_PARMS) OR
        (CONTEXT IN CNST_PARMS) AND (KIND IN LARGES)
        THEN STATE:=INDIRECT ELSE STATE:=DIRECT;
  end;

  PROCEDURE VAR_;
  BEGIN
    PUSH; VAR_REF; (*VAR*) TYPE_;
    setstate
  END;

  PROCEDURE CALL_PROC;
  BEGIN
    WITH T^ DO
      IF CLASS=ROUTINE THEN
        IF MODE=STD_MODE THEN PUT1(PROCEDURE2,DISP)
        ELSE PUT3(CALL2,MODE,DISP,PARM_SIZE);
    POP
  END;

  PROCEDURE CONSTPARM(GENERIC: BOOLEAN);
  BEGIN
    (*PARAMETER*) VAR_;
    IF COMPATIBLE THEN IF T^.CONTEXT = UNIV_CONST THEN S^.KIND:= T^.KIND;
    POP (*PARAMETER*);
    (*ARGUMENT*) VALUE_;
    IF GENERIC THEN S^ (*FUNCTION RESULT*) :=
      T^ (*ACTUAL ARGUMENT*);
    POP (*ARGUMENT*)
  END;

  PROCEDURE VARPARM;
  BEGIN
    (*ARGUMENT*) ADDRESS;
    (*PARAMETER*) VAR_;
    IF COMPATIBLE THEN
      IF NOT (S^.CONTEXT IN ASSIGNS) THEN ERROR2(ASSIGN_ERROR);
    POP (*PARAMETER*);
    POP (*ARGUMENT*)
  END;

  PROCEDURE FALSE_JUMP;
  VAR L:DISPLACEMENT;
  BEGIN
    (*BOOLEAN*) VALUE_;
    IF T^.KIND<>BOOL_KIND THEN ERROR1(TYPE_ERROR);
    READ_IFL(L); PUT1(FALSEJUMP2,L);
    POP
  END;

  PROCEDURE CASE_JUMP;
  VAR L: DISPLACEMENT;
  BEGIN
    (*SELECTOR*) VALUE_;
    READ_IFL(L); PUT1(caseJUMP2,L)
  END;

  PROCEDURE DEF_LABEL;
  VAR L:DISPLACEMENT;
  BEGIN
    READ_IFL(L); PUT1(DEFLABEL2,L)
  END;

  PROCEDURE JUMP;
  VAR L:DISPLACEMENT;
  BEGIN
    READ_IFL(L); PUT1(JUMP2,L)
  END;

  PROCEDURE JUMP_DEF;
  BEGIN
    JUMP; DEF_LABEL
  END;

  PROCEDURE CHK_TYPE;
  BEGIN
    PUSH; T^:=INT_EXPR; TYPE_;
    IF COMPATIBLE THEN (*OK*);
    POP
  END;

  PROCEDURE CASE_LIST;
  VAR I,MIN,MAX:INTEGER; L:DISPLACEMENT;
  BEGIN
    POP (*SELECTOR*);
    DEF_LABEL;
    READ_IFL(MIN); READ_IFL(MAX); PUT2(CASElist2,MIN,MAX);
    FOR I:=MIN TO MAX DO BEGIN
      READ_IFL(L); PUT_ARG(L)
    END;
    DEF_LABEL
  END;

  PROCEDURE POP_TEMP;
  BEGIN
    POP;
    PUT1(POP2,fw_size)
  END;

  PROCEDURE FOR_STORE;
  CONST LEAVE_FOR_VAR=FALSE;
  BEGIN
    (*INITIAL*) VALUE_; STORE(LEAVE_FOR_VAR); setstate;
  END;

  PROCEDURE FOR_LIM;
  VAR OP:INTEGER; LIMIT_DISP:DISPLACEMENT; LAB:DISPLACEMENT; oldstate:addr_state;
  BEGIN
    (* final *) value_;  put0(for_lim2);  pop (* limit *);
    def_label;
    oldstate := t^.state; (*value trashes the state*)
    (*CONTROL VAR*) VALUE_;
    t^.state := oldstate; READ_IFL(LIMIT_DISP);
    PUT3(PUSHVAR2,fw_typ,TEMP_MODE,LIMIT_DISP);
    READ_IFL((*COMPARISON*)OP);
    PUT2(COMPARE2,OP,ttyp);  (* 20OCT80 MDS*)
    READ_IFL(LAB); PUT1(FALSEJUMP2,LAB)
  END;

  PROCEDURE FOR_LOOP(OP:INTEGER);
  BEGIN
    (*CONTROL VAR*) ADDRESS;
    PUT1(OP, ttyp);
    JUMP_DEF;
    POP_TEMP
  END;

  PROCEDURE INIT_;
  BEGIN
    WITH T^ DO
      IF CLASS=ROUTINE THEN PUT4(INIT2,MODE,DISP,PARM_SIZE,VAR_SIZE)
        ELSE PUT4(INIT2,PROCESS_MODE,0,0,0);
      POP
  END;

  PROCEDURE INTF_LBL;
  VAR L:DISPLACEMENT;
  BEGIN
    READ_IFL(L); PUT1(PUSHLABEL2,L)
  END;

  PROCEDURE PROG_CALL;
  VAR INTF_LENGTH:INTEGER;
  BEGIN
    READ_IFL(INTF_LENGTH);
    PUT0(CALLPROG2);  PUT1(POP2,INTF_LENGTH);
    POP
  END;
(*##########*)
(*EXPRESSION*)
(*##########*)

  PROCEDURE EQUALITY(OP:INTEGER);
  BEGIN
    (*RIGHT OPERAND*) VALUE_;
    IF COMPATIBLE THEN
      CASE T^.KIND OF
        CHAR_KIND,INT_KIND,BOOL_KIND,  ENUM_KIND,POINTER_KIND,
        REAL_KIND,SET_KIND: PUT2(COMPARE2,OP,TTYP);
        STRING_KIND,PASSIVE_KIND: PUT2(COMPSTRCT2,OP,T^.LENGTH);
        ACTIVE_KIND,QUEUE_KIND,GENERIC_KIND,UNDEF_KIND,
        SYSCOMP_KIND,ROUTINE_KIND: ERROR2(TYPE_ERROR)
      END;
    POP; T^:=BOOL_EXPR
  END;

  PROCEDURE INEQUALITY(OP:INTEGER);
  BEGIN
    (*RIGHT OPERAND*) VALUE_;
    IF COMPATIBLE THEN
      CASE T^.KIND OF
        INT_KIND,REAL_KIND,CHAR_KIND,BOOL_KIND,ENUM_KIND,SET_KIND:
          PUT2(COMPARE2,OP,TTYP);
        STRING_KIND: PUT2(COMPSTRCT2,OP,T^.LENGTH);
        PASSIVE_KIND,ACTIVE_KIND,POINTER_KIND,QUEUE_KIND,GENERIC_KIND,
        UNDEF_KIND,SYSCOMP_KIND,ROUTINE_KIND: ERROR2(TYPE_ERROR)
      END;
    POP; T^:=BOOL_EXPR
  END;

  PROCEDURE STRICT_INEQUALITY(OP:INTEGER);
  BEGIN
    (*RIGHT OPERAND*) VALUE_;
    IF COMPATIBLE THEN
      CASE T^.KIND OF
        INT_KIND,REAL_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND:
          PUT2(COMPARE2,OP,TTYP);
        STRING_KIND: PUT2(COMPSTRCT2,OP,T^.LENGTH);
        SET_KIND,POINTER_KIND,PASSIVE_KIND,ACTIVE_KIND,QUEUE_KIND,
        SYSCOMP_KIND,ROUTINE_KIND,UNDEF_KIND: ERROR2(TYPE_ERROR)
      END;
    POP; T^:=BOOL_EXPR
  END;

  PROCEDURE INCLUSION;
  BEGIN
    (*RIGHT OPERAND*) VALUE_;
    IF (T^.KIND=SET_KIND) AND (S^.KIND IN INDEXS)
      AND (S^.NOUN=T^.NOUN) THEN PUT2(COMPARE2,INSET,SET_TYP)
    ELSE ERROR2(TYPE_ERROR);
    POP; T^:=BOOL_EXPR
  END;

  PROCEDURE UMINUS;
  BEGIN
    (*OPERAND*) VALUE_;
    IF T^.KIND IN ARITHMETIC THEN PUT1(NEG2,TTYP) ELSE ERROR1(TYPE_ERROR)
  END;

  PROCEDURE UPLUS;
  BEGIN
    (*OPERAND*) VALUE_;
    IF T^.KIND IN ARITHMETIC THEN (*OK*) ELSE ERROR1(TYPE_ERROR)
  END;

  PROCEDURE PLUS_MINUS_STAR(OP:INTEGER);
  VAR TNOUN:INTEGER;
  BEGIN
    (*RIGHT OPERAND*) VALUE_;
    IF T^.KIND=S^.KIND THEN
      IF T^.KIND=INT_KIND THEN BEGIN
        PUT1(OP,fw_typ);
        POP; T^:=INT_EXPR
      END ELSE IF T^.KIND=REAL_KIND THEN BEGIN
        PUT1(OP,REAL_TYP);
        POP; T^:=REAL_EXPR
      END ELSE IF (T^.KIND=SET_KIND) AND (OP=SUB2)
        AND COMPATIBLE THEN BEGIN
        PUT1(SUB2,SET_TYP); TNOUN:=T^.NOUN;
        POP; T^:=SET_EXPR; T^.NOUN:=TNOUN
      END ELSE ERROR2P(TYPE_ERROR)
    ELSE ERROR2P(TYPE_ERROR)
  END;

  PROCEDURE SLASH;
  BEGIN
    (*RIGHT OPERAND*) VALUE_;
    IF (T^.KIND=REAL_KIND) AND (S^.KIND=REAL_KIND) THEN
      PUT1(DIV2,REAL_TYP)
    ELSE ERROR2(TYPE_ERROR);
    POP; T^:=REAL_EXPR
  END;

  PROCEDURE DIV_MOD(OP:INTEGER);
  BEGIN
    (*RIGHT OPERAND*) VALUE_;
    IF (T^.KIND=INT_KIND) AND (S^.KIND=INT_KIND) THEN
      PUT1(OP,fw_typ)
    ELSE ERROR2(TYPE_ERROR);
    POP; T^:=INT_EXPR
  END;

  PROCEDURE OR_AND(OP:INTEGER);
  VAR TNOUN:INTEGER;
  BEGIN
    (*RIGHT OPERAND*) VALUE_;
    IF T^.KIND=S^.KIND THEN
      IF T^.KIND=BOOL_KIND THEN BEGIN
        PUT1(OP,hw_typ);
        POP; T^:=BOOL_EXPR
      END ELSE IF (T^.KIND=SET_KIND)
        AND COMPATIBLE THEN BEGIN
        PUT1(OP,SET_TYP); TNOUN:=T^.NOUN;
        POP; T^:=SET_EXPR; T^.NOUN:=TNOUN
      END ELSE ERROR2P(TYPE_ERROR)
    ELSE ERROR2P(TYPE_ERROR)
  END;

  PROCEDURE NOT_;
  BEGIN
    (*OPERAND*) VALUE_;
    IF T^.KIND<>BOOL_KIND THEN ERROR1(TYPE_ERROR);
    T^:=BOOL_EXPR;
    PUT0(NOT2)
  END;

  PROCEDURE EMPTY_SET;
  BEGIN
    PUSH; T^:=SET_EXPR;
    PUT3(PUSHVAR2,SET_TYP,LCONST_MODE,0)
  END;

  PROCEDURE INCLUDE;
  BEGIN
    (*SET MEMBER*) VALUE_;
    IF T^.KIND IN INDEXS THEN BEGIN
      IF S^.NOUN=XUNDEF     THEN S^.NOUN:=T^.NOUN
      ELSE IF S^.NOUN<>T^.NOUN THEN ERROR2(TYPE_ERROR);
      PUT0(BUILDSET2)
    END ELSE ERROR2(TYPE_ERROR);
    POP
  END;

  PROCEDURE FUNCTION_;
  BEGIN
    PUSH; T^:= UNDEF_EXPR; T^.CONTEXT:= FUNC_RESULT;
    (*FUNC*) TYPE_;
    WITH S^ DO
      IF (CLASS = ROUTINE) AND (MODE <> STD_MODE) 
      THEN PUT2(FUNCVALUE2, MODE, TTYP)
  END;

  PROCEDURE CALL_FUNC;
  BEGIN
    WITH S^ DO
     IF CLASS = ROUTINE THEN
      IF MODE=STD_MODE THEN PUT2(FUNCTION2, DISP, TTYP)
      ELSE PUT3(CALL2, MODE, DISP, PARM_SIZE);
    S^:=T^; POP
  END;

  PROCEDURE CALL_GEN;
  BEGIN
    WITH S^ DO PUT2(FUNCTION2,DISP,TTYP);
    T^.CONTEXT:= FUNC_RESULT; S^:= T^;
    POP (*ARG*)
  END;
(*########*)
(*VARIABLE*)
(*########*)

  PROCEDURE UNDEF;
  BEGIN
    PUSH; T^:=UNDEF_EXPR;
    PUT1(PUSHCONST2,0)
  END;

  PROCEDURE VCOMP;
  VAR SAVE_CONTEXT:INTEGER;
  BEGIN
    (*RECORD OR CLASS*) ADDRESS; SAVE_CONTEXT:=T^.CONTEXT;
    VAR_REF; TYPE_;
    WITH T^ DO BEGIN
      PUT1(FIELD2,DISP);
      STATE:=ADDR;
      IF CONTEXT=VARIABLE THEN CONTEXT:=ENTRY_VAR
      ELSE CONTEXT:=SAVE_CONTEXT;
    END
  END;

  PROCEDURE RCOMP;
  VAR INITABLE: BOOLEAN;
  BEGIN
    WITH T^ DO
      IF CLASS = VALUE THEN
        IF CONTEXT IN PARMS THEN INITABLE:= FALSE ELSE INITABLE:= TRUE
      ELSE INITABLE:= TRUE;
    (*SYSCOMP*) ADDRESS;
    POP;
    (*ENTRY*) ROUTINE_;
    IF T^.MODE IN INIT_MODES THEN
      IF NOT INITABLE THEN ERROR1(INIT_ERROR)
  END;

  PROCEDURE SUB(op: integer);
  VAR MIN,MAX,SIZE: INTEGER;
  BEGIN
    (*SUBSCRIPT*) VALUE_;
    READ_IFL(MIN); READ_IFL(MAX); READ_IFL(SIZE);
    PUT3(op,MIN,MAX,SIZE);
    PUSH; T^:=UNDEF_EXPR; (*INDEX*) TYPE_;
    IF COMPATIBLE THEN (*OK*);
    POP; POP;
    (*ELEMENT*) TYPE_
  END;

  PROCEDURE ARROW;
  VAR SAVE_CONTEXT:CONTEXT_KIND;
  BEGIN
    WITH T^ DO
      IF KIND=POINTER_KIND THEN BEGIN
        SAVE_CONTEXT:=CONTEXT;
        (*POINTER*) VALUE_; CONTEXT:=SAVE_CONTEXT;
        STATE:=ADDR
      END ELSE ERROR1(TYPE_ERROR);
    (*OBJECT*) TYPE_
  END;
(*#########*)
(*MAIN LOOP*)
(*#########*)

BEGIN (*MAIN PROGRAM*)
INITIALIZE;
REPEAT (*MAIN LOOP*)
 READ_IFL(SY);
 CASE SY OF

 ADDRESS1: ADDRESS;
 AND1: OR_AND(AND2);
 ARROW1: ARROW;
 BODY_END1: BODY_END;
 BODY1: BODY;
 CALL_FUNC1: CALL_FUNC;
 CALL_GEN1: CALL_GEN;
 CALL_PROC1: CALL_PROC;
 CASE_JUMP1: CASE_JUMP;
 CASE_LIST1: CASE_LIST;
 CHK_TYPE1: CHK_TYPE;
 CONSTPARM1: CONSTPARM(FALSE);
 DEF_LABEL1: DEF_LABEL;
 DIV1: DIV_MOD(DIV2);
 EMPTY_SET1: EMPTY_SET;
 EOM1: EOM;
 EQ1: EQUALITY(EQUAL);
 FALSEJUMP1: FALSE_JUMP;
 FOR_DOWN1: FOR_LOOP(DECREMENT2);
 FOR_LIM1: FOR_LIM;
 FOR_STORE1: FOR_STORE;
 FOR_UP1: FOR_LOOP(INCREMENT2);
 FUNCTION1: FUNCTION_;
 GE1: INEQUALITY(NOTLESS);
 GT1: STRICT_INEQUALITY(GREATER);
 INCLUDE1: INCLUDE;
 INIT1: INIT_;
 INTF_LBL1: INTF_LBL;
 IN1: INCLUSION;
 JUMP_DEF1: JUMP_DEF;
 JUMP1: JUMP;
 LCONST1: LCONST;
 LE1: INEQUALITY(NOTGREATER);
 LT1: STRICT_INEQUALITY(LESS);
 MESSAGE1: messpass;
 MINUS1: PLUS_MINUS_STAR(SUB2);
 MOD1: DIV_MOD(MOD2);
 NEW_LINE1: begin 
              read_ifl(line_no); 
              put1(NEWLINE2, line_no) 
            end;
 NE1: EQUALITY(NOTEQUAL);
 NOT1: NOT_;
 OR1: OR_AND(OR2);
 PLUS1: PLUS_MINUS_STAR(ADD2);
 PROG_CALL1: PROG_CALL;
 RANGE1: IGNORE2(RANGE2);
 RCOMP1: RCOMP;
 RESULT1: RESULT;
 ROUTINE1: ROUTINE_;
 SAVEPARM1: CONSTPARM(TRUE);
 SLASH1: SLASH;
 STAR1: PLUS_MINUS_STAR(MUL2);
 STORE1: STORE(TRUE);
 SUB1: SUB(index2);
 subnc1: sub(indexnc2);
 UMINUS1: UMINUS;
 UNDEF1: UNDEF;
 UPLUS1: UPLUS;
 VALUE1: VALUE_;
 VARPARM1: VARPARM;
 VAR1: VAR_;
 VCOMP1: VCOMP;
 WITH1: POP_TEMP
 END

 UNTIL DONE;
 break(list);
 NEXTPASS(this_pass)
END   (* cpas5e *) ;

begin end.
  @A)