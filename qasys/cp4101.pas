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
: **  File Name          : cp4101.pas                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     Cp4101.pas contains the code for the 4th pass of the     **
: **     concurrent pascal compler.  This pass is responsible     **
: **     for declaration analysis.                                **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.01 04/12/79 DEG  1162     Parms, then vars, allocated at  **
: **                              increasing addresses starting   **
: **                              at zero.                        **
: ** 01.01 04/12/79 DEG  1162     Pointers allocated a fullword   **
: **                              each                            **
: ** 01.01 04/12/79 DEG  1162     Booleans and chars allocated a  **
: **                              byte each                       **
: ** 01.01 04/12/79 DEG  1162     Each parameter right justified  **
: **                              within a fullword               **
: ** 01.01 04/12/79 DEG  1162     Function result at zero offset  **
: **                              (pass 5 will fix)               **
: ** 01.01 04/16/79 DEG  1162     Negative offset for function    **
: **                              result                          **
: ** 01.01 04/18/79 DEG  1162     Corrected index arithmetic in   **
: **                              pend                            **
: ** 01.01 04/18/79 DEG  1162     Initialize record_inheritance   **
: ** 01.01 04/19/79 DEG  1162     In body, reset current_disp     **
: **                              correctly for initial statement **
: ** 01.01 05/03/79 DEG  1162     In var_list and pend, offset    **
: **                              variable and parameter displace-**
: **                              ments by base_size.  Function   **
: **                              result is identified by zero    **
: **                              offset                          **
: ** 01.01 05/03/79 DEG  1162     Also in for_lim and with_temp   **
: ** 01.01 05/21/79 DEG  1162     Integers allocated a fullword   **
: **                              each                            **
: ** 01.01 05/21/79 DEG  1162     Subrange types allocated the    **
: **                              same amount of storage as the   **
: **                              base type                       **
: ** 01.01 06/11/79 DEG  1162     Force array sizes to an even    **
: **                              number of bytes                 **
: ** 01.01 06/27/79 DEG  1162     Increased max_int to a value    **
: **                              appropriate for an engine       **
: ** 01.01 08/03/79 DEG  1162     Allocate a halfword for         **
: **                              booleans, so level 1 code       **
: **                              generator can generate rx       **
: **                              instructions                    **
: ** 01.01 08/28/79 DEG  1162     Raised limit on the extent of   **
: **                              enumeration types               **
: ** 01.01 09/14/79 DEG  1162     Increased setlength to 32 bits  **
: ** 01.01 09/14/79 DEG  1162     Increased upper limit of type   **
: **                              char to 255                     **
: ** 01.01 09/14/79 DEG  1162     Increased set_max to 255        **
: ** 01.01 09/25/79 DEG  1162     Increased noun_max to 1000      **
: ** 01.01 09/27/79 DEG  1162     In pend, allow parameter size   **
: **                              be larger than a fullword       **
: ** 01.01 10/29/79 DEG  1162     Fixed Hartmann's divide-by-zero **
: **                              bug in function multiply        **
: ** 01.01 11/26/79 DEG  1162     Increased noun_max to 1500      **
: ** 01.01 12/27/79 RNP  1162     Increased noun_max to 3000      **
: ** 01.01 04/18/80 SLC  1162     Increased max_level to 3        **
: ** 01.01 04/18/80 SLC  1162     Display statistics on console   **
: ** 01.01 05/13/80 SLC  1162     Increased stack_max to 100      **
: ** 01.01 05/13/80 SLC  1162     Increased noun_max to 3500      **
: ** 01.01 05/13/80 SLC  1162     Increased max_level to 10       **
: ** 01.01 07/09/80 SLC  1162     Increased noun_max to 3800      **
: ** 01.01 09/26/80 MDS  1162     Increased noun_max to 4500      **
: ** 01.01 10/28/80 MDS  1162     Increased noun_max to 4800      **
: ** 01.01 12/04/80 MDS  1162     Increased noun_max to 5000      **
: ** 01.01 12/12/80 MDS  1162     Increased max_level to 20       **
: ** 01.01 01/20/81 MDS  1162     Increased noun_max to 5500      **
: ** 01.01 06/01/81 BH   1162     Increased noun_max to 6000      **
: ** 01.01 09/30/81 BH   1162     Increased noun_max to 7000      **
: ** 01.02 12/15/86 PJH  1162     Increased noun_max to 18000     **
: ** 01.02 12/15/86 PJH  1162     ADDED PROPRIETARY BANNER        **
: **                                                              **
: *****************************************************************)

program cp4e, cpas4e;

(*###########
#  PREFIX  #
###########*)

CONST
byte_size = 1;
hw_size = 2 (* bytes *) ;
fw_size = 4 (* bytes *) ;
REALLENGTH = 8 (*BYTES*);
SETLENGTH = 32 (*BYTES*);
LISTOPTION = 0;    SUMMARYOPTION = 1;  TESTOPTION = 2;     CHECKOPTION = 3;
CODEOPTION = 4;    NUMBEROPTION = 5;   xrefoption = 6;     bstackoption = 7;
dumpoption = 9;   lgxrefoption = 8;

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
    errstr = packed array[1..22] of char;
(*************)
(* externals *)
(*************)

procedure initpass(p: pass_range);  extern;

procedure nextpass(p: pass_range);  extern;

procedure giverr(line, pass, number: integer); extern;

procedure printarg(var f: text;  arg: integer);  extern;

procedure printff(var f: text;  p: pass_range);  extern;

procedure printop(var f: text;  op: integer);  extern;
procedure cpas4e(inter_pass_ptr: passptr;
	         var ilin, ilout: int_file;
                 var list: text;
		 var xrefile: int_file);

CONST

(*INPUT OPERATORS*)

EOM1=1;            TYPE_DEF1=2;        NEW_NOUN1=3;        VAR_LIST1=4;
EVAR_LIST1=5;      INITS_DEF1=6;       PROC_DEF1=7;        PROCE_DEF1=8;
FUNC_DEF1=9;       FUNCE_DEF1=10;      PROG_DEF1=11;       TYPE1=12;
ENUM_DEF1=13;      SUBR_DEF1=14;       SET_DEF1=15;        INTF1=16;
ARRAY_DEF1=17;     REC1=18;            FIELDLIST1=19;      REC_DEF1=20;
CLASS1=21;         MONITOR1=22;        PROCESS1=23;        STACK1=24;
PSTART1=25;        PARM_TYPE1=26;      UNIV_TYPE1=27;      CPARMLIST1=28;
VPARMLIST1=29;     BODY1=30;           BODY_END1=31;       ADDRESS1=32;
RESULT1=33;        STORE1=34;          CALL_PROC1=35;      CALL_PROG1=36;
INTF_ID1=37;       PARM1=38;           FALSEJUMP1=39;      DEF_LABEL1=40;
JUMP_DEF1=41;      FUNCF_DEF1=42;      JUMP1=43;           CASE_LIST1=44;
FOR_STORE1=45;     FOR_LIM1=46;        FOR_UP1=47;         FOR_DOWN1=48;
WITH_VAR1=49;      WITH_TEMP1=50;      WITH1=51;           INIT1=52;
VALUE1=53;         LT1=54;             EQ1=55;             GT1=56;
LE1=57;            NE1=58;             GE1=59;             IN1=60;
UPLUS1=61;         UMINUS1=62;         PLUS1=63;           MINUS1=64;
OR1=65;            STAR1=66;           SLASH1=67;          DIV1=68;
MOD1=69;           AND1=70;            NOT1=71;            EMPTY_SET1=72;
INCLUDE1=73;       FUNCTION1=74;       CALL_FUNC1=75;      ROUTINE1=76;
VAR1=77;           ARROW1=78;          VCOMP1=79;          RCOMP1=80;
SUB1=81;           INDEX1=82;          REAL1=83;           STRING1=84;
LCONST1=85;        MESSAGE1=86;        NEW_LINE1=87;       FWD_DEF1=88;
CHK_TYPE1=89;      PROCF_DEF1=90;      UNDEF1=91;          PEND1=92;
CASE_JUMP1=93;	   

(*OUTPUT OPERATORS*)

EOM2=1;            BODY2=2;            BODY_END2=3;        ADDRESS2=4;
RESULT2=5;         STORE2=6;           CALL_PROC2=7;       CONSTPARM2=8;
VARPARM2=9;        FALSEJUMP2=10;      DEF_LABEL2=11;      JUMP_DEF2=12;
CASE_JUMP2=13;     JUMP2=14;           CASE_LIST2=15;      FOR_STORE2=16;
FOR_LIM2=17;       FOR_UP2=18;         FOR_DOWN2=19;       WITH2=20;
INIT2=21;          CALL_PROG2=22;      INTF_LBL2=23;       VALUE2=24;
LT2=25;            EQ2=26;             GT2=27;             LE2=28;
NE2=29;            GE2=30;             IN2=31;             UPLUS2=32;
UMINUS2=33;        PLUS2=34;           MINUS2=35;          OR2=36;
STAR2=37;          SLASH2=38;          DIV2=39;            MOD2=40;
AND2=41;           EMPTY_SET2=42;      INCLUDE2=43;        FUNCTION2=44;
CALL_FUNC2=45;     ROUTINE2=46;        VAR2=47;            ARROW2=48;
VCOMP2=49;         RCOMP2=50;          SUB2=51;            LCONST2=52;
MESSAGE2=53;       NEW_LINE2=54;       CHK_TYPE2=55;       SAVEPARM2=56;
CALL_GEN2=57;      NOT2=58;            UNDEF2=59;          RANGE2=60;
subnc2=61;

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

(*ERRORS*)

NESTING_ERROR=1;   ADDRESS_ERROR=2;    ACTIVE_ERROR=3;     QUEUE_ERROR=4;
PROCESS_ERROR=5;   ENTRY_ERROR=6;      FUNCTYPE_ERROR=7;   TYPEID_ERROR=8;
ENUM1_ERROR=9;     ENUM2_ERROR=10;     INDEX_ERROR=11;     MEMBER_ERROR=12;
STACK_ERROR=13;    PARM1_ERROR=14;     PARM2_ERROR=15;     PARM3_ERROR=16;
PARM4_ERROR=17;    PARM5_ERROR=18;     PARM6_ERROR=19;     PARM7_ERROR=20;
COMPILER_ERROR=21; STRING_ERROR=22;

(*CONTEXT*)

FUNC_RESULT=1;     ENTRY_VAR=2;        VARIABLE=3;         VAR_PARM=4;
UNIV_VAR=5;        CONST_PARM=6;       UNIV_CONST=7;       FIELD=8;
EXPR=10;           CONSTANT=11;        SAVE_PARM=12;       WITH_CONST = 13;
WITH_VAR = 14;

(*TYPE KIND*)

INT_KIND=0;        REAL_KIND=1;        BOOL_KIND=2;        CHAR_KIND=3;
ENUM_KIND=4;       SET_KIND=5;         STRING_KIND=6;      PASSIVE_KIND=7;
POINTER_KIND=8;    QUEUE_KIND=9;       GENERIC_KIND=10;    UNDEF_KIND=11;
SYSCOMP_KIND=12;   ROUTINE_KIND=13;    ACTIVE_KIND=14;

(*INPUT_MODES*)

CLASS1_MODE=1;     MONITOR1_MODE=2;    PROCESS1_MODE=3;    PROC1_MODE=4;
PROCE1_MODE=5;     FUNC1_MODE=6;       FUNCE1_MODE=7;      PROGRAM1_MODE=8;
RECORD_MODE=9;     VARIANT_MODE=10;

(*OUTPUT_MODES*)

SCONST2_MODE=11;   LCONST2_MODE=0;     PROC2_MODE=1;       PROGRAM2_MODE=2;
PE2_MODE=3;        CE2_MODE=4;         ME2_MODE=5;         PROCESS2_MODE=6;
CLASS2_MODE=7;     MONITOR2_MODE=8;    STD2_MODE=9;        UNDEF2_MODE=10;

(*MISCELLANEOUS*)

INITIAL_LEVEL=0;   RESOLVE=TRUE;       DONT_RESOLVE=FALSE;
MAX_INT="FFFF0;    SET_MIN=0;          SET_MAX=255;        THIS_PASS=4;
STACK_MAX=100;     NOUN_MAX=18000;      MAX_LEVEL=20;
enum_max="FFFF;
base_size = fw_size (* line number or component index *) ;

TYPE

  INPUT_MODE=CLASS1_MODE..RECORD_MODE;

  OUTPUT_MODE= LCONST2_MODE..SCONST2_MODE;

  DISPLACEMENT=INTEGER;

  STACK_INDEX=0..STACK_MAX;

  NOUN_INDEX=0..NOUN_MAX;

  LEGACY_TYPE=(CLASS_LEGACY,MONITOR_LEGACY,PROCESS_LEGACY,QUEUE_LEGACY);

  LEGACYS=SET OF LEGACY_TYPE;

  TYPE_KIND=INT_KIND..ACTIVE_KIND;

  TYPE_KINDS=SET OF TYPE_KIND;

  CONTEXT_KIND=FUNC_RESULT..WITH_VAR;

  CONTEXTS=SET OF CONTEXT_KIND;

  PACKED_SET=0..65535;

  ENTRY_CLASS=(UNDEFINED,VALUE,ROUTINE,TEMPLATE);

  ENTRY_PTR=^ENTRY;

  ENTRY=
    RECORD
      CASE CLASS:ENTRY_CLASS OF
        VALUE:(
          VMODE:OUTPUT_MODE; VDISP:DISPLACEMENT;
          CONTEXT:CONTEXT_KIND);
        ROUTINE:(
          RMODE:OUTPUT_MODE; RDISP:DISPLACEMENT;
          PARM_SIZE,VAR_SIZE,STACK_SIZE:DISPLACEMENT);
        TEMPLATE:(
          NOUN:NOUN_INDEX; SIZE:DISPLACEMENT; INHERITANCE:PACKED_SET;
          CASE KIND:TYPE_KIND OF
            passive_kind, active_kind:(
	      base_entry:entry_ptr);
            INT_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND:(
              MIN,MAX:INTEGER);
            SYSCOMP_KIND:(SMODE:OUTPUT_MODE;
              OFFSET:DISPLACEMENT;
	      initstat:noun_index))
    END;

  DISPLAY_INDEX=0..MAX_LEVEL;

  DISPLAY_REC=
    RECORD
      LAST_MODE: OUTPUT_MODE;
      LAST_ADDRESS:DISPLACEMENT;
      LAST_INHERITANCE:PACKED_SET
    END;

    errstr = packed array[1..22] of char;

VAR

  SY,PARM_NUMBER:INTEGER;

  WITH_CONTEXT:CONTEXT_KIND;

  PACKED_CLASS,PACKED_MONITOR,PACKED_PROCESS,PACKED_QUEUE: PACKED_SET;

  N:NOUN_INDEX;

  DEBUG,xref,DONE,UNIVERSAL,SAVE_CONTEXT,GENERIC_FUNCTION,INITIAL_ENTRY: BOOLEAN;

  NOUN_TABLE:ARRAY [NOUN_INDEX] OF ENTRY_PTR;

  STACK:ARRAY [STACK_INDEX] OF ENTRY_PTR;

  THIS_LEVEL,levelsum: INTEGER;

  DISPLAY: ARRAY [DISPLAY_INDEX] OF DISPLAY_REC;

  INTF_LENGTH,CURRENT_DISP,CURRENT_LABEL,
  COMPVAR_LENGTH: DISPLACEMENT;

  CHK_MODE:INPUT_MODE;

  MODE: OUTPUT_MODE;

  T,stacksum: INTEGER;

  PASS_BY_REFERENCE, ASSIGNABLE: CONTEXTS;

  RECORD_INHERITANCE: LEGACYS;

  UENTRY,NEW_ENTRY,OLD_ENTRY,UTYPE: ENTRY_PTR;

  SMALLS,ACTIVES,PASSIVES,FUNC_TYPES,INDEXS,LARGES: TYPE_KINDS;

  NONVARPARMS: SET OF INPUT_MODE;

  NONCOMPS: SET OF OUTPUT_MODE;

  change: packed record
                   case boolean of
                     false: (s: legacys);
                     true : (i: 0..65535;
                             foo: 0..1048575;
                             bar: integer)
	       end;

  line_no: integer;
(******************)
(* i/o procedures *)
(******************)

PROCEDURE READ_IFL (VAR I: INTEGER);
BEGIN
  i := ilin^;  get(ilin)
END;

function peek_ifl: integer;
begin
  peek_ifl := ilin^
end;

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
    WRITE_IFL(OP);
    IF DEBUG THEN PRINTOP(list, OP)
  END;

  PROCEDURE PUT1(OP,ARG1:INTEGER);
  BEGIN
    WRITE_IFL(OP); WRITE_IFL(ARG1);
    IF DEBUG THEN BEGIN
      PRINTOP(list, OP); PRINTARG(list, ARG1)
    END
  END;

  PROCEDURE PUT2(OP,ARG1,ARG2:INTEGER);
  BEGIN
    WRITE_IFL(OP); WRITE_IFL(ARG1); WRITE_IFL(ARG2);
    IF DEBUG THEN BEGIN
      PRINTOP(list, OP); PRINTARG(list, ARG1); PRINTARG(list, ARG2)
    END
  END;

  PROCEDURE PUT3(OP,ARG1,ARG2,ARG3:INTEGER);
  BEGIN
    WRITE_IFL(OP); WRITE_IFL(ARG1); WRITE_IFL(ARG2); WRITE_IFL(ARG3);
    IF DEBUG THEN BEGIN
      PRINTOP(list, OP); PRINTARG(list, ARG1); PRINTARG(list, ARG2); PRINTARG(list, ARG3)
    END
  END;

  PROCEDURE PUT3_ARG(ARG1,ARG2,ARG3:INTEGER);
  BEGIN
    WRITE_IFL(ARG1); WRITE_IFL(ARG2); WRITE_IFL(ARG3);
    IF DEBUG THEN BEGIN
      PRINTARG(list, ARG1); PRINTARG(list, ARG2); PRINTARG(list, ARG3)
    END
  END;

  PROCEDURE PUT4(OP,ARG1,ARG2,ARG3,ARG4:INTEGER);
  BEGIN
    PUT3(OP,ARG1,ARG2,ARG3); PUT_ARG(ARG4)
  END;

  PROCEDURE PUT5(OP,ARG1,ARG2,ARG3,ARG4,ARG5:INTEGER);
  BEGIN
    PUT3(OP,ARG1,ARG2,ARG3);
    PUT_ARG(ARG4); PUT_ARG(ARG5)
  END;

procedure wrxref(i: integer);
begin
  xrefile^ := i;
  put(xrefile)
end;

procedure wrxdisps;
var
  i: noun_index;
begin
  if xref (* only if the xref flag is on *)
  then begin 
    for i := 0 to noun_max do begin(* for each noun... *)
      if noun_table[i] <> nil (* ...that's really defined... *)
      then with noun_table[i]^ do begin
	wrxref(i); (* write out the noun_index *)
	case class of
	  value:
	    wrxref(vdisp);
	  routine:
	    wrxref(rdisp);
	  template:
	    if kind = syscomp_kind
	    then with noun_table[initstat]^ do begin
	      if class = routine
	      then wrxref(parm_size+var_size)
	      else wrxref(-1)
	    end
            else wrxref(-1);
	  undefined:
	    wrxref(-1)
        end
      end
    end;
  wrxref(-1); (* seal off the end *)
  end
end;

(*NOTE: A PASS RUNNING WITH TEST OUTPUT SHOULD START WITH PRINTFF*)
(*#######*)
(*NG*)
(*#######*)

  PROCEDURE PACK (VAR PACKED_SET: INTEGER;
                  UNPACKED_SET: legacys);
  BEGIN
    with change do
      begin
        s := unpacked_set;
        packed_set := i
      end
  END;

  PROCEDURE UNPACK (PACKED_SET: INTEGER;
                    VAR UNPACKED_SET: legacys);
  BEGIN
    with change do
      begin
        s := [];
        i := packed_set;
        unpacked_set := s
      end
  END;
(*##########*)
(*INITIALIZE*)
(*##########*)

  PROCEDURE STD_INDEX(N:NOUN_INDEX; K:TYPE_KIND;
                      s:displacement; L,U:INTEGER);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE[N]:=E;
    WITH E^ DO BEGIN
      CLASS:=TEMPLATE; NOUN:=N;
      SIZE:=s; INHERITANCE:=0;
      KIND:=K; MIN:=L; MAX:=U
    END
  END;

  PROCEDURE STD_PARM(N:NOUN_INDEX; C:CONTEXT_KIND);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE[N]:=E;
    WITH E^ DO BEGIN
      CLASS:=VALUE; VMODE:=UNDEF2_MODE;
      VDISP:= 0;
      CONTEXT:=C
    END
  END;

  PROCEDURE STD_ROUTINE(N:NOUN_INDEX; NO:INTEGER);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE[N]:=E;
    WITH E^ DO BEGIN
      CLASS:= ROUTINE; RMODE:= STD2_MODE; RDISP:= NO;
      PARM_SIZE:= 0; VAR_SIZE:= 0;
    END
  END;

  PROCEDURE STD_NONINDEX(N:NOUN_INDEX; K:TYPE_KIND; S:DISPLACEMENT;
    I:PACKED_SET);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE[N]:=E;
    WITH E^ DO BEGIN
      CLASS:=TEMPLATE;
      NOUN:=N; SIZE:=S; INHERITANCE:=I;
      KIND:=K
    END
  END;

  PROCEDURE INITIALIZE;
  VAR I:INTEGER;  TEMP_LEGACY: LEGACYS;
  BEGIN
    stacksum:= 0; levelsum:= 0;
    rewrite(ilout);  reset(ilin);
    INITPASS(this_pass);
    WITH INTER_PASS_PTR^ DO BEGIN
      DEBUG:=TESTOPTION IN OPTIONS;
      xref:=xrefoption in options;
      IF DEBUG THEN PRINTFF(list, this_pass)
    END;
    for i := 0 to noun_max do
      noun_table[i]:=nil;
    GENERIC_FUNCTION:=FALSE;
    CURRENT_DISP:=0;
    T:=-1; DONE:=FALSE;
    THIS_LEVEL:=-1;
    MODE:=PROCESS2_MODE;
    INITIAL_ENTRY:=FALSE;
    SAVE_CONTEXT:=FALSE;
    COMPVAR_LENGTH:=0;
    record_inheritance := [];
    PASSIVES:=[INT_KIND,REAL_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND,SET_KIND,
      STRING_KIND,PASSIVE_KIND,UNDEF_KIND];
    ASSIGNABLE:= [FUNC_RESULT, VARIABLE, VAR_PARM, UNIV_VAR, WITH_VAR];
    NONCOMPS:= [PROC2_MODE, CE2_MODE, ME2_MODE, PE2_MODE,
      PROGRAM2_MODE, UNDEF2_MODE];
    CURRENT_LABEL:=0;(*THIS AUTOMATICALLY ASSIGNS LABEL 1 TO THE INITIAL PROCESS*)
    NEW(UTYPE);
    WITH UTYPE^ DO BEGIN
      CLASS:=TEMPLATE;
      NOUN:=XUNDEF; SIZE:=1;
      INHERITANCE:=0; KIND:=UNDEF_KIND
    END;
    INDEXS:=[INT_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND];
    PASS_BY_REFERENCE:=[VAR_PARM,UNIV_VAR];
    LARGES:=[STRING_KIND,PASSIVE_KIND,ACTIVE_KIND,SYSCOMP_KIND];
    TEMP_LEGACY:= [CLASS_LEGACY];
    PACK(PACKED_CLASS, TEMP_LEGACY);
    TEMP_LEGACY:= [MONITOR_LEGACY];
    PACK(PACKED_MONITOR, TEMP_LEGACY);
    TEMP_LEGACY:= [PROCESS_LEGACY];
    PACK(PACKED_PROCESS, TEMP_LEGACY);
    TEMP_LEGACY:= [QUEUE_LEGACY];
    PACK(PACKED_QUEUE, TEMP_LEGACY);
    SMALLS:=[INT_KIND,REAL_KIND,CHAR_KIND,BOOL_KIND,ENUM_KIND,SET_KIND,
      QUEUE_KIND];
    ACTIVES:=[QUEUE_KIND,SYSCOMP_KIND,ACTIVE_KIND];
    FUNC_TYPES:= [INT_KIND, CHAR_KIND, BOOL_KIND,
      ENUM_KIND, REAL_KIND];
    NONVARPARMS:=[CLASS1_MODE,MONITOR1_MODE,PROCESS1_MODE,FUNC1_MODE,
      FUNCE1_MODE];
    NEW(UENTRY); UENTRY^.CLASS:=UNDEFINED; NOUN_TABLE[XUNDEF]:=UENTRY;
    STD_INDEX(XINTEGER,INT_KIND,fw_size,-"80000000,"7FFFFFFF);
    STD_NONINDEX(XREAL,REAL_KIND,REALLENGTH,0);
    STD_INDEX(XBOOLEAN,BOOL_KIND,hw_size,0,1);
    STD_INDEX(XCHAR,CHAR_KIND,byte_size,0,255);
    STD_NONINDEX(XQUEUE,QUEUE_KIND,fw_size,PACKED_QUEUE);
    STD_NONINDEX(ZWITH,POINTER_KIND,fw_size,0);
    STD_NONINDEX(ZARITHMETIC,GENERIC_KIND,0,0);
    STD_NONINDEX(ZPASSIVE,GENERIC_KIND,0,0);
    STD_NONINDEX(ZINDEX,GENERIC_KIND,0,0);
    STD_PARM(ZVPARM,VAR_PARM);
    STD_PARM(ZCPARM,CONST_PARM);
    STD_PARM(ZSPARM,SAVE_PARM);
    STD_ROUTINE( XTRUNC,0);
    STD_ROUTINE( XABS,1);
    STD_ROUTINE( XSUCC,2);
    STD_ROUTINE( XPRED,3);
    STD_ROUTINE( XCONV,4);
    STD_ROUTINE( XEMPTY,5);
    STD_ROUTINE( XATTRIBUTE,6);
    STD_ROUTINE( XREALTIME,7);
    STD_ROUTINE( XORD,8);
    STD_ROUTINE( XCHR,9);
    STD_ROUTINE( XDELAY,0);
    STD_ROUTINE( XCONTINUE,1);
    STD_ROUTINE( XIO,2);
    STD_ROUTINE( XSTART,3);
    STD_ROUTINE( XSTOP,4);
    STD_ROUTINE( XSETHEAP,5);
    STD_ROUTINE( XWAIT,6);
  END;
(*######*)
(*ERRORS*)
(*######*)

  PROCEDURE ERROR(NUMBER:INTEGER);
  BEGIN
    giverr(line_no, this_pass, number);
    PUT2(MESSAGE2,THIS_PASS,NUMBER)
  END;

  procedure messpass;
  var
    mess, pass: integer;
  begin
    read_ifl(pass); read_ifl(mess);
    put2(message2, pass, mess);
    giverr(line_no, pass, mess)
  end;

  PROCEDURE EOM;
  BEGIN
    WITH INTER_PASS_PTR^ DO BEGIN
      BLOCKS:=CURRENT_LABEL;
    END;
    PUT1(EOM2,COMPVAR_LENGTH (*INITIAL PROCESS VAR SIZE*));
    DONE:=TRUE
  END;

  PROCEDURE ABORT(i : integer);
  BEGIN
    PUT2(MESSAGE2,THIS_PASS,COMPILER_ERROR);
    case i of
	1 : message(' stack_max overflow ');
        2 : message(' max_level overflow ');
       end;
       halt;
    EOM
  END;
(*######*)
(*IGNORE*)
(*######*)

  PROCEDURE CASE_LIST;
  VAR I,ARG,MIN,MAX:INTEGER;
  BEGIN
    READ_IFL(ARG); READ_IFL(MIN); READ_IFL(MAX);
    PUT3(CASE_LIST2,ARG,MIN,MAX);
    FOR I:=MIN TO MAX+1 DO BEGIN
      READ_IFL(ARG); PUT_ARG(ARG)
    END
  END;

  PROCEDURE LCONST;
  VAR LENGTH,I,ARG:INTEGER;
  BEGIN
    READ_IFL(LENGTH); PUT1(LCONST2,LENGTH);
    FOR I:=1 TO LENGTH DIV hw_size DO BEGIN
      READ_IFL(ARG); PUT_ARG(ARG)
    END
  END;

  PROCEDURE IGNORE1(OP:INTEGER);
  VAR ARG1:INTEGER;
  BEGIN
    READ_IFL(ARG1); PUT1(OP,ARG1)
  END;

  PROCEDURE IGNORE2(OP:INTEGER);
  VAR ARG1,ARG2:INTEGER;
  BEGIN
    READ_IFL(ARG1); READ_IFL(ARG2);
    PUT2(OP,ARG1,ARG2)
  END;
(*#############*)
(*NOUN HANDLING*)
(*#############*)

  procedure push_entry(e: entry_ptr);
  begin
    IF T>=STACK_MAX THEN ABORT(1) ELSE T:=T+1;
    stacksum:= t;
    STACK[T]:=E
  end;
    
  PROCEDURE PUSH_NEW_ENTRY(VAR E:ENTRY_PTR);
  BEGIN
    READ_IFL(N); NEW(E);
    IF N<>XUNDEF THEN NOUN_TABLE[N]:=E;
    push_entry(e);
  END;

  PROCEDURE PUSH_OLD_ENTRY(VAR E:ENTRY_PTR);
  BEGIN
    READ_IFL(N); E:=NOUN_TABLE[N];
    push_entry(e)
  END;
(*#######*)
(*NESTING*)
(*#######*)

  PROCEDURE PUSH_LEVEL(M:INPUT_MODE);
  BEGIN
    IF THIS_LEVEL>=MAX_LEVEL THEN ABORT(2) ELSE THIS_LEVEL:=THIS_LEVEL+1;
    levelsum:= this_level;
    WITH DISPLAY[THIS_LEVEL] DO BEGIN
      LAST_MODE:=MODE;
      PACK(LAST_INHERITANCE,RECORD_INHERITANCE);
      LAST_ADDRESS:=CURRENT_DISP; CURRENT_DISP:=0;
      IF MODE IN NONCOMPS THEN
        IF M<>RECORD_MODE THEN BEGIN
          ERROR(NESTING_ERROR); MODE:=CLASS2_MODE
        END;
      CASE M OF
        CLASS1_MODE: MODE:=CLASS2_MODE;
        MONITOR1_MODE: MODE:=MONITOR2_MODE;
        PROCESS1_MODE: MODE:=PROCESS2_MODE;
        PROC1_MODE,FUNC1_MODE: MODE:=PROC2_MODE;
        PROCE1_MODE,FUNCE1_MODE:
          CASE MODE OF
            CLASS2_MODE: MODE:=CE2_MODE;
            MONITOR2_MODE: MODE:=ME2_MODE;
            PROCESS2_MODE: MODE:=PE2_MODE
          END;
        PROGRAM1_MODE: MODE:=PROGRAM2_MODE;
        RECORD_MODE: BEGIN
          RECORD_INHERITANCE:=[];
          MODE:=UNDEF2_MODE
        END
      END
    END
  END;

  PROCEDURE POP_LEVEL;
  BEGIN
    WITH DISPLAY[THIS_LEVEL] DO BEGIN
      MODE:=LAST_MODE;
      UNPACK(LAST_INHERITANCE,RECORD_INHERITANCE);
      CURRENT_DISP:=LAST_ADDRESS
    END;
    THIS_LEVEL:=THIS_LEVEL-1
  END;
(*###################*)
(*ADDRESS COMPUTATION*)
(*###################*)

  FUNCTION ADD(A,B:INTEGER):INTEGER;
  BEGIN
  (*ASSERT (A>=0) AND (B>=0);*)
    IF MAX_INT-A>=B THEN ADD:=A+B
    ELSE BEGIN
      ERROR(ADDRESS_ERROR);
      ADD:=A
    END
  END;

  FUNCTION MULTIPLY(A,B:INTEGER):INTEGER;
  BEGIN
    (*ASSERT (A>=0) AND (B>=0);*)
    if b = 0
    then multiply := 0
    else IF A<=MAX_INT DIV B THEN MULTIPLY:=A*B
    ELSE BEGIN
      MULTIPLY:=A;
      ERROR(ADDRESS_ERROR)
    END
  END;

  FUNCTION SUBTRACT(A,B:INTEGER):INTEGER;
  BEGIN
    (*ASSERT A>=B;*)
    IF (A>=0) AND (B>=0) THEN SUBTRACT:=A-B
    ELSE IF (A<0) AND (B<0) THEN SUBTRACT:=A-B
    ELSE SUBTRACT:=ADD(A,-B)
  END;

(*    Round D up to a byte address suitable for an object of size     *)
(*    S.  If fullword objects must be aligned with fullword boun-     *)
(*    daries, this is where it happens.  This version allocates       *)
(*    bytes anywhere, larger objects on a halfword boundary.          *)

function align(d, s: integer): integer;
begin
  if s = byte_size
  then align := d
  else align := d + d mod hw_size
end;
(*#################*)
(*TYPE DECLARATIONS*)
(*#################*)

  PROCEDURE TYPE_;
  VAR TYP:ENTRY_PTR;
  BEGIN
    PUSH_OLD_ENTRY(TYP);
    IF TYP=UENTRY THEN STACK[T]:=UTYPE;
  END;

  PROCEDURE ENUM_DEF;
  VAR ENUM_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(ENUM_ENTRY);
    WITH ENUM_ENTRY^ DO BEGIN
      CLASS:=TEMPLATE;
      NOUN:=N; SIZE:=hw_size; INHERITANCE:=0;
      KIND:=ENUM_KIND;
      MIN:=0; READ_IFL(MAX);
      IF MAX>enum_max THEN ERROR(ENUM2_ERROR)
    END;
    IF MODE=UNDEF2_MODE THEN ERROR(ENUM1_ERROR)
  END;

  PROCEDURE SUBR_DEF;
  VAR SUBR_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(SUBR_ENTRY);
    WITH SUBR_ENTRY^ DO BEGIN
      CLASS:=TEMPLATE;
      READ_IFL(NOUN); INHERITANCE:=0;
      if noun = xundef
      then
        begin
          kind := enum_kind;
          size := hw_size
        end
      else
        begin
          kind := noun_table[noun]^.kind;
          size := noun_table[noun]^.size
        end;
      READ_IFL(MIN); READ_IFL(MAX);
    END
  END;

  PROCEDURE MEMBER_CHECK;
  BEGIN
    WITH STACK[T]^ DO
      IF KIND IN INDEXS THEN
        IF (MIN<SET_MIN) OR (MAX>SET_MAX) THEN ERROR(MEMBER_ERROR)
        ELSE (*OK*)
      ELSE ERROR(MEMBER_ERROR)
  END;

  PROCEDURE SET_DEF;
  VAR SET_NOUN:NOUN_INDEX; SET_ENTRY:ENTRY_PTR;
  BEGIN
    MEMBER_CHECK;
    SET_NOUN:=STACK[T]^.NOUN;
    T:=T-1 (*POP MEMBER TYPE*);
    PUSH_NEW_ENTRY(SET_ENTRY);
    WITH SET_ENTRY^ DO BEGIN
      CLASS:=TEMPLATE;
      NOUN:=SET_NOUN;
      SIZE:=SETLENGTH; INHERITANCE:=0;
      KIND:=SET_KIND
    END
  END;

  PROCEDURE ARRAY_DEF;
  VAR SPAN,ARRAY_SIZE:DISPLACEMENT; ARRAY_KIND:TYPE_KIND;
    ARRAY_INHERITANCE:PACKED_SET; ARRAY_ENTRY,base_pointer:ENTRY_PTR;
  BEGIN
    WITH STACK[T-1]^ DO
      IF KIND IN INDEXS THEN SPAN:=ADD(SUBTRACT(MAX,MIN),1)
      ELSE BEGIN
        SPAN:=1; ERROR(INDEX_ERROR)
      END;
    base_pointer:=stack[t]; (* save the base type for later *)
    WITH STACK[T]^ DO BEGIN
      IF KIND=CHAR_KIND THEN BEGIN
        IF SPAN MOD hw_size <>0 THEN BEGIN
          ERROR(STRING_ERROR);
          SPAN:=hw_size
        END;
        ARRAY_KIND:=STRING_KIND;
        ARRAY_SIZE:=SPAN
      END ELSE BEGIN
        IF KIND IN PASSIVES THEN ARRAY_KIND:=PASSIVE_KIND
        ELSE ARRAY_KIND:=ACTIVE_KIND;
        ARRAY_SIZE:=MULTIPLY(SPAN,SIZE)
      END;
      ARRAY_INHERITANCE:=INHERITANCE
    END;
    T:=T-2 (*POP INDEX AND ELEMENT TYPES*);
    PUSH_NEW_ENTRY(ARRAY_ENTRY);
    WITH ARRAY_ENTRY^ DO BEGIN
      CLASS:=TEMPLATE;
      NOUN:=N; SIZE:=align(ARRAY_SIZE, hw_size);
      INHERITANCE:=ARRAY_INHERITANCE;
      KIND:=ARRAY_KIND;
      base_entry:=base_pointer
    END
  END;

  PROCEDURE FIELDLIST;
  VAR THIS_SIZE:DISPLACEMENT; INHERITED:LEGACYS; NUMBER,I:INTEGER;
  BEGIN
    WITH STACK[T]^ DO BEGIN
      UNPACK(INHERITANCE,INHERITED);
      RECORD_INHERITANCE:=RECORD_INHERITANCE + INHERITED;
      THIS_SIZE:=SIZE
    END;
    READ_IFL(NUMBER);
    FOR I:=NUMBER DOWNTO 1 DO (*ASSIGN ADDRESSES IN FORWARD DIRECTION*)
      WITH STACK[T-I]^ DO BEGIN
        CLASS:=VALUE; VMODE:=MODE; CONTEXT:=FIELD;
        vdisp := align(current_disp, this_size);
        current_disp := add(vdisp, this_size)
      END;
    T:=T-NUMBER-1 (*POP DECLARATION LIST*)
  END;

  PROCEDURE REC_DEF;
  VAR REC_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(REC_ENTRY);
    WITH REC_ENTRY^ DO BEGIN
      CLASS:=TEMPLATE;
      NOUN:=N; SIZE:=align(current_disp, hw_size);
      PACK(INHERITANCE,RECORD_INHERITANCE);
      IF INHERITANCE=0 THEN KIND:=PASSIVE_KIND
      ELSE KIND:=ACTIVE_KIND;
      base_entry:=nil
    END;
    POP_LEVEL
  END;

  PROCEDURE ROUTINE_DEF (RESOLVE: BOOLEAN);  FORWARD;

  PROCEDURE COMP_DEF(LEGACY:PACKED_SET);
  VAR COMP_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(COMP_ENTRY);
    WITH COMP_ENTRY^ DO BEGIN
      CLASS:=TEMPLATE;
      NOUN:=N; INHERITANCE:=LEGACY;
      KIND:=SYSCOMP_KIND;
      SMODE:=MODE;
      initstat := peek_ifl;
    END;
    ROUTINE_DEF(DONT_RESOLVE) (*INITIAL STATEMENT*)
  END;

  PROCEDURE STACK_;
  BEGIN
    IF STACK[T-1]^.SMODE<>PROCESS2_MODE THEN ERROR(STACK_ERROR);
    READ_IFL(STACK[T]^.STACK_SIZE)
  END;
(*#####################*)
(*VARIABLE DECLARATIONS*)
(*#####################*)

  PROCEDURE VAR_LIST;
  VAR NUMBER,I:INTEGER; THIS_SIZE:DISPLACEMENT; INHERITED:LEGACYS;
  BEGIN
    WITH STACK[T]^ DO BEGIN
      IF KIND IN ACTIVES THEN BEGIN (*CHECK RULES*)
        IF MODE IN NONCOMPS THEN ERROR(ACTIVE_ERROR);
        UNPACK(INHERITANCE,INHERITED);
        IF QUEUE_LEGACY IN INHERITED THEN
          IF MODE<>MONITOR2_MODE THEN ERROR(QUEUE_ERROR);
        IF KIND=SYSCOMP_KIND THEN
          IF SMODE=PROCESS2_MODE THEN
            IF THIS_LEVEL<>INITIAL_LEVEL THEN ERROR(PROCESS_ERROR)
      END;
      THIS_SIZE:=SIZE
    END;
    READ_IFL(NUMBER);
    FOR I:=NUMBER DOWNTO 1 DO (*ASSIGN ADDRESSES IN FORWARD DIRECTION*)
      WITH STACK[T-I]^ DO BEGIN
        CLASS:=VALUE; VMODE:=MODE; CONTEXT:=VARIABLE;
        current_disp := align(current_disp, this_size);
        vdisp := add(current_disp, base_size);
        current_disp := add(current_disp, this_size)
      END;
    T:=T-NUMBER-1 (*POP DECLARATION LIST*)
  END;

  PROCEDURE EVAR_LIST;
  BEGIN
    WITH STACK[T]^ DO
      IF (KIND IN ACTIVES) OR (MODE<>CLASS2_MODE) THEN ERROR(ENTRY_ERROR);
    VAR_LIST
  END;
(*####################*)
(*ROUTINE DECLARATIONS*)
(*####################*)

  PROCEDURE ROUTINE_DEF(* resolve: boolean *);
  VAR ROUTINE_ENTRY:ENTRY_PTR;
  BEGIN
    IF RESOLVE THEN BEGIN
      PUSH_OLD_ENTRY(ROUTINE_ENTRY);
      WITH ROUTINE_ENTRY^ DO BEGIN
        PARM_SIZE:=CURRENT_DISP;
        VAR_SIZE:= 0;
        STACK_SIZE:=0; RMODE:=MODE
      END
    END ELSE BEGIN
      PUSH_NEW_ENTRY(ROUTINE_ENTRY);
      WITH ROUTINE_ENTRY^ DO BEGIN
        CLASS:=ROUTINE;
        PARM_SIZE:=CURRENT_DISP;
        STACK_SIZE:=0; RMODE:=MODE;
        CURRENT_LABEL:=CURRENT_LABEL+1; RDISP:=CURRENT_LABEL
      END
    END;
  END;

  PROCEDURE FUNC_DEF(RESOLVE:BOOLEAN);
  VAR FUNC_TYPE:ENTRY_PTR;
  BEGIN
    TYPE_;
    IF NOT(STACK[T]^.KIND IN FUNC_TYPES) THEN ERROR(FUNCTYPE_ERROR);
    T:=T-1 (*POP FUNC TYPE*);
    ROUTINE_DEF(RESOLVE)
  END;

  PROCEDURE INITS_DEF;
  BEGIN
    INITIAL_ENTRY:=TRUE;
    (*TOP OF STACK IS INITIAL STATEMENT ENTRY; SECOND IS COMPONENT ENTRY*)
    WITH STACK[T-1]^ DO
      IF SMODE=PROCESS2_MODE THEN BEGIN
        SIZE:=fw_size (*CENTER*); OFFSET:=0
      END ELSE BEGIN
        current_disp := align(current_disp, fw_size);
        size := current_disp (* parms & vars *) + fw_size (* center *) ;
        offset := current_disp - stack[t]^.parm_size (* vars only *)
      END;
  END;

  PROCEDURE PROG_DEF;
  BEGIN
    ROUTINE_DEF(DONT_RESOLVE);
    POP_LEVEL;
    T:=T-1
  END;

  PROCEDURE FWD_DEF;
  VAR ROUTINE_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(ROUTINE_ENTRY);
    WITH ROUTINE_ENTRY^ DO BEGIN
      CLASS:=ROUTINE;
      CURRENT_LABEL:=CURRENT_LABEL+1; RDISP:=CURRENT_LABEL
    END;
    T:=T-1
  END;

  PROCEDURE PSTART;
  BEGIN
    READ_IFL(CHK_MODE); PUSH_LEVEL(CHK_MODE);
    PARM_NUMBER:=0
  END;

  PROCEDURE PEND;
  VAR VSIZE:DISPLACEMENT; I, temp:INTEGER;
  BEGIN
    current_disp := 0;
    for i := parm_number downto 1 do
      with stack[t-i+1]^ do
        begin
          vsize := vdisp;  vmode := mode;
          if vsize < fw_size
          then temp := fw_size
          else temp := vsize;
          current_disp := add(current_disp, temp);
          vdisp := add(subtract(current_disp, vsize), base_size)
        end;
    T:=T-PARM_NUMBER (*POP PARMS*);
  END;

  PROCEDURE PARM_CHECK;
  VAR INHERIT:LEGACYS;
  BEGIN
    WITH STACK[T]^ DO  (*APPLY CHECKS*)
      CASE CHK_MODE OF
        MONITOR1_MODE,PROCESS1_MODE,CLASS1_MODE:
          IF NOT(KIND IN SMALLS) THEN
            IF KIND=SYSCOMP_KIND THEN
              IF SMODE=MONITOR2_MODE THEN (*OK*)
              ELSE IF (SMODE=CLASS2_MODE) AND
                (CHK_MODE=CLASS1_MODE) THEN (*OK*)
                ELSE ERROR(PARM1_ERROR)
            ELSE if ((kind=active_kind) or 
 	             (kind=passive_kind)) and
                    (base_entry<>nil) then begin
		      push_entry(base_entry);
		      parm_check;
		      t := t-1
		    end
		  else ERROR(PARM2_ERROR);
        PROC1_MODE,FUNC1_MODE: ;
        PROCE1_MODE,FUNCE1_MODE: BEGIN
          UNPACK(INHERITANCE,INHERIT);
          IF QUEUE_LEGACY IN INHERIT THEN ERROR(PARM4_ERROR)
        END;
        PROGRAM1_MODE:
          IF KIND IN ACTIVES THEN ERROR(PARM5_ERROR)
      END
  END;

  PROCEDURE PARM_TYPE;
  BEGIN
    TYPE_;
    PARM_CHECK
  END;

  PROCEDURE UNIV_TYPE;
  BEGIN
    TYPE_;
    IF STACK[T]^.KIND IN ACTIVES THEN ERROR(PARM6_ERROR);
    UNIVERSAL:=TRUE;
    PARM_CHECK
  END;

  PROCEDURE PARMLIST(C:CONTEXT_KIND);
  VAR I,NUMBER:INTEGER; THIS_SIZE:DISPLACEMENT;
  BEGIN
    READ_IFL(NUMBER); PARM_NUMBER:=PARM_NUMBER+NUMBER;
    WITH STACK[T]^ DO
      IF (C IN PASS_BY_REFERENCE) OR (KIND IN LARGES)
        THEN THIS_SIZE:=fw_size ELSE THIS_SIZE:=SIZE;
    FOR I:=1 TO NUMBER DO
      WITH STACK[T-I]^ DO BEGIN
        CLASS:=VALUE; VDISP:=THIS_SIZE;
        CONTEXT:=C
      END;
    T:=T-1 (*POP TYPE*)
  END;

  PROCEDURE CPARM_LIST;
  VAR C:CONTEXT_KIND;
  BEGIN
    IF UNIVERSAL THEN BEGIN
      C:=UNIV_CONST; UNIVERSAL:=FALSE
    END ELSE C:=CONST_PARM;
    PARMLIST(C)
  END;

  PROCEDURE VPARMLIST;
  VAR C:CONTEXT_KIND;
  BEGIN
    IF CHK_MODE IN NONVARPARMS THEN ERROR(PARM7_ERROR);
    IF UNIVERSAL THEN BEGIN
      C:=UNIV_VAR; UNIVERSAL:=FALSE
    END ELSE C:=VAR_PARM;
    PARMLIST(C)
  END;
(*####*)
(*BODY*)
(*####*)

  PROCEDURE BODY;
  BEGIN
    current_disp := align(current_disp, fw_size);
    WITH STACK[T]^ DO BEGIN
      VAR_SIZE:=CURRENT_DISP - parm_size;
      IF INITIAL_ENTRY THEN BEGIN
        INITIAL_ENTRY:=FALSE;
        COMPVAR_LENGTH:=var_size (*SAVE LENGTH OF COMPONENT VARIABLES*);
        CURRENT_DISP:=0 (*INITIAL STATEMENT IS parm- & VARIABLE-LESS*);
        PUT5(BODY2,RMODE,RDISP,0,0,STACK_SIZE)
      END ELSE
        PUT5(BODY2,RMODE,RDISP,PARM_SIZE,VAR_SIZE,STACK_SIZE)
    END
  END;

  PROCEDURE BODY_END;
  BEGIN
    PUT0(BODY_END2);
    T:=T-1;
    POP_LEVEL
  END;
(*##########*)
(*STATEMENTS*)
(*##########*)

  PROCEDURE PUT_TYPE;
  VAR N:NOUN_INDEX; LENGTH:DISPLACEMENT;
  BEGIN
    READ_IFL(N);
    WITH NOUN_TABLE[N]^ DO
      IF CLASS=TEMPLATE THEN BEGIN
        IF KIND=SYSCOMP_KIND THEN LENGTH:=OFFSET ELSE LENGTH:=SIZE;
        PUT3_ARG(KIND,NOUN,LENGTH)
      END ELSE PUT3_ARG(UNDEF_KIND,XUNDEF,1)
  END;

  PROCEDURE RESULT;
  BEGIN
    put1(result2, 0);
    PUT_TYPE
  END;

  PROCEDURE INTF_ID;
  VAR N:NOUN_INDEX;
  BEGIN
    READ_IFL(N);
    INTF_LENGTH:=INTF_LENGTH+fw_size;
    PUT1(INTF_LBL2,NOUN_TABLE[N]^.RDISP)
  END;

  PROCEDURE PARM;
  VAR PARM_NOUN:NOUN_INDEX; OP:INTEGER;  PARM_CONTEXT:CONTEXT_KIND;
  BEGIN
    READ_IFL(PARM_NOUN);
    IF PARM_NOUN<>XUNDEF THEN
    WITH NOUN_TABLE[PARM_NOUN]^ DO BEGIN
      PARM_CONTEXT:= CONTEXT;
      CASE PARM_CONTEXT OF
        VAR_PARM,UNIV_VAR: OP:=VARPARM2;
        CONST_PARM,UNIV_CONST: OP:=CONSTPARM2;
        SAVE_PARM: BEGIN GENERIC_FUNCTION:=TRUE; OP:=SAVEPARM2 END
      END;
      PUT3(OP,VMODE,VDISP,CONTEXT)
    END
   ELSE PUT3(CONSTPARM2,UNDEF2_MODE,0,CONST_PARM);
    TYPE_;
    WITH STACK[T]^ DO BEGIN
      PUT3_ARG(KIND,NOUN,SIZE);
      IF PARM_CONTEXT = CONST_PARM THEN
        IF KIND IN INDEXS THEN
          IF N (*TYPE NOUN*) <> XINTEGER THEN PUT2(RANGE2,MIN,MAX)
    END;
    T:=T-1
  END;

  PROCEDURE FOR_LIM;
  VAR ARG1,ARG2,ARG4:INTEGER;
  BEGIN
    READ_IFL(ARG1); READ_IFL(ARG2); READ_IFL(ARG4);
    put4(for_lim2, arg1, add(current_disp, base_size), arg2, arg4);
    current_disp := add(current_disp, fw_size)
  END;

  PROCEDURE FOR_LOOP(OP:INTEGER);
  BEGIN
    CURRENT_DISP:=CURRENT_DISP-fw_size;
    IGNORE2(OP)
  END;

  PROCEDURE WITH_TEMP;
  VAR WITH_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(WITH_ENTRY);
    WITH WITH_ENTRY^ DO BEGIN
      CLASS:=VALUE; VMODE:=PROC2_MODE (*ALL TEMPS HAVE PROCEDURE MODE*);
      vdisp := add(current_disp, base_size);
      current_disp := add(current_disp, fw_size);
      IF WITH_CONTEXT IN ASSIGNABLE THEN CONTEXT:= WITH_VAR
        ELSE CONTEXT:= WITH_CONST
    END;
    T:=T-1;
    PUT0(ADDRESS2)
  END;

  PROCEDURE WITH_;
  BEGIN
    CURRENT_DISP:=CURRENT_DISP-fw_size;
    PUT0(WITH2)
  END;
(*################*)
(*VALUE OR ROUTINE*)
(*################*)
  
  PROCEDURE FUNCTION_;
  BEGIN
    PUT0(FUNCTION2);
    PUT_TYPE
  END;
  
  PROCEDURE CALL_FUNC;
  BEGIN
    IF GENERIC_FUNCTION THEN BEGIN
      PUT0(CALL_GEN2);
      GENERIC_FUNCTION:= FALSE
    END ELSE PUT0(CALL_FUNC2)
  END;
  
  PROCEDURE INDEX;
  VAR VALUE:INTEGER;
  BEGIN
    READ_IFL(VALUE);
    PUT3(VAR2,SCONST2_MODE,VALUE,CONSTANT);
    PUT_TYPE
  END;

  PROCEDURE REAL_;
  VAR DISP:DISPLACEMENT;
  BEGIN
    READ_IFL(DISP);
    PUT3(VAR2,LCONST2_MODE,DISP,CONSTANT);
    PUT3_ARG(REAL_KIND,XREAL,REALLENGTH)
  END;

  PROCEDURE STRING;
  VAR LENGTH:INTEGER;  DISP:DISPLACEMENT;
  BEGIN
    READ_IFL(LENGTH);  READ_IFL(DISP);
    PUT3(VAR2,LCONST2_MODE,DISP,CONSTANT);
    PUT3_ARG(STRING_KIND,LENGTH,LENGTH)
  END;

  PROCEDURE RCOMP(OP:INTEGER);
  VAR N:NOUN_INDEX;
  BEGIN
    READ_IFL(N);
    WITH NOUN_TABLE[N]^ DO
      IF CLASS=ROUTINE THEN
        PUT5(OP,RMODE,RDISP,PARM_SIZE,VAR_SIZE,STACK_SIZE)
      ELSE PUT0(UNDEF2)
  END;

  PROCEDURE VCOMP(OP:INTEGER);
  VAR N:NOUN_INDEX;
  BEGIN
    READ_IFL(N);
    WITH NOUN_TABLE[N]^ DO BEGIN
      PUT3(OP,VMODE,VDISP,CONTEXT); PUT_TYPE;
      IF SAVE_CONTEXT THEN BEGIN
        WITH_CONTEXT:=CONTEXT; SAVE_CONTEXT:=FALSE
      END
    END
  END;

  PROCEDURE ARROW;
  BEGIN
    PUT0(ARROW2); PUT_TYPE
  END;

  PROCEDURE SUB;
  VAR N:NOUN_INDEX; INDEX,ELEMENT:ENTRY_PTR;
    LENGTH:DISPLACEMENT;
  BEGIN
    (*INDEX*) TYPE_; INDEX:=STACK[T]; T:=T-1;
    (*ELEMENT*) TYPE_; ELEMENT:=STACK[T]; T:=T-1;
    length := element^.size;
    WITH INDEX^ DO BEGIN
      IF KIND IN INDEXS THEN PUT3(SUB2,MIN,MAX,LENGTH) ELSE PUT3(SUB2,0,0,1);
      PUT3_ARG(KIND,NOUN,SIZE);
    END;
    WITH ELEMENT^ DO BEGIN
      IF KIND=SYSCOMP_KIND THEN LENGTH:=OFFSET;
      PUT3_ARG(KIND,NOUN,LENGTH)
    END
  END;
(*#########*)
(*MAIN LOOP*)
(*#########*)

BEGIN
 INITIALIZE;
 REPEAT READ_IFL(SY); CASE SY OF

 ADDRESS1: PUT0(ADDRESS2);
 AND1: PUT0(AND2);
 ARRAY_DEF1: ARRAY_DEF;
 ARROW1: ARROW;
 BODY_END1: BODY_END;
 BODY1: BODY;
 CALL_FUNC1: CALL_FUNC;
 CALL_PROC1: PUT0(CALL_PROC2);
 CALL_PROG1: PUT1(CALL_PROG2,INTF_LENGTH);
 CASE_JUMP1: IGNORE1(CASE_JUMP2);
 CASE_LIST1: CASE_LIST;
 CHK_TYPE1: BEGIN PUT0(CHK_TYPE2); PUT_TYPE END;
 CLASS1: COMP_DEF(PACKED_CLASS);
 CPARMLIST1: CPARM_LIST;
 DEF_LABEL1: IGNORE1(DEF_LABEL2);
 DIV1: PUT0(DIV2);
 EMPTY_SET1: PUT0(EMPTY_SET2);
 EOM1: EOM;
 ENUM_DEF1: ENUM_DEF;
 EQ1: PUT0(EQ2);
 EVAR_LIST1: EVAR_LIST;
 FALSEJUMP1: IGNORE1(FALSEJUMP2);
 FIELDLIST1: FIELDLIST;
 FOR_DOWN1: FOR_LOOP(FOR_DOWN2);
 FOR_LIM1: FOR_LIM;
 FOR_STORE1: PUT0(FOR_STORE2);
 FOR_UP1: FOR_LOOP(FOR_UP2);
 FUNC_DEF1,FUNCE_DEF1: FUNC_DEF(DONT_RESOLVE);
 FUNCF_DEF1: FUNC_DEF(RESOLVE);
 FUNCTION1: FUNCTION_;
 FWD_DEF1: FWD_DEF;
 GE1: PUT0(GE2);
 GT1: PUT0(GT2);
 INCLUDE1: PUT0(INCLUDE2);
 INDEX1: INDEX;
 INITS_DEF1: INITS_DEF;
 INIT1: PUT0(INIT2);
 INTF_ID1: INTF_ID;
 INTF1: INTF_LENGTH:=0;
 IN1: PUT0(IN2);
 JUMP_DEF1: IGNORE2(JUMP_DEF2);
 JUMP1: IGNORE1(JUMP2);
 LCONST1: LCONST;
 LE1: PUT0(LE2);
 LT1: PUT0(LT2);
 MESSAGE1: messpass;
 MINUS1: PUT0(MINUS2);
 MOD1: PUT0(MOD2);
 MONITOR1: COMP_DEF(PACKED_MONITOR);
 NEW_LINE1: begin read_ifl(line_no); put1(NEW_LINE2, line_no) end;
 NEW_NOUN1: PUSH_NEW_ENTRY(NEW_ENTRY);
 NE1: PUT0(NE2);
 NOT1: PUT0(NOT2);
 OR1: PUT0(OR2);
 PARM_TYPE1: PARM_TYPE;
 PARM1: PARM;
 PEND1: PEND;
 PLUS1: PUT0(PLUS2);
 PROC_DEF1,PROCE_DEF1: ROUTINE_DEF(DONT_RESOLVE);
 PROCF_DEF1: ROUTINE_DEF(RESOLVE);
 PROCESS1: COMP_DEF(PACKED_PROCESS);
 PROG_DEF1: PROG_DEF;
 PSTART1: PSTART;
 RCOMP1: RCOMP(RCOMP2);
 REAL1: REAL_;
 REC_DEF1: REC_DEF;
 REC1: PUSH_LEVEL(RECORD_MODE);
 RESULT1: RESULT;
 ROUTINE1: RCOMP(ROUTINE2);
 SET_DEF1: SET_DEF;
 SLASH1: PUT0(SLASH2);
 STACK1: STACK_;
 STAR1: PUT0(STAR2);
 STORE1: PUT0(STORE2);
 STRING1: STRING;
 SUBR_DEF1: SUBR_DEF;
 SUB1: SUB;
 TYPE_DEF1: T:=T-1;
 TYPE1: TYPE_;
 UMINUS1: PUT0(UMINUS2);
 UNDEF1: PUT0(UNDEF2);
 UNIV_TYPE1: UNIV_TYPE;
 UPLUS1: PUT0(UPLUS2);
 VALUE1: PUT0(VALUE2);
 VAR_LIST1: VAR_LIST;
 VAR1: VCOMP(VAR2);
 VCOMP1: VCOMP(VCOMP2);
 VPARMLIST1: VPARMLIST;
 WITH_TEMP1: WITH_TEMP;
 WITH_VAR1: SAVE_CONTEXT:=TRUE;
 WITH1: WITH_
 END

 UNTIL DONE;
  with inter_pass_ptr^ do
  if summaryoption in options then
    begin
	stacksum:= (100 * stacksum) div stack_max;
	message('stack_max:',stacksum:3,'% ');
	levelsum:= (100 * levelsum) div max_level;
	message('max_level:',levelsum:3,'% ');
    end;
 wrxdisps; (* write out the displacements for all the nouns *)
 break(list);
 with inter_pass_ptr^ do begin
   dispose(RESETPOINT);  new(resetpoint);
 end;
 NEXTPASS(this_pass)
END   (* cpas4e *);

begin end.
 ]Fg?