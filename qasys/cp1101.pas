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
: **  File Name          : cp1101.pas                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     Cp1101.pas contains the code for the first pass of the   **
: **     concurrent pascal compiler.  This pass is responsible    **
: **     for lexical analysis.                                    **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.01 03/30/79 DEG  1162     End last line of listing with   **
: **                              eol                             **
: ** 01.01 08/28/79 DEG  1162     Increased constant max_integer  **
: **                              to reflect the allocation of    **
: **                              fullwords for integer variables **
: ** 01.01 09/06/79 DEG  1162     Rewrote procedure number to use **
: **                              fixed-point accumulator         **
: ** 01.01 09/10/79 DEG  1162     Added eject feature             **
: ** 01.01 09/13/79 DEG  1162     Increased big_set from 128 to   **
: **                              256 elements                    **
: ** 01.01 09/13/79 DEG  1162     Increased set types from 128 to **
: **                              256 elements                    **
: ** 01.01 09/13/79 DEG  1162     Increased type char from 128 to **
: **                              256 elements                    **
: ** 01.01 09/17/79 DEG  1162     Changed definition and handling **
: **                              of string_type to accommodate   **
: **                              8-bit characters                **
: ** 01.01 12/27/79 RNP  1162     Increased the hash table to     **
: **                              1500 entries                    **
: ** 01.01 04/21/80 SLC  1162     Increased max_index to 2000     **
: ** 01.01 04/21/80 SLC  1162     Increased hash_max to 3000      **
: ** 01.01 04/21/80 SLC  1162     Display of statistics, abort on **
: **                              overflow                        **
: ** 01.01 03/10/81 MDS  1162     Increased max_index to 2500     **
: ** 01.01 03/10/81 MDS  1162     Increased hash_max to 4000      **
: ** 01.01 03/18/81 BH   1162     Added mixed case character      **
: **                              strings and listing output      **
: ** 01.01 04/24/81 BH   1162     Made line numbers five digits   **
: ** 01.01 09/30/81 BH   1162     Increased max_index to 300      **
: ** 01.01 09/05/85 PJH  1162     Increased max_index to 6000     **
: ** 01.02 01/20/86 PJH  1162     Increased max_index to 8000     **
: ** 01.02 01/20/86 PJH  1162     Increased hash_max to 8000      **
: ** 01.02 01/20/86 PJH  1162     Increased hash_max1 to 8009     **
: ** 01.02 12/15/86 PJH  1162     Increased hash_max to 8512      **
: ** 01.02 12/15/86 PJH  1162     Increased hash_max1 to 8513     **
: ** 01.02 12/15/86 PJH  1162     Increased max_index to 8512     **
: ** 01.02 12/15/86 PJH  1162     Made line numbers 7 digits long **
: ** 01.02 12/15/86 PJH  1162     ADDED PROPRIETARY BANNER        **
: **                                                              **
: *****************************************************************)

program cp1e, cpas1e;

(*###########
#  PREFIX  #
###########*)
const
  eol = lf;
  eom = em;
WORDLENGTH = 2 (*BYTES*);
SETLENGTH = 32 (*BYTES*);
MAX_STRING_LENGTH = 80 (*CHARS*);
LISTOPTION = 0;    SUMMARYOPTION = 1;  TESTOPTION = 2;     CHECKOPTION = 3;
CODEOPTION = 4;    NUMBEROPTION = 5;   XREFOPTION = 6;     BSTACKOPTION = 7;
DUMPOPTION = 9;    LGXREFOPTION = 8;
IDLENGTH = 12;
big_set_limit = 3;

(* hash table constants *)

id_piece_length = 9;    (* piece upper bound *)
max_pieces = 13;        (* 14 pieces => 140 chars *)
hash_max = 8512;        (* hash table upper bound *)
hash_max1 = 8513;       (* prime length of hash table *)
max_index = 8512;       (* max hash table loading *)
null = 32767;           (* the null spelling index *)

TYPE IDENTIFIER = packed ARRAY [1..IDLENGTH] OF CHAR;
    big_set_index = 0 .. big_set_limit;
    big_set = array[big_set_index] of set of 0..63;
    big_set_element = 0 .. 255;
    int_file = file of integer;
    pass_range = 0 .. 7;

(* hash table types *)

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
(* inter-pass communication types *)

     POINTER = ^ INTEGER;
     OPTION = LISTOPTION..DUMPOPTION;
     PASSPTR = ^PASSLINK;
    PASSLINK =
     RECORD
     OPTIONS: SET OF OPTION;
     LABELS, BLOCKS, CONSTANTS: INTEGER;
     resetpoint: pointer;
     TABLES: spell_POINTER
     END;

(*************)
(* externals *)
(*************)

procedure initpass(p: pass_range);  extern;

procedure printeol(var f: text);  extern;

procedure nextpass(p: pass_range);  extern;

procedure printarg(var f: text;  arg: integer);  extern;

procedure rdchar(var f: text;  var c: ascii);  extern;

procedure setdifference(var s1: big_set;  s2: big_set);  extern;

procedure setinitialize(var s: big_set);  extern;

procedure setinsert(var s: big_set;  e: big_set_element);  extern;

function setmember(s: big_set;  e: big_set_element): boolean;  extern;

procedure setunion(var s1: big_set;  s2: big_set);  extern;

procedure wrchar(var f: text;  c: ascii);  extern;

procedure giverr(line, pass, number: integer); extern;

procedure cpas1e(var inter_pass_ptr: passptr;
		 var source, list: text;
		 var ilout: int_file;
		 var xrefile: int_file);

CONST

(*OUTPUT OPERATORS*)

EOM2=0;            BEGIN2=1;           IF2=2;              CASE2=3;
WHILE2=4;          REPEAT2=5;          FOR2=6;             CYCLE2=7;
WITH2=8;           INIT2=9;            ID2=10;             REAL2=11;
STRING2=12;        INTEGER2=13;        CHAR2=14;           OPEN2=15;
NOT2=16;           SUB2=17;            SET2=18;            ARRAY2=19;
RECORD2=20;        CLASS2=21;          MONITOR2=22;        PROCESS2=23;
PERIOD2=24;        STAR2=25;           SLASH2=26;          DIV2=27;
MOD2=28;           AND2=29;            PLUS2=30;           MINUS2=31;
OR2=32;            EQ2=33;             NE2=34;             LE2=35;
GE2=36;            LT2=37;             GT2=38;             IN2=39;
CONST2=40;         TYPE2=41;           VAR2=42;            PROCEDURE2=43;
FUNCTION2=44;      PROGRAM2=45;        SEMICOLON2=46;      CLOSE2=47;
UP_TO2=48;         OF2=49;             COMMA2=50;          BUS2=51;
COLON2=52;         END2=53;            ENTRY2=54;          UNIV2=55;
BECOMES2=56;       THEN2=57;           ELSE2=58;           DO2=59;
UNTIL2=60;         TO2=61;             DOWNTO2=62;         LCONST2=63;
MESSAGE2=64;       NEW_LINE2=65;

(*OTHER CONSTANTS*)

(*ERRORS*)

COMMENT_ERROR=1;   NUMBER_ERROR=2;     INSERT_ERROR=3;     STRING_ERROR=4;
CHAR_ERROR=5;

(*STANDARD SPELLING/NOUN INDICES*)

XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XQUEUE=6;           XABS=7;
XATTRIBUTE=8;      XCHR=9;             XCONTINUE=10;       XCONV=11;
XDELAY=12;         XEMPTY=13;          XIO=14;             XORD=15;
XPRED=16;          XSTOP=17;           XREALTIME=18;       XSETHEAP=19;
XSUCC=20;          XTRUNC=21;          XSTART=22;          XWAIT=23;
XREAL=24;

TEST_MAX = 50;
THIS_PASS=1;       SPAN=26 (*NUMBER OF DISTINCT ID CHARS*);

MIN_ORD=0;         MAX_ORD=255;

TYPE

  full_ascii = 0 .. 255;

  string_type = packed array[1..max_string_length] of full_ascii;

VAR
  CH:Ascii;
  cur_col: integer;
  missing_spaces: integer;

  LETTERS, DIGITS, ALFAMERICS, NON_ALFAS, STRING_SPECIAL:
  big_set;

  TEST, UPTO_SW, BUS_SW, END_SCAN: BOOLEAN;

  CL1,CL2,CL3,CL4,CL5,cl6,cl7 (*LINE NUMBER*): CHAR;

  LINE_NO:INTEGER;

  PIECES: INTEGER       (*ID LENGTH IN PIECES*);

  TEST_BUF: ARRAY [1..TEST_MAX] OF INTEGER;

  TEST_INDEX: INTEGER;

  ID_TEXT: ARRAY[0..MAX_PIECES] OF PIECE;

  BLANK: PIECE (*BLANK PADDING*);

  CHAR_INDEX:0..ID_PIECE_LENGTH (*CURRENT CHAR INDEX*);

  SYMB: INTEGER (*ID SYMBOL*);

  STRING_LENGTH:INTEGER;

  HASH_KEY: hash_index; (*INDEX TO HASH_TABLE*)
  HASH_KEY_LIMIT : INTEGER; (* CHECK ON HASH_TABLE OVERFLOW *)

  sum_max,
  CURRENT_INDEX  (*LAST ASSIGNED INDEX*),
  INDEX  (*LAST SCANNED INDEX*)  : SPELLING_INDEX;

  STRING_TEXT: string_type;

  hash_ptr: hash_pointer;
  spell_ptr: spell_pointer;

  XREF, listopt:BOOLEAN;
  
(*****************************)
(* debug and output routines *)
(*****************************)

procedure wtchar(c:ascii);
begin
  if listopt
  then wrchar(list, c);
end;

procedure next_char(var ch:ascii; uponly:boolean);
begin
  cur_col := cur_col + 1;
  if missing_spaces > 0
  then begin
    missing_spaces := missing_spaces-1;
    wtchar(' ');
    ch := ' '
  end
  else begin
    rdchar(source, ch);
    if ((ch = eol) or (ch = cr))
    then begin
      wtchar(ch);
      cur_col := 0
    end
    else if ch = ht
    then begin
      missing_spaces := 7-((cur_col-1) mod 8);
      wtchar(' ');
      ch := ' '
    end
    else if ((ord(ch)<=31) or (ord(ch)>=127)) and (ord(ch)<>0)
    then wtchar('?')
    else wtchar(ch);
    if uponly and (ord(ch)<=122) and (ord(ch)>=97)
    then ch := chr(ord(ch)-32);
  end;
end;

procedure write_ifl(i: integer);
begin
  ilout ^ := i;  put(ilout)
end;

procedure wrxref(i: integer);
begin
  xrefile^ := i; put(xrefile)
end;

PROCEDURE STORE_TEST (ARG: INTEGER);
BEGIN
  IF TEST_INDEX < TEST_MAX THEN BEGIN
    TEST_INDEX:= TEST_INDEX + 1;
    TEST_BUF[TEST_INDEX]:= ARG
  END
END;

PROCEDURE PRINT_TEST;
VAR I: INTEGER;
BEGIN
  FOR I:= 1 TO TEST_INDEX DO PRINTARG(list, TEST_BUF[I]);
  test_index:=0;
  printeol(list);
END;

  PROCEDURE PUT_ARG(ARG:INTEGER);
  BEGIN
    WRITE_IFL(ARG);
    IF TEST THEN STORE_TEST(ARG)
  END;

  PROCEDURE PUT0NC(OP:INTEGER);
  BEGIN
    WRITE_IFL(OP);
    IF TEST THEN STORE_TEST(OP);
    next_char(ch, true)
  END;

  PROCEDURE PUT0(OP:INTEGER);
  BEGIN
    WRITE_IFL(OP);
    IF TEST THEN STORE_TEST(OP)
  END;

  PROCEDURE PUT1(OP,ARG:INTEGER);
  BEGIN
    put0(op); put_arg(arg);
  END;

  PROCEDURE PUT2(OP,ARG1,ARG2:INTEGER);
  BEGIN
    put0(op); put_arg(arg1); put_arg(arg2)
  END;

  PROCEDURE PUT_STRING (STRING: string_type; STRING_LENGTH: INTEGER);
  VAR I: INTEGER;
  BEGIN
    PUT1(STRING2, STRING_LENGTH);  PUT1(LCONST2, STRING_LENGTH);
    FOR I:= 1 TO STRING_LENGTH DIV WORDLENGTH DO
      PUT_ARG(STRING[2*i-1]*256+string[2*i])
  END;

  PROCEDURE ERROR(ERROR_NUM:INTEGER);
  BEGIN
   PUT2(MESSAGE2,THIS_PASS,ERROR_NUM);
   if error_num = insert_error then
    begin
     message('max index overflow');
     halt;
    end;
    giverr(line_no, this_pass, error_num);
  END;
(*##########*)
(*INITIALIZE*)
(*##########*)

  PROCEDURE STD_ID(ID:PIECE; INDEX:SPELLING_INDEX);
  VAR S:SPELLING_INDEX; CHAR_INDEX:INTEGER;
  BEGIN
    HASH_KEY:=1;
    FOR CHAR_INDEX:=0 TO ID_PIECE_LENGTH DO
      IF ID[CHAR_INDEX]<>' ' THEN
	HASH_KEY:=HASH_KEY*(ORD(ID[CHAR_INDEX]) MOD SPAN +1) MOD HASH_MAX1;
    WHILE HASH_ptr^[HASH_KEY].SPIX<>NULL DO
      HASH_KEY:=(HASH_KEY+1) MOD HASH_MAX1;
    (*NOW WE HAVE ENTRY SLOT*)
    WITH HASH_ptr^[HASH_KEY] DO BEGIN
      SPIX:=INDEX;
      WITH NAME DO BEGIN PART:=ID; NEXT:=NIL END
    END;
    spell_ptr^.spells[abs(index)] := hash_key;
  END;

  PROCEDURE STD_NAMES;
  BEGIN
    STD_ID('END       ',-END2);
    STD_ID('IF        ',-IF2);
    STD_ID('THEN      ',-THEN2);
    STD_ID('BEGIN     ',-BEGIN2);
    STD_ID('ELSE      ',-ELSE2);
    STD_ID('DO        ',-DO2);
    STD_ID('WITH      ',-WITH2);
    STD_ID('IN        ',-IN2);
    STD_ID('OF        ',-OF2);
    STD_ID('WHILE     ',-WHILE2);
    STD_ID('CASE      ',-CASE2);
    STD_ID('REPEAT    ',-REPEAT2);
    STD_ID('UNTIL     ',-UNTIL2);
    STD_ID('PROCEDURE ',-PROCEDURE2);
    STD_ID('VAR       ',-VAR2);
    STD_ID('FOR       ',-FOR2);
    STD_ID('ARRAY     ',-ARRAY2);
    STD_ID('RECORD    ',-RECORD2);
    STD_ID('SET       ',-SET2);
    STD_ID('TO        ',-TO2);
    STD_ID('DOWNTO    ',-DOWNTO2);
    STD_ID('MOD       ',-MOD2);
    STD_ID('OR        ',-OR2);
    STD_ID('AND       ',-AND2);
    STD_ID('NOT       ',-NOT2);
    STD_ID('DIV       ',-DIV2);
    STD_ID('CONST     ',-CONST2);
    STD_ID('TYPE      ',-TYPE2);
    STD_ID('FUNCTION  ',-FUNCTION2);
    STD_ID('PROGRAM   ',-PROGRAM2);
    STD_ID('CLASS     ',-CLASS2);
    STD_ID('CYCLE     ',-CYCLE2);
    STD_ID('ENTRY     ',-ENTRY2);
    STD_ID('INIT      ',-INIT2);
    STD_ID('MONITOR   ',-MONITOR2);
    STD_ID('PROCESS   ',-PROCESS2);
    STD_ID('UNIV      ',-UNIV2);
    STD_ID('FALSE     ',XFALSE);
    STD_ID('TRUE      ',XTRUE);
    STD_ID('INTEGER   ',XINTEGER);
    STD_ID('BOOLEAN   ',XBOOLEAN);
    STD_ID('CHAR      ',XCHAR);
    STD_ID('QUEUE     ',XQUEUE);
    STD_ID('ABS       ',XABS);
    STD_ID('ATTRIBUTE ',XATTRIBUTE);
    STD_ID('CHR       ',XCHR);
    STD_ID('CONTINUE  ',XCONTINUE);
    STD_ID('CONV      ',XCONV);
    STD_ID('DELAY     ',XDELAY);
    STD_ID('EMPTY     ',XEMPTY);
    STD_ID('IO        ',XIO);
    STD_ID('ORD       ',XORD);
    STD_ID('PRED      ',XPRED);
    STD_ID('STOP      ',XSTOP);
    STD_ID('REALTIME  ',XREALTIME);
    STD_ID('SETHEAP   ',XSETHEAP);
    STD_ID('SUCC      ',XSUCC);
    STD_ID('TRUNC     ',XTRUNC);
    STD_ID('START     ',XSTART);
    STD_ID('WAIT      ',XWAIT);
    STD_ID('REAL      ',XREAL);
  END;

  PROCEDURE END_LINE;
  VAR I: INTEGER;
  BEGIN
    IF TEST THEN PRINT_TEST;
    LINE_NO:=LINE_NO+1;
    PUT1(NEW_LINE2,LINE_NO);
    if cl7<'9' then cl7:=chr(ord(cl7)+1) else begin
      cl7:='0';
      IF CL6<'9' THEN CL6:=CHR(ORD(CL6)+1) ELSE BEGIN
  	CL6:='0';
        if cl5<'9' then cl5:=chr(ord(cl5)+1) else begin
          cl5:='0';
          IF CL4<'9' THEN CL4:=CHR(ORD(CL4)+1) ELSE BEGIN
 	    CL4:='0';
	    IF CL3<'9' THEN CL3:=CHR(ORD(CL3)+1) ELSE BEGIN
	      CL3:='0';
	      IF CL2<'9' THEN CL2:=CHR(ORD(CL2)+1) ELSE BEGIN
	        CL2:='0';
	        IF CL1<'9' THEN CL1:=CHR(ORD(CL1)+1) ELSE CL1:='0'
	      end
	    end
	  END
	END
      END
    end;
    wtchar(CL1); wtchar(CL2); wtchar(CL3);
    wtchar(CL4); wtchar(CL5); wtchar(cl6);
    wtchar(cl7); wtchar(' ');
    next_char(ch, true)
  END;

  PROCEDURE GET_CHAR(SKIP_FIRST:BOOLEAN);
  BEGIN
    IF SKIP_FIRST THEN BEGIN next_char(ch, true) END;
    REPEAT
      IF CH='"' THEN BEGIN
	REPEAT
	  REPEAT next_char(ch, true)
	  UNTIL (ch=eom) or (CH=EOL) OR (CH='"');
	  WHILE CH = EOL DO END_LINE
	UNTIL (CH=EOM) OR (CH='"');
	IF CH = '"'
	THEN BEGIN next_char(ch, true) END
	ELSE ERROR(COMMENT_ERROR)
      END;
      WHILE CH = ' ' DO BEGIN next_char(ch, true) END;
      WHILE CH=EOL DO END_LINE
    UNTIL (CH<>' ') AND (CH<>'"')
  END;

  PROCEDURE INIT_OPTIONS;
  VAR STOP:big_set;
  BEGIN
    END_LINE;
    WITH INTER_PASS_PTR^ DO BEGIN
      OPTIONS:=[LISTOPTION,CHECKOPTION,NUMBEROPTION];
      GET_CHAR(FALSE);
      IF CH='(' THEN BEGIN
	setinitialize(stop);
	setinsert(stop, ord(','));
	setinsert(stop, ord(')'));
	setinsert(stop, ord(eom));
	REPEAT
	  GET_CHAR(TRUE);
	  IF CH='L' THEN OPTIONS:=OPTIONS-[LISTOPTION] ELSE
	  IF CH='S' THEN OPTIONS:=OPTIONS + [SUMMARYOPTION] ELSE
	  IF CH='T' THEN OPTIONS:=OPTIONS + [TESTOPTION] ELSE
	  IF CH='C' THEN OPTIONS:=OPTIONS-[CHECKOPTION] ELSE
	  IF CH='N' THEN OPTIONS:=OPTIONS-[NUMBEROPTION] ELSE
	  IF CH='D' THEN OPTIONS:=OPTIONS + [DUMPOPTION] ELSE
          if ch='B' then options:=options + [BSTACKOPTION] else
	  if ch='R' then options:=options + [LGXREFOPTION] + [XREFOPTION] else
	  IF CH='X' THEN OPTIONS:=OPTIONS + [XREFOPTION];
	  WHILE NOT setmember(stop, ord(ch)) DO GET_CHAR(TRUE)
	UNTIL (CH=EOM) OR (CH=')');
      IF CH=')' THEN BEGIN next_char(ch, true) END
      END;
      XREF:= XREFOPTION IN OPTIONS;
      listopt:=listoption in options;
      if not listopt
      then rewrite(list); (* zap the first line *)
      IF TESTOPTION IN OPTIONS THEN BEGIN
	TEST:=TRUE;
	TEST_INDEX:= 0  END
    END
  END;

  PROCEDURE INITIALIZE;
  VAR S:SPELLING_INDEX; C:MIN_ORD..MAX_ORD; I:INTEGER;
  BEGIN
    TEST:= FALSE; sum_max:= 0;
    listopt := true;
    rewrite(ilout);
    rewrite(xrefile);
    (*EMPTY SET*) PUT1(LCONST2, SETLENGTH);
    FOR I:=1 TO SETLENGTH DIV WORDLENGTH DO PUT_ARG(0);
    END_SCAN:=FALSE;
    LINE_NO:=0;
    CL1:='0'; CL2:='0'; CL3:='0'; CL4:='0'; CL5:='0';
    cl6:='0'; cl7:='0';
    ch := eol; cur_col := 0; missing_spaces := 0;
    UPTO_SW:=FALSE; BUS_SW:=FALSE;
    setinitialize(digits);
    for i := ord('0') to ord('9') do setinsert(digits, i);
    setinitialize(letters);
    for i := ord('A') to ord('Z') do setinsert(letters, i);
    setinsert(letters, ord('_'));
    alfamerics := letters;  setunion(alfamerics, digits);
    setinitialize(string_special);
    setinsert(string_special, ord(''''));
    setinsert(string_special, ord(eol));
    setinsert(string_special, ord(eom));
    setinsert(string_special, ord('('));
    setinitialize(non_alfas);
    FOR C:= MIN_ORD TO MAX_ORD DO setinsert(non_alfas, c);
    setdifference(non_alfas, alfamerics);
    BLANK:='          ';
    new(inter_pass_ptr);
    with inter_pass_ptr^ do
      begin
	new(resetpoint);
	new(hash_ptr);
	new(spell_ptr);
	spell_ptr^.hash_ptr := hash_ptr;
	tables := spell_ptr
      end;
    for s:=1 to max_index do spell_ptr^.spells[s] := xundef;
    FOR S:=0 TO HASH_MAX DO HASH_ptr^[S].SPIX:=NULL;
    CURRENT_INDEX:=XREAL;
    STD_NAMES;
    INIT_OPTIONS;
  END;
(*######*)
(*NUMBER*)
(*######*)

procedure number;
const
  max_int_div_10 = 214748364 (* (2^31 - 1) div 10 *) ;
var
  accum: integer;
  error_flag: boolean;
begin
  accum := 0;  error_flag := false;
  repeat
    if
      (accum < max_int_div_10) or
      (accum = max_int_div_10) and (ch < '8')
    then
      accum := 10 * accum + (ord(ch) - ord('0'))
    else
      error_flag := true;
    next_char(ch, true)
  until
    not setmember(digits, ord(ch));
  if
    error_flag
  then
    begin
      error(number_error);
      accum := 0
    end;
  put1(integer2, accum)
end (* number *) ;
(*#######*)
(*HASHING*)
(*#######*)

  FUNCTION SAME_ID:BOOLEAN;
  VAR SAME:BOOLEAN; THIS_PIECE:PIECE_PTR; I:INTEGER;
  BEGIN
    WITH HASH_ptr^[HASH_KEY] DO BEGIN
      SAME:=NAME.PART=ID_TEXT[0];
      IF PIECES>0 THEN
	IF SAME THEN BEGIN
	  THIS_PIECE:=NAME.NEXT;
	  I:=1;
	  REPEAT
	    IF THIS_PIECE=NIL THEN BEGIN
	      SAME:=FALSE (*CANDIDATE IS TOO SHORT*);
	      I:=PIECES+1 (*QUIT*)
	    END ELSE BEGIN (*COMPARE AND INCREMENT*)
	      SAME:=SAME AND (THIS_PIECE^.PART=ID_TEXT[I]);
	      THIS_PIECE:=THIS_PIECE^.NEXT;
	      I:=I+1;
	    END
	  UNTIL I>PIECES;
	  SAME:=SAME AND (THIS_PIECE=NIL)
	END;
      SAME_ID:=SAME
    END
  END;

  PROCEDURE INSERT_ID;
  VAR I:INTEGER; P,P1:PIECE_PTR;
procedure wrpiece(p:piece);
type
  convert = record
    case boolean of
      true:  (inp: array[1..2] of integer);
      false: (out: piece)
  end;
      
var
  conv: convert;

  begin
    conv.out := p;
    for i := 1 to 2 do begin
      wrxref(conv.inp[i])
    end;
  end;
    
  BEGIN
    WITH HASH_ptr^[HASH_KEY] DO BEGIN
      CURRENT_INDEX:=CURRENT_INDEX+1;
      sum_max:= current_index;
      IF CURRENT_INDEX>=MAX_INDEX THEN BEGIN
	ERROR(INSERT_ERROR); CH:=EOM; wtchar(EOL)
      END;
      SPIX:=CURRENT_INDEX;
      WITH NAME DO BEGIN PART:=ID_TEXT[0]; NEXT:=NIL END;
      IF PIECES>0 THEN BEGIN
	NEW(P); NAME.NEXT:=P; P^.PART:=ID_TEXT[1];
	FOR I:=2 TO PIECES DO BEGIN
	  NEW(P1); P^.NEXT:=P1;
	  P1^.PART:=ID_TEXT[I]; P:=P1
	END;
	P^.NEXT:=NIL
      END;
      spell_ptr^.spells[current_index] := hash_key;
      if xref then begin (* <<< *)
	wrxref(current_index);
	wrxref(pieces);
	wrpiece(name.part);
	p:=name.next;
	while p<>nil do begin
	  wrpiece(p^.part);
	  p:=p^.next
	end
      end
    END
  END;

  PROCEDURE SEARCH_ID;
  VAR FINISHED:BOOLEAN;
  BEGIN
    FINISHED:=FALSE;
    HASH_KEY_LIMIT := (HASH_KEY MOD HASH_MAX1) - 1;
    IF HASH_KEY_LIMIT < 0 THEN
       HASH_KEY_LIMIT := 0;
    REPEAT
      WITH HASH_ptr^[HASH_KEY] DO
	IF SPIX<>NULL THEN
	    IF SAME_ID THEN (*FOUND IT*) BEGIN
	      FINISHED:=TRUE;
	      IF SPIX>=0 THEN BEGIN
		SYMB:=ID2; INDEX:=SPIX
	      END ELSE SYMB:=ABS(SPIX)
	    END ELSE HASH_KEY:=(HASH_KEY+1) MOD HASH_MAX1
	ELSE (*SYM=NULL*) BEGIN
	  INSERT_ID;
	  SYMB:=ID2;
	  INDEX:=CURRENT_INDEX;
	  FINISHED:=TRUE
	END;
      IF (HASH_KEY = HASH_KEY_LIMIT) THEN
        ERROR(INSERT_ERROR);
    UNTIL FINISHED (*WITH SEARCH*)
  END;
(*######*)
(*STRING*)
(*######*)

  PROCEDURE STRING_CHAR;
  BEGIN
   IF STRING_LENGTH = MAX_STRING_LENGTH THEN ERROR(STRING_ERROR)
   ELSE BEGIN
    STRING_LENGTH:=STRING_LENGTH+1;
    STRING_TEXT[STRING_LENGTH]:= ord(CH);
      next_char(ch, false)
   END
  END;

  PROCEDURE STRING;
  VAR ORD_VALUE, I: INTEGER; DONE: BOOLEAN;
  BEGIN
    STRING_LENGTH:= 0;
    next_char(ch, false); DONE:= FALSE;
    REPEAT
      WHILE NOT setmember(string_special, ord(ch)) DO STRING_CHAR;
      CASE CH OF
	'''':
	  BEGIN
	    STRING_CHAR;
	    IF CH = ''''
	    THEN BEGIN next_char(ch, false) END
	    ELSE DONE:= TRUE
	  END;
	EOL, EOM:
	  BEGIN
	    ERROR(STRING_ERROR);
	    DONE:= TRUE
	  END;
	'(':
	  BEGIN
	    STRING_CHAR;
	    IF CH = ':' THEN BEGIN
	    REPEAT next_char(ch, true)
	    UNTIL CH <> ' ';
	      ORD_VALUE:= 0;
	      IF setmember(digits, ord(ch)) THEN
		REPEAT
		  IF ORD_VALUE <= MAX_ORD THEN
		    ORD_VALUE:= ORD_VALUE * 10 + (ORD(CH) - ORD('0'));
		next_char(ch, true)
		UNTIL NOT setmember(digits, ord(ch))
	      ELSE ERROR(STRING_ERROR);
	  WHILE CH=' ' DO
	  BEGIN next_char(ch, true) END;
	  IF CH=':'
	  THEN BEGIN next_char(ch, true) END
	  ELSE ERROR(STRING_ERROR);
	  IF CH=')'
	  THEN BEGIN next_char(ch, true) END
	  ELSE ERROR(STRING_ERROR);
	      IF ORD_VALUE > MAX_ORD THEN BEGIN
		ERROR(STRING_ERROR);
		ORD_VALUE:= ORD('?')
	      END;
	      STRING_TEXT[STRING_LENGTH]:= ORD_VALUE
	    END
	  END
      END
    UNTIL DONE;
    IF STRING_LENGTH <= 1 THEN BEGIN
      ERROR(STRING_ERROR);
      STRING_LENGTH:= 1;  STRING_TEXT[1]:= ord('?')
    END ELSE STRING_LENGTH:= STRING_LENGTH - 1;
    IF STRING_LENGTH > 1 THEN IF STRING_LENGTH MOD WORDLENGTH <> 0 THEN
      BEGIN ERROR(STRING_ERROR); STRING_LENGTH:= 1 END;
    IF STRING_LENGTH = 1 THEN PUT1(CHAR2, STRING_TEXT[1])
    ELSE PUT_STRING(STRING_TEXT, STRING_LENGTH)
  END;
(*##########*)
(*IDENTIFIER*)
(*##########*)

  PROCEDURE IDENTIFIER;
  BEGIN
    PIECES:=-1; CHAR_INDEX:=ID_PIECE_LENGTH;
    HASH_KEY:= 1;
    REPEAT
       IF CHAR_INDEX=ID_PIECE_LENGTH THEN BEGIN
	CHAR_INDEX:= 0;  PIECES:= SUCC(PIECES);
	ID_TEXT[PIECES]:=BLANK;
      END ELSE CHAR_INDEX:= SUCC(CHAR_INDEX);
      ID_TEXT[PIECES,CHAR_INDEX]:=CH;
      HASH_KEY:=HASH_KEY*(ORD(CH) MOD SPAN +1) MOD HASH_MAX1;
      next_char(ch, true)
    UNTIL setmember(non_alfas, ord(ch));
    SEARCH_ID;
    IF SYMB=ID2 THEN PUT1(ID2,INDEX)
    ELSE BEGIN
      PUT0(SYMB);
      IF SYMB=END2 THEN BEGIN
	GET_CHAR(FALSE);
	IF CH='.' THEN BEGIN
	  PUT0(PERIOD2);
	  REPEAT 
		next_char(ch, true) 
	  UNTIL eof(source);
	  wtchar(ch);
	  END_SCAN:=TRUE
	END
      END
    END
  END;
(*#######*)
(*SCANNER*)
(*#######*)

  PROCEDURE SCAN;
VAR END_COMMENT: BOOLEAN;
  BEGIN
    REPEAT
      CASE CH OF
	' ', nul, cr:
	  BEGIN next_char(ch, true) END;

	EOL:
	  END_LINE;

	EOM:
	  END_SCAN:=TRUE;

	'"': BEGIN
	REPEAT
	  REPEAT next_char(ch, true)
	  UNTIL (CH = '"') OR (CH = EOL) or (ch = eom);
	  WHILE CH = EOL DO END_LINE
	UNTIL (CH='"') OR (CH=EOM);
	IF CH=EOM
	THEN ERROR(COMMENT_ERROR)
	ELSE BEGIN next_char(ch, true) END
	END;

(*****  the following case has been disabled until Pascal-10 can
	handle the full Ascii character set:
 
    '{' : BEGIN
       REPEAT
	  REPEAT next_char(ch, true)
	  UNTIL (CH = '}') OR (CH = EOL);
	  WHILE CH = EOL DO END_LINE;
       UNTIL (CH = '}') OR (CH = EOM);
       IF CH= EOM
	THEN ERROR(COMMENT_ERROR)
	else next_char(ch, true);
    END;

******  end of disabled case *)
 
    ']': PUT0NC(BUS2);
 
    '[': PUT0NC(SUB2);

	'.': BEGIN
	next_char(ch, true);
	  IF UPTO_SW THEN BEGIN
	    PUT0(UP_TO2);
	    UPTO_SW:=FALSE
	  END ELSE IF CH='.' THEN PUT0NC(UP_TO2)
	  ELSE IF CH=')' THEN PUT0NC(BUS2)
	  ELSE PUT0(PERIOD2)
	END;

	':' : BEGIN
	next_char(ch, true);
	  IF CH='=' THEN PUT0NC(BECOMES2) ELSE PUT0(COLON2)
	END;

	'<': BEGIN
	next_char(ch, true);
	  IF CH='=' THEN PUT0NC(LE2) ELSE
	  IF CH='>' THEN PUT0NC(NE2) ELSE
	  PUT0(LT2)
	END;

	'=':
	  PUT0NC(EQ2);

	'>': BEGIN
	next_char(ch, true);
	  IF CH='=' THEN PUT0NC(GE2) ELSE PUT0(GT2)
	END;

	'''':
	  STRING;
	  
	'0','1','2','3','4','5','6','7','8','9':
	  NUMBER;
	  
	'A','B','C','D','E','F','G','H','I','J','K','L','M','N',
	'O','P','Q','R','S','T','U','V','W','X','Y','Z','_':
	  IDENTIFIER;

	'(': BEGIN
	next_char(ch, true);
	  IF CH='.' THEN PUT0NC(SUB2) 
	  ELSE IF CH = '*' THEN BEGIN
	     END_COMMENT:= FALSE;
	     REPEAT
		next_char(ch, true);
		WHILE CH = EOL DO END_LINE;
		WHILE CH = '*' DO BEGIN
		   next_char(ch, true);
		   END_COMMENT:= CH = ')'
		END;
	     UNTIL END_COMMENT OR (CH = EOM);
	     IF CH = EOM THEN ERROR (COMMENT_ERROR)
	     ELSE BEGIN next_char(ch, true) END
	  END ELSE PUT0(OPEN2)
	END;

	')':
	  IF BUS_SW THEN BEGIN
	    PUT0NC(BUS2);
	    BUS_SW:=FALSE
	  END ELSE PUT0NC(CLOSE2);

	',':
	  PUT0NC(COMMA2);

	';':
	  PUT0NC(SEMICOLON2);

	'*':
	  PUT0NC(STAR2);

	'/':
	  PUT0NC(SLASH2);

	'+':
	  PUT0NC(PLUS2);

	'-':
	  PUT0NC(MINUS2);

	'&':
	  PUT0NC(AND2);

	'!':
	  begin
	    if listopt
	    then page(list);
	    next_char(ch, true)
	  end;

	others:
	BEGIN
	next_char(ch, true);
	  ERROR(CHAR_ERROR)
	END
      END
    UNTIL END_SCAN;
    PUT0(EOM2)
  END;
(*####*)
(*MAIN*)
(*####*)

BEGIN (* Cpas1e *)
  INITPASS(this_pass);
  INITIALIZE;
  SCAN;

  with inter_pass_ptr^ do
   if summaryoption in options then
    begin
     sum_max:=  (100 * sum_max) div max_index;
     message('max_index:',sum_max:3,'% ');
    end;
  break(list);
  NEXTPASS(this_pass);
  if xref 
  then wrxref(-1);
END   (* Cpas1e *);

begin end.
 @>È