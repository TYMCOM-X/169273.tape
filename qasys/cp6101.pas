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
: **  File Name          : cp6101.pas                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     Cp6101.pas contains the code for pass 6 of the concurrent**
: **     pascal compiler.  This pass selects the type of engine   **
: **     instructions to be generated.                            **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.01 07/18/79 DEG           Diverged from level zero        **
: ** 01.01 08/07/79 DEG  1162     In index, pop result of         **
: **                              mul_int_ec from operand stack   **
: ** 01.01 08/07/79 DEG  1162     In pushaddress, treat zero      **
: **                              displacement (function result)  **
: **                              as a special case               **
: ** 01.01 08/07/79 DEG  1162     In load_index, treat selector-  **
: **                              class operands like expressions,**
: **                              since caselist calls it         **
: ** 01.01 08/09/79 DEG  1162     In comparestruc, corrected dis- **
: **                              placement in bnefs instruction  **
: ** 01.01 08/09/79 DEG  1162     Corrected definition of         **
: **                              procedure reverse               **
: ** 01.01 08/09/79 DEG  1162     In mul_in, corrected order of   **
: **                              parameters in last call of      **
: **                              mul_int_vc                      **
: ** 01.01 08/10/79 DEG  1162     Added for_lim input il operator **
: **                              and procedure to handle it      **
: ** 01.01 08/10/79 DEG  1162     In cmp_int and subordinates, be **
: **                              prepared for negated operands   **
: ** 01.01 09/06/79 DEG  1162     Moved transfer vector to x'10200**
: ** 01.01 09/14/79 DEG  1162     Increased set_size to 32 bytes  **
: ** 01.01 09/17/79 DEG  1162     Increased max_set to 255        **
: ** 01.01 09/24/79 DEG  1162     Check tagfield before assessing **
: **                              variant part                    **
: ** 01.01 09/24/79 DEG  1162     In impossible situations, push  **
: **                              something reasonable            **
: ** 01.01 10/03/79 DEG  1162     Changes to make value parameters**
: **                              of set types work               **
: ** 01.01 10/31/79 DEG  1162     Prevent case error in procedure **
: **                              load                            **
: ** 01.01 11/01/79 DEG  1162     On finding error, stop generat- **
: **                              ing code                        **
: ** 01.01 11/13/79 DEG  1162     But continue defining labels,   **
: **                              since code referencing them may **
: **                              have been emitted already       **
: ** 01.01 11/23/79 DEG  1162     Replaced selector operands by a **
: **                              separate case selector stack    **
: ** 01.01 12/24/79 RNP  1162     Fixed bug in set_membership;    **
: **                              resetting of stack pointer was  **
: **                              destroying the con code         **
: ** 01.01 03/10/81 BH   1162     Fixed bug in handling "realtime"**
: **                              which takes no arguments in     **
: **                              routine "std_func".             **
: ** 01.01 03/10/81 BH   1162     Moved pop_operand inside case   **
: **                              statements                      **
: ** 01.01 05/24/81 BH   1162     Changed the multiplier for      **
: **                              external refs to be six rather  **
: **                              than four to agree with the     **
: **                              June 1 nucleus                  **
: ** 01.01 01/20/82 BH   1162     Changed mul_int family of       **
: **                              routines to ignore signs of     **
: **                              operands                        **
: ** 01.01 11/13/84 PJH  1162     Add a switch to gen a RI2 format**
: **                              in procedure gen_initproc when  **
: **                              the stacklength is > "7fff      **
: ** 01.02 12/15/86 PJH  1162     ADDED PROPRIETARY BANNER        **
: **                                                              **
: *****************************************************************)

program p61, pass61;

const
  maxword = 100;

(* sizes *)

  hw_size   = 2 (* bytes *);
  fw_size   = 4;
  real_size = 8;
  set_size  = 32;

(* compiler options *)

  listoption    = 0;
  summaryoption = 1;
  testoption    = 2;
  checkoption   = 3;
  codeoption    = 4;
  numberoption  = 5;
  xrefoption    = 6;
  bstackoption  = 7;
  dumpoption    = 9;
  lgxrefoption  = 8;

type
  pointer = ^integer;
  tableptr = ^table;
  table = record
            nextportion: tableptr;
            contents: array[1..maxword] of integer
          end;
  option = listoption .. dumpoption;
  tablesptr = ^tablepart;
  tablepart = record
                proglength,
                codelength,
                stacklength,
                varlength: integer;
                jumptable,
                blocktable,
                stacktable,
                consttable: tableptr
              end;
  passptr = ^passlink;
  passlink = record
               options: set of option;
               labels,
               blocks,
               constants: integer;
               resetpoint: pointer;
               tables: tablesptr
             end;
  pass_range = 0 .. 7;
  int_file = file of integer;
  errstr = packed array[1..22] of char;
(**********************************************************************)
(*                                                                    *)
(*                             externals                              *)
(*                                                                    *)
(**********************************************************************)

procedure initpass(p: pass_range);  extern;

procedure nextpass(p: pass_range);  extern;

procedure giverr(line, pass, number: integer); extern;

procedure printarg(var f: text;  arg: integer);  extern;

procedure printff(var f: text;  p: pass_range);  extern;

procedure printop(var f: text;  op: integer);  extern;

procedure wrchar(var f:  text; c: ascii); extern;

procedure printeol(var f: text); extern;
(**********************************************************************)
(*                                                                    *)
(*                            entry point                             *)
(*                                                                    *)
(**********************************************************************)

procedure pass61(link: passptr;
                 var ilin, ilout: int_file;
                 var list: text;
                 concurrent: boolean);

const
  this_pass = 6;
  initialblock = 1;
  base_size = fw_size (* line number or component index *) ;

(* input IL operators *)

PUSHCONST1 = 0;    PUSHVAR1 = 1;       PUSHIND1 = 2;       PUSHADDR1 = 3;
FIELD1 = 4;        INDEX1 = 5;         POINTER1 = 6;       VARIANT1 = 7;
RANGE1 = 8;        ASSIGN1 = 9;        ASSIGNTAG1 = 10;    COPY1 = 11;
NEW1 = 12;         NOT1 = 13;          AND1 = 14;          OR1 = 15;
NEG1 = 16;         ADD1 = 17;          SUB1 = 18;          MUL1 = 19;
DIV1 = 20;         MOD1 = 21;          caselist1=22;       for_lim1=23;
FUNCTION1 = 24;    BUILDSET1 = 25;     COMPARE1 = 26;      COMPSTRUC1 = 27;
FUNCVALUE1 = 28;   DEFLABEL1 = 29;     JUMP1 = 30;         FALSEJUMP1 = 31;
CASEJUMP1 = 32;    INITVAR1 = 33;      CALL1 = 34;         ENTER1 = 35;
RETURN1 = 36;      POP1 = 37;          NEWLINE1 = 38;      ERROR1 = 39;
CONSTANT1 = 40;    MESSAGE1 = 41;      INCREMENT1 = 42;    DECREMENT1 = 43;
PROCEDURE1 = 44;   INIT1 = 45;         PUSHLABEL1 = 46;    CALLPROG1 = 47;
EOM1=48;           noop1 = 49;         indexnc1   = 50;

(* virtual data types *)

byte_type = 0;
hw_type   = 1;
fw_type   = 2;
real_type = 3;
set_type  = 4;

(* virtual addressing modes *)

const_mode   = 0;
proc_mode    = 1;
prog_mode    = 2;
pe_mode      = 3;
ce_mode      = 4;
me_mode      = 5;
process_mode = 6;
class_mode   = 7;
monitor_mode = 8;
std_mode     = 9;
undef_mode   = 10;

(* Engine opcodes *)

balr_op  = 1;
btcr_op  = 2;
bfcr_op  = 3;
nr_op    = 4;
clr_op   = 5;
or_op    = 6;
xr_op    = 7;
lr_op    = 8;
cr_op    = 9;
ar_op    = "a;
sr_op    = "b;
mhr_op   = "c;
dhr_op   = "d;
srls_op  = "10;
slls_op  = "11;
cvhr_op  = "12;
lpswr_op = "18;
mr_op    = "1c;
dr_op    = "1d;
btbs_op  = "20;
btfs_op  = "21;
bfbs_op  = "22;
bffs_op  = "23;
lis_op   = "24;
lcs_op   = "25;
ais_op   = "26;
sis_op   = "27;
bffo_op  = "31;
bffoh_op = "33;
exhr_op  = "34;
copy_op  = "3f;
sth_op   = "40;
bal_op   = "41;
btc_op   = "42;
bfc_op   = "43;
nh_op    = "44;
clh_op   = "45;
oh_op    = "46;
xh_op    = "47;
lh_op    = "48;
ch_op    = "49;
ah_op    = "4a;
sh_op    = "4b;
mh_op    = "4c;
dh_op    = "4d;
st_op    = "50;
am_op    = "51;
n_op     = "54;
cl_op    = "55;
o_op     = "56;
x_op     = "57;
l_op     = "58;
c_op     = "59;
a_op     = "5a;
s_op     = "5b;
m_op     = "5c;
d_op     = "5d;
ahm_op   = "61;
lra_op   = "63;
lhl_op   = "73;
tbt_op   = "74;
sbt_op   = "75;
rbt_op   = "76;
cbt_op   = "77;
srhls_op = "90;
slhls_op = "91;
stbr_op  = "92;
lbr_op   = "93;
exbr_op  = "94;
epsr_op  = "95;
cio_op   = "99;
wdr_op   = "9a;
rdr_op   = "9b;
ssr_op   = "9d;
ocr_op   = "9e;
lpsw_op  = "c2;
thi_op   = "c3;
nhi_op   = "c4;
clhi_op  = "c5;
ohi_op   = "c6;
xhi_op   = "c7;
lhi_op   = "c8;
chi_op   = "c9;
ahi_op   = "ca;
shi_op   = "cb;
srhl_op  = "cc;
slhl_op  = "cd;
srha_op  = "ce;
stm_op   = "d0;
lm_op    = "d1;
stb_op   = "d2;
lb_op    = "d3;
clb_op   = "d4;
upsw_op  = "d5;
wd_op    = "da;
oc_op    = "de;
ts_op    = "e0;
svc_op   = "e1;
la_op    = "e6;
rrl_op   = "ea;
rll_op   = "eb;
srl_op   = "ec;
sll_op   = "ed;
sra_op   = "ee;
ti_op    = "f3;
ni_op    = "f4;
cli_op   = "f5;
oi_op    = "f6;
xi_op    = "f7;
li_op    = "f8;
ci_op    = "f9;
ai_op    = "fa;
si_op    = "fb;

(* Engine condition code masks *)

falsemask = 0;
lsmask    = 1;
grmask    = 2;
nemask    = 3;
oflomask  = 4;
carrymask = 8;
truemask  = 15;

(* Engine instruction sizes *)

sf_length  = 2 (* bytes *);
rr_length  = 2;
rx1_length = 4;
rx2_length = 4;
rx3_length = 6;
ri1_length = 4;
ri2_length = 6;

extref_length = rx3_length;

(* output IL operators *)

eom2       = 0;
rr_op2     = 1;
sf_op2     = 2;
rx1_op2    = 3;
rx2_op2    = 4;
rx3_op2    = 5;
ri1_op2    = 6;
ri2_op2    = 7;
truejump2  = 8;
falsejump2 = 9;
casejump2  = 10;
new2       = 11;
message2   = 12;
offset2    = 13;
enter2     = 14;
init2      = 15;
data2      = 16;

(* register definitions *)

r0 = 0;
r1 = 1;
r2 = 2;
r3 = 3;
r4 = 4;
r5 = 5;
r6 = 6;
r7 = 7;
r8 = 8;
r9 = 9;
ra = 10;
rb = 11;
rc = 12;
rd = 13;
re = 14;
rf = 15;
hd_reg = rb;
s_reg  = rc;
g_reg  = rd;
b_reg  = re;
q_reg  = rf;
first_reg = r0;
last_reg  = ra   (* rb - rf are reserved *) ;

(* limits *)

stack_limit = "FFFFF;
code_limit  = "FFFFF;
max_set     = 255 (* limit on set size *) ;
max_tag     = 15 (* limit on number of variants *) ;

(* errors *)

stack_error      = 1;
code_error       = 2;
out_of_registers = 3;
not_imp_error    = 4;
cant_happen      = 5;
range_error      = 6;
arith_overflow   = 7;
divide_by_zero   = 8;

(* comparison operators *)

LESS = 0;          EQUAL = 1;          GREATER = 2;        NOTLESS = 3;
NOTEQUAL = 4;      NOTGREATER = 5;     INSET = 6;

(* standard function indices *)

(*    Note:  these indices are assigned in pass 4.   *)

trunc1     = 0;
abs1       = 1;
succ1      = 2;
pred1      = 3;
conv1      = 4;
empty1     = 5;
attribute1 = 6;
realtime1  = 7;
ord1       = 8;
chr1       = 9;

min_func = trunc1;
max_func = chr1;

(* standard procedure indices *)

delay1    = 0;
continue1 = 1;
io1       = 2;
start1    = 3;
stop1     = 4;
setheap1  = 5;
wait1     = 6;

min_proc = delay1;
max_proc = wait1;

(* run-time support entry point indices *)

rts_terminate    = 0;
rts_overflow     = 1;
rts_pointererror = 2;
rts_rangeerror   = 3;
rts_varianterror = 4;
rts_heaplimit    = 5;
rts_stacklimit   = 6;
rts_codelimit    = 7;
rts_timelimit    = 8;
rts_callerror    = 9;
rts_delay        = 10;
rts_continue     = 11;
rts_io           = 12;
rts_stop         = 13;
rts_wait         = 14;
rts_realtime     = 15;
rts_constaddr    = 16;
rts_initgate     = 17;
rts_entergate    = 18;
rts_leavegate    = 19;
rts_initproc     = 20;
rts_exitproc     = 21;
rts_rangec1      = 22;

first_extref = rts_terminate;  last_extref = rts_rangec1;

rts_xfervector = "10200 (* address of transfer vector *) ;

(* pcb field offsets *)

pcb_index1 = 0;
pcb_heapt1 = 4;
pcb_line1  = 8;
pcb_resul1 = 12;
pcb_runti1 = 16;
pcb_slice1 = 22;
pcb_nesti1 = 24;
pcb_prior1 = 26;
pcb_overt1 = 28;
pcb_job1   = 30;
pcb_conti1 = 32;

(* object code prefix field offsets *)

obj_proglength  = 0;
obj_codelength  = 4;
obj_stacklength = 8;
obj_varlength   = 12;
obj_prefix_len  = 32;

(*    limits for compile-time arithmetic    *)

max_int =  "7FFFFFFF;
min_int = -"80000000;

type
  boolop = (andop, orop);
  updown = (decrement, increment);
  comparison = less .. inset;
  extref_index = first_extref .. last_extref;
  ilop = eom2 .. data2 ;
  modes = const_mode .. undef_mode;
  opcode_range = 0 .. 255;
  reg_range = 0 .. 15;
  work_reg = first_reg .. last_reg;
  types = byte_type .. set_type;

(* Engine instruction displacement fields *)

sf_disp  = 0 .. 15;
rx1_disp = 0 .. 16383 (* 14 bits unsigned *) ;
rx2_disp = -16384 .. 16383 (* 15 bits signed *) ;
rx3_disp = 0 .. 16777215 (* 24 bits unsigned *) ;
ri1_disp = -32768 .. 32767 (* 16 bits signed *) ;
ri2_disp = -2147483648 .. 2147483647 (* 32 bits signed *) ;

(*    operand class definitions    *)

operand_class = (address, constant, expression,
                 relation, variable);
operand =
  record
    case o_class: operand_class of
      address:
        (o_a_disp: integer;
         o_a_base,
         o_a_ixreg: reg_range;
         o_a_indexed: Boolean);
      constant:
        (o_c_value: integer);
      expression:
        (o_e_reg: reg_range;
         o_e_negated: Boolean);
      relation:
        (o_r_cond: comparison);
      variable:
        (o_v_type: types;
         o_v_disp: integer;
         o_v_base,
         o_v_ixreg: reg_range;
         o_v_indexed,
         o_v_negated: Boolean)
  end (* operand *) ;
operand_ptr = ^ internal_operand;
internal_operand =
  record
    info: operand;
    rlink,
    flink: operand_ptr
  end (* internal_operand *) ;

(*    constant classification    *)

const_classification = (zero, one, poweroftwo,
                        twomminus1, twomplus1, other);

(*    case stack types    *)

case_pointer = ^ case_stack_entry;
case_stack_entry =
  record
    sel_reg: work_reg;
    rlink,
    flink: case_pointer
  end;

var
  afterbegin,
  aftererror,
  calling_program,
  check,
  bstack,
  done,
  generate,
  number,
  justnewline,
  stillcond,
  summary,
  test: boolean;

  blocktable,
  consttable,
  jumptable,
  stacktable: tableptr;

  arg1, arg2, arg3, arg4, arg5,
  block,
  constants,
  curmode,
  implicitlength,
  initialstack,
  lastlength,
  lastop,
  line,
  linklength,
  listlinecount,
  location,
  maxtemp,
  op,
  paramlength,
  prog_set_temps,
  saved_temps,
  savelength,
  stacklength,
  temp,
  temp_origin,
  varlength: integer;

(*    register data base    *)

  reg_desc: array[work_reg] of integer   (* use counts *) ;

(*    operand class variables    *)

  head: operand_ptr;
  stack_depth: integer;

(*    case stack variables    *)

  case_head: case_pointer;
(**********************************************************************)
(*                                                                    *)
(*                          input procedures                          *)
(*                                                               *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                              Read_Ifl                              *)
(*                                                                    *)
(**********************************************************************)

procedure read_ifl(var i: integer);
begin
  i := ilin^;  get(ilin); 
end;


function peek_ifl: integer;
begin
  peek_ifl := ilin^
end;

(**********************************************************************)
(*                                                                    *)
(*                              Read1Arg                              *)
(*                                                                    *)
(**********************************************************************)

procedure read1arg;
begin
  read_ifl(arg1)
end;



(**********************************************************************)
(*                                                                    *)
(*                             Read2Args                              *)
(*                                                                    *)
(**********************************************************************)

procedure read2args;
begin
  read_ifl(arg1);  read_ifl(arg2)
end;



(**********************************************************************)
(*                                                                    *)
(*                             Read3Args                              *)
(*                                                                    *)
(**********************************************************************)

procedure read3args;
begin
  read_ifl(arg1);  read_ifl(arg2);  read_ifl(arg3)
end;



(**********************************************************************)
(*                                                                    *)
(*                             Read4Args                              *)
(*                                                                    *)
(**********************************************************************)

procedure read4args;
begin
  read_ifl(arg1);  read_ifl(arg2);
  read_ifl(arg3);  read_ifl(arg4)
end;



(**********************************************************************)
(*                                                                    *)
(*                             Read5Args                              *)
(*                                                                    *)
(**********************************************************************)

procedure read5args;
begin
  read_ifl(arg1);  read_ifl(arg2);  read_ifl(arg3);
  read_ifl(arg4);  read_ifl(arg5)
end;
(**********************************************************************)
(*                                                                    *)
(*                         output procedures                          *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             Write_Ifl                              *)
(*                                                                    *)
(**********************************************************************)

procedure write_ifl(i: integer);
begin
  ilout^ := i;  put(ilout)
end;



(**********************************************************************)
(*                                                                    *)
(*                              Put_Arg                               *)
(*                                                                    *)
(**********************************************************************)

procedure put_arg(arg: integer);
begin
  write_ifl(arg);
  if test then printarg(list, arg)
end;



(**********************************************************************)
(*                                                                    *)
(*                                Put0                                *)
(*                                                                    *)
(**********************************************************************)

procedure put0(op: integer);
begin
  justnewline := false; (* we DIDN'T just do a newline *)
  write_ifl(op);
  if test then printop(list, op)
end;



(**********************************************************************)
(*                                                                    *)
(*                                Put1                                *)
(*                                                                    *)
(**********************************************************************)

procedure put1(op, arg: integer);
begin
  put0(op);  put_arg(arg)
end;



(**********************************************************************)
(*                                                                    *)
(*                                Put2                                *)
(*                                                                    *)
(**********************************************************************)

procedure put2(op, arg1, arg2: integer);
begin
  put1(op, arg1);  put_arg(arg2)
end;



(**********************************************************************)
(*                                                                    *)
(*                                Put3                                *)
(*                                                                    *)
(**********************************************************************)

procedure put3(op, arg1, arg2, arg3: integer);
begin
  put2(op, arg1, arg2);  put_arg(arg3)
end;



(**********************************************************************)
(*                                                                    *)
(*                                Put4                                *)
(*                                                                    *)
(**********************************************************************)

procedure put4(op, arg1, arg2, arg3, arg4: integer);
begin
  put3(op, arg1, arg2, arg3);  put_arg(arg4)
end;



(**********************************************************************)
(*                                                                    *)
(*                                Put5                                *)
(*                                                                    *)
(**********************************************************************)

procedure put5(op, arg1, arg2, arg3, arg4, arg5: integer);
begin
  put4(op, arg1, arg2, arg3, arg4);  put_arg(arg5)
end;



(**********************************************************************)
(*                                                                    *)
(*                             listline                               *)
(*                                                                    *)
(**********************************************************************)

procedure listline(line, loc: integer);

procedure writenum(i, base: integer; zerosup: boolean);
var
  j: integer; (* assigned to to keep our parameter clean *)
  digit: integer; (* loop index *)
  out: packed array[1..6] of char; (* the output *)
  digits: packed array[0..15] of char; (* hex digits *)

begin
  j := i; (* save the parameter *)
  digits := '0123456789abcdef'; (* set up hex digits *)
  for digit := 6 downto 1 do begin
    out[digit] := digits[j mod base];
    j := j div base
end;
  digit := 1; (* now we'll get rid of zeros *)
  if zerosup
  then while (out[digit] = '0') and (digit<6) do begin (* we don't want to zap the last digit *)
    out[digit] := ' ';
    digit := succ(digit)
  end;
  for digit := 1 to 6 do
    wrchar(list, out[digit]);
  wrchar(list, ' ');
  wrchar(list, ' ')
end;

begin (* listline *)
  listlinecount := succ(listlinecount);
  if listlinecount mod 6 = 1
  then printeol(list);
  writenum(line, 10, true);
  writenum(loc, 16, false);
end   (* listline *) ;



(**********************************************************************)
(*                                                                    *)
(*                               Error                                *)
(*                                                                    *)
(**********************************************************************)

procedure error(pass: pass_range;  number: integer);
begin
  put3(message2, pass, number, line);
  giverr(line, pass, number);
  generate := false
end   (* error *) ;



(**********************************************************************)
(*                                                                    *)
(*                            Inc_Location                            *)
(*                                                                    *)
(**********************************************************************)

procedure inc_location(length: integer);
begin
  if location < code_limit
  then location := location + length
  else
    begin
      error(this_pass, code_error);
      location := 0  
    end;
  lastlength := length
end   (* inc_location *) ;



(**********************************************************************)
(*                                                                    *)
(*    Note concerning code-generating procedures:                     *)
(*                                                                    *)
(*    Operands appear in the same order as in assembler notation.     *)
(*    They are emitted in the order in which they appear in the       *)
(*    machine instruction.                                            *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                               Gen_RR                               *)
(*                                                                    *)
(**********************************************************************)

procedure gen_rr(op: opcode_range;  r1, r2: reg_range);
begin
  put3(rr_op2, op, r1, r2);
  inc_location(rr_length)
end;



(**********************************************************************)
(*                                                                    *)
(*                               Gen_SF                               *)
(*                                                                    *)
(**********************************************************************)

procedure gen_sf(op: opcode_range;  r1, n: reg_range);
begin
  put3(sf_op2, op, r1, n);
  inc_location(sf_length)
end;



(**********************************************************************)
(*                                                                    *)
(*                              Gen_RX1                               *)
(*                                                                    *)
(**********************************************************************)

procedure gen_rx1(op: opcode_range;
                  r1: reg_range;
                  d2: rx1_disp;
                  x2: reg_range);
begin
  put4(rx1_op2, op, r1, x2, d2);
  inc_location(rx1_length)
end;



(**********************************************************************)
(*                                                                    *)
(*                              Gen_RX2                               *)
(*                                                                    *)
(**********************************************************************)

procedure gen_rx2(op: opcode_range;
                  r1: reg_range;
                  d2: rx2_disp;
                  x2: reg_range);
begin
  put4(rx2_op2, op, r1, x2, d2);
  inc_location(rx2_length)
end;



(**********************************************************************)
(*                                                                    *)
(*                              Gen_RX3                               *)
(*                                                                    *)
(**********************************************************************)

procedure gen_rx3(op: opcode_range;
                  r1: reg_range;
                  a2: rx3_disp;
                  fx2, sx2: reg_range);
begin
  put5(rx3_op2, op, r1, fx2, sx2, a2);
  inc_location(rx3_length)
end;



(**********************************************************************)
(*                                                                    *)
(*                              Gen_RI1                               *)
(*                                                                    *)
(**********************************************************************)

procedure gen_ri1(op: opcode_range;
                  r1: reg_range;
                  i2: ri1_disp;
                  x2: reg_range);
begin
  put4(ri1_op2, op, r1, x2, i2);
  inc_location(ri1_length)
end;



(**********************************************************************)
(*                                                                    *)
(*                              Gen_RI2                               *)
(*                                                                    *)
(**********************************************************************)

procedure gen_ri2(op: opcode_range;
                  r1: reg_range;
                  i2: ri2_disp;
                  x2: reg_range);
begin
  put4(ri2_op2, op, r1, x2, i2);
  inc_location(ri2_length)
end;



(**********************************************************************)
(*                                                                    *)
(*                               Gen_RX                               *)
(*                                                                    *)
(*    Generates RX1 if displacement is small enough and second        *)
(*    index register is zero, RX3 otherwise.                          *)
(*                                                                    *)
(**********************************************************************)
procedure gen_rx(op: opcode_range;
                 r1: reg_range;
                 d2: rx3_disp;
                 fx2, sx2: reg_range);
begin
  if (d2 > "3FFF) or (sx2 <> 0)
  then gen_rx3(op, r1, d2, fx2, sx2)
  else gen_rx1(op, r1, d2, fx2)
end;



(**********************************************************************)
(*                                                                    *)
(*                           Gen_Immediate                            *)
(*                                                                    *)
(*    OP is LI, AI, SI, CI, or NI.  Generates the smallest Engine     *)
(*    instruction consistent with the size of ARG.                    *)
(*                                                                    *)
(**********************************************************************)

procedure gen_immediate(op: opcode_range;
                        reg: reg_range;
                        arg: integer);
begin
  if arg < 0
  then
    if op = ai_op
    then
      begin
        op := si_op;  arg := - arg
      end
    else if op = si_op
    then
      begin
        op := ai_op;  arg := - arg
      end;
  if
    (op = li_op) and (arg < 0) and (arg >= -15)
  then
    gen_sf(lcs_op, reg, -arg)
  else if
    (op <> ni_op) and (op <> ci_op) and (op <> cli_op) and (arg >= 0) and (arg <= 15)
  then   (* use immediate short *)
    gen_sf(op-212, reg, arg)
  else if
    (arg >= -32768) and (arg <= 32767)
  then   (* use halfword immediate *)
    gen_ri1(op-48, reg, arg, 0)
  else   (* must use fullword immediate *)
    gen_ri2(op, reg, arg, 0)
end   (* gen_immediate *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Gen_ExtRef                             *)
(*                                                                    *)
(**********************************************************************)

procedure gen_extref(index: extref_index);
begin
  gen_rx3(bal_op, q_reg, rts_xfervector+6*index, 0, 0)
end;



(**********************************************************************)
(*
                              Gen_data

*)
procedure gen_data(h: integer);
begin
  put1(data2, h);
  inc_location(hw_size)
end;



(**********************************************************************)
(*                                                                    *)
(*                             Gen_Enter                              *)
(*                                                                    *)
(*    Causes pass 7 to generate                                       *)
(*      CHI     R0,stacklength(S)                                     *)
(*                                                                    *)
(**********************************************************************)

procedure gen_enter(lab: integer);
begin
  put1(enter2, lab);
  inc_location(ri1_length)
end;



(**********************************************************************)
(*                                                                    *)
(*                            Gen_InitProc                            *)
(*                                                                    *)
(* 	if bstack is on then it causes pass 7 to generate             *)
(*   		LI     R3,stacklength				      *)
(*	otherwise it generates					      *)
(*              LHI    R3,stacklength                                 *)
(*                                                                    *)
(**********************************************************************)

procedure gen_initproc(lab: integer);
begin
  put1(init2, lab);
  if bstack then
    inc_location(ri2_length) 
  else
    inc_location(ri1_length)
end;



(**********************************************************************)
(*                                                                    *)
(*                              Gen_New                               *)
(*                                                                    *)
(*    Causes pass 7 to generate                                       *)
(*              CHI     R0,stacklength(B)                             *)
(*                                                                    *)
(**********************************************************************)

procedure gen_new(block: integer);
begin
  put1(new2, block);
  inc_location(ri1_length)
end;



(**********************************************************************)
(*                                                                    *)
(*                             Write_Jump                             *)
(*                                                                    *)
(*    Emits IL for TRUEJUMP and FALSEJUMP operations.                 *)
(*                                                                    *)
(**********************************************************************)

procedure write_jump(op: ilop;  mask: reg_range;  lab: integer);
begin
  inc_location(rx2_length);
  put3(op, location, mask, lab)
end;



(**********************************************************************)
(*                                                                    *)
(*                             Gen_Offset                             *)
(*                                                                    *)
(*    Causes pass 7 to generate                                       *)
(*              LI      reg,blockaddress-*                            *)
(*                                                                    *)
(**********************************************************************)

procedure gen_offset(reg: reg_range; block: integer);
begin
  inc_location(ri2_length);
  put3(offset2, location, block, reg)
end;



(**********************************************************************)
(*                                                                    *)
(*                           Gen_RangeCheck                           *)
(*                                                                    *)
(*    optionaly generates code to check that 0 <= (REG) <= LIMIT.     *)
(*                                                                    *)
(**********************************************************************)

procedure gen_rangecheck(reg: reg_range;  limit: integer);
begin
  if check
  then
    begin
      if (reg = ra) and (limit > -32768) and (limit < 32767)
      then
        begin
          gen_extref(rts_rangec1);
          gen_data(limit+1)
        end
      else
        begin
          gen_immediate(cli_op, reg, limit+1);
          gen_sf(bffs_op, carrymask, (sf_length+extref_length) div hw_size);
          gen_extref(rts_rangeerror)
        end
    end
end;



(**********************************************************************)
(*                                                                    *)
(*                             Gen_Shift                              *)
(*                                                                    *)
(*    OP is SLL or SRL.  Generates the smallest shift instruction     *)
(*    consistent with the size of SHIFT.                              *)
(*                                                                    *)
(**********************************************************************)

procedure gen_shift(op: opcode_range;
                    reg: reg_range;
                    shift: integer);
begin
  if shift < 16
  then gen_sf(op-"DC, reg, shift)   (*    short form    *)
  else gen_rx1(op, reg, shift, 0)
end (* gen_shift *) ;
(**********************************************************************)
(*                                                                    *)
(*                          stack procedures                          *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             Push_Stack                             *)
(*                                                                    *)
(*    Simulates pushing the run-time stack.  Adjusts the high-water   *)
(*    mark (MAXTEMP) if necessary.                                    *)
(*                                                                    *)
(**********************************************************************)

procedure push_stack(length (* bytes *) : integer);
begin
  if temp < stack_limit - length
  then temp := temp + length
  else error(this_pass, stack_error);
  if temp > maxtemp
  then begin
    maxtemp := temp;
  end
end   (* push_stack *);



(**********************************************************************)
(*                                                                    *)
(*                             Pop_Stack                              *)
(*                                                                    *)
(*    Simulates popping the run-time stack.                           *)
(*                                                                    *)
(**********************************************************************)

procedure pop_stack(length (* bytes *) : integer);
begin
  temp := temp - length
end;
(**********************************************************************)
(*                                                                    *)
(*                   temporary management routines                    *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                           Allocate_Temp                            *)
(*                                                                    *)
(*    Allocates a temporary of SIZE bytes on top of the stack and     *)
(*    returns its DISPLACEMENT from B_REG.                            *)
(*                                                                    *)
(**********************************************************************)

procedure allocate_temp(size: integer;  var displacement: integer);
begin
  displacement := temp_origin + temp;
  gen_immediate(ai_op, s_reg, size);
  push_stack(size)
end (* allocate_temp *) ;



(**********************************************************************)
(*                                                                    *)
(*                          Deallocate_Temp                           *)
(*                                                                    *)
(*    Deallocates a temporary of SIZE bytes which is assumed to be    *)
(*    located at the top of the stack.                                *)
(*                                                                    *)
(**********************************************************************)

procedure deallocate_temp(size: integer);
begin
  gen_immediate(si_op, s_reg, size);
  pop_stack(size)
end (* deallocate_temp *) ;



(**********************************************************************)
(*                                                                    *)
(*                          Reallocate_Temp                           *)
(*                                                                    *)
(*    Equivalent to the sequence:                                     *)
(*      deallocate_temp(old_size);                                    *)
(*      allocate_temp(new_size, displacement)                         *)
(*                                                                    *)
(**********************************************************************)

procedure reallocate_temp(old_size, new_size: integer;
                          var displacement: integer);
var
  diff: integer;
begin
  displacement := temp_origin + temp - old_size;
  diff := new_size - old_size;
  if diff > 0
  then
    begin
      gen_immediate(ai_op, s_reg, diff);
      push_stack(diff)
    end
  else if diff < 0
  then
    begin
      gen_immediate(si_op, s_reg, - diff);
      pop_stack(- diff)
    end
end (* reallocate_temp *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Temporary                              *)
(*                                                                    *)
(*    Returns TRUE iff its argument describes a stack temporary.      *)
(*                                                                    *)
(**********************************************************************)

function temporary(opnd: operand): boolean;
begin
  with opnd do
    if o_class = variable
    then temporary := (o_v_base = b_reg) and (o_v_disp >= temp_origin)
    else temporary := false
end (* temporary *) ;
(**********************************************************************)
(*                                                                    *)
(*                   register data base procedures                    *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             Clear_Regs                             *)
(*                                                                    *)
(**********************************************************************)

procedure clear_regs;
var
  i: work_reg;
begin
  for i := first_reg to last_reg do reg_desc[i] := 0
end (* clear_regs *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Dec_Ref                               *)
(*                                                                    *)
(**********************************************************************)

procedure dec_ref(i: work_reg);
begin
  if reg_desc[i] = 0
  then error(this_pass, cant_happen)
  else reg_desc[i] := reg_desc[i] - 1
end (* dec_ref *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Inc_Ref                               *)
(*                                                                    *)
(**********************************************************************)

procedure inc_ref(i: work_reg);
begin
  reg_desc[i] := reg_desc[i] + 1
end (* inc_ref *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Use_Count                              *)
(*                                                                    *)
(**********************************************************************)

function use_count(i: work_reg): integer;
begin
  use_count := reg_desc[i]
end (* use_count *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Get_Reg                               *)
(*                                                                    *)
(**********************************************************************)

procedure get_reg(first, last: work_reg;  var r: work_reg);
type
  status = (searching, success, failure);
var
  state: status;
begin
  r := last;  state := searching;
  repeat
    if reg_desc[r] = 0
    then state := success
    else if r = first
    then state := failure
    else r := pred(r)
  until
    state <> searching;
  if state = failure
  then error(this_pass, out_of_registers);
  inc_ref(r)
end (* get_reg *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Get_Pair                              *)
(*                                                                    *)
(**********************************************************************)

procedure get_pair(first: work_reg;  var r: work_reg);
type
  status = (searching, success, failure);
var
  state: status;
begin
  r := first;  state := searching;
  repeat
    if (reg_desc[r] = 0) and (reg_desc[succ(r)] = 0)
    then state := success
    else if succ(r) = last_reg
    then state := failure
    else r := succ(r)
  until
    state <> searching;
  if state = failure
  then error(this_pass, out_of_registers);
  inc_ref(r);  inc_ref(succ(r))
end (* get_pair *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Free_Regs                              *)
(*                                                                    *)
(**********************************************************************)

procedure free_regs(opnd: operand);
begin
  with opnd do
    case o_class of
      address:
        begin
          if not (o_a_base in [b_reg, g_reg])
          then dec_ref(o_a_base);
          if o_a_ixreg <> 0
          then dec_ref(o_a_ixreg)
        end;
      constant,
      relation:
        ;
      expression:
        dec_ref(o_e_reg);
      variable:
        begin
          if not (o_v_base in [b_reg, g_reg])
          then dec_ref(o_v_base);
          if o_v_ixreg <> 0
          then dec_ref(o_v_ixreg)
        end
    end (* case, with *)
end (* free_regs *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Mark_Regs                              *)
(*                                                                    *)
(**********************************************************************)

procedure mark_regs(opnd: operand);
begin
  with opnd do
    case o_class of
      address:
        begin
          if not (o_a_base in [b_reg, g_reg])
          then inc_ref(o_a_base);
          if o_a_ixreg <> 0
          then inc_ref(o_a_ixreg)
        end;
      constant,
      relation:
        ;
      expression:
        inc_ref(o_e_reg);
      variable:
        begin
          if not (o_v_base in [b_reg, g_reg])
          then inc_ref(o_v_base);
          if o_v_ixreg <> 0
          then inc_ref(o_v_ixreg)
        end
    end (* case, with *)
end (* mark_regs *) ;
(**********************************************************************)
(*                                                                    *)
(*                       Case Stack procedures                        *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                          Init_Case_Stack                           *)
(*                                                                    *)
(**********************************************************************)

procedure init_case_stack;
begin
  new(case_head);
  with case_head ^ do
    begin
      sel_reg := r0;
      rlink := case_head;
      flink := case_head
    end (* with *)
end (* init_case_stack *) ;



(**********************************************************************)
(*                                                                    *)
(*                         Push_Case_Selector                         *)
(*                                                                    *)
(**********************************************************************)

procedure push_case_selector(s: work_reg);
var
  n, t: case_pointer;
begin
  new(n);  t := case_head^.flink;
  with n^ do
    begin
      sel_reg := s;
      flink := t;
      rlink := case_head
    end (* with *) ;
  case_head^.flink := n;
  t^.rlink := n
end (* push_case_selector *) ;



(**********************************************************************)
(*                                                                    *)
(*                         Pop_Case_Selector                          *)
(*                                                                    *)
(**********************************************************************)

procedure pop_case_selector(var s: work_reg);
var
  p, q: case_pointer;
begin
  p := case_head^.flink;
  with p^ do
    begin
      s := sel_reg;
      q := flink
    end (* with *) ;
  if p = case_head
  then error(this_pass, cant_happen)
  else
    begin
      case_head^.flink := q;
      q^.rlink := case_head;
      dispose(p)
    end
end (* pop_case_selector *) ;
(**********************************************************************)
(*                                                                    *)
(*                      operand stack procedures                      *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                         Init_Operand_Stack                         *)
(*                                                                    *)
(**********************************************************************)

procedure init_operand_stack;
begin
  new(head);
  with head^, info do
    begin
      flink := head;
      rlink := head;
      o_class := constant;
      o_c_value := 0
    end (* with *) ;
  stack_depth := 0
end (* init_operand_stack *) ;



(**********************************************************************)
(*                                                                    *)
(*                            Push_Operand                            *)
(*                                                                    *)
(**********************************************************************)

procedure push_operand(opnd: operand);
var
  n, t: operand_ptr;
begin
  new(n);  t := head^.flink;
  with n^ do
    begin
      info := opnd;
      flink := t;
      rlink := head
    end (* with *) ;
  head^.flink := n;
  t^.rlink := n;
  stack_depth := stack_depth + 1
end (* push_operand *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Pop_Operand                            *)
(*                                                                    *)
(**********************************************************************)

procedure pop_operand(var opnd: operand);
var
  p, q: operand_ptr;
begin
  p := head^.flink;
  with p^ do
    begin
      opnd := info;
      q := flink
    end (* with *) ;
  head^.flink := q;
  q^.rlink := head;
  if stack_depth = 0
  then error(this_pass, cant_happen)
  else
    begin
      stack_depth := stack_depth - 1;
      dispose(p)
    end
end (* pop_operand *) ;



(**********************************************************************)
(*                                                                    *)
(*                            Get_Operand                             *)
(*                                                                    *)
(*    Returns the Nth operand from the bottom of the stack, counting  *)
(*    from zero.                                                      *)
(*                                                                    *)
(**********************************************************************)

procedure get_operand(n: integer;  var opnd: operand);
var
  p: operand_ptr;
begin
  if n >= stack_depth
  then error(this_pass, cant_happen);
  p := head^.rlink;
  while n > 0 do
    begin
      p := p^.rlink;
      n := n - 1
    end (* while *) ;
  opnd := p^.info
end (* get_operand *) ;
(**********************************************************************)
(*                                                                    *)
(*                  operand manipulation procedures                   *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                           Dummy_Operand                            *)
(*                                                                    *)
(*    Used in error recovery.  Makes OPND an operand of the given     *)
(*    KLASS with innocuous values.                                    *)
(*                                                                    *)
(**********************************************************************)

procedure dummy_operand(var opnd: operand;  klass: operand_class);
begin
  with opnd do
    begin
      o_class := klass;
      case o_class of
        address:
          begin
            o_a_disp := 0;
            o_a_base := r0;
            o_a_ixreg := r0;
            o_a_indexed := false
          end;
        constant:
          o_c_value := 0;
        expression:
          begin
            o_e_reg := r0;
            o_e_negated := false
          end;
        relation:
          o_r_cond := less;
        variable:
          begin
            o_v_type := byte_type;
            o_v_disp := 0;
            o_v_base := r0;
            o_v_ixreg := r0;
            o_v_indexed := false;
            o_v_negated := false
          end
      end (* case *)
    end (* with *)
end (* dummy_operand *) ;



(**********************************************************************)
(*                                                                    *)
(*                               Ensure                               *)
(*                                                                    *)
(*    Ensures that OPND is of the specified KLASS.                    *)
(*                                                                    *)
(**********************************************************************)

procedure ensure(var opnd: operand; klass: operand_class);
begin
  if opnd.o_class <> klass
  then
    begin
      error(this_pass, cant_happen);
      dummy_operand(opnd, klass)
    end
end (* ensure *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Load                                *)
(*                                                                    *)
(*    Generates code to load the value of OPND into a register and    *)
(*    returns the register number in R.  On exit, OPND is of class    *)
(*    EXPRESSION.                                                     *)
(*                                                                    *)
(**********************************************************************)

procedure load(var opnd: operand;  var r: work_reg);
var
  op: opcode_range;
  negated: Boolean;
begin
  negated := false;
  with opnd do
    begin
      case o_class of
        address:
          begin
            free_regs(opnd);
            get_reg(1, last_reg, r);
            gen_rx(la_op, r, o_a_disp, o_a_base, o_a_ixreg)
          end;
        constant:
          begin
            get_reg(1, last_reg, r);
            gen_immediate(li_op, r, o_c_value)
          end;
        expression:
          begin
            r := o_e_reg;
            negated := o_e_negated
          end;
        relation:
          error(this_pass, cant_happen);
        variable:
          begin
            negated := o_v_negated;
            case o_v_type of
              byte_type: op := lb_op;
              hw_type  : op := lh_op;
              fw_type  : op := l_op;
              real_type,
              set_type :
                begin
                  error(this_pass, cant_happen);
                  op := l_op
                end
            end (* case o_v_type *) ;
            free_regs(opnd);
            get_reg(1, last_reg, r);
            gen_rx(op, r, o_v_disp, o_v_base, o_v_ixreg)
          end
      end (* case o_class *) ;
      o_class := expression;
      o_e_reg := r;
      o_e_negated := negated
    end (* with *)
end (* load *) ;



(**********************************************************************)
(*                                                                    *)
(*                         Load_Destructible                          *)
(*                                                                    *)
(*    Generates code to load the value of OPND into a register that   *)
(*    can be overwritten (i.e., that is referenced by no other        *)
(*    operand) and returns the register number in R.  On exit, OPND   *)
(*    is of class EXPRESSION.                                         *)
(*                                                                    *)
(**********************************************************************)

procedure load_destructible(var opnd: operand;  var r: work_reg);
var
  temp: work_reg;
begin
  load(opnd, r);   (*    OPND is now of class EXPRESSION    *)
  if use_count(r) > 1
  then
    with opnd do
      begin
        get_reg(1, last_reg, temp);
        gen_rr(lr_op, temp, o_e_reg);
        dec_ref(o_e_reg);
        o_e_reg := temp;  r := temp
      end (* with *)
end (* load_destructible *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Load_Index                             *)
(*                                                                    *)
(*    Generates code to load the value of an operand representing     *)
(*    an index into a register and subtracts the lower limit.         *)
(*    On exit, OPND is of class EXPRESSION.                           *)
(*                                                                    *)
(**********************************************************************)

procedure load_index(var opnd: operand;  min, max: integer);
var
  reg: work_reg;
  save_class: operand_class;
begin
  with opnd do
    case o_class of
      address,
      relation:
        begin
          error(this_pass, cant_happen);
          dummy_operand(opnd, expression)
        end;
      constant:
        begin
          if (o_c_value < min) or (o_c_value > max)
          then error(this_pass, range_error);
          o_c_value := o_c_value - min;
          load(opnd, reg)
        end;
      expression,
      variable:
        begin
          save_class := o_class;
          load_destructible(opnd, reg);
          if (min <> 0) or (save_class <> variable)
          then gen_immediate(si_op, reg, min);(*<<<*)
(*          if check
          then gen_rangecheck(reg, max-min)*)
        end
    end (* case *)
end (* load_index *) ;



(**********************************************************************)
(*                                                                    *)
(*                           Force_Address                            *)
(*                                                                    *)
(*    Called when context requires an address.  On exit, OPND is      *)
(*    of class ADDRESS.                                               *)
(*                                                                    *)
(**********************************************************************)

procedure force_address(var opnd: operand);
var
  reg: work_reg;
begin
  with opnd do
    case o_class of
      address:
        ;
      constant,
      relation:
        begin
          error(this_pass, cant_happen);
          dummy_operand(opnd, address)
        end;
      expression,       (*    address expression    *)
      variable:         (*    pointer variable      *)
        begin
          load(opnd, reg);
          o_class := address;
          o_a_base := reg;
          o_a_disp := 0;
          o_a_ixreg := 0;
          o_a_indexed := false
        end
    end (* case *)
end (* force_address *) ;



(**********************************************************************)
(*                                                                    *)
(*                              UnIndex                               *)
(*                                                                    *)
(*    Argument must be of class ADDRESS or VARIABLE.                  *)
(*    Ensures that second index register field is free for use as     *)
(*    a loop index.                                                   *)
(*                                                                    *)
(**********************************************************************)

procedure unindex(var opnd: operand);
var
  temp: work_reg;
begin
  with opnd do
    case o_class of
      address:
        if o_a_ixreg <> 0
        then
          begin
            if use_count(o_a_ixreg) > 1
            then
              begin
                get_reg(1, last_reg, temp);
                gen_rr(lr_op, temp, o_a_ixreg);
                dec_ref(o_a_ixreg);
                o_a_ixreg := temp
              end;
            gen_rr(ar_op, o_a_ixreg, o_a_base);
            if not (o_a_base in [b_reg, g_reg])
            then dec_ref(o_a_base);
            o_a_base := o_a_ixreg;
            o_a_ixreg := 0
          end;
      variable:
        if o_v_ixreg <> 0
        then
          begin
            if use_count(o_v_ixreg) > 1
            then
              begin
                get_reg(1, last_reg, temp);
                gen_rr(lr_op, temp, o_v_ixreg);
                dec_ref(o_v_ixreg);
                o_v_ixreg := temp
              end;
            gen_rr(ar_op, o_v_ixreg, o_v_base);
            if not (o_v_base in [b_reg, g_reg])
            then dec_ref(o_v_base);
            o_v_base := o_v_ixreg;
            o_v_ixreg := 0
          end;
      others:
        error(this_pass, cant_happen)
    end (* case, with *)
end (* unindex *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Commute                               *)
(*                                                                    *)
(*    Both arguments must be of class EXPRESSION.  Ensures that       *)
(*    the register containing LEFT can be overwritten (by an RR       *)
(*    instruction).  Attempts to exchange roles of LEFT and RIGHT     *)
(*    before resorting to copying.                                    *)
(*                                                                    *)
(**********************************************************************)

procedure commute(var left, right: operand);
var
  temp: operand;
  reg: work_reg;
begin
  if use_count(left.o_e_reg) > 1
  then
    if use_count(right.o_e_reg) > 1
    then load_destructible(left, reg)
    else
      begin
        temp := left;  left := right;  right := temp
      end
end (* commute *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Gen_Code                              *)
(*                                                                    *)
(*    OPERAND argument must be of class VARIABLE.  Generates an RX    *)
(*    instruction (symbolically:  OP  REG,VRBL) and decrements the    *)
(*    reference counts of registers referenced by VRBL.               *)
(*                                                                    *)
(**********************************************************************)

procedure gen_code(op: opcode_range;
                   reg: reg_range;
                   vrbl: operand);
begin
  with vrbl do
    gen_rx(op, reg, o_v_disp, o_v_base, o_v_ixreg);
  free_regs(vrbl)
end (* gen_code *) ;
(**********************************************************************)
(*                                                                    *)
(*                    Stack Frame Component Sizes                     *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*    Components of a stack frame:                                    *)
(*                                                                    *)
(*      base:           BASE_SIZE       line # or component index     *)
(*     {linkage:        LINKLENGTH      G, B, & Q}                    *)
(*     {implicit arg:   IMPLICITLENGTH  mon/class addr or const addr} *)
(*      arguments:      PARAMLENGTH                                   *)
(*      variables:      VARLENGTH                                     *)
(*      temporaries:    MAXTEMP                                       *)
(*     {extra:          STACKLENGTH     for seql programs}            *)
(*                                                                    *)
(*      {} = not always applicable                                    *)
(*                                                                    *)
(*    Modes MONITOR and CLASS refer to the global data area when      *)
(*    applied to arguments and variables, but to the initial          *)
(*    statement's frame when applied to routines.  Different size     *)
(*    calculations are therefore necessary.                           *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                            V_Link_Size                             *)
(*                                                                    *)
(*    Linkage area size for use in computing variable offsets:        *)
(*                                                                    *)
(**********************************************************************)

function v_link_size(mode:modes): integer;
begin
  if mode in [proc_mode, prog_mode, pe_mode, ce_mode, me_mode]
  then v_link_size := 12 (* bytes = 3 fullwords *)
  else v_link_size := 0
end;



(**********************************************************************)
(*                                                                    *)
(*                            R_Link_Size                             *)
(*                                                                    *)
(*    Linkage area size for routine calls and prologs:                *)
(*                                                                    *)
(**********************************************************************)

function r_link_size(mode: modes): integer;
begin
  if mode in [proc_mode, prog_mode, pe_mode, ce_mode, me_mode,
              monitor_mode, class_mode]
  then r_link_size := 12 (* bytes = 3 fullwords *)
  else r_link_size := 0
end;



(**********************************************************************)
(*                                                                    *)
(*                            V_Impl_Size                             *)
(*                                                                    *)
(*    Implicit argument size for computing variable displacements:    *)
(*                                                                    *)
(**********************************************************************)

function v_impl_size(mode: modes): integer;
begin
  if mode in [prog_mode, ce_mode, me_mode]
  then v_impl_size := fw_size
  else v_impl_size := 0
end;



(**********************************************************************)
(*                                                                    *)
(*                            R_Impl_Size                             *)
(*                                                                    *)
(*    Implicit argument size for routine calls and prologs:           *)
(*                                                                    *)
(**********************************************************************)

function r_impl_size(mode: modes): integer;
begin
  if mode in [prog_mode, ce_mode, me_mode, class_mode, monitor_mode]
  then r_impl_size := fw_size
  else r_impl_size := 0
end;
(**********************************************************************)
(*                                                                    *)
(*                         Classify_Constant                          *)
(*                                                                    *)
(*    Classifies a constant into one of the following categories:     *)
(*      Zero            0                                             *)
(*      One             1                                             *)
(*      PowerOfTwo      2^N,  N>0                                     *)
(*      TwoMMinus1      2^N(2^M-1), N>=0, M>1                         *)
(*      TwoMPlus1       2^N(2^M+1), N>=0, M>0                         *)
(*      Other           none of the above                             *)
(*                                                                    *)
(*    Note:  for numbers like 6 = 2(2^2-1) = 2(2^1+1), the second     *)
(*    form is preferred, as it requires fewer shifts.                 *)
(*                                                                    *)
(**********************************************************************)

procedure classify_constant(c: integer;   (* non-negative *)
                            var klass: const_classification;
                            var n, m: integer);
var
  twom: integer;
begin
  if c = 0
  then klass := zero
  else if c = 1
  then klass := one
  else
    begin
      n := 0;
      while not odd(c) do
        begin
          n := n + 1;
          c := c div 2
        end (* while *) ;
      if c = 1
      then klass := poweroftwo
      else
        begin
          m := 1;  twom := 2;
          klass := zero (* marker *) ;
          repeat
            if c = twom - 1
            then klass := twomminus1
            else if c = twom + 1
            then klass := twomplus1
            else if twom > c
            then klass := other
            else
              begin
                m := m + 1;
                twom := twom + twom
              end
          until
            klass <> zero
        end
    end
end (* classify_constant *) ;
(**********************************************************************)
(*                                                                    *)
(*                      compile-time arithmetic                       *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                               CT_Add                               *)
(*                                                                    *)
(**********************************************************************)

function ct_add(a, b: integer): integer;
begin
  if (a >= 0) and (b >= 0)
  then
    if max_int - a >= b
    then ct_add := a + b
    else
      begin
        error(this_pass, arith_overflow);
        ct_add := a
      end
  else if (a < 0) and (b < 0)
  then
    if min_int - a <= b
    then ct_add := a + b
    else
      begin
        error(this_pass, arith_overflow);
        ct_add := a
      end
  else ct_add := a + b
end (* ct_add *) ;



(**********************************************************************)
(*                                                                    *)
(*                               CT_Sub                               *)
(*                                                                    *)
(**********************************************************************)

function ct_sub(minuend, subtrahend: integer): integer;
begin
  if (minuend < 0) = (subtrahend < 0)
  then ct_sub := minuend - subtrahend
  else ct_sub := ct_add(minuend, -subtrahend)
end (* ct_sub *) ;



(**********************************************************************)
(*                                                                    *)
(*                               CT_Mul                               *)
(*                                                                    *)
(**********************************************************************)

function ct_mul(a, b: integer): integer;
begin
  if a = 0
  then ct_mul := 0
  else if max_int div abs(a) >= abs(b)
  then ct_mul := a * b
  else
    begin
      error(this_pass, arith_overflow);
      ct_mul := a
    end
end (* ct_mul *) ;



(**********************************************************************)
(*                                                                    *)
(*                               CT_Div                               *)
(*                                                                    *)
(**********************************************************************)

function ct_div(dividend, divisor: integer): integer;
begin
  if divisor = 0
  then
    begin
      error(this_pass, divide_by_zero);
      ct_div := dividend
    end
  else ct_div := dividend div divisor
end (* ct_div *) ;



(**********************************************************************)
(*                                                                    *)
(*                               CT_Mod                               *)
(*                                                                    *)
(**********************************************************************)

function ct_mod(dividend, divisor: integer): integer;
begin
  if divisor = 0
  then
    begin
      error(this_pass, divide_by_zero);
      ct_mod := dividend
    end
  else ct_mod := dividend mod divisor
end (* ct_mod *) ;
(**********************************************************************)
(*                                                                    *)
(*                       comparison procedures                        *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                            Materialize                             *)
(*                                                                    *)
(*    Stack:  relation -> expression                                  *)
(*    Converts a condition code setting to a Boolean in a register.   *)
(*                                                                    *)
(**********************************************************************)

procedure materialize;
var
  opnd: operand;
  mask, reg: reg_range;
  op:   opcode_range;
begin
  pop_operand(opnd);
  ensure(opnd, relation);
  with opnd do
    begin
      case o_r_cond of
        less,
        notless:
          mask := lsmask;
        equal,
        notequal:
          mask := nemask;
        inset,
        greater,
        notgreater:
          mask := grmask
      end (* case *) ;
      if o_r_cond in [notless, equal, notgreater]
      then op := btfs_op
      else op := bffs_op
    end (* with *) ;
  get_reg(1, last_reg, reg);
  gen_sf(op, mask, 3);
  gen_sf(lis_op, reg, 1);
  gen_sf(bffs_op, falsemask, 2);
  gen_sf(lis_op, reg, 0);
  with opnd do
    begin
      o_class := expression;
      o_e_reg := reg;
      o_e_negated := false
    end (* with *) ;
  push_operand(opnd);
  stillcond := false
end   (* materialize *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Reverse                               *)
(*                                                                    *)
(*    If A REL B, then B REVERSE(REL) A.                              *)
(*                                                                    *)
(**********************************************************************)

function reverse(comp: comparison): comparison;
begin
  case comp of
    less:       reverse := greater;
    equal:      reverse := equal;
    greater:    reverse := less;
    notless:    reverse := notgreater;
    notequal:   reverse := notequal;
    notgreater: reverse := notless;
    inset:      error(this_pass, cant_happen)
  end (* case *)
end (* reverse *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Cmp_Int_CC                             *)
(*                                                                    *)
(**********************************************************************)

procedure cmp_int_cc(left, right: integer;
                     comp: comparison;
                     var result: operand);
begin
  with result do
    begin
      o_class := constant;
      case comp of
        less:           o_c_value := ord(left < right);
        equal:          o_c_value := ord(left = right);
        greater:        o_c_value := ord(left > right);
        notless:        o_c_value := ord(left >= right);
        notequal:       o_c_value := ord(left <> right);
        notgreater:     o_c_value := ord(left <= right);
        inset:          error(this_pass, cant_happen)
      end (* case *)
    end (* with *)
end (* cmp_int_cc *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Cmp_Int_EC                             *)
(*                                                                    *)
(**********************************************************************)

procedure cmp_int_ec(expr, cnst: operand;
                     comp: comparison;
                     var result: operand);
var
  reg: reg_range;
begin
  if expr.o_e_negated 
  then
    begin
      load_destructible(expr, reg);
      gen_ri1(xhi_op, reg, 1, 0)
    end;
  gen_immediate(ci_op, expr.o_e_reg, cnst.o_c_value);
  dec_ref(expr.o_e_reg);
  with result do
    begin
      o_class := relation;
      o_r_cond := comp
    end (* with *) ;
  stillcond := true
end (* cmp_int_ec *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Cmp_Int_VC                             *)
(*                                                                    *)
(**********************************************************************)

procedure cmp_int_vc(var vrbl, cnst: operand;
                     comp: comparison;
                     typ: types;
                     var result: operand);
var
  op: opcode_range;
  reg: reg_range;
begin
  if (abs(cnst.o_c_value) < 16) and
     (typ <> byte_type) and
     not vrbl.o_v_negated
  then
    begin
      case typ of
        hw_type: op := ch_op;
        fw_type: op := c_op
      end (* case *) ;
      load(cnst, reg);
      gen_code(op, reg, vrbl);
      dec_ref(reg);
      with result do
        begin
          o_class := relation;
          o_r_cond := reverse(comp)
        end (* with *) ;
      stillcond := true
    end
  else
    begin
      load(vrbl, reg);
      cmp_int_ec(vrbl, cnst, comp, result)
    end
end (* cmp_int_vc *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Cmp_Int_EE                             *)
(*                                                                    *)
(**********************************************************************)

procedure cmp_int_ee(left, right: operand;
                     comp: comparison;
                     var result: operand);
var
  reg: reg_range;
begin
  if left.o_e_negated
  then
    begin
      load_destructible(left, reg);
      gen_ri1(xhi_op, reg, 1, 0)
    end;
  if right.o_e_negated
  then
    begin
      load_destructible(right, reg);
      gen_ri1(xhi_op, reg, 1, 0)
    end;
  gen_rr(cr_op, left.o_e_reg, right.o_e_reg);
  dec_ref(left.o_e_reg);  dec_ref(right.o_e_reg);
  with result do
    begin
      o_class := relation;
      o_r_cond := comp
    end (* with *) ;
  stillcond := true
end (* cmp_int_ee *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Cmp_Int_EV                             *)
(*                                                                    *)
(**********************************************************************)

procedure cmp_int_ev(var expr, vrbl: operand;
                     comp: comparison;
                     typ: types;
                     var result: operand);
var
  op: opcode_range;
  reg: reg_range;
begin
  if (typ = byte_type) or vrbl.o_v_negated
  then
    begin
      load(vrbl, reg);
      cmp_int_ee(expr, vrbl, comp, result)
    end
  else
    begin
      if expr.o_e_negated
      then
        begin
          load_destructible(expr, reg);
          gen_ri1(xhi_op, reg, 1, 0)
        end;
      case typ of
        hw_type: op := ch_op;
        fw_type: op := c_op
      end (* case *) ;
      gen_code(op, expr.o_e_reg, vrbl);
      dec_ref(expr.o_e_reg);
      with result do
        begin
          o_class := relation;
          o_r_cond := comp
        end (* with *) ;
      stillcond := true
    end
end (* cmp_int_ev *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Cmp_Int                               *)
(*                                                                    *)
(*    Stack:  value X value -> relation                               *)
(*                                                                    *)
(**********************************************************************)

procedure cmp_int(comp: comparison;  typ: types);
var
  left, right, result: operand;
  reg: reg_range;
begin
  pop_operand(right);  pop_operand(left);
  if not ([left.o_class, right.o_class] <=
          [constant, expression, variable])
  then
    begin
      error(this_pass, cant_happen);
      dummy_operand(result, relation)
    end
  else
    case left.o_class of
      constant:
        case right.o_class of
          constant:
            cmp_int_cc(left.o_c_value, right.o_c_value, comp, result);
          expression:
            cmp_int_ec(right, left, reverse(comp), result);
          variable:
            cmp_int_vc(right, left, reverse(comp), typ, result)
        end (* case right.o_class *) ;
      expression:
        case right.o_class of
          constant:
            cmp_int_ec(left, right, comp, result);
          expression:
            cmp_int_ee(left, right, comp, result);
          variable:
            cmp_int_ev(left, right, comp, typ, result)
        end (* case right.o_class *) ;
      variable:
        case right.o_class of
          constant:
            cmp_int_vc(left, right, comp, typ, result);
          expression:
            cmp_int_ev(right, left, reverse(comp), typ, result);
          variable:
            begin
              load(left, reg);
              cmp_int_ev(left, right, comp, typ, result)
            end
        end (* case right.o_class *)
    end (* case left.o_class *) ;
  push_operand(result)
end (* cmp_int *) ;



(**********************************************************************)
(*                                                                    *)
(*                          Set_Membership                            *)
(*                                                                    *)
(*    Stack:  value X (set) variable -> relation                      *)
(*                                                                    *)
(**********************************************************************)

procedure set_membership;
var
  left, right, result: operand;
  reg: work_reg;
  save_class: operand_class;
begin
  pop_operand(right);
  ensure(right, variable);
  pop_operand(left);
  save_class := left.o_class;
  load(left, reg);
  if check
  then gen_rangecheck(reg, max_set);
  if temporary(right)
     then deallocate_temp(set_size);
  gen_code(tbt_op, reg, right);
  dec_ref(reg);
  with result do
    begin
      o_class := relation;
      o_r_cond := greater (* tbt sets the G bit *)
    end;
  push_operand(result);
  stillcond := true
end (* set_membership *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Set_Relop                              *)
(*                                                                    *)
(*    Stack:  (set)variable X (set)variable -> (Boolean)expression    *)
(*    Assumes that a set occupies an integral number of fullwords.    *)
(*                                                                    *)
(**********************************************************************)

procedure set_relop(comp: comparison);
var
  left, right, result: operand;
  resulte, resultn: 0 .. 1;
  jump, templength: integer;
  ix, temp: work_reg;
begin
  templength := 0;
  pop_operand(right);
  ensure(right, variable);
  if temporary(right)
  then templength := templength + set_size;
  pop_operand(left);
  ensure(left, variable);
  if temporary(left)
  then templength := templength + set_size;
  if templength > 0
  then deallocate_temp(templength);
  unindex(left);  unindex(right);
  get_reg(1, last_reg, ix);
  get_reg(first_reg, last_reg, temp);
  gen_immediate(li_op, ix, set_size-fw_size);
  case comp of
    equal,
    notequal:
      begin
        with left do
          gen_rx3(l_op, temp, o_v_disp, o_v_base, ix);
        with right do
          gen_rx3(cl_op, temp, o_v_disp, o_v_base, ix);
        jump := 8;
        resultn := ord(comp = notequal);
        resulte := ord(comp = equal)
      end;
    notgreater:
      begin
        with left do
          gen_rx3(l_op, temp, o_v_disp, o_v_base, ix);
        with right do
          gen_rx3(n_op, temp, o_v_disp, o_v_base, ix);
        with left do
          gen_rx3(x_op, temp, o_v_disp, o_v_base, ix);
        jump := 11;  resultn := 0;  resulte := 1
      end;
    notless:
      begin
        with right do
          gen_rx3(l_op, temp, o_v_disp, o_v_base, ix);
        with left do
          gen_rx3(n_op, temp, o_v_disp, o_v_base, ix);
        with right do
          gen_rx3(x_op, temp, o_v_disp, o_v_base, ix);
        jump := 11;  resultn := 0;  resulte := 1
      end
  end (* case *) ;
  gen_sf(btfs_op, nemask, 5);
  gen_sf(sis_op, ix, fw_size);
  gen_sf(bfbs_op, lsmask, jump);
        (* here if all equal *)
  gen_sf(lis_op, ix, resulte);
  gen_sf(bffs_op, falsemask, 2);
        (* here if not all equal *)
  gen_sf(lis_op, ix, resultn);
  dec_ref(temp);
  free_regs(left);  free_regs(right);
  with result do
    begin
      o_class := expression;
      o_e_reg := ix;
      o_e_negated := false
    end (* with *) ;
  push_operand(result)
end   (* set_relop *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Compare                               *)
(*                                                                    *)
(**********************************************************************)

procedure compare(comp: comparison;  typ: types);
begin
  case typ of
    byte_type,
    hw_type,
    fw_type:
      cmp_int(comp, typ);
    real_type:
      error(this_pass, not_imp_error);
    set_type:
      if comp = inset
      then set_membership
      else set_relop(comp)
  end   (* case *)
end   (* compare *) ;



(**********************************************************************)
(*                                                                    *)
(*                            CompareStruc                            *)
(*                                                                    *)
(*    Stack:  address X address -> value                              *)
(*                                                                    *)
(**********************************************************************)

procedure comparestruc(comp: comparison;  length (*in bytes*): integer);
var
  left, right, result: operand;
  resulte, resultn, resultg, resultl: 0 .. 1;
  jump: integer;
  ixreg, temp, compreg: reg_range;
  loader, comper: integer;
  size: integer;
begin
  case comp of
    less:
      begin
        resultl := 1;  resulte := 0;  resultg := 0;  jump := 5
      end;
    equal:
      begin
        resulte := 1;  resultn := 0;  jump := 2
      end;
    greater:
      begin
        resultg := 1;  resulte := 0;  resultl := 0;  jump := 5
      end;
    notless:
      begin
        resultl := 0;  resulte := 1;  resultg := 1;  jump := 5
      end;
    notequal:
      begin
        resulte := 0;  resultn := 1;  jump := 2
      end;
    notgreater:
      begin
        resultg := 0;  resulte := 1;  resultl := 1;  jump := 5
      end
  end   (* case *) ;
  if length mod 4 = 0
  then begin
    size := fw_size;
    loader := l_op;
    comper := cl_op
  end
  else begin
    size := hw_size;
    loader := lh_op;
    comper := clh_op
  end;
  pop_operand(right);  pop_operand(left);
  force_address(left);  unindex(left);
  force_address(right);  unindex(right);
  get_reg(first_reg, last_reg, temp);
  get_reg(1, last_reg, ixreg);
  if length > size
  then begin
    gen_immediate(li_op, ixreg, -length);
    with left do
      gen_rx( loader, temp, o_a_disp+length, o_a_base, ixreg);
    with right do
      gen_rx(comper, temp, o_a_disp+length, o_a_base, ixreg);
    gen_sf(btfs_op, nemask, 5);
    gen_sf(ais_op, ixreg, size);
    gen_sf(btbs_op, lsmask, 8)
  end
  else begin
    with left do
      gen_rx( loader, temp, o_a_disp, o_a_base, r0);
    with right do
      gen_rx(comper, temp, o_a_disp, o_a_base, r0);
    gen_sf(btfs_op, nemask, 3)
  end;
    (* here if structures are all equal *)
  gen_sf(lis_op, ixreg, resulte);
  gen_sf(bffs_op, falsemask, jump);
    (* here if not all equal *)
  if comp in [equal, notequal]
  then gen_sf(lis_op, ixreg, resultn)
  else
    begin
      gen_sf(btfs_op, lsmask, 3);
        (* here if left structure greater *)
      gen_sf(lis_op, ixreg, resultg);
      gen_sf(bffs_op, falsemask, 2);
        (* here if left structure less *)
      gen_sf(lis_op, ixreg, resultl)
    end;
  free_regs(left);  free_regs(right);  dec_ref(temp);
  with result do
    begin
      o_class := expression;
      o_e_reg := ixreg;
      o_e_negated := false
    end (* with *) ;
  push_operand(result)
end   (* comparestruc *) ;
(**********************************************************************)
(*                                                                    *)
(*                          table procedures                          *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                              New_Zero                              *)
(*                                                                    *)
(*    Allocates a TABLE record and zeroes its CONTENTS array.         *)
(*                                                                    *)
(**********************************************************************)

procedure new_zero(var t: tableptr);
var
  i: 1 .. maxword;
begin
  new(t);
  with t^ do
    for i := 1 to maxword do
      contents[i] := 0
end;



(**********************************************************************)
(*                                                                    *)
(*                              Allocate                              *)
(*                                                                    *)
(*    Allocates and zeroes a table large enough to contain ENTRIES    *)
(*    entries.                                                        *)
(*                                                                    *)
(**********************************************************************)

procedure allocate(var t: tableptr;  entries: integer);
var
  i: integer;
  portion: tableptr;
begin
  new_zero(t);  portion := t;
  i := entries - maxword;
  while i > 0 do
    with portion^ do
      begin
        new_zero(nextportion);  portion := nextportion;
        i := i - maxword
      end
end   (* allocate *) ;



(**********************************************************************)
(*                                                                    *)
(*                               Enter                                *)
(*                                                                    *)
(*    Enters an item in a table.                                      *)
(*                                                                    *)
(**********************************************************************)

procedure enter(t: tableptr;  index, value: integer);
var
  k: integer;
  portion: tableptr;
begin
  portion := t;  k := index;
  while k > maxword do
    begin
      portion := portion^.nextportion;
      k := k - maxword
    end;
  portion^.contents[k] := value
end   (* enter *) ;



(**********************************************************************)
(*                                                                    *)
(*                               Entry                                *)
(*                                                                    *)
(*    Retrieves an item from a table.                                 *)
(*                                                                    *)
(**********************************************************************)

function entry(t: tableptr;  index: integer): integer;
var
  j: integer;
  portion: tableptr;
begin
  if index = 0   (* reference to undefined routine *)
  then entry := 0
  else
    begin
      portion := t;  j := index;
      while j > maxword do
        begin
          portion := portion^.nextportion;
          j := j - maxword
        end;
      entry := portion^.contents[j]
    end
end   (* entry *) ;
(**********************************************************************)
(*                                                                    *)
(*                          line procedures                           *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                              InitLine                              *)
(*                                                                    *)
(**********************************************************************)

procedure initline;
begin
  line := 0;
  afterbegin := false
end;



(**********************************************************************)
(*                                                                    *)
(*                              NewLine                               *)
(*                                                                    *)
(**********************************************************************)

procedure newline(lno: integer);
var
  reg: reg_range;
begin
  line := lno;
  aftererror := false;
  if afterbegin and (peek_ifl<>newline1) AND (not justnewline)
  then if number
  then begin
    get_reg(r0, r0, reg);
    gen_immediate(li_op, reg, lno);
    gen_rx1(sth_op, reg, hw_size, b_reg);
    dec_ref(reg);
  end
  else listline(lno, location+obj_prefix_len);
  justnewline := true
end   (* newline *) ;
(**********************************************************************)
(*                                                                    *)
(*                      routine entry procedures                      *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             EnterBlock                             *)
(*                                                                    *)
(*    Bookkeeping for routine entry.                                  *)
(*                                                                    *)
(**********************************************************************)

procedure enterblock(mode: modes; lab, paraml, varl, templ: integer);
begin
  curmode := mode;
  block := lab;
  paramlength := paraml;
  varlength := varl;
  initialstack := paraml + varl + base_size;
  stacklength := templ;
  implicitlength := r_impl_size(mode);
  linklength := r_link_size(mode);
  temp_origin := initialstack + linklength + implicitlength;
  temp := 0;  maxtemp := 0; 
  enter(blocktable, lab, location);
    (* Initial block is referenced by the first jump instruction      *)
    (* in the object file, but not by any call or init instruction.   *)
  afterbegin := true
end   (* enterblock *) ;



(**********************************************************************)
(*                                                                    *)
(*                              EnterCom                              *)
(*                                                                    *)
(*    Common code for routine entry.                                  *)
(*                                                                    *)
(**********************************************************************)

procedure entercom;
begin
  gen_rx1(stm_op, g_reg, fw_size, s_reg);
  gen_rr(lr_op, b_reg, s_reg);
  gen_immediate(ai_op, s_reg, linklength+implicitlength+initialstack)
end   (* entercom *) ;



(**********************************************************************)
(*                                                                    *)
(*                               Enter_                               *)
(*                                                                    *)
(**********************************************************************)

procedure enter_(mode: modes;
                 lab, paramlength, varlength, templength: integer);
var
  arg_disp, junk, leng, chars, i: integer;

procedure entername;
begin
  if peek_ifl<>constant1
  then error(this_pass, cant_happen)
  else begin
    read_ifl(junk);
    read_ifl(leng);
    read_ifl(chars); (* the first pair gets marked *)
    gen_data(chars+"8000);
    for i := 2 to leng div 2 do begin
      read_ifl(chars);
      gen_data(chars)
    end;
  end;
end;

begin (* enter_ *)
  entername;
  enterblock(mode, lab, paramlength, varlength, templength);
  arg_disp := base_size + linklength;
  case mode of
    proc_mode:          (* proc/func entry *)
      entercom;
    prog_mode:          (* sequential program entry *)
      begin
        gen_sf(lis_op, r0, 1);
        gen_rx1(ahm_op, r0, pcb_job1, hd_reg);
        entercom;
        gen_rr(lr_op, g_reg, b_reg)
      end;
    pe_mode:            (* interface proc/func entry *)
      begin
        entercom;
        gen_rx1(l_op, g_reg, fw_size, g_reg);
        gen_sf(lis_op, r0, 0);
        gen_rx1(sth_op, r0, pcb_job1, hd_reg)
      end;
    ce_mode,            (* class proc/func entry *)
    class_mode:         (* class initialization entry *)
      begin
        entercom;
        gen_rx1(l_op, g_reg, arg_disp, b_reg)
      end;
    me_mode:            (* monitor proc/func entry *)
      begin
        entercom;
        gen_rx1(l_op, g_reg, arg_disp, b_reg);
        gen_extref(rts_entergate)
      end;
    process_mode:       (* concurrent process initialization entry *)
      begin
        gen_immediate(li_op, r0, line);
        gen_rx1(st_op, r0, 0, b_reg)
      end;
    monitor_mode:       (* monitor initialization entry *)
      begin
        entercom;
        gen_rx1(l_op, g_reg, arg_disp, b_reg);
        gen_extref(rts_initgate)
      end;
    undef_mode:
      (* ignore *)
  end   (* case *)
end   (* enter_ *) ;
(**********************************************************************)
(*                                                                    *)
(*                      routine exit procedures                       *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             ExitBlock                              *)
(*                                                                    *)
(*    Bookkeeping for block exit.                                     *)
(*                                                                    *)
(**********************************************************************)

procedure exitblock;
begin
  if stacklength < stack_limit - initialstack - linklength -
                   implicitlength - maxtemp
  then stacklength := stacklength + initialstack + linklength +
                      implicitlength + maxtemp
  else error(this_pass, stack_error);
  enter(stacktable, block, stacklength);
  curmode := undef_mode;
  afterbegin := false
end   (* exitblock *) ;



(**********************************************************************)
(*                                                                    *)
(*                              ExitCom                               *)
(*                                                                    *)
(*    Common code for routine exit.                                   *)
(*                                                                    *)
(**********************************************************************)

procedure exitcom;
begin
  gen_rr(lr_op, s_reg, b_reg);
  gen_rx1(lm_op, g_reg, fw_size, s_reg);
  gen_rr(bfcr_op, falsemask, q_reg)
end   (* exitcom *) ;



(**********************************************************************)
(*                                                                    *)
(*                               Return                               *)
(*                                                                    *)
(**********************************************************************)

procedure return(mode: modes);
begin
  case mode of
    proc_mode,          (* proc/func return *)
    ce_mode,            (* class proc/func return *)
    class_mode:         (* class initialization return *)
      exitcom;
    prog_mode:          (* sequential program termination *)
      gen_extref(rts_terminate);
    pe_mode:            (* interface proc/func return *)
      begin
        gen_sf(lis_op, r0, 1);
        gen_rx1(ahm_op, r0, pcb_job1, hd_reg);
        exitcom
      end;
    me_mode,            (* monitor proc/func return *)
    monitor_mode:       (* monitor initialization return *)
      begin
        gen_extref(rts_leavegate);
        exitcom
      end;
    process_mode:       (* concurrent process termination *)
      gen_extref(rts_exitproc);
    undef_mode:
      (* ignore *)
  end   (* case *) ;
  exitblock
end   (* return *) ;
(**********************************************************************)
(*                                                                    *)
(*                          call procedures                           *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                              Gen_Call                              *)
(*                                                                    *)
(*    Generates a position-independent branch-and-link to the         *)
(*    given routine.                                                  *)
(*                                                                    *)
(**********************************************************************)

procedure gen_call(block: integer);
var
  target: integer;
begin
  target := entry(blocktable, block);
  if
    (target > 0)   (* defined *)   and
    (location - target < "3FFC)   (* reachable by RX2 *)
  then
    gen_rx2(bal_op, q_reg, target-location-rx2_length, 0)
  else
    begin
      gen_offset(q_reg, block)   (* li q_reg,target-location *) ;
      gen_rx2(bal_op, q_reg, -rx2_length, q_reg)
    end
end (* gen_call *) ;



(**********************************************************************)
(*                                                                    *)
(*                            Gen_Address                             *)
(*                                                                    *)
(*    Generates position-independent code to load the address of      *)
(*    the given routine into R0.                                      *)
(*                                                                    *)
(**********************************************************************)

procedure gen_address(block: integer);
var
  target: integer;
begin
  target := entry(blocktable, block);
  if
    (target > 0)   (* defined *)   and
    (location - target < "3FFC)   (* reachable by RX2 *)
  then
    gen_rx2(la_op, r0, target-location-rx2_length, 0)
  else
    begin
      gen_offset(q_reg, block)   (* li q_reg,target-location *) ;
      gen_rx2(la_op, r0, -rx2_length, q_reg)
    end
end (* gen_address *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Save                                *)
(*                                                                    *)
(*    Generates code to save a register in the stack.                 *)
(*    Uses global variable SaveLength.                                *)
(*                                                                    *)
(**********************************************************************)

procedure save(reg: work_reg);
begin
  gen_rx1(st_op, reg, savelength, s_reg);
  savelength := savelength + fw_size
end (* save *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Save_Regs                              *)
(*                                                                    *)
(*    Generates code to stack the registers referenced by OPND.       *)
(*                                                                    *)
(**********************************************************************)

procedure save_regs(opnd: operand);
begin
  with opnd do
    case o_class of
      address:
        begin
          if not (o_a_base in [b_reg, g_reg])
          then save(o_a_base);
          if o_a_ixreg <> 0
          then save(o_a_ixreg)
        end;
      constant,
      relation:
        ;
      expression:
        save(o_e_reg);
      variable:
        begin
          if not (o_v_base in [b_reg, g_reg])
          then save(o_v_base);
          if o_v_ixreg <> 0
          then save(o_v_ixreg)
        end
    end (* case, with *)
end (* save_regs *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Restore                               *)
(*                                                                    *)
(*    Generates code to restore REG from the stack.                   *)
(*    Uses global variable SaveLength.                                *)
(*                                                                    *)
(**********************************************************************)

procedure restore(reg: work_reg);
begin
  gen_rx1(l_op, reg, savelength, s_reg);
  savelength := savelength + fw_size
end (* restore *) ;



(**********************************************************************)
(*                                                                    *)
(*                            Restore_Regs                            *)
(*                                                                    *)
(*    Generates code to unstack the registers referenced by OPND.     *)
(*                                                                    *)
(**********************************************************************)

procedure restore_regs(opnd: operand);
begin
  with opnd do
    case o_class of
      address:
        begin
          if not (o_a_base in [b_reg, g_reg])
          then restore(o_a_base);
          if o_a_ixreg <> 0
          then restore(o_a_ixreg)
        end;
      constant,
      relation:
        ;
      expression:
        restore(o_e_reg);
      variable:
        begin
          if not (o_v_base in [b_reg, g_reg])
          then restore(o_v_base);
          if o_v_ixreg <> 0
          then restore(o_v_ixreg)
        end
    end (* case, with *)
end (* restore_regs *) ;



(**********************************************************************)
(*                                                                    *)
(*                          Count_Arguments                           *)
(*                                                                    *)
(*    Determines the number of arguments by backing down the stack    *)
(*    adding up lengths until it has seen enough.  This procedure     *)
(*    is necessary because the existence of value set arguments       *)
(*    means that PARAMLENGTH <> FW_SIZE * NPARMS in general.          *)
(*                                                                    *)
(**********************************************************************)

procedure count_arguments(paramlength: integer;
                          var nparms: integer);
var
  i, p: integer;
  opnd: operand;
begin
  p := 0;  nparms := 0;  i := stack_depth;
  while p < paramlength do
    begin
      nparms := nparms + 1;  i := i - 1;
      get_operand(i, opnd);
      with opnd do
        if o_class = variable
        then
          if o_v_type = set_type
          then p := p + set_size
          else p := p + fw_size
        else p := p + fw_size
    end (* while *) ;
  if p <> paramlength
  then error(this_pass, cant_happen)
end (* count_arguments *) ;



(**********************************************************************)
(*                                                                    *)
(*                           Pass_Arguments                           *)
(*                                                                    *)
(*    Passes NARGS arguments, which should include the implicit       *)
(*    argument if appropriate.  Accumulates size of set temporaries   *)
(*    in TEMPLENGTH.                                                  *)
(*                                                                    *)
(**********************************************************************)

procedure pass_arguments(nargs, arglength: integer;
                         base_reg: reg_range;
                         var templength: integer);
var
  i, offset: integer;
  ix, temp: work_reg;
  opnd: operand;
  set_arg: Boolean;
begin
  offset := arglength;  templength := 0;
  for i := 1 to nargs do
    begin
      pop_operand(opnd);
      with opnd do
        if o_class = variable
        then set_arg := o_v_type = set_type
        else set_arg := false;
      if set_arg
      then
        begin
          offset := offset - set_size;
          if temporary(opnd)
          then templength := templength + set_size;
          unindex(opnd);
          get_reg(first_reg, last_reg, temp);
          get_reg(1, last_reg, ix);
          gen_immediate(li_op, ix, set_size-fw_size);
          with opnd do
            gen_rx3(l_op, temp, o_v_disp, o_v_base, ix);
          gen_rx3(st_op, temp, offset, base_reg, ix);
          gen_sf(sis_op, ix, fw_size);
          gen_sf(bfbs_op, lsmask, 7);
          dec_ref(temp);  dec_ref(ix);
          free_regs(opnd)
        end
      else (* not set argument *)
        begin
          offset := offset - fw_size;
          load(opnd, temp);
          gen_rx1(st_op, temp, offset, base_reg);
          dec_ref(temp)
        end
    end (* for *)
end (* pass_arguments *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Call                                *)
(*                                                                    *)
(*    Generate calls on regular proc/funcs, interface proc/funcs,     *)
(*    and monitor/class proc/funcs.  Note:                            *)
(*      NTEMPS includes the function result, if any                   *)
(*      NPARMS includes the implicit zeroth argument, if any          *)
(*      function result is popped last, setting condition code        *)
(*    Uses global variables SaveLength and Stack_Depth.               *)
(*                                                                    *)
(**********************************************************************)

procedure call(mode: modes;  lab, paramlength: integer);
var
  opnd: operand;
  reg, temp: reg_range;
  i, nparms, ntemps, overhead, pl, stackreq, templength: integer;
begin
  overhead := base_size + r_link_size(mode);
  pl := paramlength;
  count_arguments(paramlength, nparms);
  if mode in [ce_mode, me_mode]
  then   (* implicit zeroth parameter *)
    begin
      nparms := nparms + 1;
      pl := pl + fw_size
    end;
  ntemps := stack_depth - nparms;
  
  (* save temps if necessary *)

  savelength := 0;
  for i := 0 to ntemps-1 do
    begin
      get_operand(i, opnd);
      save_regs(opnd)
    end (* for *) ;
  if savelength > 0
  then begin
    gen_immediate(ai_op, s_reg, savelength);
  end;
  if concurrent
  then stackreq := savelength + entry(stacktable, lab)
  else stackreq := savelength + overhead + paramlength;
  push_stack(stackreq);

  (* copy args into stack beyond linkage area *)

  pass_arguments(nparms, overhead + pl, s_reg, templength);

  (* call the routine *)

  if mode = pe_mode
  then (* interface routine *)
    begin
      get_reg(1, last_reg, reg);
      gen_immediate(li_op, reg, -fw_size*(lab-1));
      dec_ref(reg);
      get_reg(first_reg, last_reg, temp);
      gen_rx3(l_op, temp, 0, g_reg, reg);
      dec_ref(temp);
      gen_rr(balr_op, q_reg, temp)
    end
  else
    gen_call(lab);

  (* restore temps if necessary *)

  pop_stack(stackreq);
  if savelength > 0
  then gen_immediate(si_op, s_reg, savelength);
  savelength := 0;
  for i := 0 to ntemps-1 do
    begin
      get_operand(i, opnd);
      restore_regs(opnd)
    end (* for *) ;

(*    free set temps if necessary    *)

  if templength > 0
  then deallocate_temp(templength)
end   (* call *) ;



(**********************************************************************)
(*                                                                    *)
(*                              CallProg                              *)
(*                                                                    *)
(*    Generates call on sequential program.                           *)
(*    Assertion:  there is always at least one argument (the code     *)
(*    address).                                                       *)
(*                                                                    *)
(**********************************************************************)

procedure callprog;
var
  code_addr, opnd: operand;
  reg: reg_range;
  i, nparms, offset, pl, stackreq: integer;
begin
  offset := base_size + r_link_size(prog_mode);
  nparms := stack_depth - saved_temps;
  pl := 0;
  for i := saved_temps to stack_depth - 1 do
    begin
      get_operand(i, opnd);
      with opnd do
        if o_class = variable
        then
          if o_v_type = set_type
          then pl := pl + set_size
          else pl := pl + fw_size
        else pl := pl + fw_size
    end (* for *) ;
  stackreq := pl + offset;
  push_stack(stackreq);

  (* pass implicit argument *)

  pop_operand(code_addr);  force_address(code_addr);
  get_reg(first_reg, last_reg, reg);
  with code_addr do
    begin
      gen_rx(la_op, reg, o_a_disp+obj_prefix_len, o_a_base, o_a_ixreg);
      gen_rx( a_op, reg, o_a_disp+obj_codelength, o_a_base, o_a_ixreg);
      gen_rx1(st_op, reg, offset, s_reg)
    end;
  dec_ref(reg);

  (* pass explicit arguments *)

  pass_arguments(nparms - 1, stackreq, s_reg, prog_set_temps);

  (* call the program *)

  with code_addr do
    gen_rx(bal_op, q_reg, o_a_disp+obj_prefix_len, o_a_base, o_a_ixreg);
  free_regs(code_addr);
  pop_stack(stackreq)
end   (* callprog *) ;



(**********************************************************************)
(*                                                                    *)
(*                              InitProc                              *)
(*                                                                    *)
(*    Uses global variables SaveLength and Stack_Depth.               *)
(*                                                                    *)
(**********************************************************************)

procedure initproc(lab, paramlength, varlength: integer);
var
  opnd: operand;
  reg: reg_range;
  i, nparms, ntemps, pl, stackreq, templength: integer;
begin
  count_arguments(paramlength, nparms);
  nparms := nparms + 1   (* implicit process variable *) ;
  pl := paramlength + fw_size;
  ntemps := stack_depth - nparms;
  
  (* save temps if necessary *)

  savelength := 0;
  for i := 0 to ntemps-1 do
    begin
      get_operand(i, opnd);
      save_regs(opnd)
    end (* for *) ;

(*    push arguments onto stack    *)

  stackreq := savelength + pl;
  pass_arguments(nparms, stackreq, s_reg, templength);
  gen_immediate(ai_op, s_reg, stackreq);
  push_stack(stackreq);

  (* load registers with INITPROC parameters and invoke *)

  gen_address(lab);
  gen_immediate(li_op, r1, paramlength);
  gen_immediate(li_op, r2, varlength);
  gen_initproc(lab);
  gen_extref(rts_initproc);

  (* restore temps if necessary *)

  pop_stack(stackreq);
  if savelength > 0
  then gen_immediate(si_op, s_reg, savelength);
  savelength := 0;
  for i := 0 to ntemps-1 do
    begin
      get_operand(i, opnd);
      restore_regs(opnd)
    end (* for *) ;

(*    free set temps if necessary    *)

  if templength > 0
  then deallocate_temp(templength);
end   (* initproc *) ;



(**********************************************************************)
(*                                                                    *)
(*                            InitMonClass                            *)
(*                                                                    *)
(*    Uses global variables SaveLength and Stack_Depth.               *)
(*                                                                    *)
(**********************************************************************)

procedure initmonclass(mode: modes;
                       lab, paramlength, varlength: integer);
var
  addr_reg, reg: reg_range;
  opnd: operand;
  i, nparms, ntemps, stackreq, templength: integer;
begin
  count_arguments(paramlength, nparms);
  ntemps := stack_depth - nparms - 1 (* mon/class address *) ;

  (* save temps if necessary *)

  savelength := 0;
  for i := 0 to ntemps-1 do
    begin
      get_operand(i, opnd);
      save_regs(opnd)
    end (* for *) ;
  if savelength > 0
  then gen_immediate(ai_op, s_reg, savelength);
  stackreq := savelength + entry(stacktable, lab);
  push_stack(stackreq);

  (* pass the monitor/class address (implicit parameter) *)

  get_operand(ntemps, opnd);
  load(opnd, addr_reg);
  gen_rx1(st_op, addr_reg, base_size+r_link_size(mode), s_reg);

  (* store args in class/mon permanent data area *)

  pass_arguments(nparms,
                 base_size + v_link_size(mode) + v_impl_size(mode) +
                 paramlength,
                 addr_reg,
                 templength);

  (*    call the initial statement    *)

  gen_call(lab);
  pop_operand(opnd)   (* remove mon/class address from stack *) ;
  dec_ref(addr_reg);
  pop_stack(stackreq);

  (* restore temps if necessary *)

  if savelength > 0
  then gen_immediate(si_op, s_reg, savelength);
  savelength := 0;
  for i := 0 to ntemps-1 do
    begin
      get_operand(i, opnd);
      restore_regs(opnd)
    end (* for *) ;

(*    free set temps if necessary    *)

  if templength > 0
  then deallocate_temp(templength)
end   (* initmonclass *) ;



(**********************************************************************)
(*                                                                    *)
(*                             FuncValue                              *)
(*                                                                    *)
(*    Stack:  nil -> value                                            *)
(*       or:  address -> value X address (monitor/class entry)        *)
(*                                                                    *)
(**********************************************************************)

procedure funcvalue(mode: modes;  typ: types);
var
  expr, addr: operand;
  reg: reg_range;
begin
  case typ of
    byte_type,
    hw_type,
    fw_type:
      case mode of
        proc_mode,
        pe_mode,
        ce_mode,
        me_mode:
          begin
            get_reg(1, last_reg, reg);
            gen_sf(lis_op, reg, 0);
            with expr do
              begin
                o_class := expression;
                o_e_reg := reg;
                o_e_negated := false
              end (* with *) ;
            if mode in [ce_mode, me_mode]
            then pop_operand(addr);
            push_operand(expr);
            if mode in [ce_mode, me_mode]
            then push_operand(addr)
          end;
        std_mode,
        undef_mode:
          (* ignore *)
      end   (* case mode *) ;
    real_type:
      error(this_pass, not_imp_error)
  end   (* case typ *)
end   (* funcvalue *) ;



(**********************************************************************)
(*                                                                    *)
(*                             PushLabel                              *)
(*                                                                    *)
(*    Uses global variables Calling_Program, Saved_Temps,             *)
(*    SaveLength, and Stack_Depth.                                    *)
(*                                                                    *)
(**********************************************************************)

procedure pushlabel(lab: integer);
var
  opnd: operand;
  reg: reg_range;
  i: integer;
begin
  if not calling_program
  then   (* this is the first PUSHLABEL in the series *)
    begin
      calling_program := true;
      saved_temps := stack_depth;
      savelength := 0;
      for i := 0 to saved_temps-1 do
        begin
          get_operand(i, opnd);
          save_regs(opnd)
        end (* for *) ;
      if savelength > 0
      then
        begin
          gen_immediate(ai_op, s_reg, savelength);
          push_stack(savelength)
        end
    end;
  get_reg(r0, r0, reg);
  gen_address(lab);
  gen_rx1(st_op, r0, 0, s_reg);
  gen_immediate(ai_op, s_reg, fw_size);
  push_stack(fw_size);
  dec_ref(reg)
end   (* pushlabel *) ;
(**********************************************************************)
(*                                                                    *)
(*                 standard procedures and functions                  *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                            Call_Support                            *)
(*                                                                    *)
(*    Uses global variable SaveLength.                                *)
(*                                                                    *)
(**********************************************************************)

procedure call_support(index, nparms: integer);
var
  opnd: operand;
  reg, temp: reg_range;
  i, ntemps: integer;
begin

  (*    save temps if necessary    *)

  ntemps := stack_depth - nparms;
  savelength := 0;
  for i := 0 to ntemps-1 do
    begin
      get_operand(i, opnd);
      save_regs(opnd)
    end (* for *) ;
  if savelength > 0
  then gen_immediate(ai_op, s_reg, savelength);
  push_stack(savelength);

  (*    load parameters into registers    *)

  for i := 1 to nparms do
    begin
      get_operand(ntemps-1+i, opnd);
      load(opnd,reg);
      if reg <> i
      then
        begin
          get_reg(i, i, temp);
          gen_rr(lr_op, temp, reg);
          dec_ref(reg)
        end
    end (* for *) ;

  (*    call the routine and free parameter registers    *)

  gen_extref(index);
  for i := 1 to nparms do
    begin
      dec_ref(i);
      pop_operand(opnd)   (* clear stack *)
    end;

  (*    restore temps if necessary    *)

  if savelength > 0
  then
    begin
      gen_immediate(si_op, s_reg, savelength);
      pop_stack(savelength)
    end;
  savelength := 0;
  for i := 0 to ntemps-1 do
    begin
      get_operand(i, opnd);
      restore_regs(opnd)
    end (* for *)
end   (* call_support *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Std_Proc                              *)
(*                                                                    *)
(**********************************************************************)

procedure std_proc(index: integer);
var
  opnd: operand;
  temp: reg_range;
begin
  if index in [min_proc .. max_proc]
  then
    case index of
      delay1:
        call_support(rts_delay, 1 (* arg *) );
      continue1:
        begin
          call_support(rts_continue, 1 (* arg *) );
          exitcom
        end;
      io1:
        call_support(rts_io, 4 (* args *) );
      start1:
        begin
          get_reg(first_reg, last_reg, temp);
          gen_sf(lis_op, temp, 1);
          gen_rx1(sth_op, temp, pcb_conti1, hd_reg);
          dec_ref(temp)
        end;
      stop1:
        call_support(rts_stop, 2 (* args *) );
      setheap1:
        begin
          pop_operand(opnd);
          load(opnd, temp);
          gen_rx1(st_op, temp, pcb_heapt1, hd_reg);
          dec_ref(temp)
        end;
      wait1:
        call_support(rts_wait, 0 (* args *) )
    end   (* case *)
end   (* std_proc *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Abs_Int                               *)
(*                                                                    *)
(**********************************************************************)

procedure abs_int(var opnd: operand);
var
  reg: reg_range;
begin
  with opnd do
    case o_class of
      address,
      relation:
        begin
          error(this_pass, cant_happen);
          dummy_operand(opnd, expression)
        end;
      constant:
        o_c_value := abs(o_c_value);
      expression:
        begin
          get_reg(1, last_reg, reg);
          gen_rr(lr_op, reg, o_e_reg);
          gen_sf(bffs_op, lsmask, 3);
          gen_sf(lis_op, reg, 0);
          gen_rr(sr_op, reg, o_e_reg);
          dec_ref(o_e_reg);
          o_e_reg := reg
        end;
      variable:
        begin
          load_destructible(opnd, reg);
          gen_sf(bffs_op, lsmask, 4);
          gen_ri1(xhi_op, reg, "FFFF, 0);
          gen_sf(ais_op, reg, 1)
        end
    end (* case, with *)
end (* abs_int *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Pred_Succ                              *)
(*                                                                    *)
(**********************************************************************)

procedure pred_succ(var opnd: operand;  which: updown);
var
  op: opcode_range;
  reg: reg_range;
begin
  case opnd.o_class of
    address,
    relation:
      begin
        error(this_pass, cant_happen);
        dummy_operand(opnd, expression)
      end;
    constant:
      with opnd do
        case which of
          decrement:  o_c_value := pred(o_c_value);
          increment:  o_c_value := succ(o_c_value)
        end (* case, with *) ;
    expression,
    variable:
      begin
        case which of
          decrement:  op := sis_op;
          increment:  op := ais_op
        end (* case *) ;
        load_destructible(opnd, reg);
        gen_sf(op, reg, 1)
      end
  end (* case *)
end (* pred_succ *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Std_Func                              *)
(*                                                                    *)
(*    Stack:  value -> value                                          *)
(*                                                                    *)
(**********************************************************************)

procedure std_func(index: integer;  typ: types);
var
  opnd: operand;
  reg, reg2: reg_range;
  save_class: operand_class;
begin
  if index in [min_func .. max_func]
  then begin
    case index of
      conv1,
      trunc1:
        begin
        pop_operand(opnd);
        error(this_pass, not_imp_error)
        end;
      abs1:
        begin
        pop_operand(opnd);
        case typ of
          hw_type,
          fw_type:
            abs_int(opnd);
          real_type:
            error(this_pass, not_imp_error)
        end   (* case typ *) ;
        end;
      succ1:
        begin
        pop_operand(opnd);
        pred_succ(opnd, increment)
        end;
      pred1:
        begin
        pop_operand(opnd);
        pred_succ(opnd, decrement)
        end;
      empty1:
        begin
          pop_operand(opnd);
          save_class := opnd.o_class;
          load(opnd, reg);
          if save_class = expression
          then gen_rr(lr_op, reg, reg);
          gen_sf(btfs_op, nemask, 3);
          gen_sf(lis_op, reg, 1);
          gen_sf(bffs_op, falsemask, 2);
          gen_sf(lis_op, reg, 0)
        end;
      attribute1:
        begin
          pop_operand(opnd);
          load_destructible(opnd, reg);
          gen_sf(slls_op, reg, 2)   (* make fw index *) ;
          gen_rx3(l_op, reg, 0, hd_reg, reg)
        end;
      realtime1:   (* a zero-argument function *)
        begin
          get_reg(r0, r0, reg);
          gen_extref(rts_realtime);
	  get_reg(r1, last_reg, reg2); (* we don't want expressions in r0 *)
	  gen_sf(lr_op, reg2, reg);
	  dec_ref(reg);
          with opnd do
            begin
              o_class := expression;
              o_e_reg := reg2;
              o_e_negated := false
            end
	end;
      ord1:
        begin
	  pop_operand(opnd); (* we have to convert it up to a fullword *)
	  load(opnd,reg)
	end;
      chr1: (* no problem converting full to byte *)
        begin
	  pop_operand(opnd)
	end
    end   (* case index *) ;
    push_operand(opnd);
  end   (* if stmt *) ;
end   (* std_func *) ;
(**********************************************************************)
(*                                                                    *)
(*                       assignment procedures                        *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                               Store                                *)
(*                                                                    *)
(*    Stack:  address X value -> nil                                  *)
(*                                                                    *)
(*    Stores a word-size object.  If the displacement part of the     *)
(*    address is negative, it's a function result, which is guar-     *)
(*    anteed to be unindexed.                                         *)
(*                                                                    *)
(**********************************************************************)

procedure store(typ: types);
var
  addr, valu: operand;
  op: opcode_range;
  ix, valreg: reg_range;
begin
  case typ of
    byte_type: op := stb_op;
    hw_type  : op := sth_op;
    fw_type  : op := st_op
  end   (* case *) ;
  pop_operand(valu);  pop_operand(addr);
  load(valu, valreg);
  with valu do
    if o_e_negated
    then
      begin
        load_destructible(valu, valreg);
        gen_ri1(xhi_op, valreg, 1, 0);
        o_e_negated := false
      end;
  force_address(addr);
  with addr do
    begin
      if o_a_disp < 0
      then
        begin
          get_reg(1, last_reg, ix);
          gen_immediate(li_op, ix, o_a_disp);
          o_a_indexed := true;
          o_a_ixreg := ix;
          o_a_disp := 0
        end;
      gen_rx(op, valreg, o_a_disp, o_a_base, o_a_ixreg);
    end;
  free_regs(addr);  free_regs(valu);
end   (* store *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Assign_Set                             *)
(*                                                                    *)
(*    Stack:  address X (set)variable -> nil                          *)
(*                                                                    *)
(**********************************************************************)

procedure assign_set;
var
  addr, valu: operand;
  ix, temp: work_reg;
begin
  pop_operand(valu);
  ensure(valu, variable);
  if temporary(valu)
  then deallocate_temp(set_size);
  unindex(valu);
  pop_operand(addr);
  force_address(addr);
  unindex(addr);
  get_reg(first_reg, last_reg, temp);
  get_reg(1, last_reg, ix);
  gen_immediate(li_op, ix, set_size - fw_size);
  with valu do
    gen_rx3( l_op, temp, o_v_disp, o_v_base, ix);
  with addr do
    gen_rx3(st_op, temp, o_a_disp, o_a_base, ix);
  gen_sf(sis_op, ix, fw_size);
  gen_sf(bfbs_op, lsmask, 7);
  dec_ref(temp);  dec_ref(ix);
  free_regs(valu);  free_regs(addr)
end (* assign_set *) ;



(**********************************************************************)
(*                                                                    *)
(*                               Assign                               *)
(*                                                                    *)
(*    Assumes that a set occupies an integral number of fullwords.    *)
(*                                                                    *)
(**********************************************************************)

procedure assign(typ: types);
begin
  case typ of
    byte_type,
    hw_type,
    fw_type:
      store(typ);
    real_type:
      error(this_pass, not_imp_error);
    set_type:
      assign_set
  end   (* case *)
end   (* assign *) ;



(**********************************************************************)
(*                                                                    *)
(*                             AssignTag                              *)
(*                                                                    *)
(*    Stack:  address X value -> nil                                  *)
(*                                                                    *)
(**********************************************************************)

procedure assigntag(length: integer;  typ: types);
var
  addr, valu: operand;
  offset: integer;
  ix, temp: reg_range;
begin
  if length > 0
  then   (* first, clear the variant part *)
    begin
      case typ of
        fw_type:
          offset := fw_size;
        hw_type:
          offset := hw_size;
        byte_type:
          if length mod hw_size = 0
          then offset := hw_size
          else
            begin
              offset := 0;  length := length + 1
            end
      end (* case *) ;
      pop_operand(valu);  pop_operand(addr);
      force_address(addr);  unindex(addr);
      get_reg(first_reg, last_reg, temp);
      get_reg(1, last_reg, ix);
      gen_sf(lis_op, temp, 0);
      gen_immediate(li_op, ix, length-hw_size);
      with addr do
        gen_rx3(sth_op, temp, o_a_disp+offset, o_a_base, ix);
      gen_sf(sis_op, ix, hw_size);
      gen_sf(bfbs_op, lsmask, 4);
      dec_ref(temp);  dec_ref(ix);
      push_operand(addr);  push_operand(valu)
    end (* length > 0 *) ;
  store(typ)   (* store the tag *)
end   (* assigntag *) ;



(**********************************************************************)
(*                                                                    *)
(*                             CopyStruc                              *)
(*                                                                    *)
(*    Stack:  address X address -> nil                                *)
(*                                                                    *)
(**********************************************************************)

procedure copystruc(length: integer);
var
  source, dest: operand;
  ix, temp: reg_range;
begin
  pop_operand(source);  pop_operand(dest);
  force_address(source);  force_address(dest);
  unindex(source);  unindex(dest);
  if length = 2
  then begin
    get_reg(first_reg, last_reg, temp);
    with source do
      gen_rx(lh_op, temp, o_a_disp, o_a_base, r0);
    with dest do
      gen_rx(sth_op, temp, o_a_disp, o_a_base, r0);
    dec_ref(temp)
  end
  else if length = 4
  then begin
    get_reg(first_reg, last_reg, temp);
    with source do
      gen_rx(l_op, temp, o_a_disp, o_a_base, r0);
    with dest do
      gen_rx(st_op, temp, o_a_disp, o_a_base, r0);
    dec_ref(temp)
  end
  else begin
    get_pair(first_reg, temp);
    get_reg(first_reg, last_reg, ix);
    with source do
      gen_rx(la_op, temp, o_a_disp+length-hw_size, o_a_base, r0);
    with dest do
      gen_rx(la_op, temp+1, o_a_disp+length-hw_size, o_a_base, r0);
    gen_immediate(li_op, ix, ((length div 2) - 1)*16);
    gen_rr(copy_op, temp, ix);
    dec_ref(temp+1);
    dec_ref(temp);  dec_ref(ix)
  end;
  free_regs(source);  free_regs(dest)
end   (* copystruc *) ;
(**********************************************************************)
(*                                                                    *)
(*                      unary negation operator                       *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                            Neg_Integer                             *)
(*                                                                    *)
(*    Stack:  value -> value                                          *)
(*                                                                    *)
(**********************************************************************)

procedure neg_integer;
var
  opnd: operand;
  op: opcode_range;
  temp: reg_range;
begin
  pop_operand(opnd);
  with opnd do
    case o_class of
      address,
      relation:
        begin
          error(this_pass, cant_happen);
          dummy_operand(opnd, expression)
        end;
      constant:
        o_c_value := - o_c_value;
      expression:
        begin
          get_reg(1, last_reg, temp);
          gen_sf(lis_op, temp, 0);
          gen_rr(sr_op, temp, o_e_reg);
          dec_ref(o_e_reg);
          o_e_reg := temp
        end;
      variable:
        begin
          if o_v_type = fw_type
          then op := s_op
          else op := sh_op;
          get_reg(1, last_reg, temp);
          gen_sf(lis_op, temp, 0);
          gen_code(op, temp, opnd);
          o_class := expression;
          o_e_reg := temp;
          o_e_negated := false
        end
    end (* case, with *) ;
  push_operand(opnd)
end (* neg_integer *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Neg                                 *)
(*                                                                    *)
(**********************************************************************)

procedure neg(typ: types);
begin
  case typ of
    hw_type,
    fw_type:
      neg_integer;
    real_type:
      error(this_pass, not_imp_error)
  end   (* case *)
end   (* neg *) ;
(**********************************************************************)
(*                                                                    *)
(*                         addition operator                          *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             add_int_vc                             *)
(*                                                                    *)
(**********************************************************************)

procedure add_int_vc(var valu, cnst: operand);
var
  reg: reg_range;
begin
  load_destructible(valu, reg);
  gen_immediate(ai_op, reg, cnst.o_c_value);
  push_operand(valu)
end (* add_int_vc *) ;



(**********************************************************************)
(*                                                                    *)
(*                             add_int_ev                             *)
(*                                                                    *)
(**********************************************************************)

procedure add_int_ev(var expr, vrbl: operand);
var
  op: opcode_range;
  reg: reg_range;
begin
  load_destructible(expr, reg);
  if vrbl.o_v_type = fw_type
  then op := a_op
  else op := ah_op;
  gen_code(op, reg, vrbl);
  push_operand(expr)
end (* add_int_ev *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Add_Int                               *)
(*                                                                    *)
(*    Stack:  value X value -> value                                  *)
(*                                                                    *)
(**********************************************************************)

procedure add_int(typ: types);
var
  left, right: operand;
  op: opcode_range;
  reg: reg_range;
begin
  pop_operand(right);  pop_operand(left);
  if not ([left.o_class, right.o_class] <=
          [constant, expression, variable])
  then
    begin
      error(this_pass, cant_happen);
      dummy_operand(left, expression);
      push_operand(left)
    end
  else
    case left.o_class of
      constant:
        case right.o_class of
          constant:
            begin
              left.o_c_value := ct_add(left.o_c_value, right.o_c_value);
              push_operand(left)
            end;
          expression,
          variable:
            add_int_vc(right, left)
        end (* case right.o_class *) ;
      expression:
        case right.o_class of
          constant:
            add_int_vc(left, right);
          expression:
            begin
              commute(left, right);
              gen_rr(ar_op, left.o_e_reg, right.o_e_reg);
              dec_ref(right.o_e_reg);
              push_operand(left)
            end;
          variable:
            add_int_ev(left, right)
        end (* case right.o_class *) ;
      variable:
        case right.o_class of
          constant:
            add_int_vc(left, right);
          expression:
            add_int_ev(right, left);
          variable:
            begin
              load_destructible(left, reg);
              if typ = fw_type
              then op := a_op
              else op := ah_op;
              gen_code(op, reg, right);
              push_operand(left)
            end
        end (* case right.o_class *)
    end (* case left.o_class *)
end (* add_int *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Add                                 *)
(*                                                                    *)
(**********************************************************************)

procedure add(typ: types);
begin
  case typ of
    hw_type,
    fw_type:
      add_int(typ);
    real_type:
      error(this_pass, not_imp_error)
  end   (* case *)
end   (* add *) ;
(**********************************************************************)
(*                                                                    *)
(*                        subtraction operator                        *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                              Sub_Set                               *)
(*                                                                    *)
(*    Stack:  (set)variable X (set)variable -> (set)variable          *)
(*    Assumes a set occupies an integral number of fullwords.         *)
(*                                                                    *)
(**********************************************************************)

procedure sub_set;
var
  left, right, result: operand;
  ix, temp: work_reg;
  templength, offset: integer;
begin
  templength := 0;
  pop_operand(right);
  ensure(right, variable);
  if temporary(right)
  then templength := templength + set_size;
  unindex(right);
  pop_operand(left);
  ensure(left, variable);
  if temporary(left)
  then templength := templength + set_size;
  unindex(left);
  reallocate_temp(templength, set_size, offset);
  with result do
    begin
      o_class := variable;
      o_v_type := set_type;
      o_v_disp := offset;
      o_v_base := b_reg;
      o_v_ixreg := r0;
      o_v_indexed := false;
      o_v_negated := false
    end;
  get_reg(first_reg, last_reg, temp);
  get_reg(1, last_reg, ix);
  gen_immediate(li_op, ix, set_size - fw_size);
  with left do
    gen_rx3( l_op, temp, o_v_disp, o_v_base, ix);
  with right do
    gen_rx3( n_op, temp, o_v_disp, o_v_base, ix);
  with left do
    gen_rx3( x_op, temp, o_v_disp, o_v_base, ix);
  with result do
    gen_rx3(st_op, temp, o_v_disp, o_v_base, ix);
  gen_sf(sis_op, ix, fw_size);
  gen_sf(bfbs_op, lsmask, 13);
  dec_ref(temp);  dec_ref(ix);
  free_regs(left);  free_regs(right);
  push_operand(result)
end (* sub_set *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Sub_Int                               *)
(*                                                                    *)
(*    Stack:  value X value -> value                                  *)
(*                                                                    *)
(**********************************************************************)

procedure sub_int(typ: types);
var
  left, right: operand;
  op: opcode_range;
  reg: reg_range;
begin
  if typ = fw_type
  then op := s_op
  else op := sh_op;
  pop_operand(right);  pop_operand(left);
  if not ([left.o_class, right.o_class] <=
          [constant, expression, variable])
  then
    begin
      error(this_pass, cant_happen);
      dummy_operand(left, expression)
    end
  else
    case left.o_class of
      constant:
        case right.o_class of
          constant:
            left.o_c_value := ct_sub(left.o_c_value, right.o_c_value);
          expression:
            begin
              load_destructible(left, reg);
              gen_rr(sr_op, reg, right.o_e_reg);
              dec_ref(right.o_e_reg)
            end;
          variable:
            begin
              load_destructible(left, reg);
              gen_code(op, reg, right)
            end
        end (* case right.o_class *) ;
      expression,
      variable:
        begin
          load_destructible(left, reg);
          case right.o_class of
            constant:
              gen_immediate(si_op, reg, right.o_c_value);
            expression:
              begin
                gen_rr(sr_op, reg, right.o_e_reg);
                dec_ref(right.o_e_reg)
              end;
            variable:
              gen_code(op, reg, right)
          end (* case right.o_class *)
        end
    end (* case left.o_class *) ;
  push_operand(left)
end (* sub_int *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Sub                                 *)
(*                                                                    *)
(**********************************************************************)

procedure sub(typ: types);
begin
  case typ of
    hw_type,
    fw_type:
      sub_int(typ);
    real_type:
      error(this_pass, not_imp_error);
    set_type:
      sub_set
  end   (* case *)
end   (* sub *) ;
(**********************************************************************)
(*                                                                    *)
(*                      multiplication operator                       *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             mul_int_ec                             *)
(*                                                                    *)
(**********************************************************************)

procedure mul_int_ec(var expr, cnst: operand;  typ: types);
var
  klass: const_classification;
  neg_const: boolean;
  n, m: integer;
  reg, temp: reg_range;
begin
  with cnst do
    begin
      if o_c_value < 0
      then
        begin
          o_c_value := - o_c_value;
          neg_const := true
        end
      else neg_const := false;
      classify_constant(o_c_value, klass, n, m)
    end (* with *) ;
  case klass of
    zero:   (* result is zero *)
      begin
        push_operand(cnst);
        dec_ref(expr.o_e_reg)
      end;
    one:
      begin
        if neg_const
        then
          with expr do
            begin
              get_reg(1, last_reg, temp);
              gen_sf(lis_op, temp, 0);
              gen_rr(sr_op, temp, o_e_reg);
              dec_ref(o_e_reg);
              o_e_reg := temp
            end (* with *) ;
        push_operand(expr)
      end;
    poweroftwo:
      begin
        load_destructible(expr, reg);
        gen_shift(sll_op, reg, n);
        if neg_const
        then
          with expr do
            begin
              get_reg(1, last_reg, temp);
              gen_sf(lis_op, temp, 0);
              gen_rr(sr_op, temp, o_e_reg);
              dec_ref(o_e_reg);
              o_e_reg := temp
            end (* with *) ;
        push_operand(expr)
      end;
    twomplus1:
      with expr do
        begin
          get_reg(1, last_reg, reg);
          gen_rr(lr_op, reg, o_e_reg);
          gen_shift(sll_op, reg, m);
          gen_rr(ar_op, reg, o_e_reg);
          if n <> 0
          then gen_shift(sll_op, reg, n);
          dec_ref(o_e_reg);
          if neg_const
          then
            begin
              get_reg(1, last_reg, temp);
              gen_sf(lis_op, temp, 0);
              gen_rr(sr_op, temp, reg);
              dec_ref(reg);
              reg := temp
            end;
          o_e_reg := reg;
          push_operand(expr)
        end;
    twomminus1:
      with expr do
        begin
          if neg_const
          then load_destructible(expr, temp);
          get_reg(1, last_reg, reg);
          gen_rr(lr_op, reg, o_e_reg);
          if neg_const
          then gen_shift(sll_op, o_e_reg, m)
          else gen_shift(sll_op, reg, m);
          gen_rr(sr_op, reg, o_e_reg);
          if n <> 0
          then gen_shift(sll_op, reg, n);
          dec_ref(o_e_reg);
          o_e_reg := reg;
          push_operand(expr)
        end;
    other:
      with expr do
        begin
          if typ = hw_type
          then
            begin
              load(cnst, temp);
              gen_rr(mhr_op, o_e_reg, temp);
              dec_ref(temp)
            end
          else   (* typ = fw_type *)
            begin
              get_pair(first_reg, reg);
              gen_rr(lr_op, reg+1, o_e_reg);
              load(cnst, temp);
              gen_rr(mr_op, reg, temp);
              dec_ref(temp);
              dec_ref(reg);
	      dec_ref(o_e_reg);
              o_e_reg := reg + 1
            end;
          push_operand(expr)
        end (* with *)
  end (* case *)
end (* mul_int_ec *) ;



(**********************************************************************)
(*                                                                    *)
(*                             mul_int_vc                             *)
(*                                                                    *)
(*    Note:  can't use RX operations;  need to get the variable       *)
(*    into a register to check its sign.                              *)
(*                                                                    *)
(**********************************************************************)

procedure mul_int_vc(var vrbl, cnst: operand;  typ: types);
var
  reg: reg_range;
begin
  with cnst do
    if o_c_value = 0
    then   (* result is zero *)
      begin
        push_operand(cnst);
        free_regs(vrbl)
      end
    else if o_c_value = 1
    then   (* result is the variable *)
      push_operand(vrbl)
    else
      begin
        load(vrbl, reg);
        mul_int_ec(vrbl, cnst, typ)
      end
end (* mul_int_vc *) ;



(**********************************************************************)
(*                                                                    *)
(*                             mul_int_ee                             *)
(*                                                                    *)
(**********************************************************************)

procedure mul_int_ee(var left, right: operand;  typ: types);
var
  flag, reg: reg_range;
begin
  if typ = hw_type
  then
    begin
      commute(left, right);
      gen_rr(mhr_op, left.o_e_reg, right.o_e_reg);
    end
  else   (* typ = fw_type *)
    begin
      get_pair(first_reg, reg);
      with left do
        begin
          gen_rr(lr_op, reg+1, o_e_reg);
          dec_ref(o_e_reg);
          o_e_reg := reg + 1;
        end;
      gen_rr(mr_op, reg, right.o_e_reg);
      dec_ref(right.o_e_reg);
      dec_ref(reg)
    end;
  push_operand(left)
end (* mul_int_ee *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Mul_Int                               *)
(*                                                                    *)
(*    Stack:  value X value -> value                                  *)
(*                                                                    *)
(**********************************************************************)

procedure mul_int(typ: types);
var
  left, right: operand;
  temp: reg_range;
begin
  pop_operand(right);  pop_operand(left);
  if not ([left.o_class, right.o_class] <=
          [constant, expression, variable])
  then
    begin
      error(this_pass, cant_happen);
      dummy_operand(left, expression);
      push_operand(left)
    end
  else
    case left.o_class of 
      constant:
        case right.o_class of
          constant:
            begin
              left.o_c_value := ct_mul(left.o_c_value, right.o_c_value);
              push_operand(left)
            end;
          expression:
            mul_int_ec(right, left, typ);
          variable:
            mul_int_vc(right, left, typ)
        end (* case right.o_class *) ;
      expression:
        case right.o_class of
          constant:
            mul_int_ec(left, right, typ);
          expression,
          variable:
            begin
              load(right, temp);
              mul_int_ee(left, right, typ)
            end
        end (* case right.o_class *) ;
      variable:
        case right.o_class of
          constant:
            mul_int_vc(left, right, typ);
          expression,
          variable:
            begin
              load(left, temp);
              load(right, temp);
              mul_int_ee(left, right, typ)
            end
        end (* case right.o_class *)
    end (* case left.o_class *)
end (* mul_int *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Mul                                 *)
(*                                                                    *)
(**********************************************************************)

procedure mul(typ: types);
begin
  case typ of
    hw_type,
    fw_type:
      mul_int(typ);
    real_type:
      error(this_pass, not_imp_error)
  end (* case *)
end (* mul *) ;
(**********************************************************************)
(*                                                                    *)
(*                         division operator                          *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             div_int_ee                             *)
(*                                                                    *)
(**********************************************************************)

procedure div_int_ee(var dividend, divisor: operand;  typ: types);
var
  flag, pair, temp: reg_range;
begin
  get_pair(first_reg, pair);
  get_reg(first_reg, last_reg, flag);
  gen_sf(lcs_op, flag, 1);
  if typ = hw_type
  then
    begin
      with dividend do
        begin
          gen_rr(lr_op, pair, o_e_reg);
          gen_sf(bffs_op, lsmask, 4);
          gen_sf(lis_op, pair, 0);
          gen_rr(sr_op, pair, o_e_reg);
          dec_ref(o_e_reg);
          gen_sf(ais_op, flag, 1)
        end;
      with divisor do
        begin
          gen_rr(lr_op, pair+1, o_e_reg);
          gen_sf(bffs_op, lsmask, 4);
          gen_sf(lis_op, pair+1, 0);
          gen_rr(sr_op, pair+1, o_e_reg);
          dec_ref(o_e_reg);
          gen_sf(ais_op, flag, 1)
        end;
      gen_rr(dhr_op, pair, pair+1)
    end
  else   (* typ = fw_type *)
    begin
      with dividend do
        begin
          gen_rr(lr_op, pair+1, o_e_reg);
          gen_sf(bffs_op, lsmask, 4);
          gen_sf(lis_op, pair+1, 0);
          gen_rr(sr_op, pair+1, o_e_reg);
          dec_ref(o_e_reg);
          gen_sf(ais_op, flag, 1)
        end;
      get_reg(first_reg, last_reg, temp);
      with divisor do
        begin
          gen_rr(lr_op, temp, o_e_reg);
          gen_sf(bffs_op, lsmask, 4);
          gen_sf(lis_op, temp, 0);
          gen_rr(sr_op, temp, o_e_reg);
          dec_ref(o_e_reg);
          gen_sf(ais_op, flag, 1)
        end;
      gen_sf(lis_op, pair, 0);
      gen_rr(dr_op, pair, temp);
      dec_ref(temp)
    end;

  (* Now PAIR contains remainder;  PAIR+1, quotient.                  *)

  dec_ref(pair);
  gen_rr(lr_op, flag, flag);
  dec_ref(flag);
  gen_sf(btfs_op, nemask, 4);
  get_reg(first_reg, last_reg, temp);
  gen_rr(lr_op, temp, pair+1);
  gen_sf(lis_op, pair+1, 0);
  gen_rr(sr_op, pair+1, temp);
  dec_ref(temp);
  dividend.o_e_reg := pair+1;
  push_operand(dividend)
end (* div_int_ee *) ;



(**********************************************************************)
(*                                                                    *)
(*                             div_int_ec                             *)
(*                                                                    *)
(**********************************************************************)

procedure div_int_ec(var expr, cnst: operand;  typ: types);
var
  klass: const_classification;
  n, m: integer;
  neg_const: boolean;
  reg, temp: reg_range;
begin
  with cnst do
    begin
      if o_c_value < 0
      then
        begin
          o_c_value := - o_c_value;
          neg_const := true
        end
      else neg_const := false;
      classify_constant(o_c_value, klass, n, m)
    end;
  case klass of
    zero:
      begin
        error(this_pass, divide_by_zero);
        push_operand(expr)
      end;
    one:
      begin
        if neg_const
        then
          with expr do
            begin
              get_reg(1, last_reg, reg);
              gen_sf(lis_op, reg, 0);
              gen_rr(sr_op, reg, o_e_reg);
              dec_ref(o_e_reg);
              o_e_reg := reg
            end (* with *) ;
        push_operand(expr)
      end;
    poweroftwo:
      with expr do
        begin
          get_reg(1, last_reg, reg);
          gen_rr(lr_op, reg, o_e_reg);
          gen_sf(bffs_op, lsmask, 3);
          gen_sf(lis_op, reg, 0);
          gen_rr(sr_op, reg, o_e_reg);
          gen_rx1(sra_op, reg, n, 0);
          gen_rr(lr_op, o_e_reg, o_e_reg);
          dec_ref(o_e_reg);
          o_e_reg := reg;
          if neg_const
          then gen_sf(btfs_op, lsmask, 4)
          else gen_sf(bffs_op, lsmask, 4);
          get_reg(first_reg, last_reg, temp);
          gen_rr(lr_op, temp, reg);
          gen_sf(lis_op, reg, 0);
          gen_rr(sr_op, reg, temp);
          dec_ref(temp);
          push_operand(expr)
        end;
    twomminus1,
    twomplus1,
    other:
      begin
        load(cnst, reg);
        div_int_ee(expr, cnst, typ)
      end
  end (* case *)
end (* div_int_ec *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Div_Int                               *)
(*                                                                    *)
(*    Stack:  value X value -> value                                  *)
(*                                                                    *)
(**********************************************************************)

procedure div_int(typ: types);
var
  divisor, dividend: operand;
  temp: reg_range;
begin
  pop_operand(divisor);  pop_operand(dividend);
  if not ([divisor.o_class, dividend.o_class] <=
          [constant, expression, variable])
  then
    begin
      error(this_pass, cant_happen);
      dummy_operand(dividend, expression);
      push_operand(dividend)
    end
  else
    if divisor.o_class = constant
    then
      if dividend.o_class = constant
      then   (* do at compile time *)
        begin
          dividend.o_c_value := ct_div(dividend.o_c_value,
                                       divisor.o_c_value);
          push_operand(dividend)
        end
      else
        begin
          load(dividend, temp);
          div_int_ec(dividend, divisor, typ)
        end
    else
      begin
        load(dividend, temp);
        load(divisor, temp);
        div_int_ee(dividend, divisor, typ)
      end
end (* div_int *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Div_                                *)
(*                                                                    *)
(**********************************************************************)

procedure div_(typ: types);
begin
  case typ of
    hw_type,
    fw_type:
      div_int(typ);
    real_type:
      error(this_pass, not_imp_error)
  end (* case *)
end (* div_ *) ;
(**********************************************************************)
(*                                                                    *)
(*                          modulus operator                          *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             mod_int_ee                             *)
(*                                                                    *)
(**********************************************************************)

procedure mod_int_ee(var dividend, divisor: operand;  typ: types);
var
  upper, temp: reg_range;
begin
  get_pair(1, upper);
  if typ = hw_type
  then
    begin
      with dividend do
        begin
          gen_rr(lr_op, upper, o_e_reg);
          gen_sf(bffs_op, lsmask, 3);
          gen_sf(lis_op, upper, 0);
          gen_rr(sr_op, upper, o_e_reg)
        end;
      with divisor do
        begin
          gen_rr(lr_op, upper+1, o_e_reg);
          gen_sf(bffs_op, lsmask, 3);
          gen_sf(lis_op, upper+1, 0);
          gen_rr(sr_op, upper+1, o_e_reg)
        end;
      gen_rr(dhr_op, upper, upper+1)
    end
  else   (* typ = fw_type *)
    begin
      get_reg(first_reg, last_reg, temp);
      with dividend do
        begin
          gen_rr(lr_op, upper+1, o_e_reg);
          gen_sf(bffs_op, lsmask, 3);
          gen_sf(lis_op, upper+1, 0);
          gen_rr(sr_op, upper+1, o_e_reg)
        end;
      with divisor do
        begin
          gen_rr(lr_op, temp, o_e_reg);
          gen_sf(bffs_op, lsmask, 3);
          gen_sf(lis_op, temp, 0);
          gen_rr(sr_op, temp, o_e_reg)
        end;
      gen_sf(lis_op, upper, 0);
      gen_rr(dr_op, upper, temp);
      dec_ref(temp)
    end;
  gen_rr(lr_op, dividend.o_e_reg, dividend.o_e_reg);
  gen_sf(bffs_op, lsmask, 4);
  gen_rr(lr_op, upper+1, upper);
  gen_sf(lis_op, upper, 0);
  gen_rr(sr_op, upper, upper+1);
  dec_ref(dividend.o_e_reg);
  dec_ref(divisor.o_e_reg);
  dec_ref(upper+1);
  dividend.o_e_reg := upper;
  push_operand(dividend)
end (* mod_int_ee *) ;



(**********************************************************************)
(*                                                                    *)
(*                             mod_int_ec                             *)
(*                                                                    *)
(**********************************************************************)

procedure mod_int_ec(var expr, cnst: operand;  typ: types);
var
  klass: const_classification;
  n, m: integer;
  reg, temp: reg_range;
begin
  with cnst do
    begin
      o_c_value := abs(o_c_value);
      classify_constant(o_c_value, klass, n, m)
    end (* with *) ;
  case klass of
    zero:
      begin
        error(this_pass, divide_by_zero);
        push_operand(expr)
      end;
    one:
      begin
        cnst.o_c_value := 0;   (* one divides everything *)
        push_operand(cnst)
      end;
    poweroftwo:
      with expr do
        begin
          get_reg(1, last_reg, reg);
          gen_rr(lr_op, reg, o_e_reg);
          gen_sf(bffs_op, lsmask, 3);
          gen_sf(lis_op, reg, 0);
          gen_rr(sr_op, reg, o_e_reg);
          gen_immediate(ni_op, reg, cnst.o_c_value-1);
          gen_rr(lr_op, o_e_reg, o_e_reg);
          dec_ref(o_e_reg);
          gen_sf(bffs_op, lsmask, 4);
          get_reg(first_reg, last_reg, temp);
          gen_rr(lr_op, temp, reg);
          gen_sf(lis_op, reg, 0);
          gen_rr(sr_op, reg, temp);
          dec_ref(temp);
          o_e_reg := reg;
          push_operand(expr)
        end;
    twomplus1,
    twomminus1,
    other:
      begin
        load(cnst, reg);
        mod_int_ee(expr, cnst, typ)
      end
  end (* case *)
end (* mod_int_ec *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Mod_Int                               *)
(*                                                                    *)
(*    Note:  sign of remainder = sign of dividend.                    *)
(*                                                                    *)
(**********************************************************************)

procedure mod_int(typ: types);
var
  divisor, dividend: operand;
  reg: reg_range;
begin
  pop_operand(divisor);  pop_operand(dividend);
  if not ([divisor.o_class, dividend.o_class] <=
          [constant, expression, variable])
  then
    begin
      error(this_pass, cant_happen);
      dummy_operand(dividend, expression);
      push_operand(dividend)
    end
  else
    if divisor.o_class = constant
    then
      if dividend.o_class = constant
      then
        begin
          dividend.o_c_value := ct_mod(dividend.o_c_value,
                                       divisor.o_c_value);
          push_operand(dividend)
        end
      else
        begin
          load(dividend, reg);
          mod_int_ec(dividend, divisor, typ)
        end
    else
      begin
        load(dividend, reg);
        load(divisor, reg);
        mod_int_ee(dividend, divisor, typ)
      end
end (* mod_int *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Mod_                                *)
(*                                                                    *)
(**********************************************************************)

procedure mod_(typ: types);
begin
  case typ of
    hw_type,
    fw_type:
      mod_int(typ);
    real_type:
      error(this_pass, not_imp_error)
  end (* case *)
end (* mod_ *) ;
(**********************************************************************)
(*                                                                    *)
(*                   Increment/Decrement Operators                    *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                              Inc_Dec                               *)
(*                                                                    *)
(*    Stack:  address -> nil                                          *)
(*                                                                    *)
(**********************************************************************)

procedure inc_dec(which: updown;  typ: types);
var
  addr: operand;
  op: opcode_range;
  temp: reg_range;
begin
  pop_operand(addr);  force_address(addr);
  get_reg(first_reg, last_reg, temp);
  case typ of
    byte_type:
      begin
        case which of
          decrement:  op := sis_op;
          increment:  op := ais_op
        end (* case *) ;
        with addr do
          begin
            gen_rx(lb_op, temp, o_a_disp, o_a_base, o_a_ixreg);
            gen_sf(op, temp, 1);
            gen_rx(stb_op, temp, o_a_disp, o_a_base, o_a_ixreg)
          end
      end;
    hw_type,
    fw_type:
      begin
        case which of 
          decrement:  op := lcs_op;
          increment:  op := lis_op
        end (* case *) ;
        gen_sf(op, temp, 1);
        case typ of
          hw_type:  op := ahm_op;
          fw_type:  op := am_op
        end (* case *) ;
        with addr do
          gen_rx(op, temp, o_a_disp, o_a_base, o_a_ixreg)
      end
  end (* case *) ;
  (* A Single instance where there are no registers to free -pjh *)
  if (addr.o_class <> address) then
     free_regs(addr) 
  else
     free_regs(addr);
  dec_ref(temp)
end (* inc_dec *) ;
(**********************************************************************)
(*                                                                    *)
(*                    Boolean operator procedures                     *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             Set_Boolop                             *)
(*                                                                    *)
(*    Stack:  (set)variable X (set)variable -> (set)variable          *)
(*    Assumes a set occupies an integral number of fullwords.         *)
(*                                                                    *)
(**********************************************************************)

procedure set_boolop(which: boolop);
var
  left, right, result: operand;
  op: opcode_range;
  ix, temp: work_reg;
  offset, templength: integer;
begin
  case which of
    andop: op := n_op;
    orop : op := o_op
  end (* case *) ;
  templength := 0;
  pop_operand(right);
  ensure(right, variable);
  if temporary(right)
  then templength := templength + set_size;
  unindex(right);
  pop_operand(left);
  ensure(left, variable);
  if temporary(left)
  then templength := templength + set_size;
  unindex(left);
  reallocate_temp(templength, set_size, offset);
  with result do
    begin
      o_class := variable;
      o_v_type := set_type;
      o_v_disp := offset;
      o_v_base := b_reg;
      o_v_ixreg := r0;
      o_v_indexed := false;
      o_v_negated := false
    end;
  get_reg(first_reg, last_reg, temp);
  get_reg(1, last_reg, ix);
  gen_immediate(li_op, ix, set_size - fw_size);
  with left do
    gen_rx3( l_op, temp, o_v_disp, o_v_base, ix);
  with right do
    gen_rx3(   op, temp, o_v_disp, o_v_base, ix);
  with result do
    gen_rx3(st_op, temp, o_v_disp, o_v_base, ix);
  gen_sf(sis_op, ix, fw_size);
  gen_sf(bfbs_op, lsmask, 10);
  dec_ref(temp);  dec_ref(ix);
  free_regs(left);  free_regs(right);
  push_operand(result)
end (* set_boolop *) ;



(**********************************************************************)
(*                                                                    *)
(*                             n_bool_vc                              *)
(*                                                                    *)
(**********************************************************************)

procedure n_bool_vc(valu, cnst: operand);
begin
  if cnst.o_c_value = 0
  then   (* result is FALSE *)
    begin
      push_operand(cnst);
      free_regs(valu)
    end
  else   (* result is the value *)
    push_operand(valu)
end (* n_bool_vc *) ;



(**********************************************************************)
(*                                                                    *)
(*                             n_bool_ev                              *)
(*                                                                    *)
(**********************************************************************)

procedure n_bool_ev(var expr, vrbl: operand);
var
  op: opcode_range;
  reg: reg_range;
begin
  load_destructible(expr, reg);
  if vrbl.o_v_negated
  then op := oh_op
  else op := nh_op;
  if expr.o_e_negated <> vrbl.o_v_negated
  then
    begin
      gen_ri1(xhi_op, reg, 1, 0);
      expr.o_e_negated := not expr.o_e_negated
    end;
  gen_code(op, reg, vrbl);
  push_operand(expr)
end (* n_bool_ev *) ;



(**********************************************************************)
(*                                                                    *)
(*                            And_Boolean                             *)
(*                                                                    *)
(*    Stack:  value X value -> value                                  *)
(*                                                                    *)
(**********************************************************************)

procedure and_boolean;
var
  left, right: operand;
  op: opcode_range;
  reg: reg_range;
begin
  pop_operand(right);  pop_operand(left);
  if not ([left.o_class, right.o_class] <=
          [constant, expression, variable])
  then
    begin
      error(this_pass, cant_happen);
      dummy_operand(left, expression);
      push_operand(left)
    end
  else
    case left.o_class of 
      constant:
        case right.o_class of
          constant:   (* do at compile time *)
            begin
              if (left.o_c_value <> 0) and (right.o_c_value <> 0)
              then left.o_c_value := 1
              else left.o_c_value := 0;
              push_operand(left)
            end;
          expression,
          variable:
            n_bool_vc(right, left)
        end (* case right.o_class *) ;
      expression:
        case right.o_class of
          constant:
            n_bool_vc(left, right);
          expression:
            begin
              commute(left, right);
              if left.o_e_negated and right.o_e_negated
              then op := or_op
              else if left.o_e_negated
              then
                begin
                  gen_ri1(xhi_op, left.o_e_reg, 1, 0);
                  left.o_e_negated := false;
                  op := nr_op
                end
              else if right.o_e_negated
              then
                begin
                  load_destructible(right, reg);
                  gen_ri1(xhi_op, reg, 1, 0);
                  op := nr_op
                end
              else op := nr_op;
              gen_rr(op, left.o_e_reg, right.o_e_reg);
              push_operand(left);
              free_regs(right)
            end;
          variable:
            n_bool_ev(left, right)
        end (* case right.o_class *) ;
      variable:
        case right.o_class of
          constant:
            n_bool_vc(left, right);
          expression:
            n_bool_ev(right, left);
          variable:
            if left.o_v_negated = right.o_v_negated
            then
              begin
                if left.o_v_negated
                then op := oh_op
                else op := nh_op;
                load_destructible(left, reg);
                gen_code(op, reg, right);
                push_operand(left)
              end
            else if left.o_v_negated
            then
              begin
                load_destructible(left, reg);
                gen_ri1(xhi_op, reg, 1, 0);
                gen_code(nh_op, reg, right);
                left.o_e_negated := false;
                push_operand(left)
              end
            else   (* right.o_v_negated *)
              begin
                load_destructible(right, reg);
                gen_ri1(xhi_op, reg, 1, 0);
                gen_code(nh_op, reg, left);
                right.o_e_negated := false;
                push_operand(right)
              end
        end (* case right.o_class *)
    end (* case left.o_class *)
end (* and_boolean *) ;



(**********************************************************************)
(*                                                                    *)
(*                                And_                                *)
(*                                                                    *)
(**********************************************************************)

procedure and_(typ: types);
begin
  case typ of
    hw_type : and_boolean;
    set_type: set_boolop(andop)
  end (* case *)
end (* and_ *) ;



(**********************************************************************)
(*                                                                    *)
(*                             or_bool_vc                             *)
(*                                                                    *)
(**********************************************************************)

procedure or_bool_vc(valu, cnst: operand);
begin
  if cnst.o_c_value = 1
  then   (* result is TRUE *)
    begin
      push_operand(cnst);
      free_regs(valu)
    end
  else   (* result is the value *)
    push_operand(valu)
end (* or_bool_vc *) ;



(**********************************************************************)
(*                                                                    *)
(*                             or_bool_ev                             *)
(*                                                                    *)
(**********************************************************************)

procedure or_bool_ev(expr, vrbl: operand);
var
  op: opcode_range;
  reg: reg_range;
begin
  load_destructible(expr, reg);
  if expr.o_e_negated <> vrbl.o_v_negated
  then
    begin
      gen_ri1(xhi_op, reg, 1, 0);
      expr.o_e_negated := not expr.o_e_negated
    end;
  if vrbl.o_v_negated
  then op := nh_op
  else op := oh_op;
  gen_code(op, reg, vrbl);
  push_operand(expr)
end (* or_bool_ev *) ;



(**********************************************************************)
(*                                                                    *)
(*                             Or_Boolean                             *)
(*                                                                    *)
(*    Stack:  value X value -> value                                  *)
(*                                                                    *)
(**********************************************************************)

procedure or_boolean;
var
  left, right: operand;
  reg: reg_range;
  op: opcode_range;
begin
  pop_operand(right);  pop_operand(left);
  if not ([left.o_class, right.o_class] <=
          [constant, expression, variable])
  then
    begin
      error(this_pass, cant_happen);
      dummy_operand(left, expression);
      push_operand(left)
    end
  else
    case left.o_class of
      constant:
        case right.o_class of 
          constant:   (* do at compile time *)
            begin
              if (left.o_c_value = 0) and (right.o_c_value = 0)
              then left.o_c_value := 0
              else left.o_c_value := 1;
              push_operand(left)
            end;
          expression,
          variable:
            or_bool_vc(right, left)
        end (* case right.o_class *) ;
      expression:
        case right.o_class of
          constant:
            or_bool_vc(left, right);
          expression:
            begin
              commute(left, right);
              if left.o_e_negated and right.o_e_negated
              then op := nr_op
              else
                begin
                  op := or_op;
                  if left.o_e_negated
                  then
                    begin
                      gen_ri1(xhi_op, left.o_e_reg, 1, 0);
                      left.o_e_negated := false
                    end
                  else if right.o_e_negated
                  then
                    begin
                      load_destructible(right, reg);
                      gen_ri1(xhi_op, reg, 1, 0)
                    end
                end;
              gen_rr(op, left.o_e_reg, right.o_e_reg);
              push_operand(left);
              free_regs(right)
            end;
          variable:
            or_bool_ev(left, right)
        end (* case right.o_class *) ;
      variable:
        case right.o_class of
          constant:
            or_bool_vc(left, right);
          expression:
            or_bool_ev(right, left);
          variable:
            if left.o_v_negated and right.o_v_negated
            then
              begin
                load_destructible(left, reg);
                gen_code(nh_op, reg, right);
                push_operand(left)
              end
            else if right.o_v_negated
            then
              begin
                load_destructible(right, reg);
                gen_ri1(xhi_op, reg, 1, 0);
                gen_code(oh_op, reg, left);
                right.o_e_negated := false;
                push_operand(right)
              end
            else if left.o_v_negated
            then
              begin
                load_destructible(left, reg);
                gen_ri1(xhi_op, reg, 1, 0);
                gen_code(oh_op, reg, right);
                left.o_e_negated := false;
                push_operand(left)
              end
            else
              begin
                load_destructible(left, reg);
                gen_code(oh_op, reg, right);
                push_operand(left)
              end
        end (* case right.o_class *)
    end (* case left.o_class *)
end (* or_boolean *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Or_                                 *)
(*                                                                    *)
(**********************************************************************)

procedure or_(typ: types);
begin
  case typ of
    hw_type : or_boolean;
    set_type: set_boolop(orop)
  end (* case *)
end (* or_ *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Not_                                *)
(*                                                                    *)
(*    Stack:  value -> value                                          *)
(*                                                                    *)
(**********************************************************************)

procedure not_;
var
  opnd: operand;
begin
  pop_operand(opnd);
  with opnd do
    case o_class of
      address:
        begin
          error(this_pass, cant_happen);
          dummy_operand(opnd, expression)
        end;
      constant:
        if o_c_value = 0
        then o_c_value := 1
        else o_c_value := 0;
      expression:
        o_e_negated := not o_e_negated;
      relation:
        case o_r_cond of
          less:         o_r_cond := notless;
          equal:        o_r_cond := notequal;
          greater:      o_r_cond := notgreater;
          notless:      o_r_cond := less;
          notequal:     o_r_cond := equal;
          notgreater:   o_r_cond := greater
        end (* case o_r_cond *) ;
      variable:
        o_v_negated := not o_v_negated
    end (* case o_class, with *) ;
  push_operand(opnd)
end (* not_ *) ;
(**********************************************************************)
(*                                                                    *)
(*                        variable procedures                         *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             PushConst                              *)
(*                                                                    *)
(*    Stack:  null -> constant                                        *)
(*                                                                    *)
(**********************************************************************)

procedure pushconst(value: integer);
var
  cnst: operand;
begin
  with cnst do
    begin
      o_class := constant;
      o_c_value := value
    end (* with *) ;
  push_operand(cnst)
end (* pushconst *) ;



(**********************************************************************)
(*                                                                    *)
(*                            PushIndirect                            *)
(*                                                                    *)
(*    Stack:  address -> value                                        *)
(*    Assumes that a set occupies an integral number of fullwords.    *)
(*                                                                    *)
(**********************************************************************)

procedure pushindirect(typ: types);
var
  addr, valu: operand;
begin
  pop_operand(addr);  force_address(addr);
  case typ of
    byte_type,
    hw_type,
    fw_type,
    set_type:
      with valu do
        begin
          o_class := variable;
          o_v_type := typ;
          o_v_negated := false;
          o_v_disp := addr.o_a_disp;
          o_v_base := addr.o_a_base;
          o_v_ixreg := addr.o_a_ixreg;
          o_v_indexed := addr.o_a_indexed
        end (* with *) ;
    real_type:
      begin
        error(this_pass, not_imp_error);
        dummy_operand(valu, variable)
      end
  end   (* case *) ;
  push_operand(valu)
end   (* pushindirect *);



(**********************************************************************)
(*                                                                    *)
(*                            PushAddress                             *)
(*                                                                    *)
(*    Stack:  null -> address                                         *)
(*                                                                    *)
(**********************************************************************)

procedure pushaddress(mode: modes;  disp: integer);
var
  addr: operand;
  fnreg, reg: reg_range;
  offset: integer;
begin
  offset := disp + v_link_size(mode) + v_impl_size(mode);
  with addr do
    begin
      o_class := address;
      o_a_indexed := false;
      o_a_ixreg := 0;
      case mode of
        const_mode:
          begin
            get_reg(1, last_reg, reg);
            if concurrent
            then
              begin
                get_reg(0, 0, fnreg);
                gen_extref(rts_constaddr);
                gen_rr(lr_op, reg, fnreg);
                dec_ref(fnreg)
              end
            else gen_rx1(l_op, reg,
                         base_size+v_link_size(prog_mode), g_reg);
            o_a_base := reg;
            o_a_disp := disp
          end;
        proc_mode,
        pe_mode,
        ce_mode,
        me_mode:
          begin
            o_a_base := b_reg;
            if disp = 0   (* function result *)
            then o_a_disp := 0
            else o_a_disp := offset
          end;
        prog_mode,
        process_mode,
        class_mode,
        monitor_mode:
          begin
            o_a_base := g_reg;
            o_a_disp := offset
          end;
        undef_mode:
          (* ignore *)
      end   (* case *)
    end (* with *) ;
  push_operand(addr)
end   (* pushaddress *) ;



(**********************************************************************)
(*                                                                    *)
(*                             PushValue                              *)
(*                                                                    *)
(*    Stack:  null -> value                                           *)
(*    Pushes a single-word object.                                    *)
(*                                                                    *)
(**********************************************************************)

procedure pushvalue(typ: types;  mode: modes;  disp: integer);
var
  opnd: operand;
begin
  if (mode in [proc_mode, pe_mode, ce_mode, me_mode]) and
     (disp >= initialstack)
  then   (* push a stacked temporary *)
    begin
      get_operand( (disp-initialstack) div fw_size, opnd);
      mark_regs(opnd)
    end
  else   (* push a variable or parameter *)
    with opnd do
      begin
        o_class := variable;
        o_v_type := typ;
        if mode in [proc_mode, pe_mode, ce_mode, me_mode]
        then o_v_base := b_reg
        else o_v_base := g_reg;
        o_v_ixreg := 0;
        o_v_disp := disp + v_link_size(mode) + v_impl_size(mode);
        o_v_negated := false;
        o_v_indexed := false
      end (* with *) ;
  push_operand(opnd)
end   (* pushvalue *) ;



(**********************************************************************)
(*                                                                    *)
(*                              PushVar                               *)
(*                                                                    *)
(**********************************************************************)

procedure pushvar(typ: types;  mode: modes;  disp: integer);
begin
  case typ of
    byte_type,
    hw_type,
    fw_type:
      pushvalue(typ, mode, disp);
    real_type:
      error(this_pass, not_imp_error);
    set_type:
      begin
        pushaddress(mode, disp);
        pushindirect(typ)
      end
  end   (* case *)
end   (* pushvar *) ;



(**********************************************************************)
(*                                                                    *)
(*                               Field                                *)
(*                                                                    *)
(*    Stack:  address -> address                                      *)
(*                                                                    *)
(**********************************************************************)

procedure field(disp: integer);
var
  addr: operand;
begin
  pop_operand(addr);
  force_address(addr);
  with addr do
    o_a_disp := ct_add(o_a_disp, disp);
  push_operand(addr)
end (* field *) ;



(**********************************************************************)
(*                                                                    *)
(*                               Index                                *)
(*                                                                    *)
(*    Stack:  address X value -> address                              *)
(*                                                                    *)
(**********************************************************************)

procedure index(min, max, length: integer; rangecheck: boolean);
var
  addr, valu, cnst: operand;
  typ: types;
  temp: integer;
begin
  pop_operand(valu);  pop_operand(addr);
  force_address(addr);
  case valu.o_class of
    address,
    relation:
      error(this_pass, cant_happen);
    constant:
      begin
        temp := valu.o_c_value;
        if (temp < min) or (temp > max)
        then
          begin
            error(this_pass, range_error);
            temp := min
          end;
        with addr do
          begin
            o_a_disp := ct_add(o_a_disp,
                               ct_mul(length,
                                      ct_sub(temp, min)));
            o_a_indexed := true
          end (* with *)
      end;
    expression,
    variable:
      begin
        unindex(addr);
        load_index(valu, min, max);
	if rangecheck
	then gen_rangecheck(valu.o_e_reg, max-min);
        if length < "8000
        then typ := hw_type
        else typ := fw_type;
        with cnst do
          begin
            o_class := constant;
            o_c_value := length
          end (* with *) ;
        mul_int_ec(valu, cnst, typ);
        pop_operand(valu)   (* result of multiplication *) ;
        with addr do
          begin
            o_a_indexed := true;
            o_a_ixreg := valu.o_e_reg
          end (* with *)
      end
  end (* case *) ;
  push_operand(addr)
end (* index *) ;
(**********************************************************************)
(*                                                                    *)
(*                         branch procedures                          *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                                Jump                                *)
(*                                                                    *)
(**********************************************************************)

procedure jump(lab: integer);
begin
  if lab = initialblock
  then
    begin
      gen_offset(r1, lab); (* we have to use r1 since reg_f is needed for calls to seq pascal progs *)
      gen_rx2(bfc_op, falsemask, -rx2_length, r1)
    end
  else write_jump(falsejump2, falsemask, lab)
end (* jump *) ;



(**********************************************************************)
(*                                                                    *)
(*                             FalseJump                              *)
(*                                                                    *)
(*    Stack:  value/relation -> null                                  *)
(*                                                                    *)
(**********************************************************************)

procedure falsejump(lab: integer);
var
  opnd: operand;
  mask, reg: reg_range;
  op: ilop;
  save_class: operand_class;
begin
  pop_operand(opnd);
  with opnd do
    if stillcond 
    then   (* operand must be a relation *)
      begin
        stillcond := false;
        ensure(opnd, relation);
        case o_r_cond of
          less:
            begin op := falsejump2;  mask := lsmask end;
          equal:
            begin op := truejump2;  mask := nemask end;
          greater,
          inset:
            begin op := falsejump2;  mask := grmask end;
          notless:
            begin op := truejump2;  mask := lsmask end;
          notequal:
            begin op := falsejump2;  mask := nemask end;
          notgreater:
            begin op := truejump2;  mask := grmask end
        end   (* case *) 
      end
    else   (* not stillcond *)
      begin
        save_class := o_class;
        load(opnd, reg);
        if save_class = expression
        then gen_rr(lr_op, o_e_reg, o_e_reg);
        mask := nemask;
        if o_e_negated
        then op := truejump2
        else op := falsejump2;
        dec_ref(o_e_reg)
      end;
  write_jump(op, mask, lab)
end   (* falsejump *) ;



(**********************************************************************)
(*                                                                    *)
(*                              CaseJump                              *)
(*                                                                    *)
(*    Stack:  value -> null                                           *)
(*    Loads selector value into a register;       		      *)
(*    generates a branch to the dispatch code.                        *)
(*    Pushes the number of the register containing the selector       *)
(*    value onto the case stack, and frees the register.              *)
(*    This is basicly unsound, since this register might get          *)
(*    trashed.  In the past, a bug occured where the selector         *)
(*    register was chosen as the register for the line number         *)
(*    at the label.  But the line number register is always r0        *)
(*    now, and the case selector register can't be.  But be careful...*)
(*                                                                    *)
(**********************************************************************)

procedure casejump(lab: integer);
var
  slctr: operand;
  reg: reg_range;
begin
  pop_operand(slctr);
  load(slctr, reg);
  push_case_selector(reg);
  write_jump(falsejump2, falsemask, lab);
  dec_ref(reg)
end (* casejump *) ;



(**********************************************************************)
(*                                                                    *)
(*                              CaseList                              *)
(*                                                                    *)
(*    Selector expression has been loaded into a register. 	      *)
(*    Pops the register number from the case stack.                   *)
(*                                                                    *)
(**********************************************************************)

procedure caselist(min, max: integer);
var
  slctr: operand;
  i, len: integer;
begin
  with slctr do
    begin
      o_class := expression;
      o_e_negated := false;
      pop_case_selector(o_e_reg)
    end (* with *) ;
  len := max - min;
  load_index(slctr, min, max);
  if slctr.o_class <> constant
  then gen_rangecheck(slctr.o_e_reg, max-min);
  with slctr do
    begin
      gen_sf(slls_op, o_e_reg, 2);   (* make fw index *)
      gen_rx2(bfc_op, falsemask, 0, o_e_reg)
    end (* with *) ;
  put2(casejump2, location, len);
  for i := 0 to len do
    begin
      read1arg;  put_arg(arg1)
    end;
  inc_location(fw_size*(len+1))
end   (* caselist *) ;
(**********************************************************************)
(*                                                                    *)
(*                    checking operator procedures                    *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                              Pointer                               *)
(*                                                                    *)
(*    Stack:  value -> value                                          *)
(*    Called only if CHECK.                                           *)
(*                                                                    *)
(**********************************************************************)

procedure pointer;
var
  ptr: operand;
  reg: reg_range;
  save_class: operand_class;
begin
  pop_operand(ptr);
  save_class := ptr.o_class;
  load(ptr, reg);
  if save_class = expression
  then gen_rr(lr_op, reg, reg);
  gen_sf(btfs_op, nemask, (sf_length+extref_length) div hw_size);
  gen_extref(rts_pointererror);
  push_operand(ptr)
end (* pointer *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Variant                               *)
(*                                                                    *)
(*    Stack:  address -> address                                      *)
(*    Called only if CHECK.                                           *)
(*                                                                    *)
(**********************************************************************)

procedure variant(tagset, disp: integer;  typ: types);
var
  l1, l2: integer;
  addr, tagfield: operand;
  tagreg, temp: reg_range;
begin
  pop_operand(addr);  force_address(addr);
  with tagfield do
    begin
      o_class := variable;
      o_v_type := typ;
      o_v_negated := false;
      o_v_indexed := addr.o_a_indexed;
      o_v_base := addr.o_a_base;
      if not (o_v_base in [g_reg, b_reg])
      then inc_ref(o_v_base);
      o_v_ixreg := addr.o_a_ixreg;
      if o_v_ixreg <> 0
      then inc_ref(o_v_ixreg);
      o_v_disp := ct_add(addr.o_a_disp, disp)
    end (* with *) ;
  load(tagfield, tagreg);
  gen_sf(bffs_op, lsmask, (sf_length+extref_length) div hw_size);
  gen_extref(rts_varianterror);
  gen_immediate(ci_op, tagreg, max_tag);
  l1 := lastlength;
  gen_sf(btbs_op, grmask, (extref_length+lastlength) div hw_size);
  get_reg(first_reg, last_reg, temp);
  gen_immediate(li_op, temp, tagset);
  l2 := lastlength;
  gen_rx1(slhl_op, temp, 1, tagreg);
  gen_sf(bfbs_op, carrymask,
         (extref_length+l1+sf_length+l2+ri1_length) div hw_size);
  dec_ref(temp);  dec_ref(tagreg);
  push_operand(addr)
end   (* variant *) ;



(**********************************************************************)
(*                                                                    *)
(*                               Range                                *)
(*                                                                    *)
(*    Stack:  value -> value                                          *)
(*    Called only if CHECK.                                           *)
(*                                                                    *)
(**********************************************************************)

procedure range(min, max: integer);
var
  valu: operand;
  reg: reg_range;
  save_class: operand_class;
begin
  pop_operand(valu);
  with valu do
    case o_class of

      address,
      relation:
        begin
          error(this_pass, cant_happen);
          dummy_operand(valu, expression)
        end;

      constant:
        if (o_c_value < min) or (o_c_value > max)
        then error(this_pass, range_error);

      expression,
      variable:
        begin
          save_class := o_class;
          load(valu, reg);
          if min = 0
          then gen_rangecheck(reg, max)
	  
	  else begin
            gen_immediate(ci_op, reg, min);
            gen_sf(bffs_op, lsmask, (sf_length+extref_length) div hw_size);
            gen_extref(rts_rangeerror);
	    if max = 0
	    then gen_rr(lr_op, reg, reg)
            else gen_immediate(ci_op, reg, max);
            gen_sf(btbs_op, grmask, (extref_length+lastlength) div hw_size)
          end;
        end

    end (* case, with *) ;
  push_operand(valu)
end   (* range *) ;



(**********************************************************************)
(*                                                                    *)
(*                              InitVar                               *)
(*                                                                    *)
(*    Zeroes a routine's local variables at routine entry.            *)
(*    Called only if CHECK.                                           *)
(*                                                                    *)
(**********************************************************************)

procedure initvar(length: integer);
var
  ixreg, temp: reg_range;
begin
  get_reg(first_reg, last_reg, temp);
  get_reg(1, last_reg, ixreg);
  gen_sf(lis_op, temp, 0);
  gen_immediate(li_op, ixreg, -length);
  gen_rx3(sth_op, temp, 0, s_reg, ixreg);
  gen_sf(ais_op, ixreg, hw_size);
  gen_sf(btbs_op, nemask, 4);
  dec_ref(temp);  dec_ref(ixreg)
end   (* initvar *) ;
(**********************************************************************)
(*                                                                    *)
(*                 table-building operator procedures                 *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                              DefLabel                              *)
(*                                                                    *)
(**********************************************************************)

procedure deflabel(lab: integer);
var
  temp: reg_range;
begin
  enter(jumptable, lab, location);
  if number and generate
  then
    begin
      get_reg(r0, r0, temp);
      gen_immediate(li_op, temp, line);
      gen_rx1(sth_op, temp, hw_size, b_reg);
      dec_ref(temp)
    end
end   (* deflabel *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Constant                              *)
(*                                                                    *)
(**********************************************************************)

procedure constant_(length: integer);
var
  i: integer;
begin
  for i := 1 to length div hw_size do
    begin
      read1arg;
      if generate
      then begin
        constants := succ(constants);
        enter(consttable, constants, arg1)
      end
    end
end   (* constant *) ;
(**********************************************************************)
(*                                                                    *)
(*                 miscellaneous operator procedures                  *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                                New_                                *)
(*                                                                    *)
(*    Stack:  address -> null                                         *)
(*                                                                    *)
(**********************************************************************)

procedure new_(length, initialize: integer);
var
  addr: operand;
  ix, temp: reg_range;
begin
  pop_operand(addr);  force_address(addr);
  get_reg(0, 0, temp);
  gen_rx1(l_op, r0, pcb_heapt1, hd_reg);
  gen_immediate(si_op, r0, length);
  gen_new(block)   (* chi r0, stacklength(b) *) ;
  gen_sf(bffs_op, lsmask, (sf_length+extref_length) div hw_size);
  gen_extref(rts_heaplimit);
  gen_rx1(st_op, r0, pcb_heapt1, hd_reg);
  with addr do
    gen_rx(st_op, r0, o_a_disp, o_a_base, o_a_ixreg);
  free_regs(addr);

  if (initialize = 1) and check
  then   (* zero the allocated area *)
    begin
      get_reg(1, last_reg, ix);  get_reg(1, last_reg, temp);
      gen_rr(lr_op, temp, r0)   (* ^ allocated area *) ;
      gen_sf(lis_op, r0, 0);
      gen_immediate(li_op, ix, length-hw_size);
      gen_rx3(sth_op, r0, 0, temp, ix);
      gen_sf(sis_op, ix, hw_size);
      gen_sf(bfbs_op, lsmask, 4);
      dec_ref(ix);  dec_ref(temp)
    end;

  dec_ref(r0)
end   (* new_ *) ;



(**********************************************************************)
(*                                                                    *)
(*                              Buildset                              *)
(*                                                                    *)
(*    Stack:  (set)variable X value -> (set)variable                  *)
(*    Assumes that a set occupies an integral number of fullwords.    *)
(*                                                                    *)
(**********************************************************************)

procedure buildset;
var
  setvar, index: operand;
  temp, ix: work_reg;
  offset: integer;
begin
  pop_operand(index);  pop_operand(setvar);
  ensure(setvar, variable);
  if not temporary(setvar)
  then   (* make a copy before altering *)
    begin
      unindex(setvar);
      get_reg(first_reg, last_reg, temp);
      get_reg(1, last_reg, ix);
      allocate_temp(set_size, offset);
      gen_immediate(li_op, ix, set_size - fw_size);
      with setvar do
        gen_rx3(l_op, temp, o_v_disp, o_v_base, ix);
      gen_rx3(st_op, temp, offset, b_reg, ix);
      gen_sf(sis_op, ix, fw_size);
      gen_sf(bfbs_op, lsmask, 7);
      dec_ref(ix);  dec_ref(temp);
      free_regs(setvar);
      with setvar do
        begin
          o_v_disp := offset;
          o_v_base := b_reg;
          o_v_ixreg := r0;
          o_v_indexed := false
        end
    end;
  load_index(index, 0, max_set);
  if index.o_class <> constant
  then gen_rangecheck(index.o_e_reg, max_set);
  gen_code(sbt_op, index.o_e_reg, setvar);
  dec_ref(index.o_e_reg);
  push_operand(setvar)
end   (* buildset *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Pop                                 *)
(*                                                                    *)
(*    Uses global variable SaveLength.                                *)
(*                                                                    *)
(**********************************************************************)

procedure pop(length (* in bytes *) : integer);
var
  i, poplength: integer;
  opnd: operand;
begin
  if calling_program
  then   (* pop interface addresses and restore temps *)
    begin
      calling_program := false;
      poplength := length + savelength;
      pop_stack(poplength);
      gen_immediate(si_op, s_reg, poplength);
      savelength := 0;
      for i := 0 to saved_temps-1 do
        begin
          get_operand(i, opnd);
          restore_regs(opnd)
        end   (* for *) ;
      saved_temps := 0;
      if prog_set_temps > 0
      then
        begin
          deallocate_temp(prog_set_temps);
          prog_set_temps := 0
        end
    end
  else   (* pop temps from operand stack *)
    for i := 1 to length div fw_size do
      begin
        pop_operand(opnd);
        free_regs(opnd)
      end
end   (* pop *) ;



(**********************************************************************)
(*                                                                    *)
(*                                Eom                                 *)
(*                                                                    *)
(**********************************************************************)

procedure eom(varl: integer);
begin
  done := true;
  varlength := varl;
  put0(eom2)
end   (* eom *) ;



(**********************************************************************)
(*                                                                    *)
(*                              For_Lim                               *)
(*                                                                    *)
(*    Forces evaluation of the limit expression before entering a     *)
(*    FOR-loop.                                                       *)
(*                                                                    *)
(**********************************************************************)

procedure for_lim;
var
  limit: operand;
  reg: reg_range;
begin
  pop_operand(limit);
  case limit.o_class of
    address,
    relation:
      begin
        error(this_pass, cant_happen);
        dummy_operand(limit, expression)
      end;
    constant,
    expression:
      ;
    variable:
      load(limit, reg)
  end (* case *) ;
  push_operand(limit)
end (* for_lim *) ;
(**********************************************************************)
(*                                                                    *)
(*           pass initialization and termination procedures           *)
(*                                                                    *)
(**********************************************************************)



(**********************************************************************)
(*                                                                    *)
(*                             Beginpass                              *)
(*                                                                    *)
(**********************************************************************)

procedure beginpass;
begin
  rewrite(ilout);  reset(ilin);
  with link^ do
    begin
      summary := summaryoption in options;
      bstack := bstackoption in options;
      test := testoption in options;
      check := checkoption in options;
      number := numberoption in options;
      new(resetpoint);
      allocate(jumptable, labels);
      allocate(blocktable, blocks);
      allocate(stacktable, blocks);
      allocate(consttable, constants div hw_size)
    end;
  generate := true;  calling_program := false;  stillcond := false;
  location := 0;  constants := 0;  saved_temps := 0; listlinecount := 0;
  initline;
  clear_regs;
  init_operand_stack;  init_case_stack;
  if not number then wrchar(list, FF);
  if test then printff(list, this_pass)
end   (* beginpass *);



(**********************************************************************)
(*                                                                    *)
(*                              Endpass                               *)
(*                                                                    *)
(**********************************************************************)

procedure endpass;
begin
  with link^ do
    begin
      if generate then options := options + [codeoption];
      new(tables);
      tables^.proglength := obj_prefix_len + location + constants;
      tables^.codelength := location;
      tables^.stacklength := stacklength;
      tables^.varlength := varlength;
      tables^.jumptable := jumptable;
      tables^.blocktable := blocktable;
      tables^.stacktable := stacktable;
      tables^.consttable := consttable
    end (* with *);
  if number
  then printeol(list)
end   (* endpass *);
(**********************************************************************)
(*                                                                    *)
(*                          il operator scan                          *)
(*                                                                    *)
(**********************************************************************)

procedure scan;
begin
  done := false;  op := noop1;
  repeat
    lastop := op;  read_ifl(op);
    if stillcond and not (op in [falsejump1, not1])
    then materialize;
    case op of

      buildset1,
      callprog1,
      error1,
      for_lim1,
      not1,
      pointer1:
        ;

      add1 (* type *),
      and1 (* type *),
      assign1 (* type *),
      casejump1 (* label *),
      constant1 (* length, value *),
      copy1 (* length *),
      decrement1 (* type *),
      deflabel1 (* label *),
      div1 (* type *),
      eom1 (* varlength *),
      falsejump1 (* label *),
      field1 (* disp *),
      increment1 (* type *),
      initvar1 (* length *),
      jump1 (* label *),
      mod1 (* type *),
      mul1 (* type *),
      neg1 (* type *),
      newline1 (* number *),
      or1 (* type *),
      pop1 (* length *),
      procedure1 (* standard procedure *),
      pushconst1 (* value *),
      pushind1 (* type *),
      pushlabel1 (* label *),
      return1 (* mode *),
      sub1 (* type *):
        read1arg;

      assigntag1 (* length, type *),
      caselist1 (* min, max, labels *),
      compare1 (* comparison, type *),
      compstruc1 (* comparison, length *),
      function1 (* standard function, type *),
      funcvalue1 (* mode, type *),
      message1 (* pass, error *),
      new1 (* length, initialize *),
      pushaddr1 (* mode, disp *),
      range1 (* min, max *):
        read2args;

      call1 (* mode, label, paramlength *),
      index1 (* min, max, length *),
      indexnc1 (* min, max, length *),
      pushvar1 (* type, mode, disp *),
      variant1 (* tagset, disp, type *):
        read3args;

      init1 (* mode, label, paramlength, varlength *):
        read4args;

      enter1 (* mode, label, paramlength, varlength, templength *):
        read5args

    end (* case *) ;
    if generate
    then
      case op of

        add1 (* type *):
          add(arg1);

        and1 (* type *):
          and_(arg1);

        assign1 (* type *):
          assign(arg1);

        assigntag1 (* length, type *):
          assigntag(arg1, arg2);

        buildset1:
          buildset;

        call1 (* mode, label, paramlength *):
          call(arg1, arg2, arg3);

        callprog1:
          callprog;

        casejump1 (* label *) :
          casejump(arg1);

        caselist1 (* min, max, labels *):
          caselist(arg1, arg2);

        compare1 (* comparison, type *):
          compare(arg1, arg2);

        compstruc1 (* comparison, length *):
          comparestruc(arg1, arg2);

        constant1 (* length, value *):
          constant_(arg1);

        copy1 (* length *):
          copystruc(arg1);

        decrement1 (* type *):
          inc_dec(decrement, arg1);

        deflabel1 (* label *):
          deflabel(arg1);

        div1 (* type *):
          div_(arg1);

        enter1 (* mode, label, paramlength, varlength, templength *):
          enter_(arg1, arg2, arg3, arg4, arg5);

        eom1 (* varlength *):
          eom(arg1);

        error1:
          generate := false;

        falsejump1 (* label *):
          falsejump(arg1);

        field1 (* disp *):
          field(arg1);

        for_lim1:
          for_lim;

        function1 (* standardfunc, type *):
          std_func(arg1, arg2);

        funcvalue1 (* mode, type *):
          funcvalue(arg1, arg2);

        increment1 (* type *):
          inc_dec(increment, arg1);

        index1 (* min, max, length *):
          index(arg1, arg2, arg3, true);

        indexnc1 (* min, max, length *):
          index(arg1, arg2, arg3, false); (* don't generate the range check *)

        init1 (* mode, label, paramlength, varlength *):
          if arg1 = process_mode
          then initproc(arg2, arg3, arg4)
          else initmonclass(arg1, arg2, arg3, arg4);

        initvar1 (* length *):
          if check then initvar(arg1);

        jump1 (* label *):
          jump(arg1);

        message1 (* pass, error *):
          error(arg1, arg2);

        mod1 (* type *):
          mod_(arg1);

        mul1 (* type *):
          mul(arg1);

        neg1 (* type *):
          neg(arg1);

        new1 (* length, initialize *):
          new_(arg1, arg2);

        newline1 (* number *):
          newline(arg1);

        not1:
          not_;

        or1 (* type *):
          or_(arg1);

        pointer1:
          if check then pointer;

        pop1 (* length *):
          pop(arg1);

        procedure1 (* standardprocedure *):
          std_proc(arg1);

        pushaddr1 (* mode, disp *):
          pushaddress(arg1, arg2);

        pushconst1 (* value *):
          pushconst(arg1);

        pushind1 (* type *):
          pushindirect(arg1);

        pushlabel1 (* label *):
          pushlabel(arg1);

        pushvar1 (* type, mode, disp *):
          pushvar(arg1, arg2, arg3);

        range1 (* min, max *):
          if check then range(arg1, arg2);

        return1 (* mode *):
          return(arg1);

        sub1 (* type *):
          sub(arg1);

        variant1 (* tagset, disp, type *):
          if check then variant(arg1, arg2, arg3)

      end (* case *)
    else (* not generate *)
      if op = caselist1
      then caselist(arg1, arg2)
      else if op = constant1
      then constant_(arg1)
      else if op = deflabel1
      then deflabel(arg1)
      else if op = eom1
      then eom(arg1)
      else if op = message1
      then error(arg1, arg2)
      else if op = newline1
      then newline(arg1)
  until done
end (* scan *);
(**********************************************************************)
(*                                                                    *)
(*                            main program                            *)
(*                                                                    *)
(**********************************************************************)

begin (* pass61 *)
  initpass(this_pass);
  beginpass;
  scan;
  endpass;
  break(list);
  nextpass(this_pass)
end   (* pass61 *);

begin end.
   LRU