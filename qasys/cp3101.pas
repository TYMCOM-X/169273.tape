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
: **  File Name          : cp3101.pas                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     Cp3101.pas contains the code for the 3rd pass of the     **
: **     concurrent pascal compiler.  This pass is responsible    **
: **     for scope analysis.                                      **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.01 09/24/79 DEG  1162     Increased noun_max to 1000      **
: ** 01.01 11/12/79 DEG  1162     Initialize syscomp_level        **
: ** 01.01 11/26/79 DEG  1162     Increased noun_max to 1500      **
: ** 01.01 12/27/79 RNP  1162     Increased noun_max to 3000      **
: ** 01.01 12/27/79 RNP  1162     Increased spelling_max to 1400  **
: ** 01.01 04/18/80 SLC  1162     Increased update_max to 150     **
: ** 01.01 04/18/80 SLC  1162     Increased max_level to 30       **
: ** 01.01 04/18/80 SLC  1162     Display of statistics, Abort    **
: **                              error message on console        **
: ** 01.01 05/13/80 SLC  1162     Increased noun_max to 3500      **
: ** 01.01 05/13/80 SLC  1162     Increased op_max to 350         **
: ** 01.01 05/13/80 SLC  1162     Increased upd_max to 350        **
: ** 01.01 05/13/80 SLC  1162     Increased max_level to 20       **
: ** 01.01 07/09/80 SLC  1162     Increased noun_max to 3800      **
: ** 01.01 07/09/80 SLC  1162     Decreased op_max to 200         **
: ** 01.01 07/09/80 SLC  1162     Decreased upd_max1 to 200       **
: ** 01.01 09/25/80 MDS  1162     Decreased max_level to 10       **
: ** 01.01 09/25/80 MDS  1162     Increased noun_max to 4500      **
: ** 01.01 09/25/80 MDS  1162     Increased spelling_max to 4500  **
: ** 01.01 10/28/80 MDS  1162     Increased noun_max to 4800      **
: ** 01.01 12/04/80 MDS  1162     Increased noun_max to 5000      **
: ** 01.01 12/12/80 MDS  1162     Increased max_level to 20       **
: ** 01.01 12/31/80 MDS  1162     Increased update_max to 260     **
: ** 01.01 12/31/80 MDS  1162     Increased upd_max1 to 260       **
: ** 01.01 01/02/81 MDS  1162     Increased noun_max to 5500      **
: ** 01.01 03/05/81 MDS  1162     Increased update_max to 300     **
: ** 01.01 03/05/81 MDS  1162     Increased upd_max1 to 300       **
: ** 01.01 05/19/81 BH   1162     Increased noun_max to 6000      **
: ** 01.01 06/12/81 BH   1162     Fixed bug with attempted        **
: **                              recursive function calls by     **
: **                              changes in fname                **
: ** 01.01 08/07/81 BH   1162     Finished new xref               **
: ** 01.01 08/07/81 BH   1162     Increased noun_max to 6500      **
: ** 01.01 09/30/81 BH   1162     Increased noun_max to 7000      **
: ** 01.01 06/17/81 BH   1162     Increased noun_max to 7500      **
: ** 01.01 01/30/85 PJH  1162     Increased update_max to 400     **
: ** 01.01 01/30/85 PJH  1162     Increased upd_max1 to 401       **
: ** 01.01 09/30/85 PJH  1162     Increased noun_max to 16000     **
: ** 01.02 12/15/86 PJH  1162     Increased noun_max to 18000     **
: ** 01.02 12/15/85 PJH  1162     Increased update_max to 500     **
: ** 01.02 12/15/85 PJH  1162     Increased upd_max1 to 501       **
: ** 01.02 12/15/85 PJH  1162     Increased spelling_max to 8512  **
: ** 01.02 12/15/85 PJH  1162     ADDED PROPRIETARY BANNER        **
: **                                                              **
: *****************************************************************)

program cp3e, cpas3e;

(*###########
#  prefix  #
###########*)

const
listoption = 0;    summaryoption = 1;  testoption = 2;     checkoption = 3;
codeoption = 4;    numberoption = 5;   xrefoption = 6;     bstackoption = 7;
dumpoption = 9;    lgxrefoption = 8;

type
       pointer = ^ integer;
option = listoption..dumpoption;
passptr = ^passlink;
passlink =
  record
    options: set of option;
    labels, blocks, constants: integer;
    resetpoint: pointer;
    tables: pointer
  end;

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
procedure cpas3e(inter_pass_ptr: passptr;
		 var ilin, ilout: int_file;
		 var list: text;
		 var xrefile: int_file);

const

(*input operators*)

eom1=1;            const_id1=2;        const_def1=3;       type_id1=4;
type_def1=5;       var_id1=6;          var_list1=7;        evar_list1=8;
inits_def1=9;      inits_end1=10;      proc_id1=11;        proc_def1=12;
proce_def1=13;     proc_end1=14;       proce_end1=15;      func_id1=16;
func_def1=17;      funce_def1=18;      func_end1=19;       funce_end1=20;
prog_id1=21;       prog_def1=22;       intf_id1=23;        type1=24;
enum1=25;          enum_id1=26;        enum_def1=27;       subr_def1=28;
set_def1=29;       array_def1=30;      rec1=31;            field_id1=32;
fieldlist1=33;     rec_def1=34;        class1=35;          monitor1=36;
process1=37;       stack1=38;          pstart1=39;         parm_id1=40;
parm_type1=41;     univ_type1=42;      cparmlist1=43;      vparmlist1=44;
body1=45;          body_end1=46;       aname1=47;          store1=48;
call_name1=49;     call1=50;           arg_list1=51;       arg1=52;
falsejump1=53;     def_label1=54;      jump_def1=55;       intf1=56;
def_case1=57;      case1=58;           jump1=59;           end_case1=60;
address1=61;       for_store1=62;      for_lim1=63;        for_up1=64;
for_down1=65;      with_var1=66;       with_temp1=67;      with1=68;
init_name1=69;     init1=70;           value1=71;          lt1=72;
eq1=73;            gt1=74;             le1=75;             ne1=76;
ge1=77;            in1=78;             uplus1=79;          uminus1=80;
plus1=81;          minus1=82;          or1=83;             star1=84;
slash1=85;         div1=86;            mod1=87;            and1=88;
fname1=89;         not1=90;            empty_set1=91;      include1=92;
function1=93;      call_func1=94;      name1=95;           comp1=96;
sub1=97;           arrow1=98;          constant1=99;       real1=100;
freal1=101;        integer1=102;       finteger1=103;      char1=104;
fchar1=105;        string1=106;        fstring1=107;       new_line1=108;
lconst1=109;       message1=110;       proce_id1=111;      funce_id1=112;
pend1=113;         case_jump1=114;     blockname1=115;

(*output operators*)

eom2=1;            type_def2=2;        new_noun2=3;        var_list2=4;
evar_list2=5;      inits_def2=6;       proc_def2=7;        proce_def2=8;
func_def2=9;       funce_def2=10;      prog_def2=11;       type2=12;
enum_def2=13;      subr_def2=14;       set_def2=15;        intf2=16;
array_def2=17;     rec2=18;            fieldlist2=19;      rec_def2=20;
class2=21;         monitor2=22;        process2=23;        stack2=24;
pstart2=25;        parm_type2=26;      univ_type2=27;      cparmlist2=28;
vparmlist2=29;     body2=30;           body_end2=31;       address2=32;
result2=33;        store2=34;          call_proc2=35;      call_prog2=36;
intf_id2=37;       parm2=38;           falsejump2=39;      def_label2=40;
jump_def2=41;      funcf_def2=42;      jump2=43;           case_list2=44;
for_store2=45;     for_lim2=46;        for_up2=47;         for_down2=48;
with_var2=49;      with_temp2=50;      with2=51;           init2=52;
value2=53;         lt2=54;             eq2=55;             gt2=56;
le2=57;            ne2=58;             ge2=59;             in2=60;
uplus2=61;         uminus2=62;         plus2=63;           minus2=64;
or2=65;            star2=66;           slash2=67;          div2=68;
mod2=69;           and2=70;            not2=71;            empty_set2=72;
include2=73;       function2=74;       call_func2=75;      routine2=76;
var2=77;           arrow2=78;          vcomp2=79;          rcomp2=80;
sub2=81;           index2=82;          real2=83;           string2=84;
lconst2=85;        message2=86;        new_line2=87;       fwd_def2=88;
chk_type2=89;      procf_def2=90;      undef2=91;          pend2=92;
case_jump2=93;	   

(*other constants*)

noun_max=18000;
min_case=0;        max_case=300;       this_pass=3;        spelling_max=8512;
operand_max=225;   update_max=500;     upd_max1=501;       max_level=20;

(*modes*)

class_mode=1;      monitor_mode=2;     process_mode=3;     proc_mode=4;
proce_mode=5;      func_mode=6;        funce_mode=7;       program_mode=8;

(*standard spelling/noun indices*)

xundef=0;          xfalse=1;           xtrue=2;            xinteger=3;
xboolean=4;        xchar=5;            xqueue=6;           xabs=7;
xattribute=8;      xchr=9 ;            xcontinue=10;       xconv=11;
xdelay=12;         xempty=13;          xio=14;             xord=15;
xpred=16;          xstop=17;           xrealtime=18;       xsetheap=19;
xsucc=20;          xtrunc=21;          xstart=22;          xwait=23;
xreal=24;

(*standard noun indices*)

zarithmetic=25;    zindex=26;          zpassive=27;        zvparm=28;
zcparm=29;         zsparm=30;          zwith=31;

(*errors*)

unres_error=1;     ambiguity_error=2;  abort_error=3;      constid_error=4;
subr_error=5;      few_args_error=6;   arg_list_error=7;   many_args_error=8;
lblrange_error=9;  lbltype_error=10;   ambilbl_error=11;   with_error=12;
init_error=13;     proc_use_error=14;  name_error=15;      comp_error=16;
sub_error=17;      interface_error=18; call_name_error=19; zerdiv_error=20;
arrow_error=21;    resolve_error=22;

(*miscellaneous*)

not_possibly_forward=false;            possibly_forward=true;
output=true;       retain=false;       proc_type= 1;       std_level=0;
global_level=1;

index_const=1;	real_const=2;	string_const=3;	variable=4;
parameter=5;	field=6;	scalar_kind=7;	syscomp_kind=8;
routine_kind=9;	set_kind=10;	program_kind=11;pointer_kind=12;
array_kind=13;	record_kind=14;	with_kind=15;	undef_kind=16;
redef_kind=17;  subr_kind=18;
  
type

  entry_kind=index_const..subr_kind;

  operand_class=(var_class,routine_class,iconst_class,rconst_class,sconst_class,
    def_class,undef_class,fconst_class,program_class,case_label,
    funcvalue_class,emptyset_class);

  error_note=(yes,no,suppress);

  spelling_index=0..spelling_max;

  noun_index = 0..noun_max;

  stack_index=0..operand_max;

  update_index=0..update_max;

  name_ptr=^name_rec;

  variant_ptr=^variant_rec;

  entry_ptr=^entry_rec;

  entry_rec=
    record
      noun:noun_index;
      spix:spelling_index;
      case kind:entry_kind of
	index_const:(const_type:noun_index; const_val:integer);
	real_const:(real_disp:integer);
	string_const:(string_length,string_disp:integer);
	variable:(var_type:entry_ptr);
	parameter:(parm_type,next_parm,parm_parent:entry_ptr);
	field:(field_type:entry_ptr; variant:variant_ptr; field_parent:entry_ptr);
	scalar_kind:(range_type:noun_index);
	syscomp_kind:(init_stat:entry_ptr; entry_name:name_ptr; 
			syscomp_mode:integer);
	routine_kind:(rout_parm:entry_ptr; rout_type:noun_index;
			rout_parent:entry_ptr);
	set_kind:(set_type:noun_index);
	program_kind:(prog_parm:entry_ptr; interface:name_ptr);
	pointer_kind:(object_type,next_fwd:entry_ptr);
	array_kind:(index_type:noun_index; el_type:entry_ptr);
	with_kind:(with_type:noun_index);
	record_kind:(field_name:name_ptr);
	redef_kind:(redef_type:entry_ptr);
	subr_kind:(subr_type:noun_index; subr_max, subr_min: integer)
    end;

  operand=
    record
      case class:operand_class of
	var_class:(vtype,vnoun:entry_ptr);
	program_class:(prog,pparm:entry_ptr);
	routine_class:(rout,parm:entry_ptr);
	funcvalue_class:(func_type:noun_index);
	iconst_class:(iconst_type:noun_index; iconst_val:integer);
	rconst_class:(rconst_disp:integer);
	sconst_class:(sconst_length,sconst_disp:integer);
	case_label:(lab,index:integer);
	def_class:(def_entry:entry_ptr; def_spix:spelling_index)
    end;

  name_access=(general,external,internal,incomplete,
    unres_routine,qualified,functional,undefined);

  level_index=0..max_level;

  spelling_entry=
    record
      entry:entry_ptr;
      level:level_index;
      access:name_access
    end;

  display_rec=
    record
      base:0..upd_max1;
      level_entry:entry_ptr;
      prev_syscomp:level_index;
      prev_list: name_ptr
    end;

  update_rec=
    record
      update_spix:spelling_index;
      old_entry:spelling_entry
    end;

  packed_set=0 .. 65535;

  variant_rec=
    record
      tag_disp:integer;
      label_set:packed_set;
      next_variant:variant_ptr
    end;

  name_rec=
    record
      name_spix:spelling_index;
      name_entry:entry_ptr;
      next_name:name_ptr
    end;

var

  parameterized,constants: set of operand_class;

  qualifiable,types,const_kinds: set of entry_kind;

  name_list, old_names: name_ptr;
  
  entry_root, e_loop: entry_ptr;

  halt_flag,test,xref,resolution: boolean;

  ops:array [stack_index] of operand;

  uentry,first_parm,this_parm: entry_ptr;

  comp_modes,entry_modes: set of class_mode..program_mode;

  inaccessible,entry_access,op_access: set of name_access;

  labels: array [min_case..max_case] of integer;

  this_update: -1 .. update_max;
   upsum : -1..update_max;

  t:-1 .. operand_max;
  opsum: -1..operand_max;

  enum_val,this_label,sy,const_disp, unres_count: integer;

  enum_type,this_noun,nounsum: noun_index;

  updates:array [update_index] of update_rec;

  display:array [level_index] of display_rec;

  syscomp_level,this_level,body_level,levelsum: level_index;

  spelling_table:array [spelling_index] of spelling_entry;
  
  line_no: integer;
  
(*#############*)
(*pass routines*)
(*#############*)


procedure read_ifl(var i: integer);
begin
  i := ilin^;  get(ilin)
end;

procedure write_ifl(i: integer);
begin
  ilout^ := i;  put(ilout)
end;

procedure wrxref(i: integer);
begin
  xrefile^ := i; put(xrefile)
end;

  (* this procedure writes an entry to the xref file *)
  procedure wrxentry(entry: entry_rec);
  begin
    if xref then begin
      wrxref(1);
      with entry do begin
	wrxref(spix); 
	wrxref(noun);
	wrxref(line_no);
	wrxref(kind);
	case kind of
	index_const: wrxref(const_type);
	real_const,
	string_const:;
	variable: wrxref(var_type^.noun);
	parameter: begin
	  wrxref(parm_type^.noun);
	  wrxref(parm_parent^.noun)
	end;
	field: begin
	  wrxref(field_type^.noun);
	  wrxref(field_parent^.noun)
	end;
	scalar_kind: wrxref(range_type);
	syscomp_kind: begin
	  wrxref(syscomp_mode);
	  wrxref(init_stat^.noun)
	end;
	routine_kind:begin
	  wrxref(rout_type);
	  wrxref(rout_parent^.noun)
	end;
	set_kind: wrxref(set_type);
	program_kind:;
	pointer_kind: wrxref(object_type^.noun);
	array_kind: begin
	  wrxref(index_type);
	  wrxref(el_type^.noun)
	end;
	with_kind,
	record_kind:;
	redef_kind: wrxref(redef_type^.noun);
	subr_kind: wrxref(subr_type);
	others: 
	end
      end
    end
  end;

  (* this writes a reference note to the xref file *)
  procedure wrxrefnote(n: integer);
  begin
    if xref
    then begin
      if n>zwith then begin
	wrxref(2); (* say what sort of thing we have *)
	wrxref(line_no); (* what line... *)
	wrxref(n) (* and the noun number *)
      end
    end
  end;
  
  procedure put_arg(arg:integer);
  begin
    write_ifl(arg);
    if test then printarg(list, arg)
  end;

  procedure put0(op:integer);
  begin
    write_ifl(op);
    if test then printop(list, op)
  end;

  procedure put1(op,arg:integer);
  begin
    write_ifl(op); write_ifl(arg);
    if test then begin
      printop(list, op); printarg(list, arg)
    end
  end;

  procedure put2(op,arg1,arg2:integer);
  begin
    write_ifl(op); write_ifl(arg1); write_ifl(arg2);
    if test then begin
      printop(list, op);
      printarg(list, arg1); printarg(list, arg2)
    end
  end;

  procedure put3(op,arg1,arg2,arg3:integer);
  begin
    put2(op,arg1,arg2);
    put_arg(arg3)
  end;

  procedure put4(op,arg1,arg2,arg3,arg4:integer);
  begin
    put3(op,arg1,arg2,arg3); put_arg(arg4)
  end;

  procedure ignore1(op:integer);
  var arg:integer;
  begin
    read_ifl(arg); put1(op,arg)
  end;

  procedure i2(op:integer);
  var arg1,arg2:integer;
  begin
    read_ifl(arg1); read_ifl(arg2);
    put2(op,arg1,arg2)
  end;

  procedure ignore3(op:integer);
  var arg1,arg2,arg3:integer;
  begin
    read_ifl(arg1); read_ifl(arg2); read_ifl(arg3);
    put3(op,arg1,arg2,arg3)
  end;

  procedure lconst(include: boolean);
  var length,i,arg:integer;
  begin
    read_ifl(length); put1(lconst2,length);
    if include
    then const_disp:=const_disp+length;
    for i:=1 to length div 2 do begin
      read_ifl(arg); put_arg(arg)
    end
  end;

  procedure error(number:integer);
  begin
    put2(message2,this_pass,number);
    giverr(line_no, this_pass, number);
  end;

  procedure messpass;
  var
    mess, pass: integer;
  begin
    read_ifl(pass); read_ifl(mess);
    put2(message2, pass, mess);
    giverr(line_no, pass, mess)
  end;

  procedure abort(i : integer);
  begin
    error(abort_error); halt_flag:=true;
  case i of
     1 : message(' max_level overflow  ');
     2 : message(' operand_max overflow  ');
     3 : message(' noun_max overflow ');
     4 : message(' update_max overflow ');
     5 : message(' tag_stack_max overflow ') (* sequential pascal only... *)
    end;
   halt;
  end;
(*##############*)
(*initialization*)
(*##############*)

  procedure std_id(var std_entry:entry_ptr; index:spelling_index);
  begin
    new(std_entry); std_entry^.noun:=index;
    with spelling_table[index] do begin
      entry:=std_entry;
      level:=std_level;
      access:=general
    end
  end;

  procedure std_const(const_index,type_index:spelling_index;
    const_value:integer);
  var const_entry:entry_ptr;
  begin
    std_id(const_entry,const_index);
    with const_entry^ do begin
      kind:=index_const;
      const_type:=type_index;
      const_val:=const_value
    end
  end;

  procedure std_parm(var parm_entry:entry_ptr; parmtype:entry_ptr;
    parm_index:noun_index);
  begin
    new(parm_entry);
    with parm_entry^ do begin
      noun:=parm_index;
      kind:=parameter;
      parm_type:=parmtype;
      next_parm:=nil
    end
  end;

  procedure std_entry(var e:entry_ptr; index:noun_index);
  begin
    new(e);
    with e^ do begin
      noun:=index;
      kind:=undef_kind
    end
  end;

  procedure std_rout(rout_index,routtype:noun_index; first_parm:entry_ptr);
  var rout_entry:entry_ptr;
  begin
    std_id(rout_entry,rout_index);
    with rout_entry^ do begin
      kind:=routine_kind;
      rout_parent:=nil;
      rout_parm:=first_parm;
      rout_type:=routtype
    end
  end;

  procedure std_scalar(var scalar_entry:entry_ptr; scalar_index:spelling_index);
  begin
    std_id(scalar_entry,scalar_index);
    with scalar_entry^ do begin
      kind:=scalar_kind;
      spix := scalar_index;
      range_type:=scalar_index
    end
  end;

  procedure initialize;
  var i:integer; int_type,real_type,bool_type,char_type,queue_type,
    index_type,arith_type,passive_type,arith_sparm,int_cparm,queue_vparm,
    pas2_vparm,pas1_vparm,char_cparm,index_cparm,index1_cparm,real_cparm,
    index_sparm,queue_cparm,io_vparm: entry_ptr;
  begin
    levelsum:= 0; opsum:= 0; nounsum:= 0; upsum:= 0;
    rewrite(ilout);  reset(ilin);
    initpass(this_pass);
    with inter_pass_ptr^ do begin
      test:=testoption in options;
      xref:=xrefoption in options
    end;
    if test then printff(list, this_pass);
    this_noun:=zwith;
    entry_root:=nil;
    const_disp:=0;
    halt_flag:=false; resolution:=false;
    unres_count:= 0;
    parameterized:=[routine_class,program_class];
    comp_modes:=[class_mode,monitor_mode,process_mode];
    entry_modes:=[proce_mode,funce_mode];
    qualifiable:=[syscomp_kind,record_kind];
    constants:=[iconst_class,rconst_class,sconst_class,emptyset_class];
    types:=[scalar_kind,array_kind,record_kind,pointer_kind,set_kind,
      undef_kind,subr_kind,syscomp_kind];
    op_access:=[general,internal,qualified,functional];
    const_kinds:=[index_const,real_const,string_const];
    inaccessible:=[undefined,incomplete];
    entry_access:=[external,unres_routine];
    this_update:=-1; t:=-1;
    this_level:=std_level; syscomp_level:=std_level;
    for i:=0 to spelling_max do
      spelling_table[i].access:=undefined;
    (*standard entrys*)
    std_const(xfalse,xboolean,0);
    std_const(xtrue,xboolean,1);
    std_entry(uentry,xundef);
    std_entry(index_type,zindex);
    std_entry(arith_type,zarithmetic);
    std_entry(passive_type,zpassive);
    std_id(queue_type,xqueue); queue_type^.kind:=undef_kind;
    std_scalar(int_type,xinteger);
    std_scalar(real_type,xreal);
    std_scalar(bool_type,xboolean);
    std_scalar(char_type,xchar);
    std_parm(arith_sparm,arith_type,zsparm);
    std_parm(int_cparm,int_type,zcparm);
    std_parm(queue_cparm,queue_type,zcparm);
    std_parm(queue_vparm,queue_type,zvparm);
    std_parm(char_cparm,char_type,zcparm);
    std_parm(index_cparm,index_type,zcparm);
    std_parm(index_sparm,index_type,zsparm);
    std_parm(pas2_vparm,passive_type,zcparm);
    pas2_vparm^.next_parm:=index_cparm;
    std_parm(pas1_vparm,passive_type,zvparm);
    pas1_vparm^.next_parm:=pas2_vparm;
    std_parm(io_vparm,passive_type,zvparm);
    io_vparm^.next_parm:=pas1_vparm;
    std_parm(index1_cparm,index_type,zcparm);
    index1_cparm^.next_parm:= index_cparm;
    std_parm(real_cparm,real_type,zcparm);
    std_rout(xabs,zarithmetic,arith_sparm);
    std_rout(xattribute,xinteger,index_cparm);
    std_rout(xchr,xchar,int_cparm);
    std_rout(xcontinue,proc_type,queue_vparm);
    std_rout(xconv,xreal,int_cparm);
    std_rout(xdelay,proc_type,queue_vparm);
    std_rout(xempty,xboolean,queue_cparm);
    std_rout(xio,proc_type,io_vparm);
    std_rout(xord,xinteger,char_cparm);
    std_rout(xpred,zindex,index_sparm);
    std_rout(xstop,proc_type,index1_cparm);
    std_rout(xrealtime,xinteger,nil);
    std_rout(xsetheap,proc_type,int_cparm);
    std_rout(xsucc,zindex,index_sparm);
    std_rout(xtrunc,xinteger,real_cparm);
    std_rout(xstart,proc_type,nil);
    std_rout(xwait,proc_type,nil);
  end;
(*#######*)
(*nesting*)
(*#######*)

  procedure push_level(e:entry_ptr);
  begin
    if this_level>=max_level then abort(1) else this_level:=this_level+1;
    with display[this_level] do begin
      levelsum:= this_level;
      base:=this_update+1;
      level_entry:=e;
      prev_syscomp:=syscomp_level;
      prev_list:=name_list; name_list:=nil
    end
  end;

  procedure pop_level;
  var u:update_index;
  begin (* pop_level *)
    with display [this_level] do begin
      syscomp_level:=prev_syscomp;
      name_list:=prev_list;
      for u:=this_update downto base do
	with updates[u] do begin 
	  spelling_table[update_spix]:=old_entry;
	end;
      this_update:=base-1
    end;
    this_level:=this_level-1
  end;
(*#############*)
(*name handling*)
(*#############*)

  procedure push;
  begin
    if t>= operand_max then abort(2) else
    t:=t+1;
    opsum:=t;
  end;

  procedure new_entry(var e:entry_ptr);
  begin
    if this_noun>=noun_max then abort(3) else
    this_noun:=this_noun+1;
    nounsum:= this_noun;
    new(e);
    with e^ do begin
      noun:=this_noun; 
      kind:=undef_kind;
      spix:=0;
    end;
    entry_root:=e
  end;

  procedure push_new_entry(var e:entry_ptr);
  begin
    push; new_entry(e);
    with ops[t] do begin
      class:=def_class;
      def_entry:=e; def_spix:=xundef
    end
  end;

  procedure update(spix:spelling_index; e:entry_ptr; a:name_access);
  begin
    if this_level<>global_level then begin
      (*save old entry*)
      if this_update>=update_max then abort(4) else
      this_update:=this_update+1;
      upsum:= this_update;
      with updates[this_update] do begin
	update_spix:=spix;
	old_entry:=spelling_table[spix]
      end
    end;
    with spelling_table[spix] do begin
      entry:=e; level:=this_level; access:=a
    end
  end;

  procedure push_new_name(resolve,output:boolean; a:name_access);
  var spix:spelling_index; e:entry_ptr;
  begin
    read_ifl(spix);
    if spix<>xundef then
      with spelling_table[spix] do
	if (access<>undefined) and (level=this_level) then
	  if resolve and (access=unres_routine) then begin
	    e:=entry; access:= a; resolution:= true;
	    unres_count:= unres_count - 1
	  end else begin
	    error(ambiguity_error); spix:=xundef;
	  end
	else begin
	  new_entry(e);
	  e^.spix:=spix;
	  update(spix,e,a)
	end;
    push;
    with ops[t] do
      if spix=xundef then begin
	class:=undef_class;
	if output then put1(new_noun2,xundef)
      end else begin
	class:=def_class; def_entry:=e; def_spix:=spix;
	if output then put1(new_noun2,e^.noun)
      end
  end;

  procedure push_old_name;
  var spix:spelling_index;
  begin
    push; read_ifl(spix);
    with ops[t],spelling_table[spix] do
      if access in inaccessible then begin
	error(name_error);
	class:=undef_class
      end else begin
	class:=def_class;
	def_entry:=entry; def_spix:=spix;
      end
  end;

  procedure find_name(nlist:name_ptr; spix:spelling_index; var e:entry_ptr);
  var name:name_ptr;
  begin
    e:=nil; name:=nlist;
    while name<>nil do
      with name^ do
	if name_spix=spix then begin
	  e:=name_entry; name:=nil
	end else name:=next_name;
    if e=nil then begin
      error(name_error);
      e:=uentry
    end
  end;

  procedure chain_name(e:entry_ptr; spix:spelling_index);
  var n:name_ptr;
  begin
    new(n);
    with n^ do begin
      name_spix:=spix;
      name_entry:=e;
      next_name:=name_list; name_list:=n
    end
  end;

  procedure set_access(spix:spelling_index; a:name_access);
  begin
    spelling_table[spix].access:=a;
    t:=t-1
  end;

  procedure enter_names(list:name_ptr; access:name_access);
  var this_name:name_ptr;
  begin
    this_name:=list;
    while this_name<>nil do
      with this_name^ do begin
	update(name_spix,name_entry,access);
	this_name:=next_name
      end
  end;

  function defined:boolean;
  begin
    defined:=ops[t].class<>undef_class
  end;

  procedure define (var e: entry_ptr);
  begin
    with ops[t] do
      if class = def_class then e:= def_entry else e:= uentry
  end;

(*#################*)
(*type declarations*)
(*#################*)

  procedure type_id;
  var spix:spelling_index;
  begin
    read_ifl(spix);
    with spelling_table[spix] do begin
      if (access<>undefined) and (level=this_level) then begin
	spix:=xundef;
	error(ambiguity_error)
      end else update(spix,nil,incomplete)
    end;
    push;
    with ops[t] do
      if spix=xundef then class:=undef_class else begin
	class:=def_class; def_spix:=spix; def_entry:=nil
      end
  end;

  procedure type_def;
  begin
    with ops[t-1] do
      if class=def_class then
	with spelling_table[def_spix] do begin
	  if defined 
	  then begin
	    if ops[t].def_entry^.spix = xundef
	    then entry := ops[t].def_entry
	    else begin
	      new_entry(entry);
	      entry^.kind := redef_kind;
	      entry^.redef_type := ops[t].def_entry;
	    end
	  end
	  else entry:=uentry;
	  entry^.spix:=def_spix;
	  wrxentry(entry^);
	  access:=general
	end;
    t:=t-2; put0(type_def2)
  end;

  procedure type_(output:boolean; op:integer);
  var base_type: entry_ptr;
  begin
    push_old_name;
    base_type := ops[t].def_entry; (* loop back until we find the "real" type *)
    while (base_type^.kind = redef_kind) do
      base_type := base_type^.redef_type;
    wrxrefnote(base_type^.noun); (* leave a trace of where we found it *)
    if defined then
      if not(base_type^.kind in types) then begin
	error(name_error); ops[t].class:=undef_class;
      end;
    if output then
      if defined then put1(op,base_type^.noun)
	else put1(op,xundef);
  end;

  procedure enum_id;
  begin
    push_new_name(not_possibly_forward,retain,general);
    if defined then begin
      with ops[t].def_entry^ do begin
	  kind:=index_const;
	  const_type:=enum_type;
	  enum_val:=enum_val+1; const_val:=enum_val
      end;
      wrxentry(ops[t].def_entry^);
    end;
    t:=t-1
  end;

  procedure enum;
  var e:entry_ptr;
  begin
    push_new_entry(e);
    enum_val:=-1;
    with e^ do begin
      kind:=scalar_kind;
      spix := xundef;
      range_type:=noun; enum_type:=noun
    end
  end;

  procedure enum_def;
  begin
    put2(enum_def2,enum_type,enum_val)
  end;
  
  procedure subr_def;
  var min,max:integer; type1:noun_index; e:entry_ptr;
  begin
    min:=0; max:=1; type1:=xundef;
    with ops[t] do
      if class=iconst_class then begin
	max:=iconst_val; type1:=iconst_type
      end else error(subr_error);
    with ops[t-1] do
      if class=iconst_class then begin
	min:=iconst_val;
	if (min>max) or (iconst_type<>type1) then error(subr_error)
      end else error(subr_error);
    t:=t-2;
    push_new_entry(e);
    with e^ do begin
      kind:=subr_kind;
      subr_type:=type1;
      subr_min:=min;
      subr_max:=max;
      wrxentry(e^);
      put4(subr_def2,noun,type1,min,max)
    end
  end;

  procedure set_def;
  var e:entry_ptr;
    el: entry_ptr; (* the base type of the set *)
  begin
    if defined
    then el := ops[t].def_entry
    else el := uentry;
    t:=t-1;
    push_new_entry(e); e^.kind:=set_kind;
    e^.set_type := el^.noun;
    wrxentry(e^);
    put1(set_def2,e^.noun)
  end;

  procedure array_def;
  var index:noun_index; e,el:entry_ptr;
  begin
    define(el);
    t:=t-1;
    if defined then index:=ops[t].def_entry^.noun else index:=xundef;
    t:=t-1;
    push_new_entry(e);
    with e^ do begin
      kind:=array_kind;
      index_type:=index;
      el_type:=el;
      put1(array_def2,noun)
    end;
    wrxentry(e^)
  end;

  procedure rec;
  var e:entry_ptr;
  begin
    put0(rec2);
    push_new_entry(e);
    push_level(e)
  end;

  procedure field_list;
  var i,number:integer; typ:entry_ptr;
  begin
    read_ifl(number);
    define(typ);
    t:=t-1;
    for i:=1 to number do
      with ops[t] do
      if defined then begin
	with def_entry^ do begin
	  kind:=field;
	  field_type:=typ;
	  field_parent := uentry
	end;
	chain_name(def_entry,def_spix);
	set_access(def_spix,internal)
       end else t:=t-1;
    put1(fieldlist2,number)
  end;

  procedure rec_def;
  var n:name_ptr;
  begin
    with ops[t].def_entry^ do begin
      kind:=record_kind;
      field_name:=name_list;
      wrxentry(ops[t].def_entry^);
      put1(rec_def2,noun)
    end;
    n:=name_list; (* now link down the fields filling in the parent *)
    while n <> nil do begin
      n^.name_entry^.field_parent:=ops[t].def_entry;
      wrxentry(n^.name_entry^);
      n := n^.next_name
    end;
    pop_level
  end;

  procedure comp_def(op:integer; mode: integer);
  var e:entry_ptr;
  begin
    syscomp_level:=this_level;
    with ops[t].def_entry^ do begin
      kind:=syscomp_kind;
      syscomp_mode:=mode;
      push_new_entry(e) (*initial statement*);
      init_stat:=e;
      put2(op,noun,e^.noun)
    end;
    with e^ do begin
      kind:=routine_kind;
      rout_parent:=ops[t-1].def_entry;
      rout_parm:=first_parm; rout_type:=proc_type;
      wrxentry(e^) (* get the initial statement out there *)
    end;
    t:=t-1
  end;

  procedure inits_def;
  begin
    put0(inits_def2);
    ops[t].def_entry^.entry_name:=name_list;
  end;
(*#####################*)
(*variable declarations*)
(*#####################*)

  procedure var_list(op:integer);
  var i,number:integer; typ:entry_ptr;
  begin
    read_ifl(number); put1(op,number);
    define(typ);
    t:=t-1;
    for i:=1 to number do
      with ops[t] do
       if defined then begin
	with def_entry^ do begin
	  kind:=variable;
	  var_type:=typ;
	  wrxentry(def_entry^)
	end;
	if op=evar_list2 then chain_name(def_entry,def_spix);
	set_access(def_spix,internal)
       end else t:=t-1
  end;
(*###################*)
(*routine declarations*)
(*###################*)

  procedure proc_def(op:integer);
  begin
    if defined then
      with ops[t].def_entry^ do begin
	  kind:=routine_kind;
	  rout_parent:=ops[t-1].def_entry;
	  rout_parm:=first_parm;
	  rout_type:=proc_type;
	  wrxentry(ops[t].def_entry^);
	  if resolution then begin
	    resolution:=false; put1(procf_def2,noun)
	  end else put1(op,noun)
	end
      else put1(op,xundef)
  end;

  procedure func_def(op:integer);
  const no_output=false; noop=0;
  var typ:noun_index;
    base_type: entry_ptr;
  begin
    type_(no_output,noop);
    base_type := ops[t].def_entry; (* loop back until we find the "real" type *)
    while (base_type^.kind = redef_kind) do
      base_type := base_type^.redef_type;
    if defined then typ:=base_type^.noun else typ:=xundef;
    t:=t-1;
    if defined then
      with ops[t].def_entry^ do begin
	  kind:=routine_kind;
	  rout_parent:=ops[t-1].def_entry;
	  rout_parm:=first_parm;
	  rout_type:=typ;
	  wrxentry(ops[t].def_entry^);
	  if resolution then begin
	    resolution:=false; put2(funcf_def2,typ,noun)
	  end else put2(op,typ,noun)
	end
      else put2(op,xundef,xundef)
  end;

  procedure rout_end(a:name_access);
  begin
    if defined then set_access(ops[t].def_spix,a) else t:=t-1;
    pop_level;
  end;

  procedure prog_def;
  begin
    with ops[t] do begin
      if defined then begin
	with def_entry^ do begin
	  kind:=program_kind;
	  prog_parm:=first_parm;
	  interface:=name_list;
	  put1(prog_def2,noun)
	end;
	wrxentry(def_entry^);
	set_access(def_spix,internal)
      end else begin put1(prog_def2,xundef); t:=t-1 end;
    name_list:= old_names
    end;
  end;

  procedure intf_id;
  var spix: spelling_index; intf_entry: entry_ptr;
  begin
    read_ifl(spix);
    if spix<>xundef then
    with spelling_table[spix] do
      if (access<>undefined) and (level=syscomp_level) then
	if access in entry_access then
	  chain_name(entry,spix)
	else error(interface_error)
      else begin (*forward reference*)
	new_entry(intf_entry); put1(fwd_def2, intf_entry^.noun);
	intf_entry^.spix:= spix;
	chain_name(intf_entry, spix);
	update(spix, intf_entry, unres_routine);
	unres_count:= unres_count + 1
      end
  end;

  procedure pstart;
  var m:integer; e:entry_ptr;
  begin
    read_ifl(m); put1(pstart2,m);
    if m in comp_modes then push_new_entry(e)
    else if m in entry_modes then
      if defined then begin
	with ops[t] do chain_name(def_entry,def_spix)
      end;
    if defined then e:=ops[t].def_entry else e:=uentry;
    push_level(e);
    first_parm:=nil
  end;

  procedure parmlist(op:integer);
  var i,number:integer; ptype:entry_ptr;
  begin
    define(ptype);
    read_ifl(number);
    put1(op,number);
    for i:=number downto 1 do
      with ops[t-i] do
       if class=def_class then begin
	with def_entry^ do begin
	  kind:=parameter;
	  parm_type:=ptype;
	  parm_parent:=ops[t-number-1].def_entry;
	  if first_parm=nil then first_parm:=def_entry
	  else this_parm^.next_parm:=def_entry;
	  this_parm:=def_entry;
	  next_parm:=nil;
	  wrxentry(def_entry^)
	end;
	spelling_table[def_spix].access:=internal
       end;
    t:=t-number-1
  end;
(*####*)
(*body*)
(*####*)

  procedure body;
  begin
    body_level:=this_level;
    put0(body2)
  end;

  procedure aname;
  begin
    with ops[t] do
      if class=funcvalue_class then put1(result2,func_type)
      else put0(address2)
  end;

  procedure call_name;
  var intf:name_ptr; err:boolean;
  begin
    err:=false;
    with ops[t] do begin
      if class=program_class then begin
	put0(intf2);
	intf:=prog^.interface;
	while intf<>nil do
	  with intf^ do begin
	    put1(intf_id2,name_entry^.noun);
	    intf:=next_name
	  end
      end else if class=routine_class then
	if rout^.rout_type<>proc_type then err:=true else (*ok*)
      else err:=true;
      if err then begin
	error(call_name_error);
	class:=undef_class
      end
    end
  end;

  procedure call(op:integer);
  begin
    with ops[t] do
      if class=routine_class then begin
	if parm<>nil then error(few_args_error);
	put0(op)
      end else if class=program_class then begin
	if pparm<>nil then error(few_args_error);
	put0(call_prog2)
	end else put0(op);
    if op<>call_func2 then t:=t-1
  end;

  procedure arg_list;
  begin
    with ops[t] do
      if class in parameterizzzed then (*ok*)
      else begin
	error(arg_list_error);
	class:=undef_class
      end
  end;

  procedure arg;
  var this_parm, base_type:entry_ptr; err:error_note;
  begin
    err:=no;
    with ops[t-1] do
      if class=routine_class then begin
	this_parm:=parm;
	if this_parm=nil then err:= yes else parm:=this_parm^.next_parm
      end else if class=program_class then begin
	this_parm:=pparm;
	if this_parm=nil then err:= yes else pparm:=this_parm^.next_parm
      end else err:=suppress;
    if err<>no then begin
      if err=yes then error(many_args_error);
      put2(parm2,xundef,xundef)
    end else
      with this_parm^ do begin
	base_type := parm_type; (* loop back until we find the "real" type *)
	while (base_type^.kind = redef_kind) do
	  base_type := base_type^.redef_type;
	put2(parm2,noun,base_type^.noun);
      end;
    t:=t-1 (*pop argument*); 
  end;

  procedure def_case;
  begin
    read_ifl(this_label);
    put1(def_label2,this_label)
  end;

  procedure case_;
  var val:integer;
  begin
    with ops[t] do
      if class=iconst_class then begin
	put1(chk_type2,iconst_type);
	val:=iconst_val;
	class:=case_label;
	lab:=this_label;
	if (val>=min_case) and (val<=max_case) then
	  index:=val else begin
	    error(lblrange_error);
	    val:=0
	  end
      end else begin
	t:=t-1;
	error(lbltype_error)
      end
  end;

  procedure end_case;
  var l0,ln,min,max,i:integer;
  begin
    read_ifl(l0); read_ifl(ln);
    for i:=min_case to max_case do labels[i]:=ln;
    if ops[t].class=case_label then begin
     min:=ops[t].index; max:=min;
    end else begin min:=0; max:=0 end;
    while ops[t].class=case_label do begin
	with ops[t] do begin
	  if labels[index]=ln then
	    labels[index]:=lab
	  else error(ambilbl_error);
	  if index>max then max:=index else
	    if index<min then min:=index
	end;
	t:=t-1
    end;
      t:=t-1;
      put3(case_list2,l0,min,max);
      for i:=min to max do put_arg(labels[i]);
      put_arg(ln)
  end;

  procedure with_temp;
  var temp:entry_ptr; err:boolean;
  begin
    err:=false;
    with ops[t] do
      if class=var_class then
	with vtype^ do
	  if kind in qualifiable then begin
	    new_entry(temp);
	    with temp^ do begin
	      put1(with_temp2,noun);
	      kind:=with_kind;
	      with_type:=vtype^.noun
	    end;
	    push_level(temp);
	    if kind=record_kind then enter_names(field_name,qualified)
	    else enter_names(entry_name,qualified);
	  end else err:=true
      else err:=true;
    if err then begin
      error(with_error);
      push_level(uentry); put1(with_temp2,xundef)
    end;
    t:=t-1
  end;

  procedure init_name;
  var err:boolean;
  begin
    err:=false;
    with ops[t] do begin
      if class=var_class then
	with vtype^ do
	  if kind=syscomp_kind then begin
	    with init_stat^ do begin
	      put1(rcomp2,noun);
	      class:=routine_class;
	      parm:=rout_parm
	    end;
	    rout:=init_stat
	  end else err:=true
      else err:=true;
      if err then begin
	error(init_error);
	class:=undef_class
      end
    end
  end;
(*##########*)
(*expression*)
(*##########*)

  procedure fname;
  var typ:noun_index;
  begin
    with ops[t] do
      if class=routine_class then
	with rout^ do begin
	  if rout_type=proc_type then begin
	    error(proc_use_error);
	    typ:=xundef
	  end else typ:=rout_type;
	  put1(function2, typ);
	  if parm<>nil then error(few_args_error);
	  put0(call_func2)
	end
      else if class=funcvalue_class 
      then begin
	error(name_error);
	put1(function2, xundef);
	put0(call_func2)
      end
  end;

  procedure function_error(error_num:integer);
  begin
    error(error_num);
    ops[t].class:=undef_class
  end;

  procedure function_;
  var typ: noun_index;
  begin
    typ:= xundef;
    with ops[t] do
      if class=routine_class then
	with rout^ do
	  if rout_type = proc_type then
	    function_error(proc_use_error)
	  else typ:= rout_type
      else function_error(name_error);
    put1(function2, typ)
  end;

  procedure binary(op:integer);
  begin
    put0(op);
    t:=t-1
  end;

  procedure pop2(op:integer);
  begin
    put0(op);
    t:=t-2
  end;
(*########*)
(*variable*)
(*########*)

  procedure push_operand(op_entry:entry_ptr; comp,result:boolean);
  var op:integer;
  begin
    if not comp then push;
    with ops[t] , op_entry^ do
      case kind of
	index_const: begin
	  class:=fconst_class;
	  put2(index2,const_val,const_type)
	end;
	real_const: begin
	  class:=fconst_class;
	  put1(real2,real_disp)
	end;
	string_const: begin
	  class:=fconst_class;
	  put2(string2,string_length,string_disp)
	end;
	variable,field,parameter: begin
	  class:=var_class;
	  vnoun := op_entry;
	  case kind of
	    variable:vtype:=var_type;
	    field: vtype:=field_type;
	    parameter: vtype:=parm_type
	  end;
	  while vtype^.kind = redef_kind do (* claw down to the base kind *)
	    vtype := vtype^.redef_type;
	  if comp then op:=vcomp2 else op:=var2;
	  put2(op,noun,vtype^.noun)
	end;
	routine_kind: begin
	  if result then begin
	    class:=funcvalue_class;
	    func_type:=op_entry^.rout_type
	  end else begin
	    class:=routine_class;
	    rout:=op_entry;
	    parm:=rout_parm
	  end;
	  if comp then op:=rcomp2 else op:=routine2;
	  put1(op,noun)
	end;
	program_kind: begin
	  class:=program_class;
	  prog:=op_entry;
	  pparm:=prog_parm;
	  put1(routine2,noun)
	end;
	scalar_kind,subr_kind,syscomp_kind,pointer_kind,array_kind,record_kind,
	set_kind, redef_kind, with_kind, undef_kind: begin
	  error(name_error);
	  class:=undef_class;
	  if not comp then put0(undef2)
	end;
        others: begin (* trap error for kdawson pjh *)
		writeln(tty, 'kind = ', kind);
		end
      end
  end;

  procedure name;
  var spix:spelling_index; comp,err,result:boolean; name_entry:entry_ptr;
  begin
    read_ifl(spix); err:=false; comp:=false; result:=false;
    with spelling_table[spix] do
      if access in op_access then begin
	name_entry:=entry;
	case access of
	  general: ;
	  functional: result:=true;
	  internal: if level<syscomp_level then err:=true;
	  qualified: begin
	    comp:=true; push (*with temp*);
	    with display[level].level_entry^ do begin
	      put2(var2,noun,zwith);
	      put1(arrow2,with_type)
	    end
	  end
	end
      end else err:=true;
    if err then begin
      error(name_error);
      name_entry:=uentry
    end;
    push_operand(name_entry,comp,result);
    wrxrefnote(name_entry^.noun)
  end;

  procedure comp;
  const qualified=true; not_result=false;
  var spix:spelling_index; component:entry_ptr; name_list:name_ptr;
    err:boolean;
  begin
    read_ifl(spix); err:=false;
    with ops[t] do
      if class=var_class then begin
	with vtype^ do
	  if kind=record_kind then name_list:=field_name else
	  if kind=syscomp_kind then name_list:=entry_name
	  else begin err:=true; name_list:=nil end;
	find_name(name_list,spix,component)
      end else err:=true;
    if err then error(comp_error)
    else begin
      wrxrefnote(component^.noun);
      push_operand(component,qualified,not_result)
    end
  end;

  procedure sub_err;
  begin
    error(sub_error);
    put2(sub2,xundef,xundef)
  end;

  procedure sub;
  var
    base_type: entry_ptr;
  begin
    t := t-1;
    with ops[t] do
      if class=var_class then
	with vtype^ do
	  if kind=array_kind then begin
	    base_type := el_type;
	    while base_type^.kind = redef_kind do
	      base_type := base_type^.redef_type;
	    put2(sub2,index_type,base_type^.noun);
	    vtype:=el_type
	  end else sub_err
      else sub_err;
  end;
(*########*)
(*constant*)
(*########*)

  procedure constant;
  begin
    push_old_name;
    with ops[t] do
      if class=def_class then
	with def_entry^ do
	  if kind in const_kinds then begin
	    wrxrefnote(noun);
	    case kind of
	      index_const: begin
		class:=iconst_class;
		iconst_type:=const_type;
		iconst_val:=const_val
	      end;
	      real_const: begin
		class:=rconst_class; rconst_disp:=real_disp
	      end;
	      string_const:begin
		class:=sconst_class;
		sconst_length:=string_length;
		sconst_disp:=string_disp
	      end
	    end
	  end
	  else begin class:=undef_class; error(constid_error) end
  end;

  procedure real_;
  begin
    push;
    with ops[t] do begin
      class:=rconst_class; rconst_disp:=const_disp
    end
  end;

  procedure freal;
  begin
    push; ops[t].class:=fconst_class;
    put1(real2,const_disp)
  end;

  procedure index(typ:noun_index);
  begin
    push;
    with ops[t] do begin
      class:=iconst_class;
      iconst_type:=typ;
      read_ifl(iconst_val)
    end
  end;

  procedure findex(typ:noun_index);
  var value:integer;
  begin
    push; ops[t].class:=fconst_class;
    read_ifl(value);
    put2(index2,value,typ)
  end;

  procedure string;
  begin
    push;
    with ops[t] do begin
      class:=sconst_class;
      read_ifl(sconst_length);
      sconst_disp:=const_disp
    end
  end;

  procedure fstring;
  var length:integer;
  begin
    push; ops[t].class:=fconst_class;
    read_ifl(length); put2(string2,length,const_disp)
  end;
(*#####################*)
(*constant evaluation  *)
(*#####################*)

procedure uop(sy: integer);
begin
  with ops[t] do begin
    if class <> iconst_class
    then error(constid_error)
    else if iconst_type <> xinteger
	 then error(constid_error)  
	 else if sy = uminus1
	      then iconst_val := -iconst_val
  end
end;

procedure bop(sy: integer);
var
  result : integer;
begin
  if (ops[t].class <> iconst_class) or (ops[t-1].class <> iconst_class)
  then error(constid_error)
  else if (ops[t].iconst_type <> xinteger) or (ops[t-1].iconst_type <> xinteger)
       then error(constid_error)
       else begin
	 if sy = plus1
	 then result := ops[t-1].iconst_val + ops[t].iconst_val
	 else if sy = minus1
	 then result := ops[t-1].iconst_val - ops[t].iconst_val
	 else if sy = star1
	 then result := ops[t-1].iconst_val * ops[t].iconst_val
	 else if ops[t].iconst_val = 0
	 then error(zerdiv_error)
	 else if sy = div1
	 then result := ops[t-1].iconst_val div ops[t].iconst_val
	 else if sy = mod1
	 then result := ops[t-1].iconst_val mod ops[t].iconst_val;
	 t := t-1;
	 ops[t].iconst_val := result
       end;
end;
     
(*#####################*)
(*constant declarations*)
(*#####################*)

  procedure const_id;
  var 
    top : stack_index;
  begin
    push_new_name(not_possibly_forward,retain,incomplete);
    top := t; (* remember... *)
    repeat
      read_ifl(sy);
      case sy of
	message1:
	  messpass;
	const_def1, value1, fname1:
	  ;
	finteger1:
	  index(xinteger);
	fchar1:
	  index(xchar);
	name1:
	  constant;
	freal1:
	  real_;
	fstring1:
	  string;
	lconst1:
	  lconst(true);
	uplus1,uminus1:
	  uop(sy);
	plus1,minus1,star1,div1,mod1:
	  bop(sy);
	new_line1:
	  begin read_ifl(line_no); put1(new_line2, line_no); end;
	others:
	  error(constid_error)
      end;
  until sy = const_def1;
  with ops[t-1] do
    if class=def_class then begin
      with def_entry^, ops[t] do
	if class in constants 
	then begin
	  case class of
	    iconst_class: begin
	      kind:=index_const;
	      const_type:=iconst_type; const_val:=iconst_val
	    end;
	    rconst_class: begin
	      kind:=real_const; real_disp:=rconst_disp
	    end;
	    sconst_class: begin
	      kind:=string_const;
	      string_length:=sconst_length;
	      string_disp:=sconst_disp
	    end
	  end;
	  wrxentry(ops[t-1].def_entry^)
	end
	else error(constid_error);
      t:=top; set_access(def_spix,general)
    end else t:=top-1
end;

(*#########*)
(*main loop*)
(*#########*)

begin
  initialize;
  repeat read_ifl(sy); case sy of

 address1: put0(address2);
 aname1: aname;
 and1: binary(and2);
 arg_list1: arg_list;
 arg1: arg;
 array_def1: array_def;
 blockname1: lconst(false);
 body_end1: put0(body_end2);
 body1: body;
 call_func1: call(call_func2);
 call_name1: call_name;
 call1: call(call_proc2);
 case_jump1: ignore1(case_jump2);
 case1: case_;
 char1: index(xchar);
 class1: comp_def(class2, class_mode);
 comp1: comp;
 const_id1: const_id;
 constant1: constant;
 cparmlist1: parmlist(cparmlist2);
 def_case1: def_case;
 def_label1: ignore1(def_label2);
 div1: binary(div2);
 empty_set1: begin push; put0(empty_set2) end;
 end_case1: end_case;
 enum_def1: enum_def;
 enum_id1: enum_id;
 enum1: enum;
 eom1:begin  halt_flag:=true; wrxentry(ops[t].def_entry^); end;
 eq1: binary(eq2);
 evar_list1: var_list(evar_list2);
 falsejump1: begin ignore1(falsejump2); t:=t-1 end;
 fchar1: findex(xchar);
 field_id1,parm_id1, var_id1: push_new_name(not_possibly_forward,
    output,incomplete);
 fieldlist1: field_list;
 finteger1: findex(xinteger);
 fname1: fname;
 for_down1: ignore2(for_down2);
 for_lim1: begin ignore3(for_lim2); t:=t-1 end;
 for_store1: pop2(for_store2);
 for_up1: ignore2(for_up2);
 freal1: freal;
 fstring1: fstring;
 func_def1: func_def(func_def2);
 func_end1, proc_end1: rout_end(internal);
 funce_def1: func_def(funce_def2);
 func_id1: push_new_name(not_possibly_forward,retain,functional);
 funce_end1, proce_end1: rout_end(external);
 funce_id1: push_new_name(possibly_forward,retain,functional);
 function1: function_;
 ge1: binary(ge2);
 gt1: binary(gt2);
 include1: binary(include2);
 init_name1: init_name;
 inits_def1: inits_def;
 inits_end1: pop_level;
 init1: call(init2);
 integer1: index(xinteger);
 intf_id1: intf_id;
 intf1: begin pop_level; old_names:= name_list; name_list:= nil end;
 in1: binary(in2);
 jump_def1: ignore2(jump_def2);
 jump1: ignore1(jump2);
 lconst1: lconst(true);
 le1: binary(le2);
 lt1: binary(lt2);
 message1: messpass;
 minus1: binary(minus2);
 mod1: binary(mod2);
 monitor1: comp_def(monitor2, monitor_mode);
 name1: name;
 new_line1: begin read_ifl(line_no); put1(new_line2, line_no) end;
 ne1: binary(ne2);
 not1: put0(not2);
 or1: binary(or2);
 parm_type1: type_(output,parm_type2);
 pend1: put0(pend2);
 plus1: binary(plus2);
 proc_def1: proc_def(proc_def2);
 proc_id1,prog_id1: push_new_name(not_possibly_forward,retain,incomplete);
 proce_def1: proc_def(proce_def2);
 proce_id1: push_new_name(possibly_forward,retain,incomplete);
 process1: comp_def(process2, process_mode);
 prog_def1: prog_def;
 pstart1: pstart;
 real1: real_;
 rec_def1: rec_def;
 rec1: rec;
 set_def1: set_def;
 slash1: binary(slash2);
 stack1: ignore1(stack2);
 star1: binary(star2);
 store1: pop2(store2);
 string1: string;
 subr_def1: subr_def;
 sub1: sub;
 type_def1: type_def;
 type_id1: type_id;
 type1: type_(output,type2);
 uminus1: put0(uminus2);
 univ_type1: type_(output,univ_type2);
 uplus1: put0(uplus2);
 value1: put0(value2);
 var_list1: var_list(var_list2);
 vparmlist1: parmlist(vparmlist2);
 with_temp1: with_temp;
 with_var1: put0(with_var2);
 with1: begin pop_level; put0(with2) end
 end

  until halt_flag;
  if unres_count > 0 then error(unres_error);
  put0(eom2);
  with inter_pass_ptr^ do begin
    dispose(resetpoint);  new(resetpoint);  constants:=const_disp
  end;
  with inter_pass_ptr^ do
     if summaryoption in options then
      begin
	levelsum:= (100 * levelsum) div max_level;
	message('max_level:',levelsum:3,'% ');
	opsum:= (100 * opsum) div operand_max;
	message('operand_max:',opsum:3,'% ');
	nounsum:= (100 * nounsum) div noun_max;
	message('noun_max:',nounsum:3,'% ');
	upsum:= (100 * upsum) div update_max;
	message('update_max:',upsum:3,'% ');
      end;
  break(list);
  if xref then wrxref(-1); (* signal the end of the noun/xref defs *)
  nextpass(this_pass);
end   (* cpas3e *) ;

begin end.
    b_x4Õ