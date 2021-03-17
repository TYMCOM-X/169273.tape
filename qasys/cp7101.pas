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
: **  File Name          : cp7101.pas                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     Cp7101.pas contains the code for the 7th pass of the     **
: **     concurrent pascal compiler.  This pass generates the     **
: **     engine assembly code output file.                        **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.01 04/23/79 DEG           Original                        **
: ** 01.01 04/24/79 DEG  1162     Corrected definitions of signed_**
: **                              halfword and signed_fullword    **
: ** 01.01 04/24/79 DEG  1162     In gen_rx2, corrected adjustment**
: **                              of negative displacement        **
: ** 01.01 04/25/79 DEG  1162     In casejump, generate a table   **
: **                              of branches instead of addresses**
: ** 01.01 05/15/79 DEG  1162     IL change: call and loadaddr    **
: **                              deleted, offset added           **
: ** 01.01 05/22/79 DEG  1162     IL change: jump and falsejump   **
: **	                          replaced by truejump and false- **
: **                              jump. Mask argument added       **
: ** 01.01 08/03/79 DEG  1162     Added messages for new pass61   **
: **                              error codes                     **
: ** 01.01 11/01/79 DEG  1162     Do not generate code if not     **
: **                              codeoption                      **
: ** 01.01 06/08/81 BH   1162     Removed "can't happen" message  **
: ** 01.01 11/13/84 PJH  1162     Added a switch to generate an   **
: **                              RI2 format in procedure scan at **
: **                              case init1 for when the stack-  **
: **                              length exceeds "7fff            **
: ** 01.02 12/15/86 PJH  1162     ADDED PROPRIETARY BANNER        **
: **                                                              **
: *****************************************************************)

program p7e, pass7e;

const
  maxword = 100;

(* sizes *)

  hw_size   = 2 (* bytes *);
  fw_size   = 4;

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
  errstr = packed array[1..25] of char;

(**********************************************************************)
(*								      *)
(*			       externals			      *)
(*								      *)
(**********************************************************************)

procedure initpass(p: pass_range);  extern;

procedure nextpass(p: pass_range);  extern;

procedure printsummary;  extern;

procedure errmess(pass, number: integer; var text: errstr); extern;
(**********************************************************************)
(*								      *)
(*				 entry				      *)
(*								      *)
(**********************************************************************)

procedure pass7e(link: passptr;
		 var ilin, object: int_file;
		 var list: text);

const
  this_pass = 7;

(* input IL operators *)

  eom1       = 0;
  rr_op1     = 1;
  sf_op1     = 2;
  rx1_op1    = 3;
  rx2_op1    = 4;
  rx3_op1    = 5;
  ri1_op1    = 6;
  ri2_op1    = 7;
  truejump1  = 8;
  falsejump1 = 9;
  casejump1  = 10;
  new1       = 11;
  message1   = 12;
  offset1    = 13;
  enter1     = 14;
  init1      = 15;
  data1      = 16;

(* selected Engine opcodes *)

  btc_op = "42;
  bfc_op = "43;
  lhi_op = "C8;
  chi_op = "C9;
  li_op  = "F8;

(* selected Engine condition code masks *)

  falsemask = 0;

(* selected register definitions *)

  r0 = 0;
  r3 = 3;
  s_reg = 12;
  b_reg = 14;
  q_reg = 15;
  unindexed = 0;

(* selected constants *)

  bit32 = "80000000;

type
  hw_index = (left, right);
  bit = 0..1;
  reg_range = 0..15;
  opcode_range = 0..255;
  unsigned_halfword = 0.."FFFF;
  signed_halfword = -"8000.."7FFF;
  unsigned_fullword = 0.."FFFFFFFF;
  signed_fullword = -"80000000.."7FFFFFFF;
  rx1_disp = 0.."3FFF;
  rx2_disp = -"4000.."3FFF;
  rx3_disp = 0.."FFFFFF;

var
  first_mess,
  generate,
  lines,
  bstack,
  summary:  boolean;

  prevline,
  arg1, arg2, arg3, arg4, arg5,
  tmp,
  codelength,
  constants,
  proglength,
  stacklength,
  varlength:  integer;

  blocktable,
  consttable,
  jumptable,
  stacktable:  tableptr;

  errtext: errstr;

  which_halfword:  hw_index;

  object_change:
    record
      case boolean of
	   false: (word: integer);
	   true:  (halfword: packed array [hw_index]
			     of unsigned_halfword)
    end;

  rx1_change:
    packed record
      case boolean of
	   false: (bit16: bit;
		   bit17: bit;
		   d2: rx1_disp);
	   true:  (hw: unsigned_halfword)
    end;

  rx2_change:
    packed record
      case boolean of
	   false: (bit16: bit;
		   d2: 0.."7FFF);
	   true:  (hw: unsigned_halfword)
    end;

  rx3_change:
    packed record
      case boolean of
	   false: (bit16: bit;
		   bit17: bit;
		   bit18: bit;
		   bit19: bit;
		   sx2: reg_range;
		   a2: rx3_disp);
	   true:  (hw2: unsigned_halfword;
		   hw3: unsigned_halfword)
    end;
(**********************************************************************)
(*                                                                    *)
(*                          input procedures                          *)
(*                                                                    *)
(**********************************************************************)

procedure read_ifl(var i: integer);
begin
  i := ilin^;  get(ilin);
end;

(**********************************************************************)

procedure read1arg;
begin
  read_ifl(arg1)
end;

(**********************************************************************)

procedure read2args;
begin
  read_ifl(arg1);  read_ifl(arg2)
end;

(**********************************************************************)

procedure read3args;
begin
  read_ifl(arg1);  read_ifl(arg2);  read_ifl(arg3)
end;

(**********************************************************************)

procedure read4args;
begin
  read_ifl(arg1);  read_ifl(arg2);
  read_ifl(arg3);  read_ifl(arg4)
end;

(**********************************************************************)

procedure read5args;
begin
  read_ifl(arg1);  read_ifl(arg2);  read_ifl(arg3);
  read_ifl(arg4);  read_ifl(arg5)
end;
(**********************************************************************)
(*								      *)
(*			   Object file class			      *)
(*								      *)
(**********************************************************************)

procedure init_object;
begin
  object_change.word := 0;
  which_halfword := left
end;

(**********************************************************************)

procedure emit_halfword(hw: unsigned_halfword);
begin
  if generate
  then
    with object_change do
      begin
	halfword[which_halfword] := hw;
	if which_halfword = left
	then which_halfword := right
	else
	  begin
	    object^ := word;  put(object);
	    which_halfword := left
	  end
      end (* with *)
end;

(**********************************************************************)

procedure flush_object;
begin
  if which_halfword = right
  then
    with object_change do
      begin
	halfword[right] := 0;
	object^ := word;  put(object)
      end
end;
(**********************************************************************)
(*								      *)
(*			   output procedures			      *)
(*								      *)
(**********************************************************************)

procedure emit_fullword(sfw: signed_fullword);
var
  change:
    packed record
      case boolean of
	   false: (ufw: unsigned_fullword);
	   true:  (hw1: unsigned_halfword;
		   hw2: unsigned_halfword)
    end;
begin
  with change do
    begin
      if sfw < 0
      then ufw := sfw + "100000000
      else ufw := sfw;
      emit_halfword(hw1);
      emit_halfword(hw2)
    end
end;

(**********************************************************************)

procedure pack_halfword(fop: opcode_range;
			fr1, fr2: reg_range);
var
  change:
    packed record
      case boolean of
	   false: (op: opcode_range;
		   r1: reg_range;
		   r2: reg_range);
	   true:  (hw: unsigned_halfword)
    end;
begin
  with change do
    begin
      op := fop;
      r1 := fr1;
      r2 := fr2;
      emit_halfword(hw)
    end
end;

(**********************************************************************)

procedure gen_rx1(fop: opcode_range;
		  fr1, fx2: reg_range;
		  fd2: rx1_disp);
begin
  pack_halfword(fop, fr1, fx2);
  with rx1_change do
    begin
      d2 := fd2;
      emit_halfword(hw)
    end
end;

(**********************************************************************)

procedure gen_rx2(fop: opcode_range;
		  fr1, fx2: reg_range;
		  fd2: rx2_disp);
begin
  pack_halfword(fop, fr1, fx2);
  with rx2_change do
    begin
      if fd2 < 0
      then d2 := fd2 + "8000
      else d2 := fd2;
      emit_halfword(hw)
   end
end;

(**********************************************************************)

procedure gen_rx3(fop: opcode_range;
		  fr1, ffx2, fsx2: reg_range;
		  fa2: rx3_disp);
begin
  pack_halfword(fop, fr1, ffx2);
  with rx3_change do
    begin
      sx2 := fsx2;
      a2  := fa2;
      emit_halfword(hw2);
      emit_halfword(hw3)
    end
end;

(**********************************************************************)

procedure gen_ri1(fop: opcode_range;
		  fr1, fx2: reg_range;
		  fi2: signed_halfword);
var
  uhw: unsigned_halfword;
begin
  pack_halfword(fop, fr1, fx2);
  if fi2 < 0
  then uhw := fi2 + "10000
  else uhw := fi2;
  emit_halfword(uhw)
end;

(**********************************************************************)

procedure gen_ri2(fop: opcode_range;
		  fr1, fx2: reg_range;
		  fi2: signed_fullword);
begin
  pack_halfword(fop, fr1, fx2);
  emit_fullword(fi2)
end;
(**********************************************************************)
(*								      *)
(*			    table procedures			      *)
(*								      *)
(**********************************************************************)

function entry(t: tableptr; i: integer): integer;
var
  portion: tableptr;
  j: integer;
begin
  if i = 0
  then entry := 0   (* reference to undefined routine *)
  else
    begin
      portion := t;  j := i;
      while j > maxword do
 	begin
	  portion := portion^.nextportion;
	  j := j - maxword
	end;
      entry := portion^.contents[j]
    end
end;
(**********************************************************************)
(*								      *)
(*			head and tail procedures		      *)
(*								      *)
(**********************************************************************)

procedure writehead;
begin
  emit_fullword(proglength);
  emit_fullword(codelength);
  emit_fullword(stacklength);
  emit_fullword(varlength);
  if lines
  then emit_fullword(bit32)
  else emit_fullword(0); 
  emit_fullword(0); (* reserved for future use *)
  emit_fullword(0); (* reserved for future use *)
  emit_fullword(0)  (* reserved for future use *)
end;

(**********************************************************************)

procedure writetail;
var
  i: integer;
begin
  for i := 1 to constants div hw_size do
    emit_halfword(entry(consttable, i))
end;
(**********************************************************************)
(*								      *)
(*			    error procedures			      *)
(*								      *)
(**********************************************************************)

procedure errwrite(mess: errstr);
var
  i, j: integer;
begin
  j := upperbound(mess);
  while mess[j] = ' ' do
    j := j-1;
  writeln(list, mess:j);
end;

(**********************************************************************)
(*								      *)
(*	       initialization and termination procedures	      *)
(*								      *)
(**********************************************************************)

procedure beginpass;
begin
  initpass(this_pass);
  reset(ilin);
  first_mess := true;
  with link^ do
    begin
      summary := summaryoption in options;
      bstack := bstackoption in options;
      generate := codeoption in options;
      lines := numberoption in options;
      proglength := tables^.proglength;
      codelength := tables^.codelength;
      stacklength := tables^.stacklength;
      varlength := tables^.varlength;
      jumptable := tables^.jumptable;
      blocktable := tables^.blocktable;
      stacktable := tables^.stacktable;
      consttable := tables^.consttable
    end;
  constants := link^.constants;
  init_object;
  with rx1_change do
    begin
      bit16 := 0;
      bit17 := 0
    end;
  rx2_change.bit16 := 1;
  with rx3_change do
    begin
      bit16 := 0;
      bit17 := 1;
      bit18 := 0;
      bit19 := 0
    end;
  writehead
end;

(**********************************************************************)

procedure endpass;
begin
  writetail;
  flush_object;
  dispose(link^.resetpoint);
  nextpass(this_pass);
  if summary then printsummary
end;

(**********************************************************************)
(*								      *)
(*                          compiler errors             	      *)
(*								      *)
(**********************************************************************)

procedure bugbug(line: integer);
begin
  writeln(tty, '*** Compiler bug at line ', line:5, ' ***'); 
  writeln(tty, 'Please contact your representative.  Sorry.');
  endpass;
  halt
end;

(**********************************************************************)
(*								      *)
(*			  operator procedures			      *)
(*								      *)
(**********************************************************************)

procedure casejump(location, count: integer);
var
  i: integer;
begin
  for i := 0 to count do
    begin
      read1arg;
      location := location + fw_size;
      gen_rx2(bfc_op, falsemask, unindexed,
	      entry(jumptable, arg1)-location)
    end
end;

(**********************************************************************)

procedure pmessage(pass, number, line: integer);
begin
  if (pass=6) and (number=5) (* this is the famed "can't happen" message *)
  then begin
    if first_mess (* and if there's no other error, we goofed *)
    then bugbug(line); (* and bugbug halts us in our tracks... *)
  end
  else if line>prevline
  then begin
    writeln(list);
    write(list, '****** line', line:5, ' ');
    errmess(pass, number, errtext);
    errwrite(errtext);
  end; (* if *)
  first_mess := false;
  prevline := line
end;

(*********************************************************************)
(*								      *)
(*			      operator scan			      *)
(*								      *)
(**********************************************************************)

procedure scan;
var
  done: boolean;
  op: integer;
begin
  done := false;
  repeat
    read_ifl(op);
    case op of

      casejump1:
	begin
	  read2args;
	  casejump(arg1, arg2)
	end;

      data1:
        begin
	  read1arg;
	  emit_halfword(arg1)
	end;
	
      enter1:
	begin
	  read1arg;
	  tmp := entry(stacktable, arg1);
	  gen_ri1(chi_op, r0, s_reg, tmp)
	end;

      eom1:
	done := true;

      falsejump1:
	begin
	  read3args;
	  tmp := entry(jumptable, arg3) - arg1;
	  gen_rx2(bfcarg2, unindexed, tmp)
	end;

      init1:
	begin
	  read1arg;
          if bstack then
	    begin
	      tmp := entry(stacktable, arg1);
	      gen_ri2(li_op, r3, unindexed, tmp);
	    end 
          else begin
	    arg2 := entry(stacktable, arg1);
            if ((arg2 < -"8000) OR (arg2 > "7fff)) then
    	      begin
		writeln(tty);
		writeln(tty, '*** stack length too long, rerun with option b');
		writeln(tty);
		break(tty);
		halt
	      end
	    else
              gen_ri1(lhi_op, r3, unindexed, arg2)
	  end
	end;

      message1:
	begin
	  read3args;
	  pmessage(arg1, arg2, arg3)
	end;

      new1:
	begin
	  read1arg;
	  tmp := entry(stacktable, arg1);
	  gen_ri1(chi_op, r0, b_reg, tmp)
	end;

      offset1:
	begin
	  read3args;
	  tmp := entry(blocktable, arg2) - arg1;
	  gen_ri2(li_op, arg3, unindexed, tmp)
	end;

      ri1_op1:
	begin
	  read4args;
	  gen_ri1(arg1, arg2, arg3, arg4)
	end;

      ri2_op1:
	begin
	  read4args;
	  gen_ri2(arg1, arg2, arg3, arg4)
	end;

      rr_op1,
      sf_op1:
	begin
	  read3args;
	  pack_halfword(arg1, arg2, arg3)
	end;

      rx1_op1:
	begin
	  read4args;
	  gen_rx1(arg1, arg2, arg3, arg4)
	end;

      rx2_op1:
	begin
	  read4args;
	  gen_rx2(arg1, arg2, arg3, arg4)
	end;

      rx3_op1:
	begin
	  read5args;
	  gen_rx3(arg1, arg2, arg3, arg4, arg5)
	end;

      truejump1:
	begin
	  read3args;
	  tmp := entry(jumptable, arg3) - arg1;
	  gen_rx2(btc_op, arg2, unindexed, tmp);
	end 

    end (* case *)
  until done
end;
(**********************************************************************)
(*								      *)
(*				  main				      *)
(*								      *)
(**********************************************************************)

begin
  beginpass;
  scan;
  endpass
end;

begin end.
   	X^