$TITLE vaxrel - relocatable file emission
module vaxrel;
  
(* Disable compilation of code which writes opcodes and operand specifiers
   to an instrumentation file when the OPSTATS dump option is specified.  *)

$PAGE declarations
  
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM vaxcg.typ
$SYSTEM vaxopc.inc
$SYSTEM vaxcgu.inc
$SYSTEM vaxgen.inc
$SYSTEM pasmap.inc
$SYSTEM pasfil.inc
$SYSTEM TIMUTL.inc
$SYSTEM versio.inc
$SYSTEM ptmcon.inc
$SYSTEM vaxexp.inc
$IF OPTSTATS
$SYSTEM passw.inc	(* needed only for OPSTATS code *)
$SYSTEM vaxmac.inc
$ENDIF
  
type
  relf_byte = 0..#Hff;  (* unit of output to the rel file *)
  packed_relf_bytes = packed array [1..4] of relf_byte;
  binary_file = file of packed_relf_bytes;
  
  
type
  lnkr_rec_type = 0..7;
  gsd_type = 0..3;
  tir_command = 0..81;
  packed_bytes = packed record
    fill: 0..#Hf;
    b4: relf_byte;
    b3: relf_byte;
    b2: relf_byte;
    b1: relf_byte
  end;
  std_var_string = string[35];  (* standard object language string format *)
  psect_flags = array [1..2] of relf_byte;
  
const
  max_relf_rec = 512;  (* (fcs) linker imposed limit on rec. length *)
  static_flags: psect_flags := (#H01, #H89);
  code_flags:   psect_flags := (#H00, #He9);
  
  obj$c_hdr: lnkr_rec_type = 0;  (* header record *)
  obj$c_gsd: lnkr_rec_type = 1;  (* global sym. directory rec. *)
	gsd_psc:  gsd_type = 0; (* p-section definition *)
	gsd_sym:  gsd_type = 1; (* global symbol specification *)
	gsd_epm:  gsd_type = 2; (* entry point & mask definition *)
  obj$c_tir: lnkr_rec_type = 2;  (* text inf. and reloc. rec. *)
        tir_sta_gbl:   tir_command = 0;  (* stack global symbol *)
	tir_sta_sb:    tir_command = 1;  (* stack signed byte *)
	tir_sta_sw:    tir_command = 2;  (* stack signed word *)
	tir_sta_lw:    tir_command = 3;  (* stack longword *)
	tir_sta_pb:    tir_command = 4;  (* stack psect base plus byte offset *)
	tir_sta_pw:    tir_command = 5;  (* stack psect base plus word offset *)
	tir_sta_pl:    tir_command = 6;  (* stack psect base plus longword offset *)
	tir_sto_lw:     tir_command = 22;	(* Store longword *)
	tir_sto_ld:    tir_command = 25; (* store longword displaced *)
        tir_sto_picr:  tir_command = 28;  (* store pos. indep. code ref. *)
	tir_opr_nop:   tir_command = 50;  (* nop *)
	tir_opr_add:   tir_command = 51; (* add top two longwords *)
	tir_ctl_setrb: tir_command = 80; (* set relocation base *)
  obj$c_eom: lnkr_rec_type = 3;  (* end of module record *)
  obj$c_dbg: lnkr_rec_type = 4;  (* debugger inf. record *)
  obj$c_tbt: lnkr_rec_type = 5;  (* trace back inf. record *)
  obj$c_lnk: lnkr_rec_type = 6;  (* link opt. spec. record *)
  no_record: lnkr_rec_type = 7;  (* dummy record type *)
  
var
$IF OPTSTATS
  ops_file: text;	(* instrumentation file for dump(OPSTATS) *)
  first_ops_write: boolean;	(* true until 1st write to ops_file *)
  write_next_byte: boolean;	(* triggers write of operand specifier to ops_file *)
  ops_file_open: boolean;	(* true ==> ops_file now open *)
  case_stmt_last: boolean;		(* enables writes of case stmt displacements to ops_file *)
$ENDIF
  relf: binary_file; (* the rel file *)
  last_byte:  0..max_relf_rec;
  byte_in_word: 0..4;
  byte_list: packed array [1..max_relf_rec] of relf_byte;
  stoim_start, stoim_count: 0..max_relf_rec;
  
  int_bytes: record
    case boolean of
      true: (int: int_type);
      false: (bytes: packed_bytes)
    end;
  
  rt_refs: packed array [rt_first..rt_last_used] of boolean; (* table of runtime syms. ref'd. *)
  ext_refs: def; (* head of chain of externals referenced *)
  ic: unit_range;
 
public var
  fixing_branches: boolean; (* true when fix_branches is using op_length;
			false when macro lister is using it.      *)
$IF OPSTATS
$PAGE cv_str2
(* CV_STR2 converts a byte value to 2 ascii hex digits.  It is used in
   writing opcodes and operand specifiers when the OPSTATS dump option
   is used.  *)

type
  str2 = string[ 2 ];

function cv_str2 ( value: relf_byte ): str2;

begin
  int_bytes.int := value;
  cv_str2 := cvhex ( int_bytes.bytes.b1 );
  if length ( cv_str2 ) = 1
    then cv_str2 := '0' || cv_str2
    else assert ( length ( cv_str2 ) = 2 );
end  (* proc cv_str2 *) ;
$ENDIF
$PAGE shr_op_length
Procedure SHR_OP_LENGTH ( N : UNIT_RANGE;
			  INDIRECT : Boolean;
			Var OP_LENGTH : UNIT_RANGE );

Begin
  if (n = 0) and not indirect then (* no displacement required? *)
    op_length := op_length + 1
  else if (n >= -128) and (n <= 127) then (* byte displacement suffice? *)
    op_length := op_length + 2
  else if (n >= -32768) and (n <= 32767) then (* need a word? *)
    op_length := op_length + 3
  else (* need fullword *)
    op_length := op_length + max_disp_bytes + 1
End;
$PAGE op_length
  
(* OP LENGTH determines the length in bytes required by the operands of
   the instruction pointed to by "cr".  *)
  
public function op_length (cr: code): unit_range;
  
var
  ioper: 1..6;
  n: unit_range;
  displacement: unit_range;
  
begin
  op_length := 0;

  for ioper := 1 to cr^.noperands do
    with cr^.operands[ioper] do begin
      case addr_mode of
  
	auto_inc, auto_dec:
	  op_length := op_length + 1;
  
	branch_displacement:
	  if byte_size = vax_byte
	    then op_length := op_length + 1
	    else op_length := op_length + 2;
 
	other:
  
	  if immediate then begin
	    if (offset >= min_literal) and (offset <= max_literal) then
	      op_length := op_length + 1  (* fits in short literal *)
	    else if byte_size = vax_byte then
	      op_length := op_length + 2  (* requires immediate mode byte *)
	    else if byte_size = vax_word then
	      op_length := op_length + 3  (*     "         "     "   word *)
	    else
	      op_length := op_length + 5  (*     "         "     "   fullword *)
	  end (* immediate *)
  
	  else if reloc.kind = register_sc then
	    op_length := op_length + 1
	  else if reloc.kind in [static_sc, external_sc, runtime_sc] then
	    op_length := op_length + max_disp_bytes + 1
	  else if reloc.kind = def_sc then begin

	    (* The size of the displacement field used in resolving DEF_SC
	       operands is resolved when OP_LENGTH is called from FIX_BRANCHES.
	       If the location is already defined, then the displacement is
	       determined by makind the pessimistic assumption that all
	       instructions between the reference and the definition are
	       branches which must be converted to long forms.  This would
	       result in each 2 byte branch being expanded to 5 bytes.
	       Thus the strange constants below:
		  50 * 5/2 < 127 and 13106 * 5/2 < 32767  *)

	    If RELOC.RELDEF^.DEFTYPE = TEMP_SIZE_DEF
	      Then SHR_OP_LENGTH ( RELOC.RELDEF^.ADDR , INDIRECT , OP_LENGTH )
	    Else Begin
	      if fixing_branches then begin
		if reloc.reldef^.defined then begin
		  displacement := abs(reloc.reldef^.addr + offset - 2 - ic - op_length);
		  if displacement <= 50 	(* even in worst case a byte will suffice *)
		    then byte_size := vax_byte
		  else if displacement <= 13106	(* even in worst case a word will suffice *)
		    then byte_size := vax_word
		  else byte_size := vax_long;
		end
		else byte_size := vax_long;	(* undefined, must assume worst *)
	      end;
	      if byte_size = vax_byte then
		op_length := op_length + 2
	      else if byte_size = vax_word
		then op_length := op_length + 3
	    else op_length := op_length + max_disp_bytes + 1;
	    End
	  end
  
	  else if reloc.kind in [temp_sc, local_sc, parameter_sc, absolute_sc] then begin
	    n := offset;
	    if reloc.kind in [local_sc, parameter_sc]
	      then n := n + reloc.relsym^.item_addr;
	    SHR_OP_LENGTH ( N , INDIRECT , OP_LENGTH )
	  end
  
      end (* case *);
 
      if index <> noreg then
	op_length := op_length + 1 (* indexed *)
    end (* with *);
  
end (* op_length *);
$PAGE fix_branches
  
(* FIX BRANCHES determines the optimal forms for the conditional and unconditional
   branches within "area list" using the algorithm in Syzmanski, CACM 21:4.  *)
  
  
public procedure fix_branches (area_list: code_list; init_ic: unit_range);
 
  
const
  num_conditionals = 18;
  
type
  elem_array = array [1..*] of  packed record
    span,
    long: word;
    sdi_ptr: code
  end;
  
  opc_pair_array = packed array [1..num_conditionals, 1..2] of opc_range;
  
  
const
  
  br_complements: opc_pair_array :=
		((bvs, bvc),     (bvc, bvs),     (bcs, bcc),     (bcc, bcs), 
		 (blssu, bgequ), (blequ, bgtru), (beqlu, bnequ), (bnequ, beqlu),
		 (bgequ, blssu), (bgtru, blequ), (blss, bgeq),   (bleq, bgtr),
		 (beql, bneq),   (bneq, beql),   (bgeq, blss),   (bgtr, bleq) ,
		 (bbs, bbc),     (bbc, bbs) );
  
var
  cr,
  label_chain,
  last_label,
  sdi_chain,
  last_sdi:  code;
  sdi_count: unit_range;
  sdi_list: ^elem_array;
  min_span: word;
  i, j: unit_range;
  nochange, found: boolean;
  distance: unit_range;
  br_operand: 0..6;
  
begin
  ic := init_ic;
  cr := area_list.first;
  label_chain := nil;  (* head of thread through defmarks *)
  last_label := nil;
  sdi_chain := nil;  (* head of thread through span dependent instr. *)
  last_sdi := nil;
  sdi_count := 0;
  fixing_branches := true;  (* signal to def_sc handling in op_length *)
  
  (* make a pass over code to determine minimum addresses of labels and of
     span dependent instructions.  *)
  
  while cr <> nil do begin
    case cr^.kind of
  
      instruction: begin
	if (cr^.noperands >= 1) andif (cr^.operands[cr^.noperands].addr_mode = branch_displacement) then begin
	  if last_sdi = nil
	    then sdi_chain := cr
	    else last_sdi^.branch_chain := cr;
	  cr^.branch_chain := nil;
	  last_sdi := cr;
	  cr^.operands[cr^.noperands].offset := ic;  (* hold onto instruction's minimal addr. *)
	  sdi_count := sdi_count + 1;
	end;
	ic := ic + op_length (cr) + 1
      end (* instruction *);
  
      bytelen, setbyte:		 ic := ic + 1;
      wordlen, maskword:	 ic := ic + 2;
      displacement:		 ic := ic + cr^.disp_size div byte_size;
      indirect_word,
      fullword, realword:	 ic := ic + 4;
      quadword, doubleword:	 ic := ic + 8;
      stringword:		 ic := ic + 2 + length (cr^.strvalue);
      pstringword:		 ic := ic + length (cr^.strvalue);
  
      defmark: begin
	if last_label = nil
	  then label_chain := cr
	  else last_label^.branch_chain := cr;
	cr^.branch_chain := nil;
	cr^.defname^.defined := true;
	cr^.defname^.relocatable := true;
	cr^.defname^.addr := ic;
	last_label := cr;
	cr^.defspan := sdi_count (* no. of sdi's preceding this label *)
      end (* defmark *)
  
    end (* case *);
  
    cr := cr^.next
  end (* while *);
  
  if sdi_count = 0 then return; (* <----  return if no branches need fixing *)
  
  new (sdi_list, sdi_count);  (* allocate table for span dependent instructions *)
  cr := sdi_chain;
  for i := 1 to sdi_count do begin  (* fill in the sdi table *)
    assert (cr^.operands[cr^.noperands].reloc.reldef^.defined);
    min_span := cr^.operands[cr^.noperands].reloc.reldef^.addr
      - op_length(cr) - 1 - cr^.operands[cr^.noperands].offset;   (* minimum span of sdi i *)
    if abs (min_span) <= 50 then
      sdi_list^[i].span := 0  (* resolved - in worst case byte still suffices *)
    else
      sdi_list^[i].span := min_span;  (* remains to be resolved *)
    sdi_list^[i].long := 0;
    sdi_list^[i].sdi_ptr := cr;
    cr := cr^.branch_chain
  end;
  
  
  (* iterate over span dependent instructions until all are exactly as long
     as they have to be.  *)
  
  repeat
  
    nochange := true;
    for i := 1 to sdi_count do
      with sdi_list^[i] do
	if (span > 127) or (span < -128) then begin
	  ic := sdi_ptr^.operands[sdi_ptr^.noperands].offset;  (* remember minimum address of branch instr. *)
	  if sdi_ptr^.opcode = brb then begin (* unconditional *)
	    sdi_ptr^.opcode := brw;
	    sdi_ptr^.operands[1].byte_size := vax_word;
	    long := 1 (* record by how much instr. was lengthened *)
	  end
	  else begin (* conditional *)
	    found := false;
	    for j := 1 to num_conditionals do begin
	      if br_complements [j, 1] = sdi_ptr^.opcode then begin
		sdi_ptr^.opcode := br_complements [j, 2];  (* reverse sense of coditional *)
		found := true
	      end;
	    exit if found
	    end (* for *);
	    assert (found);
   
	    new (cr, instruction, 1);
	    cr^.next := sdi_ptr^.next;
	    cr^.opcode := brw;
	    cr^.noperands := 1;
	    cr^.operands[1] := sdi_ptr^.operands[sdi_ptr^.noperands];  (* new brw has same target as conditional did *)
	    cr^.operands[1].byte_size := vax_word;
	    sdi_ptr^.next := cr; (* chain new instr. after reversed cond. *)
	    sdi_ptr^.operands[sdi_ptr^.noperands].reloc.kind := self_rel_sc; (* rev. cond. will just jump around new brw *)
	    sdi_ptr^.operands[sdi_ptr^.noperands].offset := 3;
	    long := 3  (* record by how much instr. stream was lengthened *)
	  end (* else *);
	  span := 0;  (* mark this table entry as resolved *)
  
	  for j := 1 to sdi_count do   (* look for branches whose span includes sdi i *)
	    if sdi_list^[j].span <> 0 then begin
	      br_operand := sdi_list^[ j ].sdi_ptr^.noperands;
	      distance := ic - op_length(sdi_list^[j].sdi_ptr) - 1
		- sdi_list^[j].sdi_ptr^.operands[br_operand].offset;
	      if distance > 0 then begin  (* j before i *)
		if sdi_list^[j].span > distance then (* forward jump beyond i? *)
		  sdi_list^[j].span := sdi_list^[j].span + long
	      end
	      else if distance < 0 then (* j after i *)
		if sdi_list^[j].span < distance then (* backward jump to before i? *)
		  sdi_list^[j].span := sdi_list^[j].span - long
	    end;
  
	  nochange := false (* had to change a branch *)
	end (* if span too big ... *)
  
  until nochange;
  
  (* adjust label addresses to reflect increases in instruction lengths *)
  
  for i := 2 to sdi_count do
    sdi_list^[i].long := sdi_list^[i].long + sdi_list^[i-1].long;
  
  cr := label_chain;
  while cr <> nil do begin
    if cr^.defspan > 0 then
      cr^.defname^.addr := cr^.defname^.addr + sdi_list^[cr^.defspan].long;
    cr := cr^.branch_chain;
  end;
  
  dispose (sdi_list)
  
end (* fix_branches *);
$PAGE rel_record
  
(* REL RECORD is called with a linker record type.  It terminates any record
   currently being constructed, writes it to the object file, and starts
   a new record of the specified type.  *)
  
procedure rel_record (rec_type: lnkr_rec_type);
  
  var
    i: 1..max_relf_rec;
    int_bytes: record
      case boolean of
	true: (int: int_type);
	false: (bytes: packed_bytes)
      end;

  procedure putout (byte_val: relf_byte);
    begin
      byte_in_word := byte_in_word + 1;
      relf^[byte_in_word] := byte_val;
      if byte_in_word = 4 then begin
	put (relf);
	byte_in_word := 0
      end
    end;
  
  begin

    if (last_byte > 1) and not (rel_file = '') then begin
      int_bytes.int := last_byte;
      putout (int_bytes.bytes.b1);	(* write record length word *)
      putout (int_bytes.bytes.b2);
      for i := 1 to last_byte do (* write record itself *)
	putout (byte_list[i]);
      if odd (last_byte) then
        putout (0)
    end;

    byte_list[1] := rec_type;
    last_byte := 1

  end (* rel_record *);
$PAGE rel_byte, rel_longword, rel_name
  
(* REL BYTE takes a "byte value" and adds it to the end of the current record. *)
  
procedure rel_byte (byte_value: relf_byte);
  
  begin
    last_byte := last_byte + 1;
    byte_list [last_byte] := byte_value
  end;
  
  
(* REL LONGWORD takes an "int value" and puts it into the current record as
   four bytes.  *)
  
procedure rel_longword (int_value: int_type);
  
  begin
    int_bytes.int := int_value;
    with int_bytes.bytes do begin
      rel_byte (b1); rel_byte (b2); rel_byte (b3); rel_byte (b4)
    end
  end;
  
  
(* REL NAME takes a "name string" and puts the corresponding sequence of
   bytes (in the standard object language format) on the end of the
   current record.  *)
  
procedure rel_name (name_string: std_var_string);
  
  var
    i: 1..35;

  begin
    assert (length (name_string) > 0);
    rel_byte ( min ( length(name_string), publics_length ) );	(* emit length byte *)
    for i := 1 to min ( length ( name_string ), publics_length ) do  (* emit ascii string *)
      rel_byte (ord (name_string[i]) ) 
  end;
$PAGE rel_init  
  
(* REL INIT opens the relocatable binary output file and emits those records
   which should be emitted at the start of the file, and which do not require
   processing of the intermediate form.  *)
  
public procedure rel_init;
  
  type
    lang_string = string[34];
  
  const
    lang_name: lang_string = 'PASCAL Version ';
  
  var
    compilation_date: dtime_ext;
    j: 1..34;  (* loop index *)
    rt: rt_first..rt_last_used;
  version_id: version_string;

  begin

    if rel_file <> '' then begin  
      rewrite (relf, '.' || rel_extension || '[,]' || rel_file);
      if not eof (relf) then begin  (* bad rel file *)
	writeln (tty, '?unable to write rel file ', rel_file);
	rel_file := '';   (* suppress any further rel file activity *)
	return
      end;
      rel_file := filename (relf); (* save actual file name *)
    end;

$IF OPSTATS
    ops_file_open := false;	(* ops_file not yet open *)
$ENDIF

    last_byte := 0;
    byte_in_word := 0;

    rel_record (obj$c_hdr);  (* begin header record *)
    rel_byte (0);  (* hdr type = mhd *)
    rel_byte (0);  (* linker level *)
    rel_byte (0); rel_byte (2); (* max rec size = #H02 00 *)
    rel_name (root_block^.children^.id^.text);
    rel_name ('01');  (* module version *)
    compilation_date := dc_ext (root_block^.children^.comp_dtime);
    for j := 1 to 7 do (* creation date/time *)
      rel_byte (ord (compilation_date [j] ));  (* dd-mmm-  *)
    rel_byte (ord ('1') ); rel_byte (ord ('9') );
    for j := 8 to 15 do
      rel_byte (ord (compilation_date [j] ));  (* yy hh:mm *)
    for j := 1 to 17 do  (* last_patch date/time      rel_byte (ord (' '));

    rel_record (obj$c_hdr);  (* second header record *)
    rel_byte (1);  (* hdr type = lnm *)
    for j := 1 to length (lang_name) do
      rel_byte ( ord(lang_name[j]) );
    version_id := version;
    for j := 1 to min( 33-length(lang_name),length(version_id) ) do
      rel_byte ( ord ( version_id[ j ] ) );

    for rt := rt_first to rt_last_used do
      rt_refs [rt] := false;  (* clear table of runtime syms. referenced *)
    ext_refs := nil;  (* init. chain of externals referenced *)

  end (* rel_init *);
$PAGE stoim,  stoim_?_int
  
(* STOIM builds linker tir record store-immediate commands.  Each call provides a "byte value"
   to be added to the end of the stoim command currently being constructed.  *)
  
procedure stoim ( byte_value: relf_byte);
  begin

$IF OPSTATS
    (* If WRITE_NEXT_BYTE is true then value is operand specifier or branch
       displacement.  If OPSTATS dump option was specified then, write byte
       to ops_file.  *)

    if switch ( cur_block^.dump_switches, 'OPSTATS' ) and write_next_byte then begin
      write ( ops_file, ' ' || cv_str2 ( byte_value ) );
      write_next_byte := false;
    end;
$ENDIF

    if last_byte >= (max_relf_rec - 1) then begin  (* need room for data byte & possibly count *)
      rel_record (obj$c_tir);
      stoim_count := #H100
    end;
    if (stoim_count = #H100 (* zero *)) or (stoim_count = #H80 (* -128 *)) then begin
      rel_byte (0);  (* reserve place for negative byte count *)
      stoim_start := last_byte;  (* remember where count is at *)
      stoim_count := #H100  (* note #H100 - 1 = #Hff = -1 *)
    end;
    stoim_count := stoim_count - 1;
    byte_list [stoim_start] := stoim_count;  (* update count byte *)
    rel_byte (byte_value)  (* and add on new byte *)
  end;
  
  
procedure stoim_b_int (int_value: int_type);  (* emit integer in one byte *)
  begin
    int_bytes.int := int_value;
    stoim (int_bytes.bytes.b1)
  end;
  
procedure stoim_w_int (int_value: int_type);  (* emit integer in two bytes *)
  begin
    int_bytes.int := int_value;
    stoim (int_bytes.bytes.b1);
    stoim (int_bytes.bytes.b2)
  end;
  
procedure stoim_l_int (int_value: int_type);  (* emit integer in four bytes *)
  begin
    int_bytes.int := int_value;
    with int_bytes.bytes do begin
      stoim (b1);  stoim (b2);  stoim (b3);  stoim (b4)
    end
  end;
$PAGE stoim_disp

(* STOIM_DISP does a store immediate of a 16 or 32 bit address displacement.
   If the displacement is 16 bits then the value is checked to see if the
   displacement is within the range -32768..32767.  If the value does
   not lie within that range, then a fatal error routine is called.  *)

procedure stoim_disp ( int_value: int_type; disp_size: elem_sizes );

begin
  if disp_size = 2 * byte_size then begin	(* word displacement *)
    if (int_value >= minimum(word)) and (int_value <= maximum(word)) then begin
      int_bytes.int := int_value;
      stoim ( int_bytes.bytes.b1 );
      stoim ( int_bytes.bytes.b2 );
    end
    else fatal_error ( 'current routine too large to compile' );
  end
  else if disp_size = 4 * byte_size	(* longword displacement *)
    then stoim_l_int ( int_value )
  else assert ( false );
end  (* proc stoim_disp *) ;
$PAGE make_room_for_non_stoim
  
(* MAKE ROOM FOR NON STOIM is utilized immediately before emitting something
   into the tir records that isn't data to be emitted as part of a store-
   immediate command.  The current stoim data count is jimmied so that later
   immediate data will start a new stoim command.  Also, the space remaining
   within the current tir record is checked to be sure there is sufficient
   room for the commands the caller is going to emit - if not a new tir
   record is begun.  *)
  
procedure make_room_for_non_stoim (bytes_required: int_type);
  
  begin
    stoim_count := #H100; (* later store-immediates will begin new command *)
    if last_byte > (max_relf_rec - bytes_required) then
      rel_record (obj$c_tir); (* start new record if insufficient room in this one *)
  end;
$PAGE store_longword

(* store longword causes the longword on the top of the stack to be
   stored. This is used for the indirect word used to address public
   vars and consts in overlay compilations. *)

procedure store_longword;

begin
  make_room_for_non_stoim (1);
  rel_byte ( tir_sto_lw )
end;		(* store_longword *)
$PAGE sta_gbl, sta_psect_base
  
(* STA GBL emits a tir record stack global command, given the "global sym".  *)
  
procedure sta_gbl (global_sym: std_var_string);
  begin
    make_room_for_non_stoim (length (global_sym) + 2);
    rel_byte (tir_sta_gbl);  (* stack global command *)
    rel_name (global_sym);  (* the name of the symbol *)
  end;
  
  
(* STA PSECT BASE emits a tir record stack psect base command with appropriate offset.  *)
  
procedure sta_psect_base (psect: byte; offset_in_psect: unit_range);
  begin
    make_room_for_non_stoim (6);
    int_bytes.int := offset_in_psect;

    if (offset_in_psect >= -128) and (offset_in_psect <= 127) then begin
      rel_byte (tir_sta_pb);  (* with byte offset *)
      rel_byte (psect);
      rel_byte (int_bytes.bytes.b1)
    end
    else if (offset_in_psect >= -32768) and (offset_in_psect <= 32767) then begin
      rel_byte (tir_sta_pw);  (* with word offset *)
      rel_byte (psect);
      rel_byte (int_bytes.bytes.b1);
      rel_byte (int_bytes.bytes.b2)
    end
    else begin
      rel_byte (tir_sta_pl);  (* with longword offset *)
      rel_byte (psect);
      with int_bytes.bytes do begin
	rel_byte (b1); rel_byte (b2); rel_byte (b3); rel_byte (b4)
      end
    end
  end;
$PAGE sta_int
  
(* STA INT emits a stack signed byte / signed word / longword  given the "int value"
   to be stacked.  *)
  
procedure sta_int (int_value: unit_range);
  
  begin
    make_room_for_non_stoim (5);
    int_bytes.int := int_value;
  
    if (int_value >= -128) and (int_value <= 127) then begin
      rel_byte (tir_sta_sb);  (* signed byte *)
      rel_byte (int_bytes.bytes.b1)
    end
    else if (int_value >= -32768) and (int_value <= 32767) then begin
      rel_byte (tir_sta_sw);  (* signed word *)
      rel_byte (int_bytes.bytes.b1);
      rel_byte (int_bytes.bytes.b2)
    end
    else begin
      rel_byte (tir_sta_lw);  (* longword *)
      with int_bytes.bytes do begin
        rel_byte (b1); rel_byte (b2); rel_byte (b3); rel_byte (b4)
      end
    end
  end;
$PAGE sto_picr
  
(* STO PICR emits a tir record store position independent code reference command.  *)
  
procedure sto_picr (indirect: boolean);
  begin
$IF OPSTATS
    (* Write operand specifier byte to ops_file if OPSTATS dump option
	was specified.  *)

    if switch ( cur_block^.dump_switches, 'OPSTATS' ) then begin
      assert ( write_next_byte );
      if indirect
        then write ( ops_file, ' FF' )
        else write ( ops_file, ' EF' );
      write_next_byte := false;
    end;
$ENDIF
    if indirect then stoim (#Hff); (* lw displ. deferred off pc *)
    make_room_for_non_stoim (1);
    if indirect then
      rel_byte (tir_sto_ld)  (* store longword displaced command *)
    else
      rel_byte (tir_sto_picr)  (* store picr command *)
  end;
$PAGE set_rel_base, add_top_two
  
(* SET REL BASE emits a tir record set relocation base command. *)
  
procedure set_rel_base;
  begin
    make_room_for_non_stoim (1);
    rel_byte (tir_ctl_setrb)  (* set relocation base command *)
  end;
  
  
(* ADD TOP TWO emits a tir record add operation command.  *)
  
procedure add_top_two;
  begin
    make_room_for_non_stoim (1);
    rel_byte (tir_opr_add)  (* add top two longwords *)
  end;
$PAGE general_stoim
Procedure GENERAL_STOIM ( N : CODE_ADDRESS;
			  INDIRECT : Boolean;
			  REGISTER : REGISTERS;
			  Var IC   : UNIT_RANGE );

(* This is a small portion of code shared by rel_operand. The two cases
   that share the code are : 1. A TEMP_SIZE_DEF Definition record.
   2. TEMP,LOCAL,PARAMETER, or ABSOLUTE Addressing record. *)

Begin
	  if (n = 0) and not indirect then begin (* no displacement required? *)
	    stoim (#H60 + register);
	    ic := ic + 1
	  end
	  else if (n >= -128) and (n <= 127) then begin (* byte displacement suffice? *)
	    stoim (#Ha0 + #H10 * ord(indirect) + register);
	    stoim_b_int (n);
	    ic := ic + 2
	  end
	  else if (n >= -32768) and (n <= 32767) then begin (* need a word? *)
	    stoim (#Hc0 + #H10 * ord(indirect) + register);
	    stoim_w_int (n);
	    ic := ic + 3
	  end
	  else begin (* need fullword *)
	    stoim (#He0 + #H10 * ord(indirect) + register);
	    stoim_disp ( n, max_disp_size );
	    ic := ic + max_disp_bytes + 1
	  end
End;
$PAGE chain_up_external
  
(* CHAIN UP EXTERNAL builds a chain of definition records for referenced externals.
   REL END will traverse this chain and emit the necessary entries in a
   global symbol spec. record. *)
  
procedure chain_up_external (external_symbol: sym);

  var
    next_ext: def;

  begin
    if ext_refs = nil then begin
      new (ext_refs, extern_def);
      ext_refs^.next := nil;
      ext_refs^.symbol := external_symbol
    end
    else begin
      next_ext := ext_refs;
      while (next_ext^.symbol <> external_symbol) and (next_ext^.next <> nil) do
	next_ext := next_ext^.next;  (* scan until sym found or list ends *)
      if next_ext^.symbol <> external_symbol then begin (* wasn't already in list *)
	new (next_ext^.next, extern_def);
	next_ext := next_ext^.next;
	next_ext^.next := nil;
	next_ext^.symbol := external_symbol
      end
    end
  end;
$PAGE rel_operand
  
(* REL OPERAND translates the instruction "operand" to the rel file, and
   updates the location counter.  *)
  
procedure rel_operand (operand: addr_desc; var ic: unit_range);
  
var
  n: unit_range;
  next_ref: ref;
  static_address: unit_range;
  
begin
  with operand do begin
    if index <> noreg then begin
      stoim (#H40 + index);
      ic := ic + 1
    end;
$IF OPSTATS
    (* Write index specifier to ops_file if OPSTATS dump option specified. *)

    if switch ( cur_block^.dump_switches, 'OPSTATS' ) then begin
      if index <> noreg then
        write ( ops_file, ' ' || cv_str2 ( #H40 + index ) );
      write_next_byte := true;
    end;
$ENDIF
  
    case addr_mode of
  
      auto_inc: begin
        if indirect then
          stoim (#H90 + register)  (* autoincrement deferred *)
        else
	  stoim (#H80 + register);  (* autoincrement *)
	ic := ic + 1
      end;
  
      auto_dec: begin
	stoim (#H70 + register);
	ic := ic + 1
      end;
  
      branch_displacement: begin
$IF OPSTATS
	(* if OPSTATS dump switch is on, then start a new record to
	   contain just the branch displacement.  *)

	if switch ( cur_block^.dump_switches, 'OPSTATS' ) then begin
	  writeln ( ops_file );
	  write ( ops_file, 'B' );
	end;
$ENDIF
	if reloc.kind = self_rel_sc then
	  stoim_b_int (offset)
        else if byte_size = vax_byte then
	  stoim_b_int (reloc.reldef^.addr - 1 - ic)
	else begin
	  stoim_disp (reloc.reldef^.addr - 2 - ic, 2 * bits_per_byte);
$IF OPSTATS
	  if switch ( cur_block^.dump_switches, 'OPSTATS' ) then begin
	    int_bytes.int := reloc.reldef^.addr - 2 - ic;	(* make sure 2nd byte gets written *)
	    write ( ops_file, ' ' || cv_str2 ( int_bytes.bytes.b2) ); (* to ops_file *)
	  end;
$ENDIF
	  ic := ic + 1
	end;
	ic := ic + 1
      end;
  
      other:
	if immediate then begin
	  ic := ic + 1;
	  if (offset >= min_literal) and (offset <= max_literal) then
	    stoim (offset)  (* short literal *)
	  else begin
	    stoim (#H8f);  (* autoincrement off pc *)
	    if byte_size = vax_byte then begin
	      assert ( (offset >= minimum(byte) ) and
		       (offset <= maximum(uns_byte) ) );
	      stoim_b_int (offset);
	      ic := ic + 1
	    end
	    else if byte_size = vax_word then begin
	      assert ( ( offset >= minimum( word ) ) and
		       ( offset <= maximum( uns_word ) ) );
	      stoim_w_int (offset);
	      ic := ic + 2
	    end
	    else begin
	      stoim_l_int (offset);
	      ic := ic + 4
	    end
	  end
        end (* immediate *)
  
	else if reloc.kind = register_sc then begin
	  stoim (#H50 + register);
	  ic := ic + 1
	end
 
	else if reloc.kind = runtime_sc then begin
	  sta_gbl (rts_name [reloc.relrtsym]);
	  sto_picr (indirect);
	  rt_refs [reloc.relrtsym] := true; (* remember symbol was referenced *)
	  ic := ic + max_disp_bytes + 1
	end
  
	else if reloc.kind = external_sc then begin
	  if reloc.relsym^.init_value.kind = no_value then
	    sta_gbl (reloc.relsym^.name^.text)
	  else
	    sta_gbl (reloc.relsym^.init_value.valp^.str_val);
	  if offset <> 0 then begin
	    sta_int (offset);  (* stack offset on top of global *)
	    add_top_two (* and add them together *)
	  end;
	  sto_picr (indirect);
	  ic := ic + max_disp_bytes + 1;
	  chain_up_external (reloc.relsym)
	end
  
	else if reloc.kind = static_sc then begin
	  static_address := reloc.relsym^.item_addr;
	  if reloc.relsym^.kind = conditions then
	    static_address := static_address + size_init
	  else if reloc.relsym^.init_value.kind = no_value
	    then static_address := static_address + size_init + size_cond;
	  sta_psect_base (psect_id [static_psect], static_address + offset);
	  sto_picr (indirect);
	  ic := ic + max_disp_bytes + 1
	end
  
	else if reloc.kind = def_sc then begin
	  If RELOC.RELDEF^.DEFTYPE = TEMP_SIZE_DEF
	    Then GENERAL_STOIM ( RELOC.RELDEF^.ADDR , INDIRECT , REGISTER , IC )
	  else if reloc.reldef^.defined then
	    if byte_size = vax_byte then begin
	      stoim (#Haf);  (* byte displacement mode off pc *)
	      stoim_b_int (reloc.reldef^.addr + offset - 2 - ic);
	      ic := ic + 2
	    end
	    else if byte_size = vax_word then begin
	      stoim (#Hcf);  (* word displacement mode off pc *)
	      stoim_disp (reloc.reldef^.addr + offset - 3 - ic, 2 * bits_per_byte);
	      ic := ic + 3
	    end
	    else begin
	      stoim ( #Hef );	(* longword displacement mode off pc *)
	      stoim_l_int ( reloc.reldef^.addr + offset - max_disp_bytes - 1 - ic );
	      ic := ic + max_disp_bytes + 1;
	    end
	  else begin  (* not defined yet - forward reference *)
	    stoim (#Hef);  (* longword displacement mode off pc *)
	    stoim_disp (0, max_disp_size);   (* temporary value to occupy space *)
	    new (next_ref);
	    next_ref^.addr := ic + 1; (* address of word to be filled later *)
	    next_ref^.next := reloc.reldef^.refchain;
	    next_ref^.ref_size := max_disp_size;
	    next_ref^.displacement := false;
	    next_ref^.from_addr_or_offset := offset;
	    reloc.reldef^.refchain := next_ref;
	    ic := ic + max_disp_bytes + 1
	  end
	end
  
	else if reloc.kind in [temp_sc, local_sc, parameter_sc, absolute_sc] then begin
	  n := offset;
	  if reloc.kind in [local_sc, parameter_sc] 
	    then n := n + reloc.relsym^.item_addr;
	  GENERAL_STOIM ( N , INDIRECT , REGISTER , IC )
	end
        else assert ( false )
  
    end (* case addr_mode *);
 
  end (* with operand *);
  
end (* rel_operand *);
$PAGE emit_real
(* EMIT_REAL emits a real number into the rel file.  Parameter CR is
   a realword or doubleword code record.  Parameter IC is a location
   counter and is updated by this routine.  The (single or double
   precision) real number is converted from the PDP-10 format to
   the VAX-11 format.  The mantissa of the number is rounded before
   the conversion since the VAX format has fewer mantissa bits.  *)

public procedure emit_real ( cr: code; var ic: unit_range );

type
  vax_real_template = packed record
    vax_fill: 0..#Hf;
    vax_low_fraction: 0..#Hffff;
    vax_sign: 0..1;
    vax_exponent: 0..#Hff;
    vax_high_fraction: 0..#H7f;
    vax_w2_fill: 0..#Hf;
    vax_w2_low_fraction: 0..#Hffff;
    vax_w2_high_fraction: 0..7;
    vax_w2_mid_fraction: 0..#H1fff
  end;

  ten_real_template = packed record
    pdp10_sign: 0..1;
    pdp10_exponent: 0..#Hff;
    pdp10_redundant_bit: 0..1;
    pdp10_high_fraction: 0..#H7f;
    pdp10_low_fraction: 0..#Hffff;
    pdp10_fill: 0..7;
    pdp10_w2_sign: 0..1;
    pdp10_w2_high_fraction: 0..#H1fff;
    pdp10_w2_low_fraction: 0..#Hffff;
    pdp10_w2_unused_fraction: 0..#H3f
  end;
  
  selector = 1..3;

  vax_equivalence = record
    case boolean of
      true: (template: vax_real_template);
      false: (int_equiv1: longword;
	      int_equiv2: longword)
    end;
  ten_equivalence = record
    case selector of 
      1: (template: ten_real_template);
      2: (real_equiv: real_type);
      3: (int_equiv1: integer;
	  int_equiv2: integer)
    end;
  
var
  vax_real: vax_equivalence;
  ten_real: ten_equivalence;
  negative: boolean;

begin
  with cr^ do begin

    (* If the value is negative, then convert it to positive and
       set a flag - VAX reals are stored in sign magnitude form,
       PDP-10 reals in 2's complement form.  *)

    if kind = realword 
      then ten_real.real_equiv := rvalue
      else ten_real.real_equiv := dvalue;
    negative := (ten_real.real_equiv < 0.0);
    if negative then
      ten_real.real_equiv := -ten_real.real_equiv;

    (* Single precision case.  *)

    if kind = realword then begin

      (* Round PDP-10 real first since we must truncate bits of the
	 mantissa.  Note that not only does 0.0 not require rounding
	 but also the rounding process if used on 0.0 would produce
	 an unnormalized value.  *)

      if (ten_real.real_equiv <> 0.0) andif
	 (ten_real.template.pdp10_fill >= 4) then begin
	with ten_real, template do begin
	  if (pdp10_high_fraction <> #H7f) or 
	     (pdp10_low_fraction <> #Hffff) then begin
	    int_equiv1 := int_equiv1 + 4;
	  end
	  else if pdp10_exponent <> #Hff then begin
	    pdp10_exponent := pdp10_exponent + 1;
	    pdp10_high_fraction := 0;
	    pdp10_low_fraction := 0;
	  end;
	end  (* with *) ;
      end  (* if *) ;

      (* VAX exponent range only goes down to -127.  If ten exponent
	 is -128, set the value to 0.  *)

      if ten_real.template.pdp10_exponent = 0 then begin
	ten_real.real_equiv := 0;
	negative := false;
      end;

      (* Convert to VAX format. *)

      with vax_real.template, ten_real.template do begin
	vax_fill := 0;
	vax_low_fraction := pdp10_low_fraction;
	if negative
	  then vax_sign := 1
	  else vax_sign := 0;
	vax_exponent := pdp10_exponent;
	vax_high_fraction := pdp10_high_fraction
      end;

      stoim_l_int (vax_real.int_equiv1);
      ic := ic + 4;
    end (* single precision case *)

    (* Double precision case.  *)

    else begin
      
      (* First, round the PDP-10 real.  *)

      if (ten_real.real_equiv <> 0.0) andif
	 (ten_real.template.pdp10_w2_unused_fraction >= #H20) then begin
	with ten_real, template do begin
	  if (pdp10_w2_high_fraction <> #H1fff) or
	     (pdp10_w2_low_fraction <> #Hffff) then begin
	    int_equiv2 := int_equiv2 + #H20;
	  end
	  else if (pdp10_fill <> 7) orif
		  (pdp10_low_fraction <> #Hffff) orif
		  (pdp10_high_fraction <> #H7f) orif
		  (pdp10_exponent <> #Hff) then begin (* all this => not max real *)
	    pdp10_w2_high_fraction := 0;
	    pdp10_w2_low_fraction := 0;
	    if (pdp10_fill <> 7) or
	       (pdp10_low_fraction <> #Hffff) or
	       (pdp10_high_fraction <> #H7f) then begin
	      int_equiv1 := int_equiv1 + 1;
	    end
	    else begin
	      pdp10_fill := 0;
	      pdp10_high_fraction := 0;
	      pdp10_low_fraction := 0;
	      pdp10_exponent := pdp10_exponent + 1;
	    end;
	  end;
	end  (* with *);
      end  (* if *) ;

      (* Vax exponents only go down to -127, if exponent is -128 then
	 set the value to 0.0. *)

      if ten_real.template.pdp10_exponent = 0 then begin
	ten_real.real_equiv := 0;
	negative := false;
      end;

      (* Convert to VAX format. *)

      with vax_real.template, ten_real.template do begin
	vax_fill := 0;
	vax_low_fraction := pdp10_low_fraction;
	if negative 
	  then vax_sign := 1
	  else vax_sign := 0;
	vax_exponent := pdp10_exponent;
	vax_high_fraction := pdp10_high_fraction;
	vax_w2_fill := 0;
  	vax_w2_low_fraction := pdp10_w2_low_fraction;
	vax_w2_high_fraction := pdp10_fill;
	vax_w2_mid_fraction := pdp10_w2_high_fraction;
      end;
      stoim_l_int ( vax_real.int_equiv1 );
      stoim_l_int ( vax_real.int_equiv2 );
      ic := ic + 8;
    end  (* double precision case *);
  end  (* with *) ;
end  (* proc emit_real *) ;
$PAGE wr_code
  
(* WR CODE translates code records from "area list" to the rel file, deletes
   the code records, and updates the location counter.  *)
  
public procedure wr_code (var area_list: code_list;
			  var ic: unit_range;
			  psect_of_area: byte) options special(coercions);
 
var
  cr, next_cr: code;
  ioper: 1..6;
  i: word;
  temp_byte: byte;
  r: registers;
  next_ref, temp_ref: ref;
  regs_used: set_of_registers;
  static_address: unit_range;
  
begin
  
  cr := area_list.first;
  if cr = nil then return;  (* <----- empty code list *)
  rel_record (obj$c_tir);  (* start a text inf. and reloc. record *)
  sta_psect_base (psect_of_area, ic);  (* obtain start address for this code *)
  set_rel_base;  (* stuff it into location counter *)
$IF OPSTATS
  (* If OPSTATS dump option specified then open the ops_file if not
     already open.  *)

  if switch ( cur_block^.dump_switches, 'OPSTATS' ) then begin
    if not ops_file_open then begin
      rewrite ( ops_file, rel_file || '.OPS' );
      ops_file_open := true;
      first_ops_write := true;
    end;
  end;
$ENDIF
  
  while cr <> nil do
    with cr^ do begin
  
      case kind of
  
	instruction: begin
	  stoim (opcode);
$IF OPSTATS
	  (* If OPSTATS dump option specified, then write opcode to the ops_file
	     and set the case statement flag.  Also, if this is not the first
	     record being written then do a writeln for the previous record. *)

	  if switch ( cur_block^.dump_switches, 'OPSTATS' ) then begin
	    if first_ops_write
	      then first_ops_write := false
	      else writeln ( ops_file );
	    write ( ops_file, 'I ' || cv_str2 ( opcode ) );
	    case_stmt_last := (opcode = caseb) or (opcode = casew) or (opcode = casel);
	  end;
$ENDIF
	  ic := ic + 1;
	  for ioper := 1 to noperands do
	    rel_operand (operands[ioper], ic)
	end;
  
	fullword: begin
	  stoim_l_int (fwd);
	  ic := ic + 4
	end;
  
	stringword, pstringword: begin
	  if kind = stringword then begin
	    stoim_w_int (length (strvalue));
	    ic := ic + 2
	  end;
	  for i := 1 to length (strvalue) do
	    stoim ( ord(strvalue[i]) );
	  ic := ic + length (strvalue)
	end;
  
	setbyte: begin
	  temp_byte := 0;
	  for i := 7 downto 0 do
	    temp_byte := 2 * temp_byte + ord (setval[i]);
	  stoim_b_int (temp_byte);
	  ic := ic + 1
	end;
  
	bytelen: begin
	  stoim_b_int (byte_value);
	  ic := ic + 1
	end;
  
	wordlen: begin
	  stoim_w_int (word_value);
	  ic := ic + 2
	end;
 
	quadword: begin
	  stoim_l_int (qvalue1);
	  stoim_l_int (qvalue2);
	  ic := ic + 8
	end;
  
	realword, doubleword: 
	  emit_real ( cr, ic );
  
	maskword: begin
	  i := 0;
	  if block <> nil then begin
	    regs_used := reg_use_by_block^[block^.number];
	    for r := 11 downto r0 do
	      i := 2*i + ord (r in regs_used);
	  end;
	  stoim_w_int (i + 2**14); (* with integer overflow trapping enabled *)
	  ic := ic + 2
	end;
  
	displacement: begin	(* case statement displacements *)
	  if from_def^.defined and to_def^.defined then
	    stoim_disp (to_def^.addr - from_def^.addr, disp_size)
	  else if from_def^.defined and (to_def^.deftype in [static_def, extern_def]) then begin
	    (* only handler branch tables are known to utilize this case *)
	    assert (to_def^.symbol^.kind = conditions);
	    assert (from_def^.addr = ic); (* only way hbt's need it *)
	    if to_def^.deftype = extern_def then begin
	      assert (to_def^.symbol^.init_value.kind = no_value); (* only user defined externals *)
	      sta_gbl (to_def^.symbol^.name^.text);
	      chain_up_external (to_def^.symbol)
	    end
	    else
	      sta_psect_base (psect_id [static_psect], to_def^.symbol^.item_addr + size_init);
	    sta_int (4);
	    add_top_two; (* compensate for quirk of sto_ld command *)
	    make_room_for_non_stoim (1);
	    (* store longword displaced: longword on stack - (ic + 4) *)
	    rel_byte (tir_sto_ld);
	    assert (disp_size = 32)
	  end
	  else begin
	    stoim_disp (0, disp_size);  (* reserve the space to be patched later *)
	    new (next_ref);
	    next_ref^.addr := ic;
	    next_ref^.displacement := true;
	    next_ref^.ref_size := disp_size;
	    if from_def^.defined then begin
	      next_ref^.from_addr_or_offset := from_def^.addr;
	      next_ref^.next := to_def^.refchain;
	      to_def^.refchain := next_ref;
	      next_ref^.forward_ref := true
	    end
	    else begin
	      assert (to_def^.defined and (from_def^.deftype <> extern_def));
				(* handler branch tables are the only known user of extern_def's,
				   and that would only be for the to_def *)
	      next_ref^.from_addr_or_offset := to_def^.addr;
	      next_ref^.next := from_def^.refchain;
	      from_def^.refchain := next_ref;
	      next_ref^.forward_ref := false
	    end
	  end;
	  ic := ic + (disp_size div byte_size);
$IF OPSTATS
	  (* if OPSTATS dump option specified and case statement flag set,
	     then write displacement to the ops_file.  *)

	  if switch(cur_block^.dump_switches, 'OPSTATS') and
	     case_stmt_last then begin
	    writeln ( ops_file );	(* write case opcode record *)
	    if from_def^.defined and to_def^.defined
	      then int_bytes.int := to_def^.addr - from_def^.addr
	      else int_bytes.int := 0;
	    write ( ops_file, 'C ' || cv_str2 ( int_bytes.bytes.b1 ) );
	    write ( ops_file, ' ' || cv_str2 ( int_bytes.bytes.b2 ) );
	  end;
$ENDIF
	end;
  
	defmark: begin
	  if not defname^.defined then begin
	    defname^.defined := true;
	    defname^.relocatable := true;
	    defname^.addr := ic
	  end;
	  if defname^.refchain <> nil then begin
	    next_ref := defname^.refchain;
	    defname^.refchain := nil;
	    while next_ref <> nil do begin
	      sta_psect_base (psect_id [code_psect], next_ref^.addr);
	      set_rel_base;
	      if next_ref^.displacement then	
		if next_ref^.forward_ref then
		  stoim_disp (ic - next_ref^.from_addr_or_offset, next_ref^.ref_size)
		else
		  stoim_disp (next_ref^.from_addr_or_offset - ic, next_ref^.ref_size)
	      else	(* all other forward references are longword length *)
		stoim_disp (ic + next_ref^.from_addr_or_offset -
		  (next_ref^.ref_size div byte_size) - next_ref^.addr, next_ref^.ref_size);
	      temp_ref := next_ref;
	      next_ref := next_ref^.next;
	      dispose (temp_ref)
	    end;
	    sta_psect_base (psect_of_area, ic);
	    set_rel_base
	  end
	end;
  
	source:
	  if map_opt in cur_block^.semantic_options then
	    map_write (stmtid, ic);

	indirect_word: with pub_sym^ do begin
	  assert (kind in [vars, consts, conditions]);
	  if kind = vars then begin
	    static_address := item_addr;
	    if init_value.kind = no_value
	      then static_address := static_address + size_init + size_cond;
	    sta_psect_base( psect_id[ static_psect ], static_address );
	    store_longword;
	    item_addr := ic
	  end
	  else if kind = consts then begin	(* public consts, not funcs or procs *)
	    assert (init_value.kind = alloc_cst);
	    sta_psect_base( psect_id[ code_psect ], def(init_value.defp)^.addr );
	    store_longword;
	    def ( init_value.defp )^.addr := ic
	  end
	  else begin
	    (* The previous two cases are utilized only for overlay compilations.  This
	       case is used only for standard conditions in handler branch tables. *)
	    if dcl_class = static_sc then
	      sta_psect_base (psect_id [static_psect], item_addr + size_init)
	    else begin
	      assert ((dcl_class = external_sc) and standard and (init_value.kind <> no_value));
	      sta_gbl (init_value.valp^.str_val)
	    end;
	    store_longword;
	    if dcl_class = external_sc then
	      chain_up_external (pub_sym)
	  end;
	  ic := ic + 4;
	end	(* indirect_word case *)
  
      end (* case *);
  
      next_cr := next;
      dispose (cr);
      cr := next_cr
    end (* with *);
  
  area_list.first := nil;
  area_list.last := nil
  
end (* wr_code *);
$PAGE rel_end
  
public procedure rel_end (startdef: def;  code_const_size, static_size: unit_range)
  options special(coercions);
  
var
  rt: rt_first..rt_last_used;
  next_ext, temp_def: def;
  symbol: sym;
  r: registers;
  i: 1..4;
  regs_used: set_of_registers;
  static_address: unit_range;
  
procedure prep_for_global (entry_type, data_type, flags: relf_byte; additional_overhead: unit_range);
  begin
    if last_byte > (max_relf_rec - 5 - publics_length - additional_overhead) then
      rel_record (obj$c_gsd);
    rel_byte (entry_type);
    rel_byte (data_type);
    rel_byte (flags); rel_byte (0);
  end;
  
begin
 
  if rel_file = '' then return;   (* <---- no rel file *)
  
  if p4_error_level > warning then	(* close and delete rel file *)
    scratch ( relf );				(* any previous rel file is left intact *)
  
  (* psect definitions *)
  
  rel_record (obj$c_gsd);  (* start global symbol directory record *)
  
  if static_size > 0 then begin
    rel_byte (gsd_psc);  (* psect entry *)
    rel_byte (2);  (* long word aligned *)
    rel_byte (static_flags [2]);
    rel_byte (static_flags [1]);
    rel_longword (static_size);
    rel_name ('PAX_STATIC')
  end;
  
  if code_const_size > 0 then begin
    rel_byte (gsd_psc);  (* psect entry *)
    rel_byte (2);  (* longword aligned *)
    rel_byte (code_flags [2]);
    rel_byte (code_flags [1]);
    rel_longword (code_const_size);
    rel_name ('PAX_CODE......B')
  end;
  
  
  (* global references *)
  
  rel_record (obj$c_gsd);
  
  for rt := rt_first to rt_last_used do
    if rt_refs [rt] then begin
      prep_for_global (gsd_sym (* symbol specification entry *),
		       0       (* data type = unspecified *),
		       #H08    (* flags = strong; ref; within; relative *),
		       0 );
      rel_name (rts_name [rt]);  (* the symbol reference *)
    end;
  
  next_ext := ext_refs;
  while next_ext <> nil do begin
    prep_for_global (gsd_sym (* symbol specification entry *),
		     0       (* data type = unspecified *),
		     #H08    (* flags = strong; ref; within; relative *),
		     0 );
    if next_ext^.symbol^.init_value.kind = no_value then
      rel_name (next_ext^.symbol^.name^.text)
    else
      rel_name (next_ext^.symbol^.init_value.valp^.str_val);
    temp_def := next_ext;
    next_ext := next_ext^.next;
    dispose (temp_def)
  end (* while *);
  
  
  (* global definitions *)
  
  symbol := root_block^.children^.id_list.first;
  
  while symbol <> nil do
    with symbol^ do begin
      if public_dcl then begin (* only concerned with publics here *)
  
	if kind in [vars, conditions] then begin
	  prep_for_global (gsd_sym (* symbol specification entry *),
			   0       (* data type = unspecified *),
			   #H0a    (* flags = strong; def; within; relative *),
			   5       (* additional overhead *) );
	  static_address := item_addr;
	  if prog_options.overlay_opt
	    then rel_byte ( psect_id [ code_psect ] )
	  else begin
	    rel_byte ( psect_id [ static_psect ] );
	    if kind = conditions then
	      static_address := static_address + size_init
	    else if init_value.kind = no_value
	      then static_address := static_address + size_init + size_cond
	  end;
	  rel_longword (static_address);
	  rel_name (name^.text)
	end
  
	else if kind = consts then begin
	  
	  if init_value.kind = subr_cst then begin	(* global defns for public routines *)
	    prep_for_global (gsd_epm (* entry point and mask def. etnry *),
			     #H16    (* data type = seq. of instructions *),
			     #H0a    (* flags = strong; def; within; relative *),
			     7       (* additional overhead *) );
	    rel_byte (psect_id[code_psect]);
	    rel_longword (item_addr);
	    int_bytes.int := 0;
	    regs_used := reg_use_by_block^[init_value.blkp^.number];
	    for r := 11 downto r0 do
	      int_bytes.int := 2*int_bytes.int + ord (r in regs_used);
	    rel_byte (int_bytes.bytes.b1);
	    rel_byte (int_bytes.bytes.b2);
	    rel_name (name^.text)
	  end
  
	  else if init_value.kind = alloc_cst then begin	(* global defns for public consts *)
	    prep_for_global (gsd_sym (* symbol specification entry *),
			     0       (* data type = unspecified *),
			     #H0a    (* flags = strong; def; within; relative *),
			     5       (* additional overhead *) );
	    rel_byte (psect_id[code_psect]);
	    temp_def := def (init_value.defp);
	    rel_longword (temp_def^.addr);
	    rel_name (name^.text)
	  end
  
	end (* kind = consts *);
      end (* if public_dcl *);
  
      symbol := next
    end (* with *);
  
  if startdef <> nil then begin	(* global defn for PAX_PROGRAM. *)
    prep_for_global (gsd_epm (* entry point and mask def. entry *),
		     #H16    (* data type = seq. of instructions *),
		     #H0a    (* flags = strong; def; within; relative *),
		     7       (* additional overhead *)  );
    rel_byte (psect_id[code_psect]);
    rel_longword (startdef^.addr);
    int_bytes.int := 0;
    regs_used := reg_use_by_block^[root_block^.children^.number];
    for r := 11 downto r0 do
      int_bytes.int := 2*int_bytes.int + ord (r in regs_used);
    rel_byte (int_bytes.bytes.b1);
    rel_byte (int_bytes.bytes.b2);
    rel_name ('PAX_PROGRAM.')
  end;
  
  
  (* guaranty that end-of-module record (which will need four bytes - a length
     word plus two content bytes) will finish on the last byte of a Dec-10
     word, so that the transfer program to the VAX will not move excess null
     bytes.  Recall that records are always of even length, so the previous record either
     ends on a word boundary, or is two words off.  *)
  
  rel_record (no_record);	(* force write of previous record *)
  if byte_in_word > 0 then begin
    rel_record (obj$c_tir); (* length word + record type byte *)
    rel_byte (tir_opr_nop);	(* nop commands to eat space *)
    rel_byte (tir_opr_nop);
    rel_byte (tir_opr_nop);
  end;
  
  
  (* end-of-module record *)
  
  rel_record (obj$c_eom);  (* start end-of-module record *)
  rel_byte ( ord ( p4_error_level ) );	(* max pass4 error level *)
  
  rel_record (no_record);  (* force write of last record *)
  close (relf);
$IF OPSTATS
  (* Close ops_file if open, after writing last record to the file. *)

  if ops_file_open then begin
    writeln ( ops_file );	(* close off last record *)
    close ( ops_file );
  end;
$ENDIF
  
end (* rel_end *).
 ,fzF6