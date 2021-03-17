$TITLE P10CGU - PDP-10 Code Generator Utilities
MODULE p10cgu;
$PAGE includes, forward declarations
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM p10cg.typ
$SYSTEM p10opc.inc
$SYSTEM p10rel.inc
$SYSTEM pasmap.inc
$SYSTEM pasasm.inc
PUBLIC
FUNCTION gen_cint (int_cons: int_type): rel_syllable;
FORWARD;
$PAGE public, static variables
PUBLIC VAR
code_area: code_list; (* chain of code records for code area *)
static_area: code_list; (* chain for static area image *)
cst_area: code_list; (* chain for constant pool area *)
hbt_area: code_list; (* chain for handler branch tables *)
bptr_area: code_list; (* chain for byte pointers *)
blt_area: code_list; (* chain for BLT control words *)
btmp_area: code_list; (* chain for byte pointers to temps *)
loc_code: code_address; (* location counter for code area *)
loc_static: code_address; (* location counter for static area *)
loc_cst: code_address; (* location counter for constant area *)
loc_hbt: code_address; (* location counter for handler branch tables *)

TYPE
block_vector = ARRAY [0..*] OF blk;
PUBLIC VAR
blk_list: ^ block_vector;
def_lists: ARRAY[def_types] OF def; (* start of chains of internal definitions  *)
STATIC VAR
defnum: ARRAY[def_types] OF id_range; (* next number to assign to internal symbol
					   of type constant_def, code_def *)
PUBLIC VAR
rw_request: ARRAY [rt_int_read..rt_bool_write] OF BOOLEAN;
$PAGE runtime symbol names
PUBLIC CONST
rts_name: rts_name_array := ( 'INITP.', 'PNTRY.', 'PRTRN.', 'EXIT. ', 'UW.RTN'
  , 'UW.PRG', 'II.EXP', 'RI.EXP', 'DI.EXP', 'RR.EXP', 'DD.EXP', 'D.FLT',
  'D.TRNC', 'D.RND', 'R.RND2', 'D.RND2', 'R.SQRT', 'D.SQRT', 'R.LN  ',
  'D.LN  ', 'R.LOG ', 'D.LOG ', 'R.EXP ', 'D.EXP ', 'R.SIN ', 'D.SIN ',
  'R.ASIN', 'D.ASIN', 'R.SINH', 'D.SINH', 'R.COS ', 'D.COS ', 'R.ACOS',
  'D.ACOS', 'R.COSH', 'D.COSH', 'R.TAN ', 'D.TAN ', 'R.TANH', 'D.TANH',
  'R.CTN ', 'D.CTN ', 'R.ATN ', 'D.ATN ', 'R.ATN2', 'D.ATN2', 'RAND1.',
  'RAND0.', 'NEW.  ', 'DSPOS.', 'EXTNT.', 'SUBER.', 'STRER.', 'VALER.',
  'PTRER.', 'FILER.', 'FLDER.', 'CMPER.', 'ASSER.', 'CASER.', 'MP.FC',
  'MPU.FC', 'MPL.FC', (* string move operators *)
'MP.FF', 'MPU.FF', 'MPL.FF', 'MP.FX', 'MPU.FX', 'MPL.FX', 'MP.XC', 'MPU.XC',
  'MPL.XC', 'MP.XF', 'MPU.XF', 'MPL.XF', 'MP.XX', 'MPU.XX', 'MPL.XX', 'MP.RC',
  'MPU.RC', 'MPL.RC', 'MP.RF', 'MPU.RF', 'MPL.RF', 'MP.RX', 'MPU.RX', 'MPL.RX'
  , 'M.FC', 'MU.FC', 'ML.FC', 'M.FF', 'MU.FF', 'ML.FF', 'M.FX', 'MU.FX',
  'ML.FX', 'M.XC', 'MU.XC', 'ML.XC', 'M.XF', 'MU.XF', 'ML.XF', 'M.XX', 'MU.XX'
  , 'ML.XX', 'M.RC', 'MU.RC', 'ML.RC', 'M.RF', 'MU.RF', 'ML.RF', 'M.RX',
  'MU.RX', 'ML.RX', 'CP.FC', 'CPU.FC', 'CPL.FC', (* string concatenation *)
'CP.FF', 'CPU.FF', 'CPL.FF', 'CP.FX', 'CPU.FX', 'CPL.FX', 'CP.XC', 'CPU.XC',
  'CPL.XC', 'CP.XF', 'CPU.XF', 'CPL.XF', 'CP.XX', 'CPU.XX', 'CPL.XX', 'C.FC',
  'CU.FC', 'CL.FC', 'C.FF', 'CU.FF', 'CL.FF', 'C.FX', 'CU.FX', 'CL.FX', 'C.XC'
  , 'CU.XC', 'CL.XC', 'C.XF', 'CU.XF', 'CL.XF', 'C.XX', 'CU.XX', 'CL.XX',
  'CSP.CF', 'CSP.CX', (* string comparision routines *)
'CSP.FC', 'CSP.FF', 'CSP.FX', 'CSP.XC', 'CSP.XF', 'CSP.XX', 'IX.CC', 'IX.CF',
  'IX.CX', (* "index" routines *)
'IX.FC', 'IX.FF', 'IX.FX', 'IX.XC', 'IX.XF', 'IX.XX', 'SM.SS', (* set masks *)
'SM.SD1', 'SMV.LL', 'SMV.LO', 'SMV.LZ', (* set moves *)
'SUN.LL', 'SIN.LL', 'SDF.LL', (* set operators *)
'SLE.LL', 'SEQ.LL', (* set comparisions *)
'IN.VL', (* in operators *)
'SR.CL', 'SR.FL', 'SR.XL', (* search operators *)
'SRU.CL', 'SRU.FL', 'SRU.XL', 'SR.CO', 'SR.FO', 'SR.XO', 'SRU.CO', 'SRU.FO',
  'SRU.XO', 'SR.CD', 'SR.FD', 'SR.XD', 'SRU.CD', 'SRU.FD', 'SRU.XD', 'VF.CL',
  'VF.FL', 'VF.XL', (* verify operators *)
'VFU.CL', 'VFU.FL', 'VFU.XL', 'VF.CO', 'VF.FO', 'VF.XO', 'VFU.CO', 'VFU.FO',
  'VFU.XO', 'VF.CD', 'VF.FD', 'VF.XD', 'VFU.CD', 'VFU.FD', 'VFU.XD', 'STMT. ',
  'OPEN.', 'REWRT.', 'RESET.', (****  input / output  ****)
'OPNTP.', 'OPNBN.', 'GET.', 'GETCH.', 'GETCR.', 'PUT.', 'PUTCH.', 'PUTCR.',
  'RDBIN.', 'WRBIN.', 'RDIMG.', 'WRIMG.', 'CLOSE.', 'CLOSD.', 'CLOSA.',
  'FILSZ.', 'BREAK.', 'EMPTX.', 'EMPTY.', 'PUTPG.', 'CLEAR.', 'IOSTA.',
  'IOSTL.', 'EXTST.', 'RD.FDN', 'RD.FDR', 'WR.FDN', 'WR.FDR', 'WR.SVN',
  'WR.SVR', 'RD.SSN', 'RD.SSR', 'WR.SSN', 'WR.SSR', 'RD.LNN', 'RD.LNR',
  'WR.LNN', 'WR.LNR', 'WR.DNN', 'WR.DNR', 'SEEK.', 'INT.R', 'INT.W', 'REAL.R',
  'REAL.W', 'XSTR.R', 'XSTR.W', 'FSTR.R', 'FSTR.W', 'CSTR.R', 'CSTR.W',
  'STRV.R', 'BOOL.W', 'EX.SET', 'EX.RST', 'MASK.', 'UNMSK.', 'TSMSK.', 'PEND.'
  , 'SIGNL.', 'RSGNL.', 'ST.MTH', 'ST.USR', 'ST.SPC', 'ST.IO', 'EX.MSG',
  'INS.SM', 'DATE', 'D.TIME', 'R.TIME', 'DR.RND' );
$PAGE gen_init, gen_term
(* GEN INIT initializes data used by this package. *)
PUBLIC
PROCEDURE gen_init;
VAR
d: def_types;
b: blk;
rt: rt_symbol;
BEGIN
  code_area.first := NIL;
  code_area.last := NIL;
  static_area.first := NIL;
  static_area.last := NIL;
  cst_area.first := NIL;
  cst_area.last := NIL;
  hbt_area.first := NIL;
  hbt_area.last := NIL;
  blt_area.first := NIL;
  blt_area.last := NIL;
  bptr_area.first := NIL;
  bptr_area.last := NIL;
  btmp_area.first := NIL;
  btmp_area.last := NIL;
  loc_code := 400000b; (* standard high segment base *)
  loc_static := 0;
  loc_cst := 0; (* latter merged into code area *)
  FOR d := MINIMUM (def_types) TO MAXIMUM (def_types) DO BEGIN
    def_lists[d] := NIL;
    defnum [d] := 1;
  END;
  NEW (blk_list, blk_number);
  b := root_block;
  WHILE b <> NIL DO BEGIN
    blk_list^ [b^.number] := b;
    b := b^.downward_call_thread;
  END;
  FOR rt := rt_int_read TO rt_bool_write DO rw_request [rt] := FALSE;
END;
(* GEN TERM cleans storage used by the package. *)
PUBLIC
PROCEDURE gen_term;
BEGIN
  DISPOSE (blk_list);
END;
$PAGE word_size
(* WORD SIZE returns the unit size of storage required for an expression.  This
   is used to compute the size of values to be placed in the parameter list;
   thus, it will only return sizes of 1 or 2. *)
PUBLIC
FUNCTION word_size (exp: expr): unit_range;
BEGIN
  CASE exp^.desc.kind OF
    ints, bools, chars, scalars, pointers, files: word_size := 1;
    reals: IF exp^.desc.precision > srealprec THEN word_size := 2
    ELSE word_size := 1;
    sets: word_size := (exp^.desc.set_length + 35) DIV 36;
    OTHERS: IF exp^.desc.base = NIL THEN word_size := 1 (* ARBITRARY, but it shouldn't matter *)
    ELSE word_size := (exp^.desc.base^.base_size + 35) DIV 36
  END;
END;
$PAGE make_def
(* MAKE DEF returns an all new definition record of type "dtype" and with an
   unique identifying number.  A pointer to the record is returned. *)
PUBLIC
FUNCTION make_def (dtype: def_types): def;
BEGIN
  NEW (make_def);
  WITH make_def^ DO BEGIN
    deftype := dtype; (* set true type *)
    next := def_lists [dtype]; (* add to list of definitions *)
    def_lists[dtype] := make_def;
    rbacklink := 0; (* no references yet emitted *)
    lbacklink := 0;
    defined := FALSE; (* address not yet determined *)
    relocatable := FALSE;
    fixup_required := FALSE;
    first_offset := NIL;
    IF dtype <> extern_def THEN BEGIN
      addr := 0;
      defnumber := defnum [dtype]; (* assign and advance id counter *)
      defnum [dtype] := defnum[dtype] + 1;
    END;
  END;
END;
$PAGE get_def
(* GET DEF returns the internal definition record corresponding to the "dtype" -
   "defid" pair.  If such a record cannot be found, it is created.  This is used
   to create/lookup internal definitions for subroutine identifiers, user
   declared symbols, and local label nodes whose numbering is taken from the
   numbering made in the symbol table or if.  A pointer to the definition
   record is returned. *)
PUBLIC
FUNCTION get_def (dtype: def_types; defid: id_range): def;
VAR
d: def;
BEGIN
  d := def_lists [dtype]; (* search for matching symbol *)
  WHILE d <> NIL DO BEGIN
    IF d^.defnumber = defid THEN BEGIN (* ---> return if found *)
      get_def := d;
      RETURN
    END;
    d := d^.next;
  END;
  get_def := make_def (dtype); (* not found - allocate a new definition *)
  get_def^.defnumber := defid; (* replace number assigned in make_def *)
END;
$PAGE get_extern
(* GET EXTERN returns the internal definition record corresponding to the
   external symbol with a specified radix-50 name.  If such a record cannot
   be found, it is created. *)
PUBLIC
FUNCTION get_extern (name: pdp10word): def;
VAR
d: def;
BEGIN
  d := def_lists [extern_def];
  WHILE d <> NIL DO BEGIN
    IF d^.ext_name.value = name.value THEN BEGIN (* ---> return if found *)
      get_extern := d;
      RETURN
    END;
    d := d^.next;
  END;
  get_extern := make_def (extern_def); (* not found - allocate a new definition *)
  get_extern^.ext_name := name;
END;
$PAGE def_value
(* DEF VALUE defines the address of a definition record. *)
PUBLIC
PROCEDURE def_value (d: def; value: code_address; relocatable: BOOLEAN);
VAR
d1: def;
addr_val: int_type;
BEGIN
  d^.defined := TRUE;
  d^.relocatable := relocatable;
  d^.addr := value;
  IF d^.addr >= 400000b THEN addr_val := d^.addr - 400000b
  ELSE addr_val := d^.addr;
  d1 := d^.first_offset;
  WHILE d1 <> NIL DO BEGIN
    d1^.defined := TRUE;
    d1^.relocatable := relocatable;
    d1^.fixup_required := relocatable ANDIF (addr_val + d1^.offset < 0);
    d1 := d1^.next;
  END;
END;
$PAGE del_def_list
(* DEL DEF LIST deletes a list of definitions and resets the id number counter
   for a particular type of definition ("dtype"). *)
PUBLIC
PROCEDURE del_def_list (dtype: def_types);
VAR
d, d1, next: def;
BEGIN
  d := def_lists [dtype];
  WHILE d <> NIL DO BEGIN
    next := d^.next;
    d1 := d^.first_offset;
    DISPOSE (d);
    d := next;
    WHILE d1 <> NIL DO BEGIN
      next := d1^.next;
      DISPOSE (d1);
      d1 := next;
    END;
  END;
  def_lists [dtype] := NIL;
  defnum [dtype] := 1;
END;
$PAGE get_offset
(* GET OFFSET returns an internal definition offset record for the definition
   specified by "intdef" corresponding to offset "off".  If there is already
   a record for the definition/offset pair, it is returned;  otherwise, a new
   record is created. *)
PUBLIC
FUNCTION get_offset (intdef: def; off: int_type): def;
VAR
od: def;
svalue: int_type;
BEGIN
  IF off = 0 THEN BEGIN (* offset 0 references original defintion *)
    get_offset := intdef;
    RETURN;
  END;
  od := intdef^.first_offset; (* search for same offset def *)
  WHILE od <> NIL DO BEGIN
    IF od^.offset = off THEN BEGIN (* ---> return if offset found *)
      get_offset := od;
      RETURN;
    END;
    od := od^.next;
  END;
  NEW (od, offset_def); (* not found, create new offset record *)
  WITH od^ DO BEGIN
    next := intdef^.first_offset; (* chain to target definition *)
    intdef^.first_offset := od;
    rbacklink := 0; (* no refs yet emitted *)
    lbacklink := 0;
    defined := intdef^.defined;
    relocatable := intdef^.relocatable;
    IF defined ANDIF relocatable THEN BEGIN
      svalue := intdef^.addr + off;
      fixup_required := (svalue < 0) OR ( (svalue < 400000b)
	<> (intdef^.addr < 400000b) );
    END
    ELSE fixup_required := FALSE;
    reldef := intdef; (* offset is relative to this definition *)
    offset := off;
  END;
  get_offset := od;
END;
$PAGE relrt, reldef
(* These four routines construct relocation syllables for runtime symbols, user
   defined symbols, internal definitions and internal labels.  They need not be
   the only routines to generate such records; they are used for convenience when
   the relocation must be explicitly constructed. *)
PUBLIC
FUNCTION relrt (rtsym: rt_symbol): rel_syllable;
BEGIN
  relrt.kind := runtime_sc;
  relrt.relrtsym := rtsym;
END;
PUBLIC
FUNCTION reldef (intdef: def): rel_syllable;
BEGIN
  reldef.kind := def_sc;
  reldef.reldef := intdef;
END;
$PAGE gen_emit
(* GEN EMIT chains an arbitrary code record ("cr") onto the end of the area
   denoted by "area list". *)
PUBLIC
PROCEDURE gen_emit (VAR area_list: code_list; cr: code);
BEGIN
  IF area_list.first = NIL THEN area_list.first := cr
  ELSE area_list.last^.next := cr;
  area_list.last := cr;
  cr^.next := NIL;
END;
$PAGE gen_origin
(* GEN ORIGIN generates an origin record with value "loc" and emits it in the
   area "area_list". *)
PUBLIC
PROCEDURE gen_origin (VAR area_list: code_list; loc: unit_range);
VAR
cr: code;
BEGIN
  NEW (cr, origin);
  cr^.location := loc;
  gen_emit (area_list, cr);
END;
$PAGE set_origin
(* SET ORIGIN emits an origin record with value "loc" at the start of
   area "area_list". *)
PUBLIC
PROCEDURE set_origin (VAR area_list: code_list; loc: unit_range);
VAR
cr: code;
BEGIN
  NEW (cr, origin);
  cr^.location := loc;
  cr^.next := area_list.first;
  area_list.first := cr;
  IF area_list.last = NIL THEN area_list.last := cr;
END;
$PAGE mark_def, def_temp
(* MARK DEF emits a defmark code record which defines the location for an internal
   definition denoted by "intdef".  The record is emitted in the are given by
   "area_list" . *)
PUBLIC
PROCEDURE mark_def (VAR area_list: code_list; intdef: def);
VAR
cr: code;
BEGIN
  NEW (cr, defmark);
  cr^.defname := intdef;
  gen_emit (area_list, cr);
END;
(* DEF TEMP records the introduction of a temporary by emitting a deftemp code
   record in the code area. *)
PUBLIC
PROCEDURE def_temp (temp: val_desc);
VAR
cr: code;
BEGIN
  NEW (cr, deftemp);
  cr^.tempname := temp;
  gen_emit (code_area, cr);
END;
$PAGE gen_source, gen_cmt, gen_asm_label
(* GEN SOURCE generates a code 'source' record and emits it in the code area. *)
PUBLIC
PROCEDURE gen_source (srcid: source_id; srcindx: int_type);
VAR
cr: code;
BEGIN
  NEW (cr, source);
  cr^.stmtid := srcid;
  cr^.stmtindx := srcindx;
  gen_emit (code_area, cr);
END;
(* GEN COMMENT generates a code 'comment' record and emits in the designated
   area. *)
PUBLIC
PROCEDURE gen_cmt (VAR area_list: code_list; TEXT: STRING);
VAR
cr: code;
BEGIN
  NEW (cr, comment, LENGTH (TEXT));
  cr^.ctext := TEXT;
  gen_emit (area_list, cr);
END;
(* GEN ASM LABEL generates a 'asm_label' record and emits it in the designated
   area.  NOTE - these "labels" only appear in the assembly listing.  See MARK DEF
   to emit real labels. *)
PUBLIC
PROCEDURE gen_asm_label (VAR area_list: code_list; TEXT: STRING [6]);
VAR
cr: code;
i: INTEGER;
BEGIN
  NEW (cr, asm_label);
  cr^.ltext := TEXT;
  REPEAT
    i := SEARCH (cr^.ltext, ['_']);
    IF i <> 0 THEN cr^.ltext [i] := '%'
  UNTIL i = 0;
  gen_emit (area_list, cr)
END;
$PAGE halfword
(* HALFWORD returns the low order 18 bits of a signed integer value as a positive
   value in the range 0..777777B. *)
PUBLIC
FUNCTION halfword (i: int_type): unit_range;
BEGIN
  IF i >= 0 THEN halfword := i MOD 1000000b
  ELSE halfword := 1000000b + (i MOD 1000000b);
END;
$PAGE gen, genind
(* GEN and GENIND construct instruction form code records given values for the
   fields of the instruction and the relocation syllables.  In "gen", indirection
   is turned off; in "genind", it is turned off.  The generated instruction is
   emitted in the code area. *)
PUBLIC
PROCEDURE gen ( opc: opc_range; acc, INDEX: registers; offset: int_type;
  rel: rel_syllable );
VAR
cr: code;
BEGIN
  NEW (cr, instruction);
  WITH cr^ DO BEGIN
    inst.opcode := opc;
    inst.acc := acc;
    inst.INDEX := INDEX;
    inst.indirect := FALSE;
    inst.offset := halfword (offset);
    reloc := rel;
  END;
  gen_emit (code_area, cr);
END;
PUBLIC
PROCEDURE genind ( opc: opc_range; acc, INDEX: registers; offset: int_type;
  rel: rel_syllable );
VAR
cr: code;
BEGIN
  NEW (cr, instruction);
  WITH cr^ DO BEGIN
    inst.opcode := opc;
    inst.acc := acc;
    inst.INDEX := INDEX;
    inst.indirect := TRUE;
    inst.offset := halfword (offset);
    reloc := rel;
  END;
  gen_emit (code_area, cr);
END;
$PAGE gen_rr
(* GEN RR generates a register to register instruction given the opcode, acc and
   operand fields.  The instruction code record generated is emitted in the code
   area. *)
PUBLIC
PROCEDURE gen_rr ( opc: opc_range; acc: registers; operand: registers );
VAR
cr: code;
BEGIN
  NEW (cr, instruction);
  WITH cr^ DO BEGIN
    inst.opcode := opc;
    inst.acc := acc;
    inst.INDEX := 0;
    inst.indirect := FALSE;
    inst.offset := operand;
    reloc.kind := register_sc;
  END;
  gen_emit (code_area, cr);
END;
$PAGE gen_rx
(* GEN RX generates a register to register instruction given the opcode, acc and
   index field.  The instruction is assumed to be of the form opc acc,0(index)
   where the opcode is an immediate operation.  The instruction code record
   generated is emitted in the code area. *)
PUBLIC
PROCEDURE gen_rx ( opc: opc_range; acc: registers; operand: registers );
VAR
cr: code;
BEGIN
  NEW (cr, instruction);
  WITH cr^ DO BEGIN
    inst.opcode := opc;
    inst.acc := acc;
    inst.INDEX := operand;
    inst.indirect := FALSE;
    inst.offset := 0;
    reloc.kind := register_sc;
  END;
  gen_emit (code_area, cr);
END;
$PAGE gen_ri
(* GEN RI generates a register-immediate instruction given the opcode, acc and
   immediate offset.  The instruction code record generated is put in the code area. *)
PUBLIC
PROCEDURE gen_ri ( opc: opc_range; acc: registers; operand: int_type );
VAR
cr: code;
BEGIN
  NEW (cr, instruction);
  WITH cr^ DO BEGIN
    inst.opcode := opc;
    inst.acc := acc;
    inst.INDEX := 0;
    inst.indirect := FALSE;
    inst.offset := halfword (operand);
    reloc.kind := absolute_sc;
  END;
  gen_emit (code_area, cr);
END;
$PAGE gen_rm
(* GEN RM generates a register-memory instruction given the opcode, acc and an
   addr_desc addressing the desired memory location.  The instruction code record
   generated is emitted in the code area. *)
PUBLIC
PROCEDURE gen_rm ( opc: opc_range; acc: registers; operand: addr_desc );
VAR
cr: code;
immed_val: code_address;
temp_operand: addr_desc;
BEGIN
  temp_operand := operand;
  NEW (cr, instruction);
  WITH cr^ DO BEGIN
    IF temp_operand.immediate AND temp_operndirect THEN BEGIN
      temp_operand.immediate := FALSE;
      temp_operand.indirect := FALSE
    END;
    IF temp_operand.immediate THEN BEGIN
      IF (cam <= opc) AND (opc <= cam+gtc) THEN inst.opcode := (opc - cam)
	+ cai
      ELSE IF (skip <= opc) AND (opc <= skip+gtc) THEN BEGIN
	immed_val := temp_operand.offset;
	temp_operand := absolute_reference;
	temp_operand.reloc := gen_cint (immed_val);
	inst.opcode := opc;
      END
      ELSE inst.opcode := opc + imdc;
    END
    ELSE IF temp_operand.indirect ANDIF
      (opc IN [movei, hlli, hrli, hllzi, hrlzi, hlloi, hrloi, hllei, hrlei,
      hrri, hlri, hrrzi, hlrzi, hrroi, hlroi, hrrei, hlrei]) THEN BEGIN
      IF opc = movei THEN inst.opcode := hrrz (* use of move would lose clear of lh *)
      ELSE inst.opcode := opc - imdc;
      temp_operand.indirect := FALSE
    END
    ELSE inst.opcode := opc;
    inst.acc := acc;
    inst.INDEX := temp_operand.INDEX;
    inst.indirect := temp_operand.indirect;
    inst.offset := temp_operand.offset;
    reloc := temp_operand.reloc;
  END;
  gen_emit (code_area, cr)
END;
$PAGE gen_rt, gen_rl
(* GEN RT generates an instruction whose offset is relocated by the value of
   a runtime symbol.  The instruction code record is emitted in the code area. *)
PUBLIC
PROCEDURE gen_rt (opc: opc_range; acc: registers; rtsym: rt_symbol);
BEGIN
  gen (opc, acc, 0, 0, relrt (rtsym))
END;
(* GEN RL generates an instruction referencing a label_node.  The instruction code
   record is emitted in the code area. *)
PUBLIC
PROCEDURE gen_rl (opc: opc_range; acc: registers; lab: tuple);
BEGIN
  gen (opc, acc, 0, 0, reldef (get_def (label_def, lab^.block_order_no)))
END;
$PAGE gen_word
(* GEN WORD generates a code record for a fullword, stringword or setword given
   the the kind ("wkind"), the "word" itself, and the area in which to emit it
   ("area_list"). *)
PUBLIC
PROCEDURE gen_word (VAR area_list: code_list; word: pdp10word;
  wkind: code_types);
VAR
cr: code;
BEGIN
  NEW (cr, fullword);
  cr^.kind := wkind;
  cr^.fwd := word;
  gen_emit (area_list, cr);
END;
$PAGE gen_string

(* GEN_STRING generates a string in the code area for trace, file, and page
   blocks.      *)
PUBLIC
PROCEDURE gen_string (str: STRING);
VAR
ix: int_type;
word: pdp10word;
BEGIN
  word.value := LENGTH (str);
  gen_word (code_area, word, fullword);
  FOR ix := 1 TO (LENGTH (str) + 4) DIV 5 DO BEGIN
    word.str[1:5] := SUBSTR (str, 5 * (ix - 1) + 1, MIN (5, LENGTH (str)
      - 5 * (ix - 1)));
    gen_word (code_area, word, stringword);
  END;
END (* gen_string *);
$PAGE rel_match

(* REL MATCH compares two relocation syllables, returning TRUE if they define
   the same relocation. *)

FUNCTION rel_match (r1, r2: rel_syllable): BOOLEAN;
BEGIN
  IF r1.kind = r2.kind THEN CASE r1.kind OF
    local_sc, parameter_sc, static_sc, external_sc: rel_match := (r1.relsym =
      r2.relsym);
    runtime_sc: rel_match := (r1.relrtsym = r2.relrtsym);
    temp_sc: rel_match := (r1.relval = r2.relval);
    def_sc: rel_match := (r1.reldef = r2.reldef);
    OTHERS: rel_match := TRUE
  END
  ELSE rel_match := FALSE;
END;
$PAGE addr_equal
(* ADDR EQUAL compares two addr_desc's for equality. *)
PUBLIC
FUNCTION addr_equal (adr1, adr2: addr_desc): BOOLEAN;
BEGIN
  addr_equal := (adr1.mode = adr2.mode) AND (adr1.immediate = adr2.immediate)
    AND (adr1.indirect = adr2.indirect) AND (adr1.INDEX = adr2.INDEX) AND
    (adr1.offset = adr2.offset) AND (adr1.slice_size = adr2.slice_size) AND
    (adr1.slice_offset = adr2.slice_offset) AND
    rel_match (adr1.reloc, adr2.reloc)
END;
$PAGE gen_blt
(* GEN BLT generates a constant word whose left and right halfwords are
   constants or addresses with relocation. The halfwords and their
   respective relocations are passed as parameters.  The resulting constant is placed
   in the blt area, and a rel_syllable denoting the address of the generated
   constant is returned. *)
PUBLIC
FUNCTION gen_blt (source_offset: code_address; source_reloc: rel_syllable;
  dest_offset: code_address; dest_reloc: rel_syllable) : rel_syllable;
VAR
xwd: pdp10word;
bscan, blast: code;
cr: code;
bltdef: def;
BEGIN
  xwd.lh := source_offset;
  xwd.rh := dest_offset;
  bscan := blt_area.first;
  WHILE (bscan <> NIL) ANDIF NOT ( (bscan^.kind = halfwords) ANDIF
    (bscan^.xwd.value = xwd.value) ANDIF
    rel_match (bscan^.lreloc, source_reloc) ANDIF
    rel_match (bscan^.rreloc, dest_reloc) ) DO BEGIN
    blast := bscan;
    bscan := bscan^.next;
  END;
  IF bscan <> NIL THEN bltdef := blast^.defname
  ELSE BEGIN
    bltdef := make_def (constant_def);
    mark_def (blt_area, bltdef);
    NEW (cr, halfwords);
    cr^.xwd := xwd;
    cr^.lreloc := source_reloc;
    cr^.rreloc := dest_reloc;
    gen_emit (blt_area, cr);
  END;
  gen_blt := reldef (bltdef);
END;
$PAGE gen_bptr
(* GEN BPTR generates a constant byte pointer referencing a slice of a
   particular  memory location.  The location and the byte parameters are given
   by "mem".  The byte pointer is placed in the byte pointer area, and a
   rel_syllable denoting  the address of the byte pointer is returned. *)
PUBLIC
FUNCTION gen_bptr (mem: addr_desc): rel_syllable;
VAR
bpdef: def;
bptr: pdp10word;
reloc: rel_syllable;
bscan, blast: code;
cr: code;
BEGIN
  WITH bptr DO BEGIN
    p := 36 - mem.slice_offset - mem.slice_size;
    s := mem.slice_size;
    bp1 := 0;
    bpindirect := mem.indirect;
    bpindex := mem.INDEX;
    bpoffset := halfword(mem.offset);
  END;
  reloc := mem.reloc;
  IF (reloc.kind = temp_sc) AND NOT quick THEN BEGIN
    bpdef := make_def (constant_def);
    mark_def (btmp_area, bpdef);
    NEW (cr, bytepointer);
    cr^.bptr := bptr;
    cr^.bpreloc := mem.reloc;
    gen_emit (btmp_area, cr);
    gen_bptr := reldef (bpdef);
  END
  ELSE BEGIN
    IF reloc.kind IN [parameter_sc, local_sc] THEN BEGIN
      IF bptr.bpoffset <= 377777b THEN bptr.bpoffset := halfword (bptr.
	bpoffset + reloc.relsym^.item_addr)
      ELSE bptr.bpoffset := halfword (bptr.bpoffset + reloc.relsym^.item_addr
	- 1000000b);
      reloc.kind := absolute_sc;
    END;
    bscan := bptr_area.first;
    WHILE (bscan <> NIL) ANDIF NOT ( (bscan^.kind = bytepointer) ANDIF
      (bscan^.bptr.value = bptr.value) ANDIF rel_match (bscan^.bpreloc, reloc)
      ) DO BEGIN
      blast := bscan;
      bscan := bscan^.next;
    END;
    IF bscan <> NIL THEN gen_bptr := reldef (blast^.defname)
    ELSE BEGIN
      bpdef := make_def (constant_def);
      mark_def (bptr_area, bpdef);
      NEW (cr, bytepointer);
      cr^.bptr := bptr;
      cr^.bpreloc := reloc;
      gen_emit (bptr_area, cr);
      gen_bptr := reldef (bpdef);
    END;
  END;
END;
$PAGE btmp_offsets
(* BTMP OFFSETS scans the list of byte pointers to temps for the current
   procedure, replacing all references to temporaries by absolute stack
   offsets.  The resolved byte pointers are then pooled with the ordinary
   byte pointers.  The temporary byte pointer list is assumed to have the
   structure: <defmark, bytepointer, defmark, bytepointer, ...> *)
PUBLIC
PROCEDURE btmp_offsets;
VAR
def_word, bptr_word, bscan, blast: code;
BEGIN
  WHILE btmp_area.first <> NIL DO BEGIN
    def_word := btmp_area.first; (* take the first temp byte pointer *)
    bptr_word := def_word^.next;
    btmp_area.first := bptr_word^.next;
    WITH bptr_word^ DO BEGIN (* compute the byte pointer offset *)
      bptr.INDEX := sp;
      IF bptr.bpoffset <= 377777b THEN bptr.bpoffset := halfword (bptr.
	bpoffset + bpreloc.relval^.loc.offset)
      ELSE bptr.bpoffset := halfword (bptr.bpoffset + bpreloc.relval^.loc.
	offset - 1000000b);
      bpreloc.kind := absolute_sc;
    END;
    bscan := bptr_area.first; (* find a matching byte pointer *)
    WHILE (bscan <> NIL) ANDIF NOT ( (bscan^.kind = bytepointer) ANDIF
      (bscan^.bptr.value = bptr_word^.bptr.value) ANDIF
      rel_match (bscan^.bpreloc, bptr_word^.bpreloc) ) DO BEGIN
      blast := bscan;
      bscan := bscan^.next;
    END;
    IF bscan <> NIL THEN BEGIN (* make the defmark point to it *)
      blast^.next := def_word;
      def_word^.next := bscan;
      DISPOSE (bptr_word);
    END
    ELSE BEGIN (* no matching byte pointer: add it to the area *)
      IF bptr_area.first = NIL THEN bptr_area.first := def_word
      ELSE bptr_area.last^.next := def_word;
      bptr_area.last := bptr_word;
      bptr_word^.next := NIL;
    END;
  END (* while btmp_area.first <> nil *);
  btmp_area.last := NIL;
END;
$PAGE gen_xwd
(* GEN XWD generates a pair of halfword values in a word, given the values and
   relocation for each half word.  The word is emitted in the code area. *)
PUBLIC
PROCEDURE gen_xwd ( lhv: int_type; lhrel: rel_syllable; rhv: int_type;
  rhrel: rel_syllable);
VAR
cr: code;
BEGIN
  NEW (cr, halfwords);
  WITH cr^ DO BEGIN
    xwd.lh := halfword (lhv);
    xwd.rh := halfword (rhv);
    lreloc := lhrel;
    rreloc := rhrel;
  END;
  gen_emit (code_area, cr);
END;
$PAGE gen_vnode
(* GEN VNODE allocates one or more FULLWORDs, STRINGWORDs or
   SETWORDs in the specified area to represent the specified
   value. *)
PUBLIC
PROCEDURE gen_vnode(VAR area_list: code_list; value: val_ptr);
$PAGE store_word - in gen_vnode
(* CUR_CODE and CUR_OFFSET are vars used to keep track of
   which code record is currently being initialized by GEN_VAL
   or GEN_VNODE.  They are used solely by STORE_WORD except for
   initialization by GEN_VNODE.  *)
VAR
cur_code: code; (* current code record for a constant *)
cur_offset: unit_range; (* offset of CUR_CODE within constant *)
(* STORE_WORD packs the rightmost WIDTH bits of TEN_WORD into the appropriate
   code word corresponding to the current value being generated
   beginning at offset (OFFSET mod BITS_PER_UNIT).  STORE_WORD is called
   from GEN_COMPONENT.  Accross successive calls OFFSET must be
   non-decreasing. *)

  PROCEDURE store_word (ten_word: pdp10word; offset: bit_range;
    width: elem_sizes; code_kind: code_types);
  VAR
  code_num: unit_range; (* offset of code record within constant
						     which will be stored into *)
  word_offset: bit_offset; (* bit offset within word *)
  temp_word: pdp10word; (* avoids passing heap address to Macro routine *)
  BEGIN
    code_num := offset DIV bits_per_unit;
    WHILE code_num > cur_offset DO BEGIN
      cur_offset := cur_offset + 1;
      cur_code := cur_code^.next
    END;
    WITH cur_code^ DO BEGIN
      word_offset := offset MOD bits_per_unit;
      IF (word_offset = 0) ANDIF (width = bits_per_unit) THEN BEGIN
	fwd := ten_word
      END
      ELSE BEGIN
	temp_word := fwd;
	stuff_byte (ten_word, temp_word, word_offset, width);
	fwd := temp_word
      END;
      kind := code_kind
    END
  END;
$PAGE find_variant - in gen_vnode
  (* FIND VARIANT is given a type node of kind RECORDS or VARIANTS and
     an integer representing a tag field value.  It returns the type node
     of the variant corresponding to the tag value.  NIL is returned if no
     such variant is found.  The record or variant whose type is passed
     in must contain a variant part. *)

  FUNCTION find_variant (recvar: typ; tag_val: int_type): typ;
  VAR
  cur_variant: typ;
  BEGIN
    find_variant := NIL; (* updated if variant found or
						       if others case found *)
    cur_variant := recvar^.variant_tag^.first_variant;
    WHILE cur_variant <> NIL DO BEGIN
      WITH cur_variant^ DO BEGIN
	IF others_var THEN find_variant := cur_variant
	ELSE IF (minlab <= tag_val) AND (tag_val <= maxlab) THEN BEGIN
	  find_variant := cur_variant;
	  RETURN (* <--- exit here if specific variant found *)
	END;
	cur_variant := next_variant
      END (* with *);
    END (* while *);
  END;
$PAGE gen_component - in gen_vnode
  (* GEN COMPONENT initializes code words to correspond to the value
     represented by VALUE.  BASE_OFFSET contains the bit offset, within the
     constant currently being generated, at which the value should be
     placed.  BASE_WIDTH is the width in bits of the constant represented
     by VALUE. *)

  PROCEDURE gen_component (value: val_ptr; base_offset: bit_range;
    base_width: bit_range);
  VAR
  ten_word: pdp10word; (* single word buffer *)
  words_required: unit_range; (* words needed to contain an object *)
  i, j: int_type; (* for loop indices *)
  start: char_range; (* start index of substring *)
  len: 0..chars_per_unit; (* length of substring *)
  jus_start: 1..chars_per_unit; (* for right justified word packing *)
  max_bit: bit_offset; (* high bit used in set packing *)
  field_sym: sym; (* current field's sym *)
  cur_recvar: typ; (* record/variant containing FIELD_SYM *)
  is_tag: BOOLEAN; (* true => FIELD_SYM is tag *)
  field_number: unit_range; (* number of current field *)
  num_elems: int_type; (* number of elements of array *)
  elems_per_unit: elem_sizes; (* array elements per unit *)
  bits_per_elem: bit_range; (* bits required per array element *)
  word_offset: unit_range; (* word offset of array element within array *)
  bits_offset: bit_offset; (* bit offset of start of an array element*)
  code_num: unit_range; (* base code record for field or element *)
  offset: bit_range; (* offset within constant of component of VALUE *)
  width: bit_range; (* width of a component of VALUE *)
  real_words: RECORD (* overlays double-word real with two integers *)
    CASE BOOLEAN OF
      FALSE: (
	rvalue: real_type );
      TRUE: (
	high, low: machine_word );
  END;
$PAGE gen_subval - in gen_component
  (* GEN SUBVAL generates a value corresponding to an array element
     or a record field.  Scalar and pointer constants are generated
     immediately; other VALUE_KINDs cause GEN_COMPONENT to be called
     recursively.  OFFSET is the base offset of the value within the
     current constant in bits and WIDTH is the value's width in bits. *)

    PROCEDURE gen_subval ( val_recd: val; offset: bit_range;
      width: bit_range );
    BEGIN
      WITH val_recd DO BEGIN
	CASE kind OF
	  scalar_cst: store_word (('F', ival), offset, width, fullword);
	  real_cst, string_cst, set_cst, array_cst, record_cst: gen_component(
	    valp, offset, width );
	  ptr_cst: store_word (('X', 0, int_nil), offset, width, fullword);
	  subr_cst: (* illegal !!! *)
	END (* case *)
      END (* with *)
    END; (* procedure gen_subval *)
$PAGE gen_component - body
  BEGIN
    IF base_width = 0 THEN RETURN;
    WITH value^ DO BEGIN
      CASE kind OF
	scalar_cst: store_word (('F', scalar_val)
	  , base_offset, base_width, fullword);
	real_cst: IF real_prec <= srealprec THEN store_word (('R', real_val)
	  , base_offset, bits_per_unit, realword)
	ELSE BEGIN
	  real_words.rvalue := real_val;
	  store_word (('F', real_words.high)
	    , base_offset, bits_per_unit, drealword);
	  offset := base_offset + bits_per_unit;
	  store_word (('F', real_words.low)
	    , offset, bits_per_unit, drealword2);
	END;
	string_cst: BEGIN
	  offset := base_offset;
	  IF str_varying_ref THEN BEGIN
	    width := bits_per_unit;
	    ten_word.value := LENGTH (str_val);
	    store_word (ten_word, offset, bits_per_unit, fullword);
	    offset := offset + bits_per_unit
	  END;
	  words_required := (LENGTH (str_val) + chars_per_unit - 1)
	    DIV chars_per_unit;
	  FOR i := 1 TO words_required DO BEGIN
	    start := (i - 1) * chars_per_unit + 1;
	    len := MIN(chars_per_unit, LENGTH (str_val) - start + 1);
	    jus_start := chars_per_unit - len + 1;
	    width := len * (bits_per_unit DIV chars_per_unit) + 1;
	    ten_word.value := 0;
	    ten_word.str[jus_start:len] := SUBSTR(str_val, start, len);
	    store_word (ten_word, offset, width, stringword);
	    offset := offset + bits_per_unit
	  END
	END;
	set_cst: BEGIN
	  offset := base_offset;
	  words_required := (DIMENSION (set_val) + bits_per_unit - 1)
	    DIV bits_per_unit;
	  FOR i := 1 TO words_required DO BEGIN
	    max_bit := MIN (bits_per_unit - 1, DIMENSION (set_val) - ( (i - 1)
	      * bits_per_unit) - 1);
	    FOR j := 0 TO max_bit DO ten_word.bits[bits_per_unit - max_bit - 1
	      + j] := set_val[(i - 1) * bits_per_unit + j];
	    width := max_bit + 1;
	    store_word (ten_word, offset, width, setword);
	    offset := offset + width
	  END;
	END;
	array_cst: BEGIN
	  WITH struc_type^ DO BEGIN
	    num_elems := index_type^.maxval - index_type^.minval + 1;
	    IF element_size <= bits_per_unit THEN elems_per_unit :=
	      bits_per_unit DIV element_size
	    ELSE bits_per_elem := ( (element_size + bits_per_unit - 1)
	      DIV bits_per_unit ) * bits_per_unit;
	    width := element_size;
	    FOR i := 1 TO MIN (UPPERBOUND (elem_vals), num_elems) DO BEGIN
	      IF element_size <= bits_per_unit THEN BEGIN
		word_offset := (i - 1) DIV elems_per_unit;
		bits_offset := ( (i-1) MOD elems_per_unit ) * element_size;
		offset := base_offset + (word_offset * bits_per_unit)
		  + bits_offset
	      END
	      ELSE BEGIN
		offset := base_offset + (bits_per_elem * (i - 1))
	      END;
	      gen_subval (elem_vals[i], offset, width)
	    END (* for *)
	  END (* with *)
	END (* array_cst case *);
	record_cst: BEGIN
	  field_sym := struc_type^.field_list;
	  cur_recvar := struc_type;
	  field_number := 0;
	  offset := base_offset;
	  REPEAT (* traverse appropriate sym records *)
	  (* if starting new variant then find correct variant type record
			 and its first field *)
	    WHILE ( (cur_recvar^.field_list = NIL) ANDIF
	      (cur_recvar^.variant_tag <> NIL) ) ORIF ( (field_sym <> NIL)
	      ANDIF (cur_recvar <> field_sym^.fld_variant) ) DO BEGIN
	      field_number := field_number + 1;
	      IF field_number > UPPERBOUND (elem_vals) THEN RETURN;
	      cur_recvar := find_variant (cur_recvar, elem_vals[field_number].
		ival);
	      IF cur_recvar <> NIL THEN field_sym := cur_recvar^.field_list
	      ELSE RETURN;
	      IF field_sym <> NIL THEN cur_recvar := field_sym^.fld_variant
	    END;
	    (* is this a tag field? *)
	    is_tag := (cur_recvar^.variant_tag <> NIL) ANDIF
	      (field_sym = cur_recvar^.variant_tag^.tag_field);
	    (* process the current field *)
	    field_number := field_number + 1;
	    IF field_number > UPPERBOUND (elem_vals) THEN RETURN;
	    IF field_sym <> NIL THEN BEGIN
	      offset := base_offset + field_sym^.fld_offset;
	      width := field_sym^.fld_width;
	      WITH field_sym^.type_desc^ DO BEGIN
		IF flexible AND (kind IN [arrays, strings]) THEN BEGIN
		  WITH value^.elem_vals[field_number].valp^ DO BEGIN
		    ten_word.value := 0;
		    IF kind = array_cst THEN BEGIN
		      FOR i := 1 TO (struc_type^.base_size + bits_per_unit - 1
			)
			DIV bits_per_unit DO gen_word (area_list, ten_word, fullword
			);
		      ten_word.value := struc_type^.index_type^.maxval;
		    END
		    ELSE BEGIN
		      FOR i := 1 TO (LENGTH (str_val) + chars_per_unit - 1)
			DIV chars_per_unit DO gen_word (area_list, ten_word,
			fullword);
		      ten_word.value := LENGTH (str_val);
		    END;
		  END (* with *);
		  store_word (ten_word, offset, bits_per_unit, fullword);
		  offset := offset + bits_per_unit;
		END;
	      END;
	      gen_subval(value^.elem_vals[field_number], offset, width)
	    END;
	    IF is_tag THEN field_number := field_number - 1;
	    (* get next field and check whether or not we are done *)
	    IF field_sym <> NIL THEN BEGIN
	      field_sym := field_sym^.next;
	      IF (field_sym^.fld_variant <> cur_recvar) ANDIF
		(cur_recvar^.variant_tag = NIL) THEN field_sym := NIL
	    END
	  UNTIL field_sym = NIL
	END (* record_cst case *)
      END (* case *)
    END (* with *)
  END;
$PAGE bits_required
  (* BITS REQUIRED returns the size in bits of the constant described by VALUE.  *)

  FUNCTION bits_required (value: val_ptr): bit_range;
  BEGIN
    WITH value^ DO BEGIN
      CASE kind OF
	scalar_cst: bits_required := bits_per_unit;
	real_cst: BEGIN
	  IF real_prec > srealprec THEN bits_required := 2 * bits_per_unit
	  ELSE bits_required := bits_per_unit
	END;
	string_cst: BEGIN
	  bits_required := LENGTH (str_val)
	    * (bits_per_unit DIV chars_per_unit) + (LENGTH (str_val)
	    DIV chars_per_unit);
	  IF str_varying_ref THEN bits_required := bits_required +
	    bits_per_unit
	END;
	set_cst: BEGIN
	  bits_required := DIMENSION (set_val)
	END;
	array_cst, record_cst: BEGIN
	  bits_required := struc_type^.base_size
	END
      END (* case *)
    END (* with *)
  END;
$PAGE init_required
  (* INIT REQUIRED generates NUM_REQUIRED fullwords in area AREA_LIST.
     It returns a pointer to the first code record generated.
     The fullwords generated are initialized to zero. *)

  FUNCTION init_required (VAR area_list: code_list; num_required: unit_range)
    : code;
  VAR
  ten_word: pdp10word;
  i: unit_range;
  BEGIN
    ten_word.value := 0;
    gen_word (area_list, ten_word, fullword);
    init_required := area_list.last;
    FOR i := 2 TO num_required DO BEGIN
      gen_word(area_list, ten_word, fullword)
    END
  END;
$PAGE gen_vnode - main routine
  (* GEN_VNODE main routine *)
CONST
init_offset: bit_range := 0;
VAR
recds_required: unit_range;
width: bit_range;
BEGIN
  width := bits_required (value);
  recds_required := (width + bits_per_unit - 1) DIV bits_per_unit;
  cur_code := init_required( area_list, recds_required);
  cur_offset := 0;
  gen_component ( value, init_offset, width)
END;
$PAGE gen_val
(* GEN VAL allocates one or more FULLWORDs, STRINGWORDs or SETWORDs in
   the specified area to represent the specified values.  It is identical
   to GEN_VNODE except that it takes a VAL record as a
   parameter rather than a VAL_PTR. *)
PUBLIC
PROCEDURE gen_val (VAR area_list: code_list; value: val);
BEGIN
  WITH value DO BEGIN
    CASE kind OF
      scalar_cst: gen_word (area_list, ('F', ival), fullword);
      real_cst, string_cst, set_cst, array_cst, record_cst: BEGIN
	gen_vnode (area_list, valp)
      END;
      ptr_cst: gen_word (area_list, ('X', 0, int_nil), fullword);
      OTHERS: (* illegal !!! *)
      assert (FALSE)
    END (*case *)
  END
END;
$PAGE gen_cnode
(* GEN CNODE allocates the specified value in the constant area, and
   returns a DEF_SC relocation syllable for a definition node referring
   to the first word of the constant *)
PUBLIC
FUNCTION gen_cnode (value: val_ptr): rel_syllable OPTIONS special(coercions);
VAR
cons_def: def;
BEGIN
  IF value^.def_addr = NIL THEN BEGIN
    cons_def := make_def (constant_def);
    mark_def (cst_area, cons_def);
    gen_vnode (cst_area, value);
    gen_cnode := reldef (cons_def);
    value^.def_addr := val_ptr (cons_def) (* coerce *)
  END
  ELSE gen_cnode := reldef (def (value^.def_addr)) (* coerce *)
END;
$PAGE gen_cval
(* GEN CVAL allocates the specified value in the constant area, and
   returns a DEF_SC relocation syllable for a definition node referring
   to the first word of the constant.  This routine is identical to
   GEN_CNODE except that it takes a VAL record as a parameter rather
   than a VAL_PTR. *)
PUBLIC
FUNCTION gen_cval (value: val): rel_syllable OPTIONS special(coercions);
TYPE
vkind_set = SET OF value_kind;
CONST
has_ptr: vkind_set = [real_cst, string_cst, set_cst, array_cst, record_cst];
VAR
cons_def: def;
BEGIN
  WITH value DO BEGIN
    IF NOT ((kind IN has_ptr) ANDIF (valp^.def_addr <> NIL)) AND
      (kind <> alloc_cst) THEN BEGIN
      cons_def := make_def (constant_def);
      mark_def (cst_area, cons_def);
      gen_val (cst_area, value);
      gen_cval := reldef (cons_def);
      IF kind IN has_ptr THEN valp^.def_addr := val_ptr(cons_def) (* coerce *)
    END
    ELSE IF kind = alloc_cst THEN gen_cval := reldef (def (defp)) (* coerce *)
    ELSE gen_cval := reldef (def (valp^.def_addr)) (* coerce *)
  END (* with *)
END;
$PAGE gen_cword
(* GEN CWORD allocates the specified (via parameter CONS_WORD) word in
   the constant area.  The type of the code record emitted is specified
   by parameter CONS_CODE and must be either FULLWORD, STRINGWORD or
   SETWORD. A DEFINITION record referring to the word is allocated
   also and a DEF_SC relocation syllable for the definition is the
   return value of the function. *)
PUBLIC
FUNCTION gen_cword (cons_word: pdp10word; cons_code: code_types)
  : rel_syllable;
VAR
cons_def: def;
BEGIN
  cons_def := make_def (constant_def);
  mark_def (cst_area, cons_def);
  gen_word (cst_area, cons_word, cons_code);
  gen_cword := reldef (cons_def)
END;
$PAGE gen_cint
(* GEN CINT allocates a FULLWORD code record in the constant area to hold
   the specified integer.  A DEFINITION record referring to the word is
   also allocated and a DEF_SC relocation syllable for the definition
   is the return value of the function. *)
PUBLIC
FUNCTION gen_cint (* int_cons: int_type): rel_syllable *);
BEGIN
  gen_cint := gen_cword (('F', int_cons), fullword);
END;
$PAGE gen_cst
(* GEN CST allocates a scalar value in the constant area and returns
   an address descriptor for the location in the constant area. *)
PUBLIC
FUNCTION gen_cst ( value: int_type ): addr_desc;
BEGIN
  gen_cst := absolute_reference;
  gen_cst.reloc := gen_cint ( value );
END;
$PAGE wr_code
(* WR CODE writes code records to the rel file, deletes the records, defines
   symbols, and updates the location counter.  If "write_it" is false, it only
   defines symbols. *)
PUBLIC
PROCEDURE wr_code (VAR area_list: code_list; VAR ic: unit_range;
  write_it: BOOLEAN);
VAR
cr, next_cr: code;
def_ctr: code_address;
BEGIN
  def_ctr := ic;
  cr := area_list.first;
  WHILE cr <> NIL DO BEGIN
    WITH cr^ DO BEGIN
      CASE kind OF
	origin: def_ctr := location;
	instruction..halfwords: def_ctr := def_ctr + 1;
	defmark: def_value (defname, def_ctr, TRUE);
	OTHERS:
	(* no action *)
      END (* case kind *);
      cr := next;
    END;
  END (* while cr <> nil *);
  IF write_it THEN BEGIN
    cr := area_list.first;
    WHILE cr <> NIL DO BEGIN
      WITH cr^ DO BEGIN
	CASE kind OF
	  origin: ic := location;
	  instruction: rel_code (inst, ic, none, reloc);
	  bytepointer: rel_code (bptr, ic, none, bpreloc);
	  fullword, stringword, setword, realword, drealword, drealword2:
	    rel_code (fwd, ic, none, none);
	  halfwords: rel_code (xwd, ic, lreloc, rreloc);
	  source: IF (map_opt IN cur_block^.semantic_options) AND
	    (stmtindx = 1) THEN map_write (stmtid, ic);
	  OTHERS:
	  (* no action *)
	END (* case kind *);
	next_cr := next;
	DISPOSE (cr);
	cr := next_cr;
      END;
    END (* while cr <> nil *);
    area_list.first := NIL;
    area_list.last := NIL;
  END (* if write_it *);
END.
5Y+