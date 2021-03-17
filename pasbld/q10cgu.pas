$TITLE q10cgu - quick pass code generator utilities
module q10cgu;

$PAGE includes, forward declarations
$include pascal.inc
$INCLUDE ptmcon.inc
$include pasist.inc
$include paspt.typ
$include pasif.typ
$include q10cg.typ
$include p10opc.inc
$INCLUDE p10rel.inc
$INCLUDE pasmap.inc
$INCLUDE pasasm.inc
  
external function real1 (real): real; (* rounds double prec. to single *)

public function gen_cint (int_cons: int_type): rel_syllable;  forward;
$PAGE public, static variables

public var
  code_area: code_list;                 (* chain of code records for code area *)
  static_area: code_list;               (* chain for static area image *)
  cst_area: code_list;                  (* chain for constant pool area *)
  bptr_area: code_list;                 (* chain for byte pointers *)
  blt_area: code_list;                  (* chain for BLT control words *)
  btmp_area: code_list;                 (* chain for byte pointers to temps *)

  loc_code: code_address;                       (* location counter for code area *)
  loc_static: code_address;             (* location counter for static area *)
  loc_cst: code_address;                        (* location counter for constant area *)

type
    block_vector = array [index_range] of blk;

public var
    blk_list: ^ block_vector;
  def_lists: array[def_types] of def;   (* start of chains of internal definitions  *)

static var
  defnum: array[def_types] of id_range;         (* next number to assign to internal symbol
                                           of type constant_def, code_def *)
$PAGE runtime symbol names

type rts_name_array = array [rt_init..rt_last_used] of packed array[1..6] of char;
public var
  rts_name: rts_name_array :=
      ( 'INITP.',
        'PNTRY.',
        'PRTRN.',
        'EXIT. ',
        'UW.RTN',
        'UW.PRG',
        'II.EXP',
        'RI.EXP',
        'DI.EXP',
        'RR.EXP',
        'DD.EXP',
        'D.FLT',        'D.TRNC',       'D.RND',
        'R.RND2',       'D.RND2',
        'R.SQRT',       'D.SQRT',
        'R.LN  ',       'D.LN  ',
        'R.LOG ',       'D.LOG ',
        'R.EXP ',       'D.EXP ',
        'R.SIN ',       'D.SIN ',
        'R.ASIN',       'D.ASIN',
        'R.SINH',       'D.SINH',
        'R.COS ',       'D.COS ',
        'R.ACOS',       'D.ACOS',
        'R.COSH',       'D.COSH',
        'R.TAN ',       'D.TAN ',
        'R.TANH',       'D.TANH',
        'R.CTN ',       'D.CTN ',
        'R.ATN ',       'D.ATN ',
        'R.ATN2',       'D.ATN2',
        'RAND1.',
        'RAND0.',
        'NEW.  ',
        'DSPOS.',
        'EXTNT.',
        'SUBER.',
        'STRER.',
        'VALER.',
        'PTRER.',
        'FILER.',
        'FLDER.',
        'CMPER.',
        'ASSER.',
        'CASER.',
        'MP.FC',        'MPU.FC',       'MPL.FC',       (* string move operators *)
        'MP.FF',        'MPU.FF',       'MPL.FF',
        'MP.FX',        'MPU.FX',       'MPL.FX',
        'MP.XC',        'MPU.XC',       'MPL.XC',
        'MP.XF',        'MPU.XF',       'MPL.XF',
        'MP.XX',        'MPU.XX',       'MPL.XX',
        'MP.RC',        'MPU.RC',       'MPL.RC',
        'MP.RF',        'MPU.RF',       'MPL.RF',
        'MP.RX',        'MPU.RX',       'MPL.RX',
        'M.FC',         'MU.FC',        'ML.FC',
        'M.FF',         'MU.FF',        'ML.FF',
        'M.FX',         'MU.FX',        'ML.FX',
        'M.XC',         'MU.XC',        'ML.XC',
        'M.XF',         'MU.XF',        'ML.XF',
        'M.XX',         'MU.XX',        'ML.XX',
        'M.RC',         'MU.RC',        'ML.RC',
        'M.RF',         'MU.RF',        'ML.RF',
        'M.RX',         'MU.RX',        'ML.RX',
        'CP.FC',        'CPU.FC',       'CPL.FC', (* string concatenation *)
        'CP.FF',        'CPU.FF',       'CPL.FF',
        'CP.FX',        'CPU.FX',       'CPL.FX',
        'CP.XC',        'CPU.XC',       'CPL.XC',
        'CP.XF',        'CPU.XF',       'CPL.XF',
        'CP.XX',        'CPU.XX',       'CPL.XX',
        'C.FC',         'CU.FC',        'CL.FC',
        'C.FF',         'CU.FF',        'CL.FF',
        'C.FX',         'CU.FX',        'CL.FX',
        'C.XC',         'CU.XC',        'CL.XC',
        'C.XF',         'CU.XF',        'CL.XF',
        'C.XX',         'CU.XX',        'CL.XX',
        'CSP.CF',       'CSP.CX',                       (* string comparision routines *)
        'CSP.FC',       'CSP.FF',       'CSP.FX',
        'CSP.XC',       'CSP.XF',       'CSP.XX',
        'IX.CC',        'IX.CF',        'IX.CX',        (* "index" routines *)
        'IX.FC',        'IX.FF',        'IX.FX',
        'IX.XC',        'IX.XF',        'IX.XX',
        'SM.SS',                                (* set masks *)
        'SM.SD1',
        'SMV.LL',       'SMV.LO',       'SMV.LZ',       (* set moves *)
        'SUN.LL',       'SIN.LL',       'SDF.LL',       (* set operators *)
        'SLE.LL',       'SEQ.LL',       (* set comparisions *)
        'IN.VL',                (* in operators *)
        'SR.CL',        'SR.FL',        'SR.XL',        (* search operators *)
        'SRU.CL',       'SRU.FL',       'SRU.XL',
        'SR.CO',        'SR.FO',        'SR.XO',
        'SRU.CO',       'SRU.FO',       'SRU.XO',
        'SR.CD',        'SR.FD',        'SR.XD',
        'SRU.CD',       'SRU.FD',       'SRU.XD',
        'VF.CL',        'VF.FL',        'VF.XL',        (* verify operators *)
        'VFU.CL',       'VFU.FL',       'VFU.XL',
        'VF.CO',        'VF.FO',        'VF.XO',
        'VFU.CO',       'VFU.FO',       'VFU.XO',
        'VF.CD',        'VF.FD',        'VF.XD',
        'VFU.CD',       'VFU.FD',       'VFU.XD',
        'STMT. ',
        'OPEN.',        'REWRT.',       'RESET.',       (****  input / output  ****)
        'OPNTP.',       'OPNBN.',
        'GET.',
        'GETCH.',       'GETCR.',
        'PUT.',
        'PUTCH.',       'PUTCR.',
        'RDBIN.',       'WRBIN.',
        'RDIMG.',       'WRIMG.',
        'CLOSE.',       'CLOSD.',       'CLOSA.',
        'FILSZ.',
        'BREAK.',
        'EMPTX.',       'EMPTY.',
        'PUTPG.',
        'CLEAR.',
        'IOSTA.',
        'IOSTL.',
        'EXTST.',
        'RD.FDN',       'RD.FDR',
        'WR.FDN',       'WR.FDR',
        'WR.SVN',       'WR.SVR',
        'RD.SSN',       'RD.SSR',
        'WR.SSN',       'WR.SSR',
        'RD.LNN',       'RD.LNR',
        'WR.LNN',       'WR.LNR',
        'WR.DNN',       'WR.DNR',
        'SEEK.',
        'INT.R',        'INT.W',
        'REAL.R',       'REAL.W',
        'XSTR.R',       'XSTR.W',
        'FSTR.R',       'FSTR.W',
        'CSTR.R',       'CSTR.W',
        'STRV.R',
        'BOOL.W',

        'DATE',
        'DR.RND',
	'INS.SM',
        '%LAST%'  );

public var
  rw_request: array [rt_int_read..rt_bool_write] of boolean;
$PAGE gen_init, gen_term
(* GEN INIT initializes data used by this package. *)

public procedure gen_init;

 var
   d: def_types;
   b: blk;
   rt: rt_symbol;

 begin
  code_area.first := nil;    code_area.last := nil;
  static_area.first := nil;  static_area.last := nil;
  cst_area.first := nil;     cst_area.last := nil;
  blt_area.first := nil;     blt_area.last := nil;
  bptr_area.first := nil;    bptr_area.last := nil;
  btmp_area.first := nil;    btmp_area.last := nil;
  loc_code := 400000B;                  (* standard high segment base *)
  loc_static := 0;
  loc_cst := 0;                         (* latter merged into code area *)
  for d := minimum (def_types) to maximum (def_types) do begin
    def_lists[d] := nil;
    defnum [d] := 1;
  end;
  new (blk_list: blk_number);
  b := root_block;
  while b <> nil do begin
    blk_list^ [b^.number] := b;
    b := b^.downward_call_thread;
  end;
  for rt := rt_int_read to rt_bool_write do
    rw_request [rt] := false;
 end;



(* GEN TERM cleans storage used by the package. *)

public procedure gen_term;
 begin
  dispose (blk_list);
 end;
$PAGE word_size
(* WORD SIZE returns the unit size of storage required for an expression.  This
   is used to compute the size of values to be placed in the parameter list;
   thus, it will only return sizes of 1 or 2. *)

public function word_size (exp: expr): unit_range;
 begin
  case exp^.desc.kind of
    ints, bools, chars, scalars, pointers, files:
      word_size := 1;
    reals:
      if exp^.desc.precision > srealprec
        then word_size := 2
        else word_size := 1;
     sets:
       word_size := (exp^.desc.set_length + 35) div 36;
     others:
       word_size := (exp^.desc.base^.size + 35) div 36
  end;
 end;
$PAGE make_def
(* MAKE DEF returns an all new definition record of type "dtype" and with an
   unique identifying number.  A pointer to the record is returned. *)

public function make_def (dtype: def_types): def;
 begin
  new (make_def);
  with make_def^ do begin
    deftype := dtype;                   (* set true type *)
    next := def_lists [dtype];          (* add to list of definitions *)
    def_lists[dtype] := make_def;
    rbacklink := 0;                     (* no references yet emitted *)
    lbacklink := 0;
    defined := false; (* address not yet determined *)
    relocatable := false;
    fixup_required := false;
    first_offset := nil;
    if dtype <> extern_def then begin
      addr := 0;
      defnumber := defnum [dtype];      (* assign and advance id counter *)
      defnum [dtype] := defnum[dtype] + 1;
    end;
  end;
 end;
$PAGE get_def
(* GET DEF returns the internal definition record corresponding to the "dtype" -
   "defid" pair.  If such a record cannot be found, it is created.  This is used
   to create/lookup internal definitions for subroutine identifiers, user
   declared symbols, and local label nodes whose numbering is taken from the
   numbering made in the symbol table or if.  A pointer to the definition
   record is returned. *)

public function get_def (dtype: def_types; defid: id_range): def;
 var d: def;
 begin
  d := def_lists [dtype];               (* search for matching symbol *)
  while d <> nil do begin
    if d^.defnumber = defid then begin  (* ---> return if found *)
      get_def := d;
      return
    end;
    d := d^.next;
  end;

  get_def := make_def (dtype);          (* not found - allocate a new definition *)
  get_def^.defnumber := defid;          (* replace number assigned in make_def *)
 end;
$PAGE get_extern
(* GET EXTERN returns the internal definition record corresponding to the
   external symbol with a specified radix-50 name.  If such a record cannot
   be found, it is created. *)

public function get_extern (name: pdp10word): def;
 var d: def;
 begin
  d := def_lists [extern_def];
  while d <> nil do begin
    if d^.ext_name = name then begin (* ---> return if found *)
      get_extern := d;
      return
    end;
    d := d^.next;
  end;

  get_extern := make_def (extern_def); (* not found - allocate a new definition *)
  get_extern^.ext_name := name;
 end;
$PAGE def_value
(* DEF VALUE defines the address of a definition record. *)

public procedure def_value (d: def; value: code_address; relocatable: boolean);
 var d1: def;
     addr_val: int_type;
 begin
  d^.defined := true;
  d^.relocatable := relocatable;
  d^.addr := value;
  if d^.addr >= 400000b
    then addr_val := d^.addr - 400000b
    else addr_val := d^.addr;
  d1 := d^.first_offset;
  while d1 <> nil do begin
    d1^.defined := true;
    d1^.relocatable := relocatable;
    d1^.fixup_required := relocatable andif (addr_val + d1^.offset < 0);
    d1 := d1^.next;
  end;
 end;
$PAGE del_def_list
(* DEL DEF LIST deletes a list of definitions and resets the id number counter
   for a particular type of definition ("dtype"). *)

public procedure del_def_list (dtype: def_types);
 var d, d1, next: def;
 begin
  d := def_lists [dtype];
  while d <> nil do begin
    next := d^.next;
    d1 := d^.first_offset;
    dispose (d);
    d := next;
    while d1 <> nil do begin
      next := d1^.next;
      dispose (d1);
      d1 := next;
    end;
  end;
  def_lists [dtype] := nil;
  defnum [dtype] := 1;
 end;
$PAGE get_offset
(* GET OFFSET returns an internal definition offset record for the definition
   specified by "intdef" corresponding to offset "off".  If there is already
   a record for the definition/offset pair, it is returned;  otherwise, a new
   record is created. *)

public function get_offset (intdef: def; off: int_type): def;
 var od: def;
     svalue: int_type;
 begin
  if off = 0 then begin                 (* offset 0 references original defintion *)
    get_offset := intdef;
    return;
  end;

  od := intdef^.first_offset;   (* search for same offset def *)
  while od <> nil do begin
    if od^.offset = off then begin      (* ---> return if offset found *)
      get_offset := od;
      return;
    end;
    od := od^.next;
  end;

  new (od, offset_def);                 (* not found, create new offset record *)
  with od^ do begin
    next := intdef^.first_offset;       (* chain to target definition *)
    intdef^.first_offset := od;
    rbacklink := 0;                     (* no refs yet emitted *)
    lbacklink := 0;
    defined := intdef^.defined;
    relocatable := intdef^.relocatable;
    if defined andif relocatable then begin
      svalue := intdef^.addr + off;
      fixup_required := (svalue < 0) or ( (svalue < 400000b) <> (intdef^.addr < 400000b) );
    end
    else
      fixup_required := false;
    reldef := intdef;                   (* offset is relative to this definition *)
    offset := off;
  end;
  get_offset := od;
 end;
$PAGE relrt, reldef
(* These two routines construct relocation syllables for runtime symbols,  and internal definitions.
   They need not be the only routines to generate such records; they are used for convenience when
   the relocation must be explicitly constructed. *)

public function relrt (rtsym: rt_symbol): rel_syllable;
 begin
  relrt.kind := runtime_sc;
  relrt.relrtsym := rtsym;
 end;


public function reldef (intdef: def): rel_syllable;
 begin
  reldef.kind := def_sc;
  reldef.reldef := intdef;
 end;
$PAGE gen_emit
(* GEN EMIT chains an arbitrary code record ("cr") onto the end of the area
   denoted by "area list". *)

public procedure gen_emit (var area_list: code_list; cr: code);
 begin
  if area_list.first = nil
    then area_list.first := cr
    else area_list.last^.next := cr;
  area_list.last := cr;
  cr^.next := nil;
 end;
$PAGE gen_origin
(* GEN ORIGIN generates an origin record with value "loc" and emits it in the
   area "arealist". *)

public procedure gen_origin (var arealist: code_list; loc: unit_range);
 var cr: code;
 begin
  new (cr, origin);
  cr^.location := loc;
  gen_emit (arealist, cr);
 end;
$PAGE mark_def
(* MARK DEF emits a defmark code record which defines the location for an internal
   definition denoted by "intdef".  The record is emitted in the are given by
   "area_list" . *)

public procedure mark_def (var area_list: code_list; intdef: def);
 var cr: code;
 begin
  new (cr, defmark);
  cr^.defname := intdef;
  gen_emit (area_list, cr);
 end;



$PAGE gen_source, gen_cmt
(* GEN SOURCE generates a code 'source' record and emits it in the code area. *)

public procedure gen_source (srcid: source_id);
 var cr: code;
 begin
  new (cr, source);
  cr^.stmtid := srcid;
  gen_emit (code_area, cr);
 end;



(* GEN COMMENT generates a code 'comment' record and emits in the designated
   area. *)

public procedure gen_cmt (var area_list: code_list; text: string);
 var cr: code;
 begin
  new (cr, comment: length (text));
  with cr^ do begin
    ctext_len := length (text);
    ctext [1:length (text)] := text;
  end;
  gen_emit (area_list, cr);
 end;
$PAGE halfword
(* HALFWORD returns the low order 18 bits of a signed integer value as a positive
   value in the range 0..777777B. *)

public function halfword (i: int_type): unit_range;
 begin
  if i >= 0
    then halfword := i mod 1000000B
    else halfword := 1000000B + (i mod 1000000B);
 end;
$PAGE gen, genind
(* GEN and GENIND construct instruction form code records given values for the
   fields of the instruction and the relocation syllables.  In "gen", indirection
   is turned off; in "genind", it is turned off.  The generated instruction is
   emitted in the code area. *)

public procedure gen
  ( opc: opc_range; acc, index: registers; offset: int_type; rel: rel_syllable );
 var cr: code;
 begin
  new (cr, instruction);
  with cr^ do begin
    inst.opcode := opc;
    inst.acc := acc;
    inst.index := index;
    inst.indirect := false;
    inst.offset := halfword (offset);
    reloc := rel;
  end;
  gen_emit (code_area, cr);
 end;


public procedure genind
  ( opc: opc_range; acc, index: registers; offset: int_type; rel: rel_syllable );
 var cr: code;
 begin
  new (cr, instruction);
  with cr^ do begin
    inst.opcode := opc;
    inst.acc := acc;
    inst.index := index;
    inst.indirect := true;
    inst.offset := halfword (offset);
    reloc := rel;
  end;
  gen_emit (code_area, cr);
 end;
$PAGE gen_rr
(* GEN RR generates a register to register instruction given the opcode, acc and
   operand fields.  The instruction code record generated is emitted in the code
   area. *)

public procedure gen_rr ( opc: opc_range; acc: registers; operand: registers );
 var cr: code;
 begin
  new (cr, instruction);
  with cr^ do begin
    inst.opcode := opc;
    inst.acc := acc;
    inst.index := 0;
    inst.indirect := false;
    inst.offset := operand;
    reloc.kind := register_sc;
  end;
  gen_emit (code_area, cr);
 end;
$PAGE gen_rx
(* GEN RX generates a register to register instruction given the opcode, acc and
   index field.  The instruction is assumed to be of the form opc acc,0(index)
   where the opcode is an immediate operation.  The instruction code record
   generated is emitted in the code area. *)

public procedure gen_rx ( opc: opc_range; registers; operand: registers );
 var cr: code;
 begin
  new (cr, instruction);
  with cr^ do begin
    inst.opcode := opc;
    inst.acc := acc;
    inst.index := operand;
    inst.indirect := false;
    inst.offset := 0;
    reloc.kind := register_sc;
  end;
  gen_emit (code_area, cr);
 end;
$PAGE gen_ri
(* GEN RI generates a register-immediate instruction given the opcode, acc and
   immediate offset.  The instruction code record generated is put in the code area. *)

public procedure gen_ri ( opc: opc_range; acc: registers; operand: int_type );
 var cr: code;
 begin
  new (cr, instruction);
  with cr^ do begin
    inst.opcode := opc;
    inst.acc := acc;
    inst.index := 0;
    inst.indirect := false;
    inst.offset := halfword (operand);
    reloc.kind := absolute_sc;
  end;
  gen_emit (code_area, cr);
 end;
$PAGE gen_rm
(* GEN RM generates a register-memory instruction given the opcode, acc and an
   addr_desc addressing the desired memory location.  The instruction code record
   generated is emitted in the code area. *)

public procedure gen_rm ( opc: opc_range; acc: registers; operand: addr_desc );
  
 var cr: code;
     immed_val: code_address;
     temp_operand: addr_desc;
  
 begin
  temp_operand := operand;
  new (cr, instruction);
  with cr^ do begin
    if temp_operand.immediate and temp_operand.indirect then begin
      temp_operand.immediate := false;
      temp_operand.indirect := false
    end;
    if temp_operand.immediate then begin
      if (cam <= opc) and (opc <= cam+gtc) then
        inst.opcode := (opc - cam) + cai
      else if (skip <= opc) and (opc <= skip+gtc) then begin
        immed_val := temp_operand.offset;
        temp_operand := absolute_reference;
        temp_operand.reloc := gen_cint (immed_val);
        inst.opcode := opc;
      end
      else
        inst.opcode := opc + imdc;
    end
    else if temp_operand.indirect andif
               ( (opc = movei) or (opc = hrli) ) then begin
      inst.opcode := opc - imdc;
      temp_operand.indirect := false
    end
    else inst.opcode := opc;
    inst.acc := acc;
    inst.index := temp_operand.index;
    inst.indirect := temp_operand.indirect;
    inst.offset := temp_operand.offset;
    reloc := temp_operand.reloc;
  end;
  gen_emit (code_area, cr)
 end;
$PAGE gen_rt
(* GEN RT generates an instruction whose offset is relocated by the value of
   a rumtime symbol.  The instruction code record is emitted in the code area. *)

public procedure gen_rt (opc: opc_range; acc: registers; rtsym: rt_symbol);
 var cr: code;
 begin
  new (cr, instruction);
  with cr^ do begin
    inst.opcode := opc;
    inst.acc := acc;
    inst.index := 0;
    inst.indirect := false;
    inst.offset := 0;
    reloc.kind := runtime_sc;
    reloc.relrtsym := rtsym;
  end;
  gen_emit (code_area, cr);
 end;
$PAGE gen_rl
(* GEN RL generates an instruction referencing a label_node.  The instruction code
   record is emitted in the code area. *)

public procedure gen_rl (opc: opc_range; acc: registers; lab: tuple);
 var cr: code;
 begin
  new (cr, instruction);
  with cr^ do begin
    inst.opcode := opc;
    inst.acc := acc;
    inst.index := 0;
    inst.indirect := false;
    inst.offset := 0;
    reloc.kind := def_sc;
    reloc.reldef := get_def (label_def, lab^.block_order_no);
  end;
  gen_emit (code_area, cr);
 end;
$PAGE gen_word
(* GEN WORD generates a code record for a fullword, stringword or setword given
   the the kind ("wkind"), the "word" itself, and the area in which to emit it
   ("area_list"). *)

public procedure gen_word (var area_list: code_list; word: pdp10word; wkind: code_types);
 var cr: code;
 begin
  new (cr, fullword);
  cr^.kind := wkind;
  cr^.fwd := word;
  gen_emit (area_list, cr);
 end;
$PAGE gen_string

(* GEN_STRING generates a string in the code area for trace, file, and page
   blocks.      *)

public procedure gen_string (str: string);

var ix: int_type;
    word: pdp10word;

begin
  word.value := length (str);
  gen_word (code_area, word, fullword);
  for ix := 1 to (length (str) + 4) div 5 do begin
    word.str[1:5] := substr (str, 5 * (ix - 1) + 1,
                min (5, length (str) - 5 * (ix - 1)));
    gen_word (code_area, word, stringword);
  end;
end (* gen_string *);
$PAGE rel_match

(* REL MATCH compares two relocation syllables, returning TRUE if they define
   the same relocation. *)

function rel_match (r1, r2: rel_syllable): boolean;

 begin
  if r1.kind = r2.kind then
    case r1.kind of
      local_sc,
      parameter_sc,
      static_sc,
      external_sc:
        rel_match := (r1.relsym = r2.relsym);
      runtime_sc:
        rel_match := (r1.relrtsym = r2.relrtsym);
      temp_sc:
        rel_match := true;
      def_sc:
        rel_match := (r1.reldef = r2.reldef);
      others:
        rel_match := true
    end
  else
    rel_match := false;
 end;
$PAGE gen_blt
(* GEN BLT generates a constant word whose left and right halfwords are
   constants or addresses with relocation. The halfwords and their
   respective relocations are passed as parameters.  The resulting constant is placed
   in the blt area, and a rel_syllable denoting the address of the generated
   constant is returned. *)


public function gen_blt (source_offset: code_address;
                         source_reloc: rel_syllable;
                         dest_offset: code_address;
                         dest_reloc: rel_syllable)
                            : rel_syllable;

 var
   xwd: pdp10word;
   bscan, blast: code;
   cr: code;
   bltdef: def;

 begin
  xwd.lh := source_offset;
  xwd.rh := dest_offset;

  bscan := blt_area.first;
  while (bscan <> nil) andif not
        ( (bscan^.kind = halfwords) andif (bscan^.xwd = xwd) andif
          rel_match (bscan^.lreloc, source_reloc) andif
          rel_match (bscan^.rreloc, dest_reloc) ) do begin
    blast := bscan;
    bscan := bscan^.next;
  end;

  if bscan <> nil then
    bltdef := blast^.defname

  else begin
    bltdef := make_def (constant_def);
    mark_def (blt_area, bltdef);
    new (cr, halfwords);
    cr^.xwd := xwd;
    cr^.lreloc := source_reloc;
    cr^.rreloc := dest_reloc;
    gen_emit (blt_area, cr);
  end;

  gen_blt := reldef (bltdef);
 end;
$PAGE gen_bptr
(* GEN BPTR generates a constant byte pointer referencing a slice of a particular
   memory location.  The location and the byte parameters are given by "mem".  The
   byte pointer is placed in the byte pointer area, and a rel_syllable denoting
   the address of the byte pointer is returned. *)

public function gen_bptr (mem: addr_desc): rel_syllable;

 var
   bpdef: def;
   bptr: pdp10word;
   reloc: rel_syllable;
   bscan, blast: code;
   cr: code;

 begin
  with bptr do begin
    p := 36 - mem.slice_offset - mem.slice_size;
    s := mem.slice_size;
    bpindirect := mem.indirect;
    bpindex := mem.index;
    bpoffset := mem.offset;
  end;
  reloc := mem.reloc;

  if reloc.kind in [parameter_sc, local_sc] then begin
    if bptr.bpoffset <= 377777b
      then bptr.bpoffset := halfword (bptr.bpoffset + reloc.relsym^.item_addr)
      else bptr.bpoffset := halfword (bptr.bpoffset + reloc.relsym^.item_addr - 1000000b);
    reloc.kind := absolute_sc;
  end;

  bscan := bptr_area.first;
  while (bscan <> nil) andif not
        ( (bscan^.kind = bytepointer) andif (bscan^.bptr = bptr) andif
          rel_match (bscan^.bpreloc, reloc) ) do begin
    blast := bscan;
    bscan := bscan^.next;
  end;

  if bscan <> nil then
    gen_bptr := reldef (blast^.defname)
  else begin
    bpdef := make_def (constant_def);
    mark_def (bptr_area, bpdef);
    new (cr, bytepointer);
    cr^.bptr := bptr;
    cr^.bpreloc := reloc;
    gen_emit (bptr_area, cr);
    gen_bptr := reldef (bpdef);
  end;
 end;
$PAGE gen_xwd
(* GEN XWD generates a pair of halfword values in a word, given the values and
   relocation for each half word.  The word is emitted in the code area. *)

public procedure gen_xwd
        ( lhv: int_type; lhrel: rel_syllable; rhv: int_type; rhrel: rel_syllable);
 var cr: code;
 begin
  new (cr, halfwords);
  with cr^ do begin
    xwd.lh := halfword (lhv);
    xwd.rh := halfword (rhv);
    lreloc := lhrel;
    rreloc := rhrel;
  end;
  gen_emit (code_area, cr);
 end;
$PAGE gen_vnode
(* GEN VNODE allocates one or more FULLWORDs, STRINGWORDs or
   SETWORDs in the specified area to represent the specified
   value. *)

public procedure gen_vnode(var area_list: code_list; value: val_ptr);
 
$PAGE store_word - in gen_vnode
(* CUR_CODE and CUR_OFFSET are vars used to keep track of
   which code record is currently being initialized by GEN_VAL
   or GEN_VNODE.  They are used solely by STORE_WORD except for
   initialization by GEN_VNODE.  *)

var
  cur_code: code;                               (* current code record for a constant *)
  cur_offset: unit_range;                               (* offset of CUR_CODE within constant *)

(* STORE_WORD packs the rightmost WIDTH bits of TEN_WORD into the appropriate
   code word corresponding to the current value being generated
   beginning at offset (OFFSET mod BITS_PER_UNIT).  STORE_WORD is called
   from GEN_COMPONENT.  Accross successive calls OFFSET must be
   non-decreasing. *)

procedure store_word (ten_word: pdp10word;
                 offset: bit_range;
                 width: elem_sizes;
                 code_kind: code_types);

var
  code_num: unit_range;                         (* offset of code record within constant
                                                   which will be stored into *)
  word_offset: bit_offset;                      (* bit offset within word *)
  temp_word: pdp10word;                         (* avoids passing heap address to Macro routine *)

begin
  code_num := offset div bits_per_unit;
  while code_num > cur_offset do begin
    cur_offset := cur_offset + 1;
    cur_code := cur_code^.next
  end;
  with cur_code^ do begin
    word_offset := offset mod bits_per_unit;
    if (word_offset = 0) andif (width = bits_per_unit) then begin
      fwd := ten_word
    end
    else begin
      temp_word := fwd;
      stuff_byte (ten_word, temp_word, word_offset, width);
      fwd := temp_word
    end;
    kind := code_kind
  end
end;
$PAGE find_variant - in gen_vnode
(* FIND VARIANT is given a type node of kind RECORDS or VARIANTS and
   an integer representing a tag field value.  It returns the type node
   of the variant corresponding to the tag value.  NIL is returned if no
   such variant is found.  The record or variant whose type is passed
   in must contain a variant part. *)

function find_variant (recvar: typ; tag_val: int_type): typ;

var
  cur_variant: typ;

 begin
  find_variant := nil;                          (* updated if variant found or
                                                   if others case found *)
  cur_variant := recvar^.variant_tag^.first_variant;
  while cur_variant <> nil do begin
    with cur_variant^ do begin
      if others_var 
        then find_variant := cur_variant
      else if (minlab <= tag_val) and (tag_val <= maxlab)
        then begin
          find_variant := cur_variant;
          return                                (* <--- exit here if specific variant found *)
        end;
      cur_variant := next_variant
    end (* with *);
  end (* while *);
 end;
$PAGE gen_component - in gen_vnode
(* GEN COMPONENT initializes code words to correspond to the value
   represented by VALUE.  BASE_OFFSET contains the bit offset, within the
   constant currently being generated, at which the value should be
   placed.  BASE_WIDTH is the width in bits of the constant represented
   by VALUE. *)

procedure gen_component (value: val_ptr; base_offset: bit_range;
                         base_width: bit_range);

 var
   ten_word: pdp10word;                         (* single word buffer *)
   real_words: real_rec;                        (* used to access reals *)
   words_required: unit_range;          (* words needed to contain an object *)
   i, j: int_type;                              (* for loop indices *)
   start: char_range;                           (* start index of substring *)
   len: 0..chars_per_unit;                      (* length of substring *)
   jus_start: 1..chars_per_unit;                (* for right justified word packing *)
   max_bit: bit_offset;                         (* high bit used in set packing *)
   field_sym: sym;                              (* current field's sym *)
   cur_recvar: typ;                             (* record/variant containing FIELD_SYM *)
   is_tag: boolean;                             (* true => FIELD_SYM is tag *)
   field_number: unit_range;                    (* number of current field *)
   num_elems: int_type;                         (* number of elements of array *)
   elems_per_unit: elem_sizes;                  (* array elements per unit *)
   bits_per_elem: bit_range;                    (* bits required per array element *)
   word_offset: unit_range;                     (* word offset of array element within array *)
   bits_offset: bit_offset;                     (* bit offset of start of an array element*)
   code_num: unit_range;                        (* base code record for field or element *)
   offset: bit_range;                           (* offset within constant of component of VALUE *)
   width: bit_range;                            (* width of a component of VALUE *)
$PAGE gen_subval - in gen_component
(* GEN SUBVAL generates a value corresponding to an array element
   or a record field.  Scalar and pointer constants are generated
   immediately; other VALUE_KINDs cause GEN_COMPONENT to be called
   recursively.  OFFSET is the base offset of the value within the
   current constant in bits and WIDTH is the value's width in bits. *)

procedure gen_subval ( val_recd: val; offset: bit_range; width: bit_range );

begin
  with val_recd do begin
    case kind of

      scalar_cst:
        begin
          ten_word.value := ival;
          store_word (ten_word, offset, width, fullword)
        end;

      real_cst,
      string_cst,
      set_cst,
      array_cst,
      record_cst:
        begin
          gen_component( valp, offset, width )
        end;
 
      ptr_cst:
        begin
          ten_word.lh := 0;
          ten_word.rh := int_nil;
          store_word (ten_word, offset, width, fullword)
        end;

      subr_cst:                                 (* illegal !!! *)

    end (* case *)
  end (* with *)
end; (* procedure gen_subval *)
$PAGE gen_component - body

 begin
  with value^ do begin
    case kind of

      scalar_cst:
        begin
          ten_word.value := scalar_val;
          store_word (ten_word, base_offset, base_width, fullword)
        end;

      real_cst:
        begin
          if real_prec <= srealprec then
            real_words.rvalue := real1 (real_val) (* round *)
          else
            real_words.rvalue := real_val;
          ten_word.value := real_words.ivalue_high;
          store_word (ten_word, base_offset, bits_per_unit, fullword);
          if real_prec > srealprec then begin
            ten_word.value := real_words.ivalue_low;
            offset := base_offset + bits_per_unit;
            store_word (ten_word, offset, bits_per_unit, fullword)
          end
        end;

      string_cst:
        begin
          offset := base_offset;
          if str_varying_ref then begin
            width := bits_per_unit;
            ten_word.value := str_len;
            store_word (ten_word, offset, bits_per_unit, fullword);
            offset := offset + bits_per_unit
          end;
          words_required := (str_len + chars_per_unit - 1) div chars_per_unit;
          for i := 1 to words_required do begin
            start := (i - 1) * chars_per_unit + 1;
            len := min(chars_per_unit, str_len - start + 1);
            jus_start := chars_per_unit - len + 1;
            width := len * (bits_per_unit div chars_per_unit) + 1;
            ten_word.value := 0;
            ten_word.str[jus_start:len] := substr(str_val, start, len);
            store_word (ten_word, offset, width, stringword);
            offset := offset + bits_per_unit
          end
        end;

      set_cst:
        begin
          offset := base_offset;
          words_required := (set_len + bits_per_unit - 1) div bits_per_unit;
          for i := 1 to words_required do begin
            max_bit := min (bits_per_unit - 1, 
              set_len - ( (i - 1) * bits_per_unit) - 1);
            for j := 0 to max_bit do
              ten_word.bits[bits_per_unit - max_bit - 1 + j] :=
                set_val[(i - 1) * bits_per_unit + j];
            width := max_bit + 1;
            store_word (ten_word, offset, width, setword);
            offset := offset + width
          end;
        end;

      array_cst:
        begin
          with struc_type^ do begin
            num_elems := index_type^.maxval - index_type^.minval + 1;
            if element_size <= bits_per_unit 
              then elems_per_unit := bits_per_unit div element_size
              else bits_per_elem :=
                ( (element_size + bits_per_unit - 1) div bits_per_unit ) *
                bits_per_unit;

            width := element_size;
            for i := 1 to min (n_elems, num_elems) do begin
              if element_size <= bits_per_unit then begin
                word_offset := (i - 1) div elems_per_unit;
                bits_offset := ( (i-1) mod elems_per_unit ) * element_size;
                offset :=
                  base_offset + (word_offset * bits_per_unit) + bits_offset
              end
              else begin
                offset := base_offset + (bits_per_elem * (i - 1))
              end;
              gen_subval (elem_vals[i], offset, width)
            end (* for *)
          end (* with *)
        end (* array_cst case *);

      record_cst:
        begin
          field_sym := struc_type^.field_list;
          cur_recvar := struc_type;
          field_number := 0;
          offset := base_offset;

          repeat                                (* traverse appropriate sym records *)

            (* if starting new variant then find correct variant type record
               and its first field *)
  
            while ( (cur_recvar^.field_list = nil) andif
                    (cur_recvar^.variant_tag <> nil) ) orif
                  ( (field_sym <> nil) andif
                    (cur_recvar <> field_sym^.fld_variant) ) do begin
              field_number := field_number + 1;
              if field_number > n_elems then return;
              cur_recvar := find_variant (cur_recvar, elem_vals[field_number].ival);
              if cur_recvar <> nil 
                then field_sym := cur_recvar^.field_list
                else return;
              if field_sym <> nil then cur_recvar := field_sym^.fld_variant
            end;

            (* is this a tag field? *)
  
            is_tag := (cur_recvar^.variant_tag <> nil) andif
                      (field_sym = cur_recvar^.variant_tag^.tag_field);

            (* process the current field *)

            field_number := field_number + 1;
            if field_number > n_elems then return;
            if field_sym <> nil then begin
              offset := base_offset + field_sym^.fld_offset;
              width := field_sym^.fld_width;
              gen_subval(value^.elem_vals[field_number], offset, width)
            end;
            if is_tag then field_number := field_number - 1;

            (* get next field and check whether or not we are done *)

            if field_sym <> nil then begin
              field_sym := field_sym^.next;
              if (field_sym^.fld_variant <> cur_recvar) andif
                 (cur_recvar^.variant_tag = nil)
                then field_sym := nil
            end
          until field_sym = nil
        end (* record_cst case *)

      end (* case *)
    end (* with *)
end;
$PAGE bits_required
(* BITS REQUIRED returns the size in bits of the constant described by VALUE.  *)

function bits_required (value: val_ptr): bit_range;

begin
 with value^ do begin
  case kind of
    
    scalar_cst: bits_required := bits_per_unit;

    real_cst:
      begin
        if real_prec > srealprec 
          then bits_required := 2 * bits_per_unit
          else bits_required := bits_per_unit
      end;

    string_cst:
      begin
        bits_required := str_len * (bits_per_unit div chars_per_unit) +
                         (str_len div chars_per_unit);
        if str_varying_ref then bits_required := bits_required + bits_per_unit
      end;

    set_cst:
      begin
        bits_required := set_len
      end;

    array_cst,
    record_cst:
      begin
        bits_required := struc_type^.size
      end

  end (* case *)
 end (* with *)
end;
$PAGE init_required
(* INIT REQUIRED generates NUM_REQUIRED fullwords in area AREA_LIST.
   It returns a pointer to the first code record generated. 
   The fullwords generated are initialized to zero. *)

function init_required (var area_list: code_list; 
  num_required: unit_range): code;

var
  ten_word: pdp10word;
  i: unit_range;

begin
  ten_word.value := 0;
  gen_word (area_list, ten_word, fullword);
  init_required := area_list.last;
  for i := 2 to num_required do begin
    gen_word(area_list, ten_word, fullword)
  end
end;
$PAGE gen_vnode - main routine
(* GEN_VNODE main routine *)

const
  init_offset: bit_range := 0;

var
  recds_required: unit_range;
  width: bit_range;

 begin
  width := bits_required (value);
  recds_required := (width + bits_per_unit - 1) div bits_per_unit;
  cur_code := init_required( area_list, recds_required);
  cur_offset := 0;

  gen_component ( value, init_offset, width)
 end;
$PAGE gen_val
(* GEN VAL allocates one or more FULLWORDs, STRINGWORDs or SETWORDs in 
   the specified area to represent the specified values.  It is identical 
   to GEN_VNODE except that it takes a VAL record as a
   parameter rather than a VAL_PTR. *)

public procedure gen_val (var area_list: code_list; value: val);

 var
   ten_word: pdp10word;                         (* temporary buffer *)

 begin
  with value do
    case kind of

      scalar_cst:
        begin
          ten_word.value := ival;
          gen_word (area_list, ten_word, fullword)
        end;

      real_cst,
      string_cst,
      set_cst,
      array_cst,
      record_cst:
        begin
          gen_vnode (area_list, valp)
        end;

      ptr_cst:
        begin
          ten_word.lh := 0;
          ten_word.rh := int_nil;
          gen_word (area_list, ten_word, fullword)
        end;

      others:                                           (* illegal !!! *)
        assert (false)

    end (*case *)
 end;
$PAGE gen_cnode
(* GEN CNODE allocates the specified value in the constant area, and
   returns a DEF_SC relocation syllable for a definition node referring
   to the first word of the constant *)

public function gen_cnode (value: val_ptr): rel_syllable;

 var
   cons_def: def;

 begin
  if value^.def_addr = nil then begin
    cons_def := make_def (constant_def);
    mark_def (cst_area, cons_def);
    gen_vnode (cst_area, value);
    gen_cnode := reldef (cons_def);
    value^.def_addr := valdef (cons_def)
  end 
  else
    gen_cnode := reldef (defval (value^.def_addr))
 end;

$PAGE gen_cval
(* GEN CVAL allocates the specified value in the constant area, and
   returns a DEF_SC relocation syllable for a definition node referring
   to the first word of the constant.  This routine is identical to
   GEN_CNODE except that it takes a VAL record as a parameter rather
   than a VAL_PTR. *)

public function gen_cval (value: val): rel_syllable;

 type
   vkind_set = set of value_kind;

 const
   has_ptr: vkind_set = [real_cst, string_cst, set_cst, array_cst,
      record_cst];

 var
   cons_def: def;
   prev_emitted: boolean;

 begin
  with value do begin
    prev_emitted := ((kind in has_ptr) andif (valp^.def_addr <> nil)) orif (kind = alloc_cst);
    if not prev_emitted then begin
      cons_def := make_def (constant_def);
      mark_def (cst_area, cons_def);
      gen_val (cst_area, value);
      gen_cval := reldef (cons_def);
      if kind in has_ptr then valp^.def_addr := valdef(cons_def)
    end
    else
      if kind <> alloc_cst then
        gen_cval := reldef (defval (valp^.def_addr))
      else
        gen_cval := reldef (defval (defp))
  end (* with *)
 end;

$PAGE gen_cword
(* GEN CWORD allocates the specified (via parameter CONS_WORD) word in
   the constant area.  The type of the code record emitted is specified
   by parameter CONS_CODE and must be either FULLWORD, STRINGWORD or
   SETWORD. A DEFINITION record referring to the word is allocated
   also and a DEF_SC relocation syllable for the definition is the
   return value of the function. *)

public function gen_cword (cons_word: pdp10word; 
  cons_code: code_types): rel_syllable;

 var
   cons_def: def;

 begin
  cons_def := make_def (constant_def);
  mark_def (cst_area, cons_def);
  gen_word (cst_area, cons_word, cons_code);
  gen_cword := reldef (cons_def)
 end;

$PAGE gen_cint
(* GEN CINT allocates a FULLWORD code record in the constant area to hold
   the specified integer.  A DEFINITION record referring to the word is 
   also allocated and a DEF_SC relocation syllable for the definition
   is the return value of the function. *)

public function gen_cint (* int_cons: int_type): rel_syllable *);

 var
   ten_word: pdp10word;

 begin
  ten_word.value := int_cons;
  gen_cint := gen_cword(ten_word, fullword)
 end;
$PAGE gen_cst
(* GEN CST allocates a scalar value in the constant area and returns
   an address descriptor for the location in the constant area. *)

public function gen_cst ( value: int_type ): addr_desc;

begin
  gen_cst := absolute_reference;
  gen_cst.reloc := gen_cint ( value );
end;
$PAGE wr_code
(* WR CODE writes code records to the rel file, deletes the records, defines
   symbols, and updates the location counter. *)

public procedure wr_code (var area_list: code_list; var ic: unit_range);

var
    cr, next_cr: code;
    init_ic: code_address;

begin
  init_ic := ic;
  cr := area_list.first;
  while cr <> nil do begin
    with cr^ do begin
      case kind of
        origin:
          ic := location;
        instruction..halfwords:
          ic := ic + 1;
        defmark:
          def_value (defname, ic, true);
        others:
          (* no action *)
      end (* case kind *);
      cr := next;
    end;
  end (* while cr <> nil *);

  ic := init_ic;
  cr := area_list.first;
  while cr <> nil do begin
    with cr^ do begin
      case kind of
        origin:
          ic := location;
        instruction:
          rel_code (inst, ic, none, reloc);
        bytepointer:
          rel_code (bptr, ic, none, bpreloc);
        fullword,
        stringword,
        setword:
          rel_code (fwd, ic, none, none);
        halfwords:
          rel_code (xwd, ic, lreloc, rreloc);
        source:
          if (map_opt in cur_block^.semantic_options) then
            map_write (stmtid, ic);
        others:
          (* no action *)
      end (* case kind *);
      next_cr := next;
      dispose (cr);
      cr := next_cr;
    end;
  end (* while cr <> nil *);
  area_list.first := nil;
  area_list.last := nil;
end.
   M@ y/