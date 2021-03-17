$title VAXUTL - Vax cross-compiler code generation utilities
module aevutl;
$PAGE includes
$system pascal.inc
$system pasist.inc
$system paspt.typ
$system pasif.typ
$system vaxcg.typ
$system vaxopc.inc
$system vaxcgu.inc
$system vaxexp.inc
$system vaxgen.inc
$PAGE reloc_equal

(* Compare two rel_syllables and determine if they are equal. *)

function reloc_equal ( rel1: rel_syllable; rel2: rel_syllable ):boolean;

begin
  reloc_equal := rel1.kind = rel2.kind;
  if reloc_equal
    then case rel1.kind of
	   register_sc,
	   absolute_sc: ;		(* Must be ok *)

	   local_sc,
	   parameter_sc,
	   static_sc,
	   external_sc : reloc_equal := rel1.relsym = rel2.relsym;

	   runtime_sc : reloc_equal := rel1.relrtsym = rel2.relrtsym;

	   def_sc : reloc_equal := rel1.reldef = rel2.reldef
	 End		(* Of the case statement *)

end;		(* of reloc_equal *)
$PAGE adr_equal

(* This routine compares two address descriptors. It returns a boolean
   indicating whether the two address descriptors are the same. This
   is often used in targeting where the address returned is compared to the
   specified target address. *)

public function adr_equal ( addr1: addr_desc; addr2: addr_desc ): boolean;

begin
  adr_equal := (addr1.byte_size = addr2.byte_size )
	And ( addr1.addr_mode = addr2.addr_mode )
	And ( addr1.immediate = addr2.immediate )
	And ( addr1.indirect = addr2.indirect )
	And ( addr1.index = addr2.index )
	And ( addr1.register = addr2.register )
	And ( addr1.offset = addr2.offset )
	And reloc_equal ( addr1.reloc , addr2.reloc )
end;
$PAGE def_addr, branch_addr
(* DEF ADDR accepts a DEF pointer and returns an ADDR DESC referencing
   the same.	*)

public function def_addr (d: def): addr_desc;

begin
  def_addr := absolute_reference;
  def_addr.reloc.kind := def_sc;
  def_addr.reloc.reldef := d;
end;
  
  
  
(* BRANCH ADDR accepts a DEF pointer assumed to be the target of a branch
   instruction, and returns an ADDR DESC to be used in emitting the branch.  *)
  
public function branch_addr (d: def): addr_desc;
  
begin
  branch_addr := def_addr (d);
  branch_addr.byte_size := vax_byte;
  branch_addr.addr_mode := branch_displacement
end;
$PAGE off_addr
(* OFF ADDR performs the same function but does NOT modify the
   addr_desc parameter.	*)


public function off_addr (addr: addr_desc; addition: unit_range): addr_desc;

begin
  assert ( not addr.indirect );
  assert ( not addr.immediate );
  off_addr := addr;
  if is_register (addr) then
    off_addr.register := off_addr.register + addition div bytes_per_unit
  else off_addr.offset := off_addr.offset + addition;
end;
$PAGE rt_addr
(* RT ADDR returns an ADDR DESC to a specified runtime symbol. *)

public function rt_addr (rt: rt_symbol): addr_desc;

begin
  rt_addr := absolute_reference;
  rt_addr.reloc.kind := runtime_sc;
  rt_addr.reloc.relrtsym := rt;
end;
$PAGE reg_addr   

(* REG_ADDR is passed a register and returns an address descriptor
   for the register.  *)

public function reg_addr ( reg: registers ): addr_desc;

begin
  reg_addr := reg_addr_desc;
  reg_addr.register := reg;
end;
$PAGE typ_reg_addr, get_typ_reg_addr
(* TYP REG ADDR is passed a register and returns an address descriptor
   for that register, with a caller-supplied BYTE SIZE.	*)

public function typ_reg_addr (reg: registers; size: vax_type): addr_desc;

begin
  typ_reg_addr := reg_addr_desc;
  typ_reg_addr.register := reg;
  typ_reg_addr.byte_size := size;
end;


(* GET_TYP_REG_ADDR is passed a VAX_TYPE and returns an address descriptor
   for a register large enough to accomodate the given VAX_TYPE.  *)

public function get_typ_reg_addr ( size: vax_type ): addr_desc;

begin
  get_typ_reg_addr := typ_reg_addr ( get_vax_reg ( size ), size );
end  (* proc get_typ_reg_addr *) ;
$PAGE int_value   
(* INT VALUE returns an addr_desc for any integer value.  
   The address descriptor will have the IMMEDIATE flag set and
   the BYTE_SIZE field set to LONG.  *)

public function int_value ( val: int_type ): addr_desc;
 begin
  int_value := absolute_reference;	(* init most fields to default *)
  with int_value do begin
    offset := val;
    immediate := true;
  end;
 end;
$PAGE typ_int_value

(* TYP_INT_VALUE serves the same function as INT_VALUE, but
   it allows the caller to explicitly specify the BYTE_SIZE
   field of the resulting address descriptor.  *)

public function typ_int_value ( val: int_type; vtype: vax_type ): addr_desc;

begin
  typ_int_value := int_value ( val );
  typ_int_value.byte_size := vtype;
end  (* proc typed_int_value *) ;
$PAGE typ_opc

(* TYP_OPC is given an opcode and a VAX_TYPE; it returns the
   variation of the opcode corresponding to the given VAX_TYPE.
   Note that:
	1. the longword variation of the opcode must be passed in
	   (except for, "CVTBL", because there is no "CVTLL" ! ).
	2. before using this routine check to make sure that the
	   relevant opcode is present,
	3. unused variations should be zero in the tables; this will
	   permit the assertion checks to detect illegal opcode
	   requests.	*)

public function typ_opc ( long_opc: opc_range; vtype: vax_type ): opc_range;

type
  opc_array = packed array [vax_type] of opc_range;

const
  tst_opcodes: opc_array = (tstb, tstw, tstl, 0, tstf, tstd);
  mneg_opcodes: opc_array = (mnegb, mnegw, mnegl, 0, mnegf, mnegd);
  add2_opcodes: opc_array = (addb2, addw2, addl2, 0, addf2, addd2);
  sub2_opcodes: opc_array = (subb2, subw2, subl2, 0, subf2, subd2);
  mul2_opcodes: opc_array = (mulb2, mulw2, mull2, 0, mulf2, muld2);
  div2_opcodes: opc_array = (divb2, divw2, divl2, 0, divf2, divd2);
  mova_opcodes: opc_array = (movab, movaw, moval, movaq, movaf, movad);
  inc_opcodes: opc_array = (incb, incw, incl, 0, 0, 0);
  dec_opcodes: opc_array = (decb, decw, decl, 0, 0, 0);
  cvtb_opcodes: opc_array = (0, cvtwb, cvtlb, 0, cvtfb, cvtdb);
  cvtw_opcodes: opc_array = (cvtbw, 0, cvtlw, 0, cvtfw, cvtdw);
  cvtl_opcodes: opc_array = (cvtbl, cvtwl, 0, 0, cvtfl, cvtdl);
  cvtf_opcodes: opc_array = (cvtbf, cvtwf, cvtlf, 0, 0, cvtdf);
  cvtd_opcodes: opc_array = (cvtbd, cvtwd, cvtld, 0, cvtfd, 0);
  cvtr_opcodes: opc_array = (0, 0, 0, 0, cvtrfl, cvtrdl);
  add3_opcodes: opc_array = (addb3, addw3, addl3, 0, addf3, addd3);
  cmp_opcodes: opc_array = (cmpb, cmpw, cmpl, 0, cmpf, cmpd);
  mov_opcodes: opc_array = (movb, movw, movl, movq, movf, movd);
  pusha_opcodes: opc_array = (pushab, pushaw, pushal, pushaq, pushaf, pushad);
  clr_opcodes: opc_array = (clrb, clrw, clrl, clrq, clrf, clrd);
  case_opcodes: opc_array = (caseb, casew, casel, 0, 0, 0);

  bis2_opcodes: opc_array := (bisb2, bisw2, bisl2, 0, 0, 0);
  bis3_opcodes: opc_array := (bisb3, bisw3, bisl3, 0, 0, 0);

  bic2_opcodes: opc_array := (bicb2, bicw2, bicl2, 0, 0, 0);
  bic3_opcodes: opc_array := (bicb3, bicw3, bicl3, 0, 0, 0);
  bit_opcodes: opc_array := (bitb,  bitw,  bitl,  0, 0, 0);
  mcom_opcodes: opc_array := (mcomb, mcomw, mcoml, 0, 0, 0);
  

begin

  case long_opc of

    tstl:	typ_opc := tst_opcodes[ vtype ];

    mnegl:	typ_opc := mneg_opcodes[ vtype ];

    addl2:	typ_opc := add2_opcodes[ vtype ];

    subl2:	typ_opc := sub2_opcodes[ vtype ];

    mull2:	typ_opc := mul2_opcodes[ vtype ];

    divl2:	typ_opc := div2_opcodes[ vtype ];

    moval:	typ_opc := mova_opcodes[ vtype ];

    incl:	typ_opc := inc_opcodes[ vtype ];

    decl:	typ_opc := dec_opcodes[ vtype ];

    cvtlf:	typ_opc := cvtf_opcodes[ vtype ];

    cvtld:	typ_opc := cvtd_opcodes[ vtype ];

    cvtrfl:	typ_opc := cvtr_opcodes[ vtype ];
  
    addl3:	typ_opc := add3_opcodes[ vtype ];

    cmpl:	typ_opc := cmp_opcodes[ vtype ];

    movl:	typ_opc := mov_opcodes[ vtype ];

    pushal:	typ_opc := pusha_opcodes[ vtype ];

    clrl:	typ_opc := clr_opcodes[ vtype ];

    casel:	typ_opc := case_opcodes[ vtype ];

    bicl2:	typ_opc := bic2_opcodes[ vtype];
  
    bicl3:	typ_opc := bic3_opcodes[ vtype ];

    bisl2:	typ_opc := bis2_opcodes[ vtype ];

    bisl3:	typ_opc := bis3_opcodes[ vtype ];

    bitl:	typ_opc := bit_opcodes[ vtype ];

    mcoml:	typ_opc := mcom_opcodes[ vtype];

    cvtlb:	typ_opc := cvtb_opcodes [ vtype ];

    cvtlw:	typ_opc := cvtw_opcodes [ vtype ];

    cvtbl:	typ_opc := cvtl_opcodes [ vtype ];

    others:	assert ( false )	(* add a table for it !!! *)

  end  (* case *) ;

  assert ( typ_opc <> 0 );	(* Illegal opcode ??? *)

end  (* proc typ_opc *) ;
$PAGE cons_vax_type

(* CONS_VAX_TYPE returns the minimal VAX_TYPE which a given scalar
   value (parameter SCALAR_VALUE) can be represented as.  *)

public function cons_vax_type ( scalar_value: int_type ): vax_type;

begin
  if (scalar_value >= minimum( byte ) ) and
     (scalar_value <= maximum( uns_byte ) )
    then cons_vax_type := vax_byte
  else if (scalar_value >= minimum ( word ) ) and
	  (scalar_value <= maximum ( uns_word ) ) 
    then cons_vax_type := vax_word
  else cons_vax_type := vax_long;
end  (* proc cons_vax_type *) ;
$PAGE push_constant

(* PUSH_CONSTANT pushes a scalar constant onto the stack using the
   minimal length instruction.  The value is converted to a longword,
   if necessary, during the move to the stack.  *)

procedure push_constant ( scalar_value: int_type );

var
  vtype: vax_type;
  addr: addr_desc;

begin
  vtype := cons_vax_type ( scalar_value );
  if ( scalar_value >= min_literal ) and ( scalar_value <= max_literal )
    then vtype := vax_long;	(* literal mode will be used *)
  addr := typ_int_value ( scalar_value, vtype );

  if vtype = vax_long
    then gen1 ( pushl, addr )
    else move_immediate ( scalar_value, push_reference );
end  (* proc push_constant *) ;
$PAGE push_value
(* PUSH_VALUE pushes the contents of a given location (parameter ADDR)
   onto the stack.  Parameter ALIGN specifies the object's alignment
   for the move.  Parameter ADDR is freed by this routine.
   If ADDR.BYTE_SIZE is VAX_BYTE or VAX_WORD then the value is
   converted to a longword as it is moved to the stack.  *)

public procedure push_value ( addr: addr_desc; align: data_alignment );

var
  vtype: vax_type;
  dest: addr_desc;
  scalar_value: int_type;

begin
  if aconstp ( addr, scalar_value ) and (addr.byte_size <> vax_quad) then begin
    push_constant ( scalar_value );
  end
  else begin
    vtype := addr.byte_size;
    if vtype = vax_long then begin
      gen1 ( pushl, addr );
    end
    else begin	(* use MOVx or CVTxL instruction *)
      dest := push_reference;
      if vtype in [vax_quad, vax_float, vax_double]
	then dest.byte_size := vtype;
      store ( addr, dest, align );
    end;
    free ( addr );
  end;
end  (* proc push_value *) ;
$PAGE push_address

(* PUSH_ADDRESS pushes an **address** (parameter ADDR) onto
   the stack.  Parameter ADDR is freed by this routine.  *)

public procedure push_address ( addr: addr_desc );

begin
  assert( not (is_immediate(addr) or is_register(addr)) );
  free ( addr );
  gen1 ( typ_opc(pushal, addr.byte_size), addr );
end  (* proc push_address *) ;
$PAGE push_mem_addr

(* PUSH_MEM_ADDR pushes the address of a specified value (parameter ADDR)
   onto the stack.  If the value is in a register, it is first copied to
   a temp.  If the value is immediate, then it will be generated in the
   constant area.  Parameter ALIGN is the alignment of the value and is
   used if the value must be copied to a temporary.  Note that the
   BYTE_SIZE field of parameter ADDR must correspond to the actual size
   of the value if a move or generation in the constant area is
   necessary.  *)

public procedure push_mem_addr ( addr: addr_desc; align: data_alignment );

var
  value: int_type;
  push_addr: addr_desc;

begin

  (* If immediate, then generate in the constant area. *)

  if aconstp ( addr, value ) then begin
    push_addr := gen_cst ( value, addr.byte_size );
  end

  (* If in a register, move to a temp.  *)

  else if is_register ( addr ) then begin
    PUSH_ADDR := GET_TEMP (
	INT_VALUE ( VAX_TYPE_SIZE(ADDR.BYTE_SIZE) Div BITS_PER_BYTE), VAX_BYTE );
    push_addr.byte_size := addr.byte_size;
    free ( addr );
    store ( addr, push_addr, align );
  end
  
  else push_addr := addr;

  (* Push the address.  *)

  push_address ( push_addr );
end  (* proc push_addr *) ;
$PAGE add2_constant

(* ADD2_CONSTANT adds a constant (parameter CONS_VAL) to a given location
   (parameter ADDR).  This routine is similiar to ADD_CONSTANT except
   that no result location is allocated or returned - the add is done
   directly to the given location.  The opcode variation used (byte, word
   or long) is based on the BYTE_SIZE field of ADDR.  *)

public procedure add2_constant ( addr: addr_desc; cons_val: int_type );

var
  vtype: vax_type;
  literal: boolean;
  cons_addr: addr_desc;

begin
  vtype := addr.byte_size;
  literal := (cons_val <= -(min_literal+2)) and (cons_val >= -max_literal);	(* we force literal mode for these values *)
  if literal 
    then cons_addr := typ_int_value ( -cons_val, vtype )
    else cons_addr := typ_int_value ( cons_val, vtype );

  if cons_val = 1
    then gen1 ( typ_opc ( incl, vtype ), addr )
  else if cons_val = -1
    then gen1 ( typ_opc ( decl, vtype ), addr )
  else if literal 	(* use of literal mode saves a byte *)
    then gen2 ( typ_opc ( subl2, vtype ), cons_addr, addr )
  else if cons_val <> 0
    then gen2 ( typ_opc ( addl2, vtype ), cons_addr, addr );

end  (* proc add2_constant *) ;
$PAGE add_constant
(* ADD_CONSTANT adds an integer value (parameter CONS_VAL) to a
   given location (parameter ADDR).  The address of the result
   is returned.
   Note that:
	1. the orginal address is returned if cons_val is zero,
	2. parameter ADDR is 'used' by this routine, and thus is not
	   valid upon return unless previously duplicated,
	3. the BYTE_SIZE field of the result will have the same value
	   as ADDR.BYTE_SIZE.
	4. Use the TARG_ADDR as the target of the operation if the
	   target address exists and if the size of the target matches the
	   size of the source. *)

public function add_constant ( addr: addr_desc; constant_value: int_type;
				targ_addr: addr_desc ): addr_desc;

var
  cons_val: int_type;
  opcode: opc_range;
  vtype: vax_type;
  const_addr: addr_desc;

begin
  cons_val := constant_value;
  if cons_val = 0 then begin	(* if constant is 0, then just *)
    add_constant := addr	(* return orginal address *)
  end
  else begin			(* constant is not zero *)
    vtype := addr.byte_size;
    free ( addr );

    (* If possible use targeting. The target address must be real and the
       size of the destination must match the size of the source *)

    if adr_equal(targ_addr,no_preference) or ( targ_addr.byte_size <> vtype )
      then add_constant := typ_reg_addr ( get_reg(vax_type_size(vtype)), vtype )
    else add_constant := targ_addr;

    if (cons_val <= -(min_literal + 1)) and (cons_val >= -max_literal) then begin	(* use SUB instr if *)
      cons_val := -cons_val;	(* it permits use of literal mode *)
      opcode := subl2;
    end
    else opcode := addl2;
    const_addr := typ_int_value ( cons_val, vtype );
    
    (* If ADDR is the result reg, we can generate a shorter instruction. *)

    if is_register ( addr ) andif is_register ( add_constant )
    andif ( addr.register = add_constant.register ) then begin
      if (cons_val = 1) andif (opcode = addl2)	(* can use INCx instruction *)
        then gen1 ( typ_opc ( incl, vtype ), add_constant )
      else if (cons_val = -1) orif (cons_val = 1)	(* can use DECx instruction *)
        then gen1 ( typ_opc ( decl, vtype ), add_constant )
      else gen2 ( typ_opc(opcode, vtype), const_addr, add_constant );
    end
    else begin	(* must use 3 operand add *)
      gen3(typ_opc(opcode,vtype) + 1, const_addr, addr, add_constant);
    end;
  end  (* else *) ;
end  (* proc add_constant *) .
 