$TITLE vaxmac - VAX assembly listing routine
$LENGTH 42
module vaxmac;
  
  
  
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM vaxcg.typ
$SYSTEM vaxcgu.inc
$SYSTEM pasfil.inc
$SYSTEM pascv.inc
$system vaxgen.inc
$SYSTEM vaxrel.inc
$SYSTEM vaxexp.inc
$system ptmcon.inc
  
  
  
type optable = array[0..15] of packed array[1..96] of char;

const opcodes: optable := (
'HALT  NOP   REI   BPT   RET   RSB   LDPCTXSVPCTXCVTPS CVTSP INDEX CRC   PROBERPROVEWINSQUEREMQUE',
'BSBB  BRB   BNEQ  BEQL  BGTR  BLEQ  JSB   JMP   BGEQ  BLSS  BGTRU BLEQU BVC   BVS   BGEQU BLSSU',
'ADDP4 ADDP6 SUBP4 SUBP6 CVTPT MULP  CVTTP DIVP  MOVC3 CMPC3 SCANC SPANC MOVC5 CMPC5 MOVTC MOVTUC',
'BSBW  BRW   CVTWL CVTWB MOVP  CMPP3 CVTPL CMPP4 EDITPCMATCHCLOCC  SKPC  MOVZWLACBW  MOVAW PUSHAW',
'ADDF2 ADDF3 SUBF2 SUBF3 MULF2 MULF3 DIVF2 DIVF3 CVTFB CVTFW CVTFL CVTRFLCFTBF CVTWF CVTLF ACBF',
'MOVF  CMPF  MNEGF TSTF  EMODF POLYF CVTFD **57**ADAWI **59****5A****5B****5C****5D****5E****5F**',
'ADDD2 ADDD3 SUBD2 SUBD3 MULD2 MULD3 DIVD2 DIVD3 CVTDB CVTDW CVTDL CVTRDLCVTBD CVTWD CVTLD ACBD',
'MOVD  CMPD  MNEGD TSTD  EMODD POLYD CVTDF **77**ASHL  ASHQ  EMUL  EDIV  CLRQ  MOVQ  MOVAQ PUSHAQ',
'ADDB2 ADDB3 SUBB2 SUBB3 MULB2 MULB3 DIVB2 DIVB3 BISB2 BISB3 BICB2 BICB3 XORB2 XORB3 MNEGB CASEB',
'MOVB  CMPB  MCOMB BITB  CLRB  TSTB  INCB  DECB  CVTBL CVTBW MOVZBLMOVZBWROTL  ACBB  MOVAB PUSHAB',
'ADDW2 ADDW3 SUBW2 SUBW3 MULW2 MULW3 DIVW2 DIVW3 BISW2 BISW3 BICW2 BICW3 XORW2 XORW3 MNGEW CASEW ',
'MOVW  CMPW  MCOMW BITW  CLRW  TSTW  INCW  DECW  BISPSWBICPSWPOPR  PUSHR CHMK  CHME  CHMS  CHMU  ',
'ADDL2 ADDL3 SUBL2 SUBL3 MULL2 MULL3 DIVL2 DIVL3 BISL2 BISL3 BICL2 BICL3 XORL2 XORL3 MNEGL CASEL ',
'MOVL  CMPL  MCOML BITL  CLRL  TSTL  INCL  DECL  ADWC  SWBC  MTPR  MFPR  MOVPSLPUSHL MOVAL PUSHAL',
'BBS   BBC   BBSS  BBCS  BBSC  BBCC  BBSSI BBCCI BLBS  BLBC  FFS   FFC   CMPV  CMPZV EXTV  EXTZV ',
'INSV  ACBL  AOBLSSAOBLEQSOBGEQSOBGTRCVTLB CVTLW ASHP  CVTLP CALLG CALLS XFC   **FD****FE****FF**');
$PAGE init_output
(* INIT OUTPUT sets up the listing file for I/O *)

procedure init_output;
  
  begin
    case lf_status of
      unopened:    fio_open (listfb, '.LST ' || list_file);
      prev_opened: fio_reopen (listfb)	(* opened in listing pass *)
    end;
    with listfb do begin
      if not eof (file_var) then begin
	writeln (tty, '?Unable to open listing file ', list_file);
	list_file := '';
	return;
      end;
    end;
    listfb.new_page := fio_eject;
    listfb.page_header := fio_nop;
    listfb.width := prog_options.page_width;
    listfb.plength := prog_options.page_length;
    lf_status := now_open;
  end;
$PAGE cvhex
(* CV HEX converts an integer value to a string giving its hexadecimal representation.
   Negative values are signed. *)

type cvhex_string = string[9];

public function cvhex (value: int_type): cvhex_string;
  
  const
    hexmnem: packed array [1..16] of char = '0123456789ABCDEF';
  
  var
    numbuf: packed array[1..9] of char;
    first: 0..10;
    v: int_type;
  
  begin
    if value = 0
      then cvhex := '0'

    else begin
      v := abs (value);
      first := 10;
      while v <> 0 do begin
	first := first - 1;
	numbuf [first] := hexmnem [(v mod 16) + 1];
	v := v div 16;
      end;
      if value < 0 then begin
	first := first - 1;
	numbuf [first] := '-';
      end;
      cvhex := substr (numbuf, first);
    end;
  end;
$PAGE put_defname
(* PUT DEFNAME formats and outputs the name corresponding to the internal definition
   given by "intdef". *)

procedure put_defname (intdef: def);
  
  type
    code_array = array [sym_def..label_def] of char;
  
  const
    def_code: code_array := ('S', 'K', 'H', 'B', 'D', 'T', 'C', 'X', 'L');
  
  begin
    with intdef^ do
  
      if deftype in [sym_def..label_def] - [temp_size_def] then begin
        fio_write (listfb, def_code[deftype]);
	fio_write (listfb, '.');
	fio_write (listfb, cv_int (defnumber))
      end
  
      else if deftype = offset_def then begin
	put_defname (reldef);
	if offset >= 0 then fio_write (listfb, '+');	(* otherwise minus sign comes for free *)
	fio_write (listfb, cv_int (offset));
      end

      else if deftype = temp_size_def then	(* Stack fixup for dynamic temps *)
	fio_write ( listfb , cv_int ( addr ) )
  
      else if deftype = extern_def then begin
	(* the only expected use of this is in handler branch table entries for
	   user-defined external conditions, so ... *)
	assert ((symbol^.init_value.kind = no_value) and (symbol^.kind = conditions));
	fio_write (listfb, symbol^.name^.text)
      end
  
      else if deftype = static_def then begin
	(* the only expected use of this is in handler branch table entries for
	   user-defined conditions, so ... *)
	assert (symbol^.kind = conditions);
	assert (symbol^.init_value.kind = no_value);
	fio_write (listfb, 'STATIC.+' || cv_int (symbol^.item_addr + size_init))
      end
  
      else
	assert (false)
  end;
$PAGE put_field
(* PUT FIELD formats and outputs a relocatable halfword offset, given the value of
   the offset ("off") and the relocation information ("reloc"). *)

procedure put_field (off: unit_range; reloc: rel_syllable);

  var
    area_offset: unit_range;
  
  begin
    with reloc do begin
      case kind of

	local_sc:
	  if relsym <> nil then fio_write (listfb, cv_int (relsym^.item_addr));

	parameter_sc, static_sc:
	  begin
	    if relsym <> nil then begin
	      area_offset := relsym^.item_addr;
	      if kind = static_sc then begin
		fio_write (listfb, 'STATIC.+');
		if relsym^.kind = conditions then
		  area_offset := area_offset + size_init
		else if relsym^.init_value.kind = no_value then
		  area_offset := area_offset + size_init + size_cond;
	      end;
	      fio_write (listfb, cv_int (area_offset));
	    end;
	  end;

	external_sc:
	  begin
	    if relsym^.init_value.kind = no_value then
	      fio_write (listfb, relsym^.name^.text)
	    else
	      fio_write (listfb, relsym^.init_value.valp^.str_val);
	  end;

	def_sc:
	  put_defname (reldef);

	self_rel_sc:
	  fio_write (listfb, '.');

	runtime_sc:
	  fio_write (listfb, rts_name [relrtsym])

      end (* case *) ;
    end (* with *) ;

    if not ( (reloc.kind in [register_sc, absolute_sc])
	     or ( (reloc.kind in [local_sc, parameter_sc, static_sc, external_sc])
		 andif (reloc.relsym = nil) ) ) then begin
      if off = 0 then return;		(* <-- exit if not constant offset *)
      if off > 0 then fio_write (listfb, '+');
    end;
    fio_write (listfb, cv_int (off))
  end;
$PAGE list
(* LIST produces an assembly language listing of a single code record ("cr"). *)

procedure list (cr: code; var ic: code_address)
  options special ( coercions );
  
 type
    r_mnem_array = array [0..15] of string[3];
    attr_array = array[psect_attributes] of string[5];
  
 const
    reg_mnem: r_mnem_array = ('R0','R1','R2','R3','R4','R5','R6','R7',
			      'R8','R9','R10','R11','AP','FP','SP','PC');
    true_attrs: attr_array := (
	'LONG','CON','EXE','GBL','PIC','SHR','RD','WRT','REL');
    false_attrs: attr_array := (
	'BYTE','OVL','NOEXE','LCL','NOPIC','NOSHR','NORD','NOWRT','ABS');

 var i: bit_range;
     ioper: 0..6;
     com_string: string[60];
     r: registers;
     not_first: boolean;
     attr_ix: psect_attributes;
     sym_offset : unit_range;
    blocks_regs_used: set_of_registers;

 procedure put_ic (ind: line_index);
   var ic_string: string[9];
   begin
     if listfb.column = 1 then begin
       ic_string := cvhex(ic);
       fio_write (listfb, substr ('00000' || ic_string, length (ic_string) + 1, 5))
     end;
     fio_tab (listfb, ind)
   end;
  
 begin
  with cr^ do begin
    case kind of

      instruction:
	begin
	  put_ic (17);
	  fio_write (listfb, substr (opcodes [opcode div 16], ((opcode mod 16) * 6) + 1, 6));
	  fio_tab (listfb, 25);
          com_string := '';
          for ioper := 1 to noperands do
            with operands[ioper] do begin
              if ioper > 1 then fio_write (listfb, ',');
              case addr_mode of
  
                auto_inc:
                  begin
		    if indirect then fio_write (listfb, '@');
		    fio_write (listfb, '(' || reg_mnem [register] || ')+')
		  end;
  
                auto_dec:
		  fio_write (listfb, '-(' || reg_mnem [register] || ')');
  
		branch_displacement:
		  if reloc.kind = self_rel_sc then begin
		    fio_write (listfb, '.');
		    if offset >= 0 then fio_write (listfb, '+');
		    fio_write (listfb, cv_int (offset+1))
		  end
		  else
		    put_defname (reloc.reldef);
  
		other:
		  begin
		    if immediate then begin
		      if (offset >= min_literal) and (offset <= max_literal)
			then fio_write (listfb, 'S^#')
			else fio_write (listfb, 'I^#');
		      fio_write (listfb, cv_int (offset));
		      if offset >= #H10
			then com_string := com_string || '  ^X' || cvhex (offset)
                    end
		    else begin  (* not immediate *)
		      if indirect then fio_write (listfb, '@');
		      if reloc.kind = register_sc then
			fio_write (listfb, reg_mnem [register])
		      else begin
			put_field (offset, reloc);
			if register <> noreg then
			  fio_write (listfb, '(' || reg_mnem [register] || ')')
		      end
		    end (* not immediate *);
		  end (* other *)
	      end (* case addr_mode *);
	      if index <> noreg then
		fio_write (listfb, '[' || reg_mnem [index] || ']');
              if (reloc.kind in [local_sc, parameter_sc, static_sc, external_sc])
                andif (reloc.relsym <> nil) then begin
		  with reloc.relsym^ do begin
		    if name <> nil then
		      com_string := com_string || '  ' || name^.text
		    else begin
		      if kind in [vars, labels] then
			com_string := com_string || '  V.' || cv_int (id_number)
		      else
			com_string := com_string || 'C.' || cv_int (id_number);
		    end;
		  end;
		end;
            end (* with *);
  
          if com_string <> '' then begin
            fio_tab (listfb,49);
            fio_write (listfb, '; ' || com_string)
          end;
	  fio_skip (listfb);
	  ic := ic + 1 + op_length (cr)
	end;

      fullword: begin
	put_ic (17); fio_write (listfb, '.LONG'); fio_tab (listfb, 25);
	fio_write (listfb, cv_int (fwd));
	if fwd >= #H10 then begin
	  fio_tab (listfb, 49); fio_write (listfb,'; ');
	  fio_write (listfb, '  ^X' || cvhex (fwd));
	end;
	fio_skip (listfb);
	ic := ic + 4
      end;

      stringword, pstringword: begin
	if kind = stringword then begin
	  put_ic (17);   fio_write (listfb,'.WORD'); fio_tab (listfb, 25);
	  fio_write (listfb, cv_int (length (strvalue)));
	  fio_skip (listfb);
	  ic := ic + 2
	end;
	put_ic (17);
	fio_write (listfb, '.ASCII'); fio_tab (listfb, 25);
	fio_write (listfb, '"');
	fio_write (listfb, strvalue);
        fio_line (listfb, '"');
	ic := ic + length (strvalue)
      end;

      setbyte: begin
	put_ic (17); fio_write (listfb, '.BYTE'); fio_tab (listfb, 25);
	fio_write (listfb, '^B');
	for i := 7 downto 0 do
	  fio_write (listfb, substr ('01', ord (setval[i])+1, 1));
	fio_skip (listfb);
	ic := ic + 1
      end;

      defmark: begin
	if (listfb.column = 1) and (defname^.deftype <> local_def) then
	  fio_skip (listfb); (* no preceding label *)
	put_ic (9); put_defname (defname); fio_write (listfb, ':');		(* leave in middle of line *)
      end;

      origin: begin
	fio_skip (listfb);
	fio_tab (listfb, 17); fio_write (listfb, '.PSECT'); fio_tab (listfb, 25);
	if psect = static_psect
	  then fio_write (listfb, 'PAX_STATIC')
	  else fio_write (listfb, 'PAX_CODE......B');
	for attr_ix := minimum (psect_attributes) to maximum (psect_attributes) do begin
	  fio_write (listfb, ',');
	  if attr_ix in psectattrs[psect]
	    then fio_write (listfb, true_attrs[attr_ix])
	    else fio_write (listfb, false_attrs[attr_ix]);
	end;
	fio_skip (listfb);
	if psect = static_psect then begin
	  fio_tab (listfb, 9);
	  fio_line (listfb, 'STATIC.:');
	end;
      end;

      source: begin
	fio_tab (listfb, 49);
	fio_write (listfb, '; LINE '); fio_line (listfb, cv_source_id (stmtid)); 
      end;

      comment: begin
	if (listfb.column <> 1) then fio_skip (listfb);
	fio_tab (listfb, 9);  fio_line (listfb, ';'); 
	fio_tab (listfb, 9);
	fio_write (listfb, ';'); fio_tab (listfb, 17); fio_line (listfb, substr (ctext, 1, length (ctext))); 
	fio_tab (listfb, 9);  fio_line (listfb, ';');
      end;

      bytelen: begin
	put_ic (17);
	fio_write (listfb, '.BYTE'); fio_tab (listfb, 25);
	fio_write (listfb, cv_int (byte_value));
	if byte_value >= #H10 then begin
	  fio_tab (listfb, 49); fio_write (listfb, '; ');
	  fio_write (listfb, '  ^X' || cvhex (byte_value));
	end;
        fio_skip (listfb);
	ic := ic + 1
      end;

      wordlen: begin
	put_ic (17); fio_write (listfb, '.WORD'); fio_tab (listfb, 25);
	fio_write (listfb, cv_int (word_value));
	if word_value >= #H10 then begin
	  fio_tab (listfb, 49); fio_write (listfb, '; ');
	  fio_write (listfb, '  ^X' || cvhex (word_value));
	end;
	fio_skip (listfb);
	ic := ic + 2
      end;

      quadword: begin
	put_ic (17);
	fio_write (listfb, '.LONG'); fio_tab (listfb, 25);
	fio_line (listfb, cv_int (qvalue1));
	put_ic (17);
	fio_write (listfb, '.LONG'); fio_tab (listfb, 25);
	fio_line (listfb, cv_int (qvalue2));
	ic := ic + 8
      end;

      realword: begin
        put_ic (17); fio_write (listfb, '.FLOAT');
        fio_tab (listfb, 25); fio_line (listfb, cv_real (rvalue));
	ic := ic + 4
      end;
  
      doubleword: begin
        put_ic (17); fio_write (listfb, '.DOUBLE');
        fio_tab (listfb, 25); fio_line (listfb, cv_real (rvalue));
	ic := ic + 8
      end;
  
      maskword: begin
        put_ic (17);
	if (block <> nil) andif (block^.kind = program_blk) then begin
	  fio_write (listfb, '.ENTRY');
	  fio_tab (listfb, 25);
	  fio_write (listfb, 'PAX_PROGRAM.,')
	end
	else if (block <> nil) andif (block^.kind = subr_blk) andif block^.subr_sym^.public_dcl then begin
	  fio_write (listfb, '.ENTRY');
	  fio_tab (listfb, 25);
	  fio_write (listfb, block^.subr_sym^.name^.text);
	  fio_write (listfb, ',')
	end
	else begin
	  fio_write (listfb, '.WORD');
	  fio_tab (listfb, 25)
	end;
        fio_write (listfb, '^M<');
	not_first := false;
	if block <> nil then begin
	  blocks_regs_used := reg_use_by_block^[block^.number];
	  for r := r2 to 11 do
	    if r in blocks_regs_used then begin
	      if not_first then
		fio_write (listfb, ',');
	      fio_write (listfb, reg_mnem[r]);
	      not_first := true;
	    end;
	end;
	if not_first then
	  fio_write (listfb, ',');
        fio_line (listfb, 'IV>');
	ic := ic + 2
      end;
  
      displacement: begin
	put_ic (17);
	if disp_size = 2 * byte_size
	  then fio_write ( listfb, '.WORD' )
	  else fio_write ( listfb, '.LONG' );
	fio_tab (listfb, 25);
	put_defname (to_def);
	fio_write (listfb, '-');
	put_defname (from_def);
	if (to_def^.deftype in [static_def, extern_def]) 
	     andif (to_def^.symbol^.name <> nil) then begin
	  fio_tab (listfb, 49);
	  fio_line (listfb, '; ' || to_def^.symbol^.name^.text)
	end
	else
	  fio_skip (listfb);
	ic := ic + (disp_size div byte_size)
      end;

      indirect_word: with pub_sym^ do begin
	assert (kind in [vars, consts, conditions]);
	if kind <> conditions then begin
	  fio_tab( listfb, 9);
	  fio_write( listfb, name^.text ||  '::' )
	end;
	put_ic ( 17 );
	fio_write ( listfb , '.ADDRESS ' );
	if kind = vars then begin
	  sym_offset := item_addr;
	  if init_value.kind = no_value
	    then sym_offset := sym_offset + size_init + size_cond;
	  fio_write ( listfb , 'STATIC.+' || cv_int ( SYM_OFFSET ) )
	end
	else if kind = consts then begin
	  assert ( init_value.kind = alloc_cst );
	  fio_write ( listfb , ' K.' );
	  fio_write ( listfb , cv_int ( def (init_value.defp)^.defnumber ) )
	end
	else begin
	  (* The previous two cases are utilized only for overlay compilations.  This
	     case is only used for standard conditions in handler branch tables. *)
	  if dcl_class = static_sc then
	    fio_write (listfb, 'STATIC.+' || cv_int (item_addr + size_init))
	  else begin
	    assert ((dcl_class = external_sc) and standard and (init_value.kind <> no_value));
	    fio_write (listfb, init_value.valp^.str_val)
	  end
	end;
	if name <> nil then begin
	  fio_tab (listfb, 49);
	  fio_write (listfb, '; ' || name^.text) (* comment *)
	end;
	ic := ic + 4
      end


    end (* case *) ;
  end (* with *) ;
 end;
$PAGE mac_list
(* MAC LIST generates a VAX Macro style assembly listing of code records from
   the area given by "arealist". *)

public procedure mac_list (arealist: code_list; init_ic: code_address);
  
  var cr: code;
      ic: code_address;
  
  begin
  
    if list_file = '' then return; (* <---- no listing file *)
    init_output;				(* force to head of line *)
    cr := arealist.first;
    ic := init_ic;
    fixing_branches := false; (* signal to def_sc handling in op_length (in vaxrel) *)
    while cr <> nil do begin
      list (cr, ic);
      cr := cr^.next;
    end (* while *) ;
  
    fio_skip (listfb);
  
  end;
$PAGE mac_pad
(* MAC PAD writes a .BLKB storage directive in the macro listing.  *)
  
public procedure mac_pad (block_size: unit_range);
  
  begin
    if list_file = '' then return;  (* <---- no listing file *)
    fio_tab (listfb, 17);
    fio_write (listfb, '.BLKB');
    fio_tab (listfb, 25);
    fio_line (listfb, cv_int (block_size));
    fio_skip (listfb)
  end;
$PAGE mac_header
(* MAC HEADER outputs a header for the assembly listing.  *)

public procedure mac_header;
  
  begin
    if list_file = '' then return; (* <---- no listing file *)
     init_output;
     fio_page (listfb);
     fio_tab (listfb, 17); fio_write (listfb, '.TITLE'); fio_tab (listfb, 25);
     fio_line (listfb, root_block^.children^.id^.text);
  end;
$PAGE mac_end
(* MAC END terminates the assembly language listing. *)

public procedure mac_end (startdef: def; code_size, const_size, static_size:_range)
  options special(coercions);
  
  var
    symbol: sym;
    temp_def: def;
    sym_offset : unit_range;
  
  begin
  
    if list_file = '' then return; (* <---- no listing file *)
    init_output;
  
  
    (* public symbols *)
  
    symbol := root_block^.children^.id_list.first;
  
    if not prog_options.overlay_opt then (* overlay handled indirect_word  in routine list in vaxmac *)
      while symbol <> nil do
	with symbol^ do begin
	  if public_dcl then begin (* only concerned with publics here *)

	    if kind in [vars, conditions] then begin
	      fio_tab (listfb, 9);
	      fio_write (listfb, name^.text);
	      fio_write (listfb, ' == STATIC.+');
	      sym_offset := item_addr;
	      if kind = conditions then
		sym_offset := sym_offset + size_init
	      else if init_value.kind = no_value
		then sym_offset := sym_offset + size_init + size_cond;
	      fio_line ( listfb , cv_int ( sym_offset ) )
	    end

	    else if (kind = consts) andif (init_value.kind = alloc_cst) then begin
	      fio_tab (listfb, 9);
	      fio_write (listfb, name^.text);
	      fio_write (listfb, ' == K.');
	      temp_def := def (init_value.defp);
	      fio_line (listfb, cv_int (temp_def^.defnumber))
	    end

	  end (* if public_dcl *);

	  symbol := next
	end (* with *);
  
  
    fio_tab (listfb, 9); fio_line (listfb, ';');
    fio_tab (listfb, 9); fio_line (listfb, ';    Code area:      ' || cvhex (code_size) ||
		      ' bytes (' || cv_int (code_size) || ' decimal)');
    fio_tab (listfb, 9); fio_line (listfb, ';    Constant area:  ' || cvhex (const_size) ||
		      ' bytes (' || cv_int (const_size) || ' decimal)');
    fio_tab (listfb, 9); fio_line (listfb, ';    Static area:    ' || cvhex (static_size) ||
		      ' bytes (' || cv_int (static_size) || ' decimal)');
    fio_tab (listfb, 9); fio_line (listfb, ';');
    fio_skip (listfb);
  
    fio_tab (listfb, 17); fio_write (listfb, '.END');
    fio_skip (listfb);
    fio_close (listfb);
    lf_status := prev_opened;
  
  end.
  OEui–