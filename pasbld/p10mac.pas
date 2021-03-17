$TITLE p10mac - Macro-10 assembly listing routine
$LENGTH 42
module p10mac;
$PAGE includes
$include pascal.inc
$INCLUDE ptmcon.inc
$include pasist.inc
$include paspt.typ
$include pasif.typ
$include p10cg.typ
$include p10cgu.inc
$include pasfil.inc
$include pascv.inc
$include TIMUTL.inc
$PAGE opcode table
type optable = array[0..44] of packed array[1..60] of char;
const opcodes: optable :=
     (  'Z     ***001***002***003ADJ_BPADJ_SP***006***007***010***011',
        '***012***013***014***015***016***017***020***021***022***023',
        '***024***025***026***027***030***031***032***033***034***035',
        '***036***037CALL  INIT  ***042***043***044***045***046CALLI ',
        'OPEN  TTCALL***052***053***054RENAMEIN    OUT   SETSTSSTATO ',
        'STATUSSTATZ INBUF OUTBUFINPUT OUTPUTCLOSE RELEASMTAPE UGETF ',
        'USETI USETO LOOKUPENTER UJEN  ***101***102***103***104ADJSP ',
        '***106***107DFAD  DFSB  DFMP  DFDV  DADD  DSUB  DMUL  DDIV  ',
        'DMOVE DMOVN FIX   ***123DMOVEMDMOVNMFIXR  FLTR  UFA   DFN   ',
        'FSC   IBP   ILDB  LDB   IDPB  DPB   FAD   FADL  FADM  FADB  ',
        'FADR  FADRI FADRM FADRB FSB   FSBL  FSBM  FSBB  FSBR  FSBRI ',
        'FSBRM FSBRB FMP   FMPL  FMPM  FMPB  FMPR  FMPRI FMPRM FMPRB ',
        'FDV   FDVL  FDVM  FDVB  FDVR  FDVRI FDVRM FDVRB MOVE  MOVEI ',
        'MOVEM MOVES MOVS  MOVSI MOVSM MOVSS MOVN  MOVNI MOVNM MOVNS ',
        'MOVM  MOVMI MOVMM MOVMS IMUL  IMULI IMULM IMULB MUL   MULI  ',
        'MULM  MULB  IDIV  IDIVI IDIVM IDIVB DIV   DIVI  DIVM  DIVB  ',
        'ASH   ROT   LSH   JFFO  ASHC  ROTC  LSHC  ***247EXCH  BLT   ',
        'AOBJP AOBJN JRST  JFCL  XCT   ***257PUSHJ PUSH  POP   POPJ  ',
        'JSR   JSP   JSA   JRA   ADD   ADDI  ADDM  ADDB  SUB   SUBI  ',
        'SUBM  SUBB  CAI   CAIL  CAIE  CAILE CAIA  CAIGE CAIN  CAIG  ',
        'CAM   CAML  CAME  CAMLE CAMA  CAMGE CAMN  CAMG  JUMP  JUMPL ',
        'JUMPE JUMPLEJUMPA JUMPGEJUMPN JUMPG SKIP  SKIPL SKIPE SKIPLE',
        'SKIPA SKIPGESKIPN SKIPG AOJ   AOJL  AOJE  AOJLE AOJA  AOJGE ',
        'AOJN  AOJG  AOS   AOSL  AOSE  AOSLE AOSA  AOSGE AOSN  AOSG  ',
        'SOJ   SOJL  SOJE  SOJLE SOJA  SOJGE SOJN  SOJG  SOS   SOSL  ',
        'SOSE  SOSLE SOSA  SOSGE SOSN  SOSG  SETZ  SETZI SETZM SETZB ',
        'AND   ANDI  ANDM  ANDB  ANDCA ANDCAIANDCAMANDCABSETM  SETMI ',
        'SETMM SETMB ANDCM ANDCMIANDCMMANDCMBSETA  SETAI SETAM SETAB ',
        'XOR   XORI  XORM  XORB  IOR   IORI  IORM  IORB  ANDCB ANDCBI',
        'ANDCBMANDCBBEQV   EQVI  EQVM  EQVB  SETCA SETCAISETCAMSETCAB',
        'ORCA  ORCAI ORCAM ORCAB SETCM SETCMISETCMMSETCMBORCM  ORCMI ',
        'ORCMM ORCMB ORCB  ORCBI ORCBM ORCBB SETO  SETOI SETOM SETOB ',
        'HLL   HLLI  HLLM  HLLS  HRL   HRLI  HRLM  HRLS  HLLZ  HLLZI ',
        'HLLZM HLLZS HRLZ  HRLZI HRLZM HRLZS HLLO  HLLOI HLLOM HLLOS ',
        'HRLO  HRLOI HRLOM HRLOS HLLE  HLLEI HLLEM HLLES HRLE  HRLEI ',
        'HRLEM HRLES HRR   HRRI  HRRM  HRRS  HLR   HLRI  HLRM  HLRS  ',
        'HRRZ  HRRZI HRRZM HRRZS HLRZ  HLRZI HLRZM HLRZS HRRO  HRROI ',
        'HRROM HRROS HLRO  HLROI HLROM HLROS HRRE  HRREI HRREM HRRES ',
        'HLRE  HLREI HLREM HLRES TRN   TLN   TRNE  TLNE  TRNA  TLNA  ',
        'TRNN  TLNN  TDN   TSN   TDNE  TSNE  TDNA  TSNA  TDNN  TSNN  ',
        'TRZ   TLZ   TRZE  TLZE  TRZA  TLZA  TRZN  TLZN  TDZ   TSZ   ',
        'TDZE  TSZE  TDZA  TSZA  TDZN  TSZN  TRC   TLC   TRCE  TLZE  ',
        'TRCA  TLCA  TRCN  TLCN  TDC   TSC   TDCE  TSCE  TDCA  TSCA  ',
        'TDCN  TSCN  TRO   TLO   TROE  TLOE  TROA  TLOA  TRON  TLON  ',
        'TDO   TSO   TDOE  TSOE  TDOA  TSOA  TDON  TSON  ***700      ' );
$PAGE init_output
(* INIT OUTPUT sets up the listing file for I/O *)

procedure init_output;
 begin
  case lf_status of
    unopened:    fio_open (listfb, '.LST ' || list_file);
    prev_opened: fio_reopen (listfb)    (* opened in listing pass *)
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
$PAGE cvoct
(* CV OCT converts an integer value to a string giving its octal representation.
   Negative values are signed. *)

type cvoct_string = string[13];

function cvoct (value: machine_word): cvoct_string;

begin
  if value = 0 then
    cvoct := '0'
  else if value = minimum (machine_word) then
    cvoct := '400000000000'
  else begin
    putstring (cvoct, abs (value): 12: o);
    cvoct := substr (cvoct, verify (cvoct, ['0']));
    if value < 0 then
      cvoct := '-' || cvoct;
  end;
end;
$PAGE put_external
(* PUT EXTERNAL writes out an external name. *)

type extsym = packed array[1..6] of char;

procedure put_external (name: extsym);
 var i: 1..6;
     temp: extsym;
 begin
  temp := name;
  for i := 1 to 6 do begin
    if not (temp[i] in [' ', 'A'..'Z', '0'..'9', '$', '.'])
      then temp[i] := '%';
  end;
  for i := 6 downto 1 do exit if temp[i] <> ' ';
  fio_write (listfb, substr (temp, 1, i)); fio_write (listfb, '##');
 end;
$PAGE put_defname
(* PUT DEFNAME formats and outputs the name corresponding to the internal definition
   given by "intdef". *)

procedure put_defname (intdef: def);
  
 type
   code_array = array [sym_def..label_def] of char;
 const
   codes: code_array := ('S', 'K', 'H', 'B', 'D', 'T', 'C', '.', 'L');
  
 begin
  with intdef^ do
    if deftype in [sym_def..label_def] then begin
      fio_write (listfb, codes[deftype] || '.');
      fio_write (listfb, cv_int (defnumber))
    end
    else if deftype = offset_def then begin
      put_defname (reldef);
      if offset >= 0 then fio_write (listfb, '+');  (* otherwise minus sign comes for free *)
      fio_write (listfb, cvoct (offset))
    end
 end;
$PAGE put_field
(* PUT FIELD formats and outputs a relocatable halfword offset, given the value of
   the offset ("off") and the relocation information ("reloc"). *)

procedure put_field (off: unit_range; reloc: rel_syllable);
 begin
  with reloc do begin
    case kind of

      local_sc, parameter_sc:
	if relsym <> nil then fio_write (listfb, cvoct (relsym^.item_addr));

      static_sc:
	if relsym <> nil then begin
	  if relsym^.init_value.kind = no_value
	    then fio_write (listfb, cvoct (relsym^.item_addr + size_init))
	    else fio_write (listfb, cvoct (relsym^.item_addr));
	  fio_write (listfb, '''');
	end;

      external_sc:
        begin
          if relsym^.init_value.kind = no_value
	    then put_external (relsym^.name^.text)
	    else put_external (relsym^.init_value.valp^.str_val);
        end;

      def_sc:
        put_defname (reldef);

      self_rel_sc:
        fio_write (listfb, '.');

      temp_sc:
        begin
          if relval <> nil then
            if relval^.internal
              then fio_write (listfb, '%' || cv_int (relval^.temp_id))
              else fio_write (listfb, '$' || cv_int (relval^.op^.nodeid));
        end;

      runtime_sc:
        put_external (rts_name [relrtsym])

    end (* case *) ;
  end (* with *) ;

  if not ( (reloc.kind in [register_sc, absolute_sc]) or
           ( (reloc.kind in [local_sc, parameter_sc, static_sc, external_sc]) andif
             (reloc.relsym = nil) ) or
             ( (reloc.kind = temp_sc) and (reloc.relval = nil) ) ) then begin
    if off = 0 then return;             (* <-- exit if not constant offset *)
    if off < 400000B
      then fio_write (listfb, '+');
  end;
  if off < 400000B
    then fio_write (listfb, cvoct (off))
    else fio_write (listfb, cvoct (off - 1000000B));
 end;
$PAGE put_symname
(* PUT SYMNAME writes the name of a symbol. *)

procedure put_symname ( s: sym );
 begin
  if s^.name <> nil then
    fio_write (listfb, s^.name^.text)
  else begin
    if s^.kind in [vars, labels] then
      fio_write (listfb, 'V.' || cv_int (s^.id_number))
    else
      fio_write (listfb, 'C.' || cv_int (s^.id_number));
  end;
 end;
$PAGE list
(* LIST produces an assembly language listing of a single code word ("cr"). *)

procedure list (cr: code; var ic: code_address);

  procedure put_ic ( ind: line_index );
   var ic_string: cvoct_string;
   begin
    if listfb.column = 1 then begin
      ic_string := cvoct (ic);
      fio_write (listfb, substr ('000000'||ic_string, length(ic_string)+1, 6));
    end;
    fio_tab (listfb, ind);
   end;

 var i: bit_range;

 static var real_words: record (* overlays a double real and two integers *)
      case boolean of
	false: ( rvalue: real_type );
	true:  ( high, low: machine_word );
     end;

 begin
  with cr^ do begin
    case kind of

      instruction:
        begin
          put_ic (17);
          fio_write (listfb, substr (opcodes [(inst.opcode) div 10], (((inst.opcode) mod 10) * 6) + 1, 6));
          fio_tab (listfb, 25);
          fio_write (listfb, cvoct (inst.acc)); fio_write (listfb, ',');
          if inst.indirect then fio_write (listfb, '@');
          put_field (inst.offset, reloc);
          if inst.index <> 0 then begin
            fio_write (listfb, '(');
            fio_write (listfb, cvoct (inst.index));
            fio_write (listfb, ')');
          end;
          if (reloc.kind in [local_sc, parameter_sc, static_sc, external_sc])
            andif (reloc.relsym <> nil) then begin
            fio_tab (listfb, 41);
            fio_write (listfb, ';   ');
            put_symname (reloc.relsym);
          end
          else if reloc.kind = def_sc then begin
            if reloc.reldef^.deftype = subr_def then begin
              fio_tab (listfb, 41);
              fio_write (listfb, ';   ');
              put_symname (blk_list^[reloc.reldef^.defnumber]^.subr_sym);
              if reloc.reldef^.defined then
                fio_write (listfb, ' = ' || cvoct (reloc.reldef^.addr));
            end
            else if (reloc.reldef^.deftype in [sym_def..extern_def]) andif
               (reloc.reldef^.defined) then begin
              fio_tab (listfb, 41);
              fio_write (listfb, ';   ' || cvoct (reloc.reldef^.addr));
            end;
          end;
          fio_skip (listfb);
          ic := ic + 1;
        end;

      bytepointer:
        begin
          put_ic (17); fio_write (listfb, 'POINT'); fio_tab (listfb, 25);
          fio_write (listfb, cv_int (bptr.s)); fio_write (listfb, ',');
          if bptr.bpindirect then fio_write (listfb, '@');
          put_field (bptr.offset, bpreloc);
          if bptr.index <> 0 then begin
            fio_write (listfb, '(');
            fio_write (listfb, cvoct (bptr.index));
            fio_write (listfb, ')');
          end;
          fio_write (listfb, ','); fio_write (listfb, cv_int (35 - bptr.p));
          if (bpreloc.kind in [local_sc, parameter_sc, static_sc, external_sc])
            andif (bpreloc.relsym <> nil) then begin
            fio_tab (listfb, 41);
            fio_write (listfb, '; ');
            put_symname (bpreloc.relsym);
          end;
          if (bpreloc.kind = def_sc) andif (bpreloc.reldef^.deftype in [sym_def..extern_def]) andif
             (bpreloc.reldef^.defined) then begin
            fio_tab (listfb, 41);
            fio_write (listfb, ';   ' || cvoct (bpreloc.reldef^.addr));
          end;
          fio_skip (listfb);
          ic := ic + 1;
        end;

      stringword:
        begin
          put_ic (17); fio_write (listfb, 'ASCII'); fio_tab (listfb, 25);
          fio_line (listfb, '"' || fwd.str || '"');
          ic := ic + 1;
        end;

      setword:
        begin
          put_ic (17); fio_write (listfb, 'EXP'); fio_tab (listfb, 25);
          fio_write (listfb, '^B');
          for i := 0 to 35 do
            fio_write (listfb, substr ('01', ord (fwd.bits[i])+1, 1));
          fio_skip (listfb);
          ic := ic + 1;
        end;

      fullword,
      realword:
	begin
	  put_ic (17); fio_write (listfb, 'EXP'); fio_tab (listfb, 25);
	  fio_write (listfb, cvoct (fwd.value));
	  fio_tab (listfb, 41); fio_write (listfb, '; ');
	  if kind = fullword then begin
	    fio_write (listfb, '^D');
	    fio_line (listfb, cv_int (fwd.value))
	  end
	  else
	    fio_line (listfb, cv_real (fwd.rvalue));
	  ic := ic + 1;
	end;

      drealword:
	begin
	  put_ic (17); fio_write (listfb, 'EXP'); fio_tab (listfb, 25);
	  fio_line (listfb, cvoct (fwd.value));
	  real_words.high := fwd.value;
	  ic := ic + 1;
	end;

      drealword2:
	begin
	  put_ic (17); fio_write (listfb, 'EXP'); fio_tab (listfb, 25);
	  fio_write (listfb, cvoct (fwd.value));
	  fio_tab (listfb, 41); fio_write (listfb, '; ');
	  real_words.low := fwd.value;
	  fio_line (listfb, cv_real (real_words.rvalue));
	  ic := ic + 1;
	end;

      halfwords:
        begin
          put_ic (17); fio_write (listfb, 'XWD'); fio_tab (listfb, 25);
          put_field (xwd.lh, lreloc);
          fio_write (listfb, ',');
          put_field (xwd.rh, rreloc);
	  if (rreloc.kind in [local_sc, parameter_sc, static_sc, external_sc])
	    andif (rreloc.relsym <> nil) then begin
	    fio_tab (listfb, 41);
	    fio_write (listfb, ';   ,,');
	    put_symname (rreloc.relsym)
	  end;
          fio_skip (listfb);
          ic := ic + 1;
        end;

      defmark:
        begin
          if (listfb.column = 1) and (defname^.deftype <> local_def) then
            fio_skip (listfb); (* no preceding label *)
          put_ic (9); put_defname (defname); fio_write (listfb, ':');           (* leave in middle of line *)
        end;

      origin:
        begin
          fio_skip (listfb);
          fio_tab (listfb, 17); fio_write (listfb, 'RELOC'); fio_tab (listfb, 25);
          fio_line (listfb, cvoct (location));
          ic := location;
        end;

      source:
        begin
          fio_tab (listfb, 41);
          fio_write (listfb, '; LINE '); fio_line (listfb, cv_source_id (stmtid)); 
        end;

	comment:
	  begin
	    if (listfb.column <> 1) then fio_skip (listfb);
	    fio_tab (listfb, 9);
	    fio_line (listfb, ';'); 
	    fio_tab (listfb, 9);
	    fio_write (listfb, ';'); fio_tab (listfb, 17); fio_line (listfb, ctext); 
	    fio_tab (listfb, 9);
	    fio_line (listfb, ';');
	  end;

      deftemp:
	with tempname^ do begin
	  if allocate then begin
	    if listfb.column <> 1 then fio_skip (listfb);
	    fio_tab (listfb, 9);
	    fio_write (listfb, '; ');
	    if internal
	      then fio_write (listfb, '%' || cv_int (temp_id))
	      else fio_write (listfb, '$' || cv_int (op^.nodeid));
	    fio_tab (listfb, 17);
	    fio_write (listfb, '=       ' || cvoct (loc.offset) || '(16)');
	    fio_tab (listfb, 41);
	    fio_line (listfb, '; SIZE = ' || cvoct (size));
	  end;
	end;
  
      asm_label:
	begin
	  put_ic (9);
	  fio_write (listfb, ltext);
	  fio_write (listfb, ':')
	end

    end (* case *) ;
  end (* with *) ;
 end;
$PAGE mac_list
(* MAC LIST generates a Macro-10 style assembly listing of a code records from
   the area given by "arealist". *)

public procedure mac_list (arealist: code_list; init_ic: code_address);
 var cr: code;
     ic: code_address;
 begin
  if list_file = '' then return; (* <---- no listing file *)
  init_output;                          (* force to head of line *)
  cr := arealist.first;
  ic := init_ic;
  while cr <> nil do begin
    list (cr, ic);
    cr := cr^.next;
  end (* while *) ;
  fio_skip (listfb);
 end;
$PAGE mac_pad
(* MAC PAD writes a BLOCK psuedo-op in the macro listing. *)
  
public procedure mac_pad (block_size: unit_range);
  
  begin
    if list_file = '' then return;
    fio_tab (listfb, 17);
    fio_write (listfb, 'BLOCK');
    fio_tab (listfb, 25);
    fio_line (listfb, cvoct (block_size));
    fio_skip (listfb)
  end;
$PAGE mac_header
(* MAC HEADER outputs a header for the assembly listing.  It includes a title and
   appropriate entry statements. *)

public procedure mac_header (text: string);
  
  var
    symbol: sym;
    i: integer;
    temp_str: string [6];
  
  begin
    if list_file = '' then return; (* <---- no listing file *)
    init_output;
    fio_page (listfb);
    fio_tab (listfb, 17); fio_write (listfb, 'TITLE'); fio_tab (listfb, 25);
    fio_line (listfb, root_block^.children^.id^.text);

    fio_tab (listfb, 17); fio_line (listfb, 'TWOSEG');

    fio_tab (listfb, 9); fio_line (listfb, ';');
    fio_tab (listfb, 9); fio_write (listfb, ';'); fio_tab (listfb, 17); 
    fio_line (listfb, text || ', ' || ns_d2 (extr_date (daytime)) || ', ' ||
			lowercase (ns_t1 (extr_time (daytime))));
    fio_tab (listfb, 9); fio_line (listfb, ';');
    
    symbol := root_block^.children^.id_list.first;
    while symbol <> nil do
      with symbol^ do begin
	if (kind = consts) and public_dcl and (init_value.kind = subr_cst) then begin
	  fio_tab (listfb, 17); fio_write (listfb, 'ENTRY'); fio_tab (listfb, 25);
	  temp_str := name^.text;
	  repeat
	    i := search (temp_str, ['_']);
	    if i <> 0 then
	      temp_str [i] := '%'
	  until i = 0;
	  fio_line (listfb, temp_str)
	end;
	symbol := next
      end;
  end;
$PAGE mac_end
(* MAC END terminates the assembly language listing. *)

public procedure mac_end (startdef: def; code_size, const_size, static_size: unit_range);
 begin
  if list_file = '' then return; (* <---- no listing file *)
  init_output;
  fio_line (listfb, ';');
  fio_line (listfb, ';    Code area:      ' || cv_radix (code_size, adr_width) ||
                    ' words (' || cv_int (code_size) || ' decimal)');
  fio_line (listfb, ';    Constant area:  ' || cv_radix (const_size, adr_width) ||
                    ' words (' || cv_int (const_size) || ' decimal)');
  fio_line (listfb, ';    Static area:    ' || cv_radix (static_size, adr_width) ||
                    ' words (' || cv_int (static_size) || ' decimal)');
  fio_line (listfb, ';');
  fio_skip (listfb);
  fio_tab (listfb, 17o_write (listfb, 'END');
  if startdef <> nil then begin
    fio_tab (listfb, 25);
    put_defname (startdef);
  end;
  fio_skip (listfb);
  fio_close (listfb);
  lf_status := prev_opened;
 end.
 O L*