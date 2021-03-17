$TITLE M68EMU - M68000 Emitter Utility Routines
module m68emu options check, special (word);
$PAGE includes
$SYSTEM pascal
$SYSTEM pasist
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM pascv
$PAGE rts_name table
public const
  rts_name : array [rt_symbol] of string[8] =
    ('M.ENTRY',
     'M.DYNTMB',
     'M.DYNTMW',
     'M.STOP',
     'E.ASSERT',
     'E.CASE',
     'E.VALUE',
     'E.FILE',
     'E.PTR',
     'E.INDEX',
     'E.SUBSTR',
     'E.COMPAT',
     'T.OPEN',
     'T.RESET',
     'T.REWRIT',
     'T.GET',
     'T.PUT',
     'T.READ',
     'T.WRITE',
     'T.READLN',
     'T.WRTLN',
     'T.PAGE',
     'T.EOLN',
     'T.EOPAGE',
     'T.CLEAR',
     'T.BREAK',
     'T.EMPTY',
     'Y.INIT',
     'Y.GET',
     'Y.PUT',
     'Y.BREAK',
     'Y.EMPTY',
     'B.INIT',
     'B.READ',
     'B.WRITE',
     'B.BREAK',
     'B.EMPTY',
     'I.EOF',
     'I.CURSOR',
     'I.CLOSE',
     'I.SCRTCH',
     'I.CLOSEA',
     'I.SEEK',
     'I.EXTENT',
     'I.FILNAM',
     'I.FILSTA',
     'I.GENSTA',
     'I.EXTSTA',
     'I.GETSTR',
     'I.PTSTRV',
     'I.PTSTRF',
     'I.PTSTRD',
     'E.MASK',
     'E.UNMASK',
     'E.MSKD',
     'E.PEND',
     'E.MATHST',
     'E.IOST',
     'E.PROGST',
     'E.SPCST',
     'E.SIGNAL',
     'E.RESIG',
     'E.HSET',
     'E.HREST',
     'M.GOMAIN',
     'M.RETURN',
     'G.I4MULT',
     'G.I4DIV',
     'G.I4REM',
     'G.I4PWR',
     'G.BLKMVB',
     'G.BLKMVW',
     'G.TIME',
     'G.RUNTIM',
     'S.BLKCLR',
     'S.SMVSLB',
     'S.SMVDLB',
     'S.SNGZLB',
     'S.SNGNLB',
     'S.RANZLB',
     'S.RANNLB',
     'S.SDIFF2',
     'S.SDIFF3',
     'S.INTER2',
     'S.INTER3',
     'S.UNION2',
     'S.UNION3',
     'S.INCLU',
     'S.EQUAL',
     'F.ADD',	  'D.ADD',
     'F.SUB',	  'D.SUB',
     'F.MUL',	  'D.MUL',
     'F.DIV',	  'D.DIV',
     'F.CMP',     'D.CMP',
     'F.SQT',	  'D.SQT',
     'F.SIN',	  'D.SIN',
     'F.COS',	  'D.COS',
     'F.TAN',	  'D.TAN',
     'F.CTN',	  'D.CTN',
     'F.ASIN',	  'D.ASIN',
     'F.ACOS',	  'D.ACOS',
     'F.ATAN',	  'D.ATAN',
     'F.ATAN2',	  'D.ATAN2',
     'F.LN',	  'D.LN',
     'F.LOG',	  'D.LOG',
     'F.SINH',	  'D.SINH',
     'F.COSH',	  'D.COSH',
     'F.TANH',	  'D.TANH',
     'F.EXP',	  'D.EXP',
     'F.TRUNC',	  'D.TRUNC',
     'F.ROUND',	  'D.ROUND',
     'F.ROUND2',  'D.ROUND2',
     'F.FLOAT',	  'D.FLOAT',
     'F.PWR',     'D.PWR',
     'F.PWRI',    'D.PWRI',
     'F.RANDOM',  'D.RANDOM',
     'F.RANSET',  'D.RANSET',
     'F.DOUBLE',
     'D.SINGLE',
     'H.NEW',
     'H.DISPOS',
     'H.EXTENT',
     'G.DATE',
     'C.MVCF',
     'C.MVCFU',
     'C.MVCFL',
     'C.MVCR',
     'C.MVCRU',
     'C.MVCRL',
     'C.MVFF',
     'C.MVFFU',
     'C.MVFFL',
     'C.MVFR',
     'C.MVFRU',
     'C.MVFRL',
     'C.MVCFP',
     'C.MVCFPU',
     'C.MVCFPL',
     'C.MVCRP',
     'C.MVCRPU',
     'C.MVCRPL',
     'C.MVFFP',
     'C.MVFFPU',
     'C.MVFFPL',
     'C.MVFRP',
     'C.MVFRPU',
     'C.MVFRPL',
     'C.CTCF',
     'C.CTCFU',
     'C.CTCFL',
     'C.CTFF',
     'C.CTFFU',
     'C.CTFFL',
     'C.CTCFP',
     'C.CTCFPU',
     'C.CTCFPL',
     'C.CTFFP',
     'C.CTFFPU',
     'C.CTFFPL',
     'C.CPCF',
     'C.CPFC',
     'C.CPFF',
     'C.IXCF',
     'C.IXFC',
     'C.IXFF',
     'C.SRCO',
     'C.SRCL',
     'C.SRCOU',
     'C.SRCLU',
     'C.SRFO',
     'C.SRFL',
     'C.SRFOU',
     'C.SRFLU',
     'C.VFCO',
     'C.VFCL',
     'C.VFCOU',
     'C.VFCLU',
     'C.VFFO',
     'C.VFFL',
     'C.VFFOU',
     'C.VFFLU' );
$PAGE types
type
  packed_words = packed record
    fill   : 0..15;
    word_1 : uns_word;
    word_2 : uns_word;
  end;
$PAGE cvt_sreal
(*  CVT SREAL will return an integer representing the single-precision
    M68000 representation of a specified real number.  The M68000 single
    real format is:

	 31                   8 7 6     0
	----------------------------------
	|   mantissa           |S|  exp  |
	----------------------------------

    The mantissa is unsigned and normalized (its ms bit is 1).  The S bit
    is 0 if positive, 1 if negative.  The exponent is excess 64.  The
    DEC-10 floating-point format is:

	 0 1     8 9              35
	-----------------------------
	|S|  exp  |  mantissa       |
	-----------------------------

    The exponent is excess 128.  The ms mantissa bit is the complement of
    the sign bit.  The representation of "-X" is the twos-complement of the
    representation of "X".  *)

public procedure cvt_sreal ( x : real_type; var i : integer; prt_errors : boolean );

var x_temp : packed record
	case boolean of
	  false : ( x : real );
	  true  : ( sign : boolean;
		    exp : 0..#o377;		(*  8 bits *)
		    mant_1 : 0..#o77777777;	(* 24 bits *)
		    mant_2 : 0..7 );		(*  3 bits *)
	end;

    i_temp : packed record
	case boolean of
	  false : ( i : integer );
	  true  : ( fill : 0..#hf;
		    mant : 0..#hffffff;		(* 24 bits *)
		    sign : boolean;		(*  1 bit  *)
		    exp : 0..#h7f );		(*  7 bits *)
	end;

    mant, exp : integer;
    sign : boolean;

begin

  (*  Extract the fields of the input real number.  *)

  sign := (x < 0);
  x_temp.x := abs (x);
  exp := x_temp.exp - 128;
  mant := x_temp.mant_1;

  (*  See if the mantissa must be rounded up.  *)

  if x_temp.mant_2 >= 4 then begin
    if mant < maximum (i_temp.mant) then
      mant := mant + 1
    else begin
      mant := #h800000;
      exp := exp + 1;
    end;
  end;

  (*  Construct the M68000 representation and return it.  *)

  exp := exp + 64;
  if exp > 127 then begin
    if prt_errors then
      writeln (tty, '%Real constant too large at ', cv_source_id (cur_source));
    i_temp := (true, 0, maximum (i_temp.mant), sign, maximum (i_temp.exp));
  end
  else if exp < 0 then begin
    if prt_errors and (mant <> 0) then
      writeln (tty, '%Real constant underflow at ', cv_source_id (cur_source));
    i_temp := (true, 0, 0, false, 0);
  end
  else
    i_temp := (true, 0, mant, sign, exp);
  if i_temp.mant >= #h800000 then
    i_temp.fill := #hf;
  i := i_temp.i;
end (* cvt_sreal *);
$PAGE cvt_dreal
(*  CVT DREAL will return an integer representing the double-precision
    M68000 representation of a specified real number.  The M68000 double
    real format is:

	----------------------------------
	|   mantissa ms                  |
	----------------------------------
	 31                   9 8 7     0
	----------------------------------
	|   mantissa ls        |S|  exp  |
	----------------------------------

    The mantissa is unsigned and normalized (its ms bit is 1).  The S bit
    is 0 if positive, 1 if negative.  The exponent is excess 128.  The
    DEC-10 floating-point format is:

	-----------------------------
	|S|  exp  |  mantissa ms    |
	-----------------------------
	 0 1     8 9              35 
	-----------------------------
	|X|  mantissa ls            |
	-----------------------------

    The exponent is excess 128.  The ms mantissa bit is the complement of
    the sign bit.  The representation of "-X" is the twos-complement of the
    representation of "X".  *)

public procedure cvt_dreal ( x : real_type; var i1, i2 : integer );

var x_temp : packed record
	case boolean of
	  false : ( x : real_type );
	  true  : ( sign : boolean;		(*  1 bit  *)
		    exp : 0..#o377;		(*  8 bits *)
		    mant_1 : 0..#o777777777;	(* 27 bits *)
		    fill : boolean;		(*  1 bit  *)
		    mant_2 : 0 .. #o37;		(*  5 bits *)
		    mant_3 : 0..#o37777777;	(* 23 bits *)
		    mant_4 : 0..#o177 );	(*  7 bits *)
	end;

    i_temp : packed record
	case boolean of
	  false : ( i1, i2 : integer );
	  true  : ( fill_1 : 0..#hf;
		    mant_1 : 0..#h7ffffff;	(* 27 bits *)
		    mant_2 : 0..#h1f;		(*  5 bits *)
		    fill_2 : 0..#hf;
		    mant_3 : 0..#h7fffff;	(* 23 bits *)
		    sign : boolean;		(*  1 bit  *)
		    exp : 0..#hff );		(*  8 bits *)
	end;


begin

  (*  Reformat the real number in the M68000 format.  *)

  x_temp.x := abs (x);
  i_temp := (true, 0, x_temp.mant_1, x_temp.mant_2, 0, x_temp.mant_3, (x < 0), x_temp.exp);

  (*  See if rounding is required.  *)

  if x_temp.mant_4 >= #o200 then begin
    if i_temp.mant_3 < maximum (i_temp.mant_3) then
      i_temp.mant_3 := i_temp.mant_3 + 1
    else begin
      i_temp.mant_3 := 0;
      if i_temp.mant_2 < maximum (i_temp.mant_2) then
	i_temp.mant_2 := i_temp.mant_2 + 1
      else begin
	i_temp.mant_2 := 0;
	if i_temp.mant_1 < maximum (i_temp.mant_1) then
	  i_temp.mant_1 := i_temp.mant_1 + 1
	else begin
	  i_temp.mant_1 := #h4000000;
	  if i_temp.exp < maximum (i_temp.exp) then
	    i_temp.exp := i_temp.exp + 1
	  else begin
	    i_temp.mant_1 := maximum (i_temp.mant_1);
	    i_temp.mant_2 := maximum (i_temp.mant_2);
	    i_temp.mant_3 := maximum (i_temp.mant_3);
	  end;
	end;
      end;
    end;
  end;

  if i_temp.mant_1 >= #h4000000 then
    i_temp.fill_1 := #hf;
  if i_temp.mant_3 >= #h4000000 then
    i_temp.fill_2 := #hf;
  i1 := i_temp.i1;
  i2 := i_temp.i2;
end (* cvt_dreal *);
$PAGE get_offset
function get_offset (input_reloc : reloc_value;
                     input_pc_relative_flag : boolean;
                     input_pc : code_address) : code_address;

var
  local_offset : integer;
  local_def : def;
  local_reloc : reloc_value;
  local_relocatable_flag : boolean;

begin
  local_offset := 0;
  local_reloc := input_reloc;
  while local_reloc.kind = def_sc do begin
    local_offset := local_offset + local_reloc.offset;
    local_def := local_reloc.reldef;
    if local_def^.defined then
      local_reloc := local_def^.defval
    else begin
      local_reloc := abs_zero;
      local_offset := 0;
    end (* else *);
  end (* while *);
  local_reloc.offset := local_reloc.offset + local_offset;
  local_relocatable_flag := true;
  get_offset := 0;
  with local_reloc do begin
    case kind of

      absolute_sc :
        begin
	  get_offset := offset;
          local_relocatable_flag := false;
        end;

      code_sc :
        begin
	  get_offset := offset;
        end;

      static_sc,
      local_sc,
      parameter_sc :
        begin
	  get_offset := offset + relsym^.item_addr;
          local_relocatable_flag := false;
        end;

      others : ;

    end (* case *);
    if local_relocatable_flag and
       input_pc_relative_flag then
      get_offset := get_offset - input_pc;
  end (* with input_reloc *);
end (* get_offset *);
$PAGE set_bits
procedure set_bits (var var_word : uns_word;
                    input_last_bit : 0..15;
                    input_contents : uns_word);

begin
  var_word := var_word + (input_contents * (2 ** input_last_bit));
end (* set_bits *);
$PAGE set_areg
procedure set_areg (var var_word : uns_word;
                    input_last_bit : 0..15;
                    input_areg : registers);

begin
  assert (input_areg in addr_reg_set);
  set_bits (var_word, input_last_bit,
	    ord (input_areg) - ord (minimum (addr_regs)));
end (* set_areg *);
$PAGE set_dreg
procedure set_dreg (var var_word : uns_word;
                    input_last_bit : 0..15;
                    input_dreg : registers);

begin
  assert (input_dreg in data_reg_set);
  set_bits (var_word, input_last_bit,
	    ord (input_dreg) - ord (minimum (data_regs)));
end (* set_dreg *);
$PAGE set_quik
procedure set_quik (var var_word : uns_word;
		    input_last_bit : 0..15;
		    input_operand : op_desc);

begin
  with input_operand.cst_part do begin
    assert ((offset >= 1) and (offset <= 8));
    if offset = 8 then
      set_bits (var_word, input_last_bit, 0)
    else
      set_bits (var_word, input_last_bit, offset);
  end (* with input_operand.cst_part *);
end (* set_quik *);
$PAGE set_data
procedure set_data (var var_word : uns_word;
		    input_last_bit : 0..15;
                    input_number_bits : 1..16;
		    input_operand : op_desc;
                    input_pc : code_address);

const
  local_pc_modes : addr_mode_set =
    [pc_displacement_mode, pc_index_w_mode, pc_index_l_mode];

var
  local_record : record
    case boolean of
      true  : (int : integer);
      false : (words : packed_words);
  end;

begin
  with input_operand, local_record do begin
    int := get_offset (cst_part, mode in local_pc_modes, input_pc);
    assert (abs (int) <= 2 ** input_number_bits);
    with words do begin
      word_2 := word_2 mod (2 ** input_number_bits);
      set_bits (var_word, input_last_bit, word_2);
    end (* with words *);
  end (* with input_operand, local_record *);
end (* set_data *);
$PAGE set_disp
procedure set_disp (var var_array : array [1..5] of uns_word;
                    var var_length : 1..5;
                    input_operand : op_desc;
                    input_pc : code_address);

const
  local_pc_modes : addr_mode_set =
    [pc_displacement_mode, pc_index_w_mode, pc_index_l_mode];

var
  local_record : record
    case boolean of
      true  : (int : integer);
      false : (words : packed_words);
  end;

begin
  with input_operand, local_record do begin
    int := get_offset (cst_part, mode in local_pc_modes, input_pc);
    assert (int <> 0);
    if value_size = size_byte then begin
      assert ((int >= minimum (byte)) and (int <= maximum (byte)));
      words.word_2 := words.word_2 mod 256;
      set_bits (var_array[var_length], 0, words.word_2);
    end
    else begin
      assert ((int >= minimum (word)) and (int <= maximum (word)));
      var_length := var_length + 1;
      set_bits (var_array[var_length], 0, words.word_2);
    end;
  end (* with input_operand, local_record *);
end (* set_disp *);
$PAGE set_long
procedure set_long (var var_array : array [1..5] of uns_word;
		    var var_length : 1..5;
		    input_long_flag : boolean;
		    input_operand : op_desc;
                    var var_pc : code_address);

const
  local_pc_modes : addr_mode_set =
    [pc_displacement_mode, pc_index_w_mode, pc_index_l_mode];

var
  local_record : record
    case boolean of
      true  : (int : integer);
      false : (words : packed_words);
  end;

begin
  with input_operand, local_record do begin
    int := get_offset (cst_part, mode in local_pc_modes, var_pc);
    var_length := var_length + 1;
    var_pc := var_pc + 2;
    with words do begin
      if input_long_flag then begin
	set_bits (var_array[var_length], 0, word_1);
	var_length := var_length + 1;
        var_pc := var_pc + 2;
	set_bits (var_array[var_length], 0, word_2);
      end
      else begin
        assert ((int >= minimum (word)) and (int <= maximum (uns_word)));
	set_bits (var_array[var_length], 0, word_2);
      end;
    end (* with words *);
  end (* with input_operand, local_record *);
end (* set_long *);
$PAGE set_indx
procedure set_indx (var var_array : array [1..5] of uns_word;
		    var var_length : 1..5;
		    input_long_flag : boolean;
		    input_operand : op_desc;
                    var var_pc : code_address);

begin
  var_length := var_length + 1;
  with input_operand do begin
    if index_reg in [d0..d7] then
      set_dreg (var_array[var_length], 12, index_reg)
    else begin
      set_bits (var_array[var_length], 15, 1);
      set_areg (var_array[var_length], 12, index_reg);
    end;
    set_bits (var_array[var_length], 11, ord (input_long_flag));
    set_data (var_array[var_length], 0, 8, input_operand, var_pc);
    var_pc := var_pc + 2;
  end (* with input_operand *);
end (* set_indx *);
$PAGE set_word
procedure set_word (var var_array : array [1..5] of uns_word;
                    var var_length : 1..5;
                    input_operand : op_desc;
                    var var_pc : code_address);

const
  local_word_disp_modes : addr_mode_set =
    [displacement_mode, abs_w_mode, pc_displacement_mode];
  local_long_disp_modes : addr_mode_set =
    [abs_l_mode];
  local_word_indexed_modes : addr_mode_set =
    [index_w_mode, pc_index_w_mode];
  local_long_indexed_modes : addr_mode_set =
    [index_l_mode, pc_index_l_mode];

begin
  with input_operand do begin
    if mode in local_word_disp_modes then
      set_long (var_array, var_length, false, input_operand, var_pc);
    if mode in local_long_disp_modes then
      set_long (var_array, var_length, true, input_operand, var_pc);
    if mode in local_word_indexed_modes then
      set_indx (var_array, var_length, false, input_operand, var_pc);
    if mode in local_long_indexed_modes then
      set_indx (var_array, var_length, true, input_operand, var_pc);
    if mode = immediate_mode then
      set_long (var_array, var_length, 
		value_size = size_long, input_operand, var_pc);
  end (* with input_operand *);
end (* set_word *);
$PAGE set_xreg
procedure set_xreg (var var_word : uns_word;
                    input_last_bit : 0..15;
                    input_operand : op_desc);

begin
  with input_operand do begin
    case mode of 

      areg_mode :
          set_areg (var_word, input_last_bit, reg);

      dreg_mode :
          set_dreg (var_word, input_last_bit, reg);

      indirect_mode :
          set_areg (var_word, input_last_bit, reg);

      predecrement_mode :
          set_areg (var_word, input_last_bit, reg);

      postincrement_mode :
          set_areg (var_word, input_last_bit, reg);

      displacement_mode :
          set_areg (var_word, input_last_bit, reg);

      index_w_mode :
          set_areg (var_word, input_last_bit, reg);

      index_l_mode :
          set_areg (var_word, input_last_bit, reg);

      abs_w_mode :
	  set_bits (var_word, input_last_bit, 0);

      abs_l_mode :
	  set_bits (var_word, input_last_bit, 1);

      pc_displacement_mode :
	  set_bits (var_word, input_last_bit, 2);

      pc_index_w_mode :
          set_bits (var_word, input_last_bit, 3);

      pc_index_l_mode :
          set_bits (var_word, input_last_bit, 3);

      immediate_mode :
          set_bits (var_word, input_last_bit, 4);    null_mode : ;

    end (* case *);
  end (* with input_operand *);
end (* set_xreg *);
$PAGE set_mode
procedure set_mode (var var_word : uns_word;
                    input_last_bit : 0..15;
                    input_operand : op_desc);

const
  mode_table : array [addr_mode] of 0..7 =
    (1, 0, 2, 4, 3, 5, 6, 6, 7, 7, 7, 7, 7, 7, 0);

begin
  set_bits (var_word, input_last_bit, mode_table[input_operand.mode]);
end (* set_mode *);
$PAGE set_addr
procedure set_addr (var var_word : uns_word;
		    input_operand : op_desc);

begin
  set_mode (var_word, 3, input_operand);
  set_xreg (var_word, 0, input_operand);
end (* set_addr *);
$PAGE get_code
(* GET_CODE - produces the actual object code that represents
              the given instruction.  Since up to 5 words of
              object code can be generated for an instruction,
              the code is returned in a 5 element array, with
              a variable called var_print_len set to indicate
              how many words are actually relevant for this
              instruction. *)

public procedure get_code (input_code_ptr : code;
			   var var_array : array [1..5] of uns_word;
			   var var_print_len : 1..5;
                           var var_absolute_len : 1..5;
                           var var_op_1_flag : boolean;
                           var var_op_2_flag : boolean);

type
  op_mode_array = array [op_sizes] of 0..7;

  (* FORMATS is an enumerated type that describes the object format
     associated with each element of the type SPECIFIC_OPCODES.
     Each format is made up of three elements: a prefix, an addressing
     mode indicator, and a suffix.  The prefix indicates the length
     of the instruction.  The prefixes have the following meanings:

       two_      = two byte instruction
       two_four_ = two or four byte instruction, depending on the
		   size of the displacement
       four_     = four byte instruction
       four_six_ = four or six byte instruction, depending on the
                   size of the data

     The addressing indicators are taken from the middle syllable of
     the corresponding element of SPECIFIC_OPCODES.

     The suffixes are composed of single letters that describe the
     fields of the object code.  The suffixes have the following
     meanings:

       b = bit number
       c = condition
       d = data
       e = effective address
       i = i/r bit
       k = constant
       m = register list mask
       n = count/register
       o = op-mode
       r = register
       s = size
       t = direction
       x = displacement

     Instructions that are more than one word in length have a
     suffix for each additional word, separated by an underscore. *)

  formats = (two_dm_kroe, two_ma_kroe, two_md_kroe, two_qm_kdkse,
             two_dd_kntsikr, two_m_ktke, two_qd_kntsikr,
             two_dd_krke, two_dm_krke, two_ma_krke, two_md_krke,
	     two_m_kse, two_mm_krkskr,
	     two_aa_krkor, two_da_krkor, two_dd_krkor,
             two_d_kokr, two_m_ke, two_mm_ksroor, two_ma_ksrkor,
             two_bd_krkd, two_x_k, two_m_kcke, two_a_kr, two_d_kr,

             two_four_p_kcx_x, two_four_p_kx_x,

             four_wd_ke_b, four_wm_ke_b, four_dw_kckr_x, four_wa_kr_x, 
	     four_mw_ktkse_m, four_wm_ktkse_m,

             four_six_im_kse_dd_d);

  (* INST_FORMAT_RECORD is used to store information for each opcode
     of type SPECIFIC_OPCODES, including its first word of object code,
     and its object code format type. *)

  inst_format_record = packed record
    constant_value : uns_word;
    absolute_len : 1..5;
    op_1_flag : boolean;
    op_2_flag : boolean;
    format_kind : formats
  end;

const
  opcodes : packed array [specific_opcodes] of inst_format_record :=
    (( (* ADD *)   #HD000, 1, false, true,  two_dm_kroe),
     ( (* ADDI *)  #H0600, 1, true,  true,  four_six_im_kse_dd_d),
     ( (* ADDA *)  #HD000, 1, true,  false, two_ma_kroe),
     ( (* ADD *)   #HD000, 1, true,  false, two_md_kroe),
     ( (* ADDQ *)  #H5000, 1, false, true,  two_qm_kdkse),
     ( (* AND *)   #HC000, 1, false, true,  two_dm_kroe),
     ( (* ANDI *)  #H0200, 1, true,  true,  four_six_im_kse_dd_d),
     ( (* AND *)   #HC000, 1, true,  false, two_md_kroe),
     ( (* ASL *)   #HE120, 1, false, false, two_dd_kntsikr),
     ( (* ASL *)   #HE1C0, 1, false, true,  two_m_ktke),
     ( (* ASL *)   #HE100, 1, false, false, two_qd_kntsikr),
     ( (* ASR *)   #HE020, 1, false, false, two_dd_kntsikr),
     ( (* ASR *)   #HE0C0, 1, false, true,  two_m_ktke),
     ( (* ASR *)   #HE000, 1, false, false, two_qd_kntsikr),
     ( (* BCC *)   #H6000, 1, false, true,  two_four_p_kcx_x),
     ( (* BCHG *)  #H0140, 1, false, false, two_dd_krke),
     ( (* BCHG *)  #H0140, 1, false, true,  two_dm_krke),
     ( (* BCHG *)  #H0840, 2, false, false, four_wd_ke_b),
     ( (* BCHG *)  #H0840, 2, false, true,  four_wm_ke_b),
     ( (* BCLR *)  #H0180, 1, false, false, two_dd_krke),
     ( (* BCLR *)  #H0180, 1, false, true,  two_dm_krke),
     ( (* BCLR *)  #H0880, 2, false, false, four_wd_ke_b),
     ( (* BCLR *)  #H0880, 2, false, true,  four_wm_ke_b),
     ( (* BSET *)  #H01C0, 1, false, false, two_dd_krke),
     ( (* BSET *)  #H01C0, 1, false, true,  two_dm_krke),
     ( (* BSET *)  #H08C0, 2, false, false, four_wd_ke_b),
     ( (* BSET *)  #H08C0, 2, false, true,  four_wm_ke_b),
     ( (* BSR *)   #H6100, 1, false, true,  two_four_p_kx_x),
     ( (* BTST *)  #H0100, 1, false, false, two_dd_krke),
     ( (* BTST *)  #H0100, 1, false, true,  two_dm_krke),
     ( (* BTST *)  #H0800, 2, false, false, four_wd_ke_b),
     ( (* BTST *)  #H0800, 2, false, true,  four_wm_ke_b),
     ( (* CLR *)   #H4200, 1, false, true,  two_m_kse),
     ( (* CMPI *)  #H0C00, 1, true,  true,  four_six_im_kse_dd_d),
     ( (* CMPA *)  #HB000, 1, true,  false, two_ma_kroe),
     ( (* CMP *)   #HB000, 1, true,  false, two_md_kroe),
     ( (* CMPM *)  #HB108, 1, false, false, two_mm_krkskr),
     ( (* DBCC *)  #H50C8, 2, false, false, four_dw_kckr_x),
     ( (* DIVS *)  #H81C0, 1, true,  false, two_md_krke),
     ( (* DIVU *)  #H80C0, 1, true,  false, two_md_krke),
     ( (* EOR *)   #HB000, 1, false, true,  two_dm_kroe),
     ( (* EORI *)  #H0A00, 1, true,  true,  four_six_im_kse_dd_d),
     ( (* EXG *)   #HC100, 1, false, false, two_aa_krkor),
     ( (* EXG *)   #HC100, 1, false, false, two_da_krkor),
     ( (* EXG *)   #HC100, 1, false, false, two_dd_krkor),
     ( (* EXT *)   #H4800, 1, false, false, two_d_kokr),
     ( (* JMP *)   #H4EC0, 1, false, true,  two_m_ke),
     ( (* JSR *)   #H4E80, 1, false, true,  two_m_ke),
     ( (* LEA *)   #H41C0, 1, true,  false, two_ma_krke),
     ( (* LINK *)  #H4E50, 2, false, false, four_wa_kr_x),
     ( (* LSL *)   #HE128, 1, false, false, two_dd_kntsikr),
     ( (* LSL *)   #HE3C0, 1, false, true,  two_m_ktke),
     ( (* LSL *)   #HE108, 1, false, false, two_qd_kntsikr),
     ( (* LSR *)   #HE028, 1, false, false, two_dd_kntsikr),
     ( (* LSR *)   #HE2C0, 1, false, true,  two_m_ktke),
     ( (* LSR *)   #HE008, 1, false, false, two_qd_kntsikr),
     ( (* MOVEQ *) #H7000, 1, false, false, two_bd_krkd),
     ( (* MOVEA *) #H0040, 1, true,  false, two_ma_ksrkor),
     ( (* MOVE *)  #H0000, 1, true,  true,  two_mm_ksroor),
     ( (* MOVEM *) #H4C80, 2, true,  false, four_mw_ktkse_m),
     ( (* MOVEM *) #H4880, 2, false, true,  four_wm_ktkse_m),
     ( (* MULS *)  #HC1C0, 1, true,  false, two_md_krke),
     ( (* MULU *)  #HC0C0, 1, true,  false, two_md_krke),
     ( (* NEG *)   #H4400, 1, false, true,  two_m_kse),
     ( (* NOP *)   #H4E71, 1, false, false, two_x_k),
     ( (* NOT *)   #H4600, 1, false, true,  two_m_kse),
     ( (* OR *)    #H8000, 1, false, true,  two_dm_kroe),
     ( (* ORI *)   #H0000, 1, true,  true,  four_six_im_kse_dd_d),
     ( (* OR *)    #H8000, 1, true,  false, two_md_kroe),
     ( (* PEA *)   #H4840, 1, false, true,  two_m_ke),
     ( (* ROL *)   #HE138, 1, false, false, two_dd_kntsikr),
     ( (* ROL *)   #HE7C0, 1, false, true,  two_m_ktke),
     ( (* ROL *)   #HE118, 1, false, false, two_qd_kntsikr),
     ( (* ROR *)   #HE038, 1, false, false, two_dd_kntsikr),
     ( (* ROR *)   #HE6C0, 1, false, true,  two_m_ktke),
     ( (* ROR *)   #HE018, 1, false, false, two_qd_kntsikr),
     ( (* RTS *)   #H4E75, 1, false, false, two_x_k),
     ( (* SCC *)   #H50C0, 1, false, true,  two_m_kcke),
     ( (* SUB *)   #H9000, 1, false, true,  two_dm_kroe),
     ( (* SUBI *)  #H0400, 1, true,  true,  four_six_im_kse_dd_d),
     ( (* SUBA *)  #H9000, 1, true,  false, two_ma_kroe),
     ( (* SUB *)   #H9000, 1, true,  false, two_md_kroe),
     ( (* SUBQ *)  #H5100, 1, false, true,  two_qm_kdkse),
     ( (* SWAP *)  #H4840, 1, false, false, two_d_kr),
     ( (* TST *)   #H4A00, 1, false, true,  two_m_kse),
     ( (* UNLK *)  #H4E58, 1, false, false, two_a_kr));

  table_1 : op_mode_array = (4, 5, 6, 7, 7);
  table_2 : op_mode_array = (0, 3, 7, 0, 0);
  table_3 : op_mode_array = (0, 1, 2, 7, 7);
  table_4 : op_mode_array = (0, 1, 2, 3, 3);
  table_5 : op_mode_array = (0, 2, 3, 0, 0);
  table_6 : op_mode_array = (1, 3, 2, 0, 0);

var
  local_pc : code_address;
  local_op_1_flag : boolean;
  local_op_2_flag : boolean;

begin
  local_pc := input_code_ptr^.addr + 2;
  with input_code_ptr^.inst do begin
    with opcodes[opcode] do begin
      var_array[1] := constant_value;
      var_array[2] := 0;
      var_array[3] := 0;
      var_array[4] := 0;
      var_array[5] := 0;
      var_print_len := 1;
      var_absolute_len := absolute_len;
      var_op_1_flag := op_1_flag;
      var_op_2_flag := op_2_flag;
      local_op_1_flag := op_1_flag;
      local_op_2_flag := op_2_flag;
      case format_kind of

        two_dm_kroe :
          begin
            set_dreg (var_array[1], 9, operands[1].reg);
            set_bits (var_array[1], 6, table_1[operands[2].value_size]);
            set_addr (var_array[1], operands[2]);
          end;

        two_ma_kroe :
          begin
            set_areg (var_array[1], 9, operands[2].reg);
            set_bits (var_array[1], 6, table_2[operands[1].value_size]);
            set_addr (var_array[1], operands[1]);
          end;

        two_md_kroe :
          begin
            set_dreg (var_array[1], 9, operands[2].reg);
            set_bits (var_array[1], 6, table_3[operands[2].value_size]);
            set_addr (var_array[1], operands[1]);
          end;

        two_qm_kdkse :
          begin
            set_quik (var_array[1], 9, operands[1]);
            set_bits (var_array[1], 6, table_4[operands[2].value_size]);
            set_addr (var_array[1], operands[2]);
          end;

        two_dd_kntsikr :
          begin
            set_dreg (var_array[1], 9, operands[1].reg);
            set_bits (var_array[1], 6, table_4[operands[2].value_size]);
            set_dreg (var_array[1], 0, operands[2].reg);
          end;

        two_m_ktke :
          begin
            set_addr (var_array[1], operands[2]);
          end;

        two_qd_kntsikr :
          begin
            set_quik (var_array[1], 9, operands[1]);
            set_bits (var_array[1], 6, table_4[operands[2].value_size]);
            set_dreg (var_array[1], 0, operands[2].reg);
          end;

        two_dd_krke :
          begin
            set_dreg (var_array[1], 9, operands[1].reg);
            set_addr (var_array[1], operands[2]);
          end;

        two_dm_krke :
          begin
            set_dreg (var_array[1], 9, operands[1].reg);
            set_addr (var_array[1], operands[2]);
          end;

        two_ma_krke :
          begin
            set_areg (var_array[1], 9, operands[2].reg);
            set_addr (var_array[1], operands[1]);
          end;

        two_md_krke :
          begin
            set_dreg (var_array[1], 9, operands[2].reg);
            set_addr (var_array[1], operands[1]);
          end;

        two_m_kse :
          begin
            set_bits (var_array[1], 6, table_4[operands[2].value_size]);
            set_addr (var_array[1], operands[2]);
          end;

        two_mm_krkskr :
          begin
            set_areg (var_array[1], 9, operands[1].reg);
            set_bits (var_array[1], 6, table_4[operands[2].value_size]);
            set_areg (var_array[1], 0, operands[2].reg);
          end;

        two_aa_krkor :
          begin
            set_areg (var_array[1], 9, operands[1].reg);
            set_bits (var_array[1], 3, 9);
            set_areg (var_array[1], 0, operands[2].reg);
          end;

        two_da_krkor :
          begin
            set_dreg (var_array[1], 9, operands[1].reg);
            set_bits (var_array[1], 3, 17);
            set_areg (var_array[1], 0, operands[2].reg);
          end;

        two_dd_krkor :
          begin
            set_dreg (var_array[1], 9, operands[1].reg);
            set_bits (var_array[1], 3, 8);
            set_dreg (var_array[1], 0, operands[2].reg);
          end;

        two_d_kokr :
          begin
            set_bits (var_array[1], 6, table_5[operands[2].value_size]);
            set_dreg (var_array[1], 0, operands[2].reg);
          end;

        two_m_ke :
          begin
            set_addr (var_array[1], operands[2]);
          end;

        two_mm_ksroor :
          begin
            set_bits (var_array[1], 12, table_6[operands[2].value_size]);
            set_xreg (var_array[1], 9, operands[2]);
            set_mode (var_array[1], 6, operands[2]);
            set_addr (var_array[1], operands[1]);
          end;

        two_ma_ksrkor :
          begin
            set_bits (var_array[1], 12, table_6[operands[1].value_size]);
            set_areg (var_array[1], 9, operands[2].reg);
            set_addr (var_array[1], operands[1]);
          end;

        two_bd_krkd :
          begin
            set_dreg (var_array[1], 9, operands[2].reg);
            set_data (var_array[1], 0, 8, operands[1], local_pc);
          end;

        two_x_k : ;

        two_m_kcke :
          begin
            set_bits (var_array[1], 8, ord (ccode));
            set_addr (var_array[1], operands[2]);
          end;

        two_a_kr :
          begin
            set_areg (var_array[1], 0, operands[2].reg);
          end;

        two_d_kr :
          begin
            set_dreg (var_array[1], 0, operands[2].reg);
          end;

        two_four_p_kcx_x :
          begin
            set_bits (var_array[1], 8, ord (ccode));
            set_disp (var_array, var_print_len, operands[2], local_pc);
            local_op_2_flag := false;
          end;

        two_four_p_kx_x :
          begin
            set_disp (var_array, var_print_len, operands[2], local_pc);
            local_op_2_flag := false;
          end;

        four_wd_ke_b :
          begin
            var_print_len := 2;
            set_addr (var_array[1], operands[2]);
            set_data (var_array[2], 0, 16, operands[1], local_pc);
          end;

        four_wm_ke_b :
          begin
            var_print_len := 2;
            set_addr (var_array[1], operands[2]);
            set_data (var_array[2], 0, 16, operands[1], local_pc);
          end;

        four_dw_kckr_x :
          begin
            var_print_len := 2;
            set_bits (var_array[1], 8, ord (ccode));
            set_dreg (var_array[1], 0, operands[1].reg);
            set_data (var_array[2], 0, 16, operands[2], local_pc);
          end;

        four_wa_kr_x :
          begin
            var_print_len := 2;
            set_areg (var_array[1], 0, operands[1].reg);
            set_data (var_array[2], 0, 16, operands[2], local_pc);
          end;

        four_mw_ktkse_m :
          begin
            var_print_len := 2;
            set_bits (var_array[1], 6, 
		      ord (operands[1].value_size = size_long));
            set_addr (var_array[1], operands[1]);
            set_data (var_array[2], 0, 16, operands[2], local_pc);
          end;

        four_wm_ktkse_m :
          begin
            var_print_len := 2;
            set_bits (var_array[1], 6, 
		      ord (operands[2].value_size = size_long));
            set_addr (var_array[1], operands[2]);
            set_data (var_array[2], 0, 16, operands[1], local_pc);
          end;

        four_six_im_kse_dd_d :
          begin
            set_bits (var_array[1], 6, table_4[operands[2].value_size]);
            set_addr (var_array[1], operands[2]);
          end;

      end (* case *);
      if local_op_1_flag then
        set_word (var_array, var_print_len, operands[1], local_pc);
      if local_op_2_flag then
        set_word (var_array, var_print_len, operands[2], local_pc);
    end (* with opcodes[opcode] *);
  end (* with input_code_ptr^.inst *);
end (* get_code *);
$PAGE stc_offset
(*  STC OFFSET will return the address, relative to the start of the static
    static section, of a static variable or condition symbol.  This routine
    is necessary because ITEM_ADDR fields in static symbols are relative
    to the initialized, uninitialized, or condition static subsection,
    rather than to the entire static section.

    The organization of the static section is:
	(1) Initialized variables.
	(2) Condition cells.
	(3) Uninitialized variables.  *)

public function stc_offset ( s : sym ) : unit_range;

begin
  with s^ do begin
    if kind = conditions then
      stc_offset := item_addr + size_init
    else begin
      assert ( (kind = vars) andif (dcl_class = static_sc) );
      if init_value.kind = no_value then
	stc_offset := item_addr + size_init + size_cond
      else
	stc_offset := item_addr;
    end;
  end;
end (* stc_offset *).
   Z #