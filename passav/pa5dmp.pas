$TITLE pasdmp -- symbol table dump routines
$options enable (6)
$LENGTH 42

(*   +--------------------------------------------------------------+
     |                                                              |
     |                P A S D M P   -   P a s s   2                 |
     |                - - - - - - - - - - - - - - -                 |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  This is the  debugging  dump  module.  It  contains  a
        collection  of  procedures  to  dump  portions  of the symbol
        table, intermediate form code, etc., to a .DMP file.
     
     ENTRY POINTS:
     
        dmptuples   produces  a  formatted  dump  of  a  portion   of
                    intermediate  form  code.  The  dump contains one
                    entry for each node  in  the  intermediate  form,
                    with  the tree structure of expressions indicated
                    by indentation.
     
        dmpcgraph   dumps the call graph for an  entire  module.  The
                    dump  contains  an  entry  for  each block in the
                    module, identifying the block and its owner,  and
                    listing the blocks that it calls.
     
        dmpfgraph   dumps  the  flow  graph for a block.  The dump is
                    similar to a dmptuples dump, except that only the
                    label  and  jump  nodes  for each basic block are
                    generated.
     
        dmpvlsyms   will produce a list of all the variable and label
                    symbols  in the compilation, with their symbol id
                    numbers.
     
        dmpset      will dump a set vector.
     
        dmpfrtable  will  print  the   list   of   formal   reference
                    expressions.
     
        dmpdominators
                    will  print  the  dominator tree of a flow graph.
                    It will also  note  whether  the  flow  graph  is
                    reducible.
     
        dmpwenv     dumps  the  weak  environment information for the
                    current block.
     
        dmprefcounts
                    dumps the reference allocation  counts  for  each
                    basic block.
     
        dmpstorage  prints  the  allocated  storage addresses for all
                    the  variable  and  parameter  symbols   in   the
                    program.
     
        dmp_close   closes the dump file if it is open.
     
     CHANGES:
                        PASDMP   split  into  PA1DMP  and  PA2DMP  to
                        facilitate multi-pass implementation  of  the
                        compiler.
                        Added routine dmp_close.
     
     ---------------------------------------------------------------- *)
$PAGE includes, externals, forwards

$OPTIONS nocheck, special

$INCLUDE pascal.inc
$INCLUDE pasfil.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE pasopt.typ
$INCLUDE pascv.inc
$INCLUDE passw.inc
$INCLUDE pasifu.inc
$INCLUDE passet.inc
$INCLUDE pasbnf.nam
$PAGE cv_oct
(* CV_OCT returns a fixed length string containing the octal representation of
   an integer.  Leading zeros are included. *)

function cv_oct ( val: integer; columns: line_index ): parm_string;

var
    i: 0 .. 12;
    digits: packed array [1..12] of char;
    a: integer;

begin
  a := val;
  for i := columns downto 1 do begin
    digits [i] := substr ('01234567', (a mod 8) + 1, 1);
    a := a div 8;
  end;
  cv_oct := substr (digits, 1, columns);
end; (* cv_oct *)
$PAGE start_dump

(*  START_DUMP will open the dump file if it is not currently open, and will
    start a new page on the dump file if the file is open and the 'separator'
    parameter is true.  A new page is simply a line of dashes if TTY has been
    specified as a dump switch.  *)


procedure tty_page ( var fb: file_block );
begin
  fio_skip (fb);
  fio_skip (fb);
  fio_line (fb, '----------------------------------------');
  fio_skip (fb);
  fio_skip (fb);
end;


procedure start_dump ( separator: boolean );
begin
  if df_status = unopened then begin
    fio_open (dumpfb, main_file || ' DSK:[,].DMP');
    if switch (prog_options.dump_switches, 'TTY') then begin
      dumpfb.width := 80;
      dumpfb.new_page := fio_proc (tty_page);
    end
    else begin
      dumpfb.width := 100;
      dumpfb.plength := 44;
    end;
  end
  else begin
    if df_status = prev_opened then
      fio_reopen (dumpfb);
    if switch (prog_options.dump_switches, 'TTY')
      then dumpfb.new_page := fio_proc (tty_page)
      else dumpfb.new_page := fio_proc (fio_eject);
    dumpfb.page_header := fio_proc (fio_nop);
    if separator then
      fio_page (dumpfb);
  end;
  df_status := now_open;
end (* start_dump *);
$PAGE text constants

type vki_type = array [real_cst..record_cst] of string [10];

const val_kind_id: vki_type :=
     (  'REAL', 'STRING', 'SET', 'POINTER', 'ARRAY', 'RECORD'  );


type tpki_type = array [type_kind] of string [10];

const tp_kind_id: tpki_type :=
     (  'SCALAR', 'BOOLEAN', 'INTEGER', 'REAL', 'CHAR', 'SET', 'POINTER',
	'FILE', 'STRING', 'ARRAY', 'RECORD', 'VARIANT', 'TAG', 'PROCEDURE',
	'FUNCTION', 'COND PROC', 'COND FUNC', '<UNKNOWN>', '<INDIRECT>'  );
$PAGE sym_text

(*  SymName will return the name of a symbol, if the symbol has a name.  If
    the symbol is a heap class, then 'Class of <type>' will be returned.
    Otherwise, the symbol's address will be returned, with an '@'.  *)


function type_name ( t: typ ): line_string;
  forward;


function sym_text ( s: sym ): line_string;

begin
  if s = nil then
    sym_text := '*NIL*'
  else
  with s^ do
    if name <> nil then
      with name^ do
	sym_text := substr (text, 1, len)
    else if dcl_class = dynamic_sc then
      sym_text := 'Heap class of ' || type_name (type_desc)
    else if dcl_class = fileblk_sc then
      sym_text := 'File class of ' || type_name (type_desc)
    else if kind in [vars, labels] then
      sym_text := 'V.' || cv_int (id_number)
    else
      sym_text := 'C.' || cv_int (id_number);
end (* sym_text *);
$PAGE type_name

(*  TypeName is called with a pointer to a type node, and returns the
    type name, if it has a name; 'TYPE OF <symbol>', if it is an
    unnamed scalar type; and the octal pointer value if all else fails.  *)


function type_name (* t: typ ): line_string *);

begin
  if t = nil then
    type_name := '*NIL*'
  else
    with t^ do begin
      if type_id <> nil then begin
	type_name := sym_text (type_id);
	if type_id^.kind <> types then
	  type_name := 'Type of ' || type_name;
      end
      else
	type_name := '@' || cv_oct (ord(t), 6);
    end (* with t^ *);
end (* type_name *);
$PAGE block_id

(*  BlockId will return the block number, level, type, and name of a block node.  *)


function block_id ( block: blk ): line_string;

type
    blk_names = array [block_kind] of string [10];

const
    block_kind: blk_names =
     (  'ROOT', 'PROGRAM', 'MODULE', 'SUBROUTINE', 'DATA MODULE', 'HANDLER',
	'CLASS', 'EXTERNAL'  );

begin
  if block = nil then
    block_id := '*NIL*'
  else
    with block^ do begin
      block_id := cv_int (number) || ' AT LEVEL ' || cv_int (level) ||
		  ':  ' || block_kind [kind] || ' ';
      case kind of

	root_blk:
	  ;

	program_blk,
	module_blk,
	data_blk:
	  if id <> nil then
	    with id^ do
	      block_id := block_id || substr (text, 1, len);

	subr_blk:
	  block_id := block_id || sym_text (subr_sym);

	class_blk:
	  block_id := block_id || type_name (class_type);

	others:
	  (* no action *)

      end (* case kind *);
    end (* with case^ *);
end (* block_id *);
$PAGE prt_title

(*  PrtTitle is called with a string.  It prints the string, centered,
    underlined, and followed by a blank line.  In addition, if the dump
    mode is not "TTY", then the new-page routine is set to print the
    title and a page number at the start of each new page.  *)


var saved_title: line_string;


procedure title_page ( var fb: file_block );
var col: line_index;
begin
  fio_write (fb, saved_title);
  col := fb.width - 5 - width(fb.pageno);
  if fb.column >= col
    then fio_skip (fb)
    else fio_tab (fb, col+1);
  fio_line (fb, 'PAGE ' || cv_int(fb.pageno));
  fio_skip (fb);
end;


procedure prt_title ( ttl: line_string );

var
    dashes: line_string;
    i: line_index;

begin
  start_dump (true);
  i := search (ttl, ['$']);
  if i = 0
    then saved_title := ttl
    else saved_title := substr (ttl, 1, i-1) || block_id (cur_block) || substr (ttl, i+1);
  i := max (0, (dumpfb.width - length(saved_title)) div 2) + 1;
  dumpfb.c_column := 0;
  fio_tab (dumpfb, i);
  fio_line (dumpfb, saved_title);
  fio_tab (dumpfb, i);
  dumpfb.page_header := fio_proc (title_page);
  dumpfb.pageno := 1;
  dashes := saved_title;
  for i := 1 to length (dashes) do
    if dashes [i] <> ' ' then
      dashes [i] := '-';
  fio_line (dumpfb, dashes);
  fio_skip (dumpfb);
end (* prt_title *);
$PAGE simple_cst

(*  SimpleConst will return:
	for a scalar constant, its integer value
	for a real constant, its real value
	for a string constant, its text, enclosed in primes
	for a pointer constant, "NIL" (nil is the only pointer constant)
	for a subroutine constant, the subroutine name, preceded by "!"  *)


type
    val_kind_set = set of value_kind;

const
    simple_consts: val_kind_set = [scalar_cst, real_cst, string_cst, ptr_cst, subr_cst];


function simple_cst ( v: val ): line_string;

begin
  case v.kind of
    scalar_cst:
      simple_cst := cv_int (v.ival);
    real_cst:
      simple_cst := cv_real (v.valp^.real_val);
    string_cst:
      begin
	with v.valp^ do
	  simple_cst := substr (str_val, 1, str_len);
	simple_cst := '''' || simple_cst || '''';
      end;
    ptr_cst:
      simple_cst := 'NIL';
    subr_cst:
      simple_cst := '!' || sym_text (v.blkp^.subr_sym);
    others:
      simple_cst := val_kind_id [v.kind]
  end (* case v.kind *);
end (* simple_cst *);
$PAGE prt_expr

(*  PrtExpr is called with an expression node which is an argument to an
    action node or another expression.  If the expression is an identifier
    or a simple constant, then the name/constant value is printed on the
    current line (a subr constant name is prefixed with '!').  Otherwise,
    the expression node number, prefixed with '$', is printed on the
    current line.  *)


procedure prt_expr ( e: expr );

var print_number: boolean;

begin
  if e = nil then
    fio_write (dumpfb, '~')
  else
    with e^ do begin
      print_number := false;
      if opcode = ident_ref then
	fio_write (dumpfb, sym_text (id_sym))
      else if (opcode = cst_ref) andif (cst_val.kind in simple_consts) then
	fio_write (dumpfb, simple_cst (cst_val))
      else
	print_number := true;
      if print_number or (usage_count <> 1) then
	fio_write (dumpfb, '$' || cv_int (nodeid));
    end (* with e^ *);
end (* prt_expr *);
$PAGE dmptuples

(*  DMPTUPLES dumps a chained linked list of control and expression
    operators. *)


public procedure dmptuples (title: line_string);

type tuple_narray = array [tuple_opcodes] of packed array [1..7] of char;

const tuple_names: tuple_narray :=

     (  'CONST',	'IDENT',	'DEREF',	'FIELD',
	'ELEMENT',	'SUBSTR',	'BUFFER',

	'DISPLAY',	'MEM',		'ADDR',		'IMMED',

	'*OP*',

	'IADD',		'ISUB',		'IMUL',		'IDIV',
	'IMOD',		'RADD',		'RSUB',		'RMUL',
	'RDIV',		'EXPII',	'EXPRI',	'EXPRR',
	'ILE',		'ILT',		'IGT',		'IGE',
	'IEQ',		'INE',		'RLE',		'RLT',
	'RGT',		'RGE',		'REQ',		'RNE',
	'SLE',		'SLT',		'SGT',		'SGE',
	'SEQ',		'SNE',		'SETLE',	'SETGE',
	'SETEQ',	'SETNE',	'PTREQ',	'PTRNE',
	'FILEQ',	'FILNE',	'OR',		'AND',
	'ORIF',		'ANDIF',	'IN',		'DIFF',
	'BOTH',		'STRCOMP',

	'INEG',		'RNEG',		'IABS',		'RABS',
	'NOT',		'SCLCVT',	'STRCVT',	'SETCVT',
	'FLOAT',	'TRUNC',	'ODD',		'SQRT',
	'LN',		'LOG',		'EXP',		'SIN',
	'ARCSIN',	'SINH',		'COS',		'ARCCOS',
	'COSH',		'TAN',		'TANH',		'COTAN',
	'LENGTH',	'LWCASE',	'UPCASE',	'ADDRESS',
	'LWBOUND',	'UPBOUND',	'DIM',		'EXTENT',

	'GEN SET',	'ROUND',	'ARCTAN',
	'IMIN',		'RMIN',		'IMAX',		'RMAX',
	'INDEX',	'VERIFY',	'SEARCH',
	'AGG',		'CAT',		'UNION',

	'DESC',		'SUBRVAR',	'*ANDIF',	'*ORIF',

	'NEW',		'ALCTEMP',
	'IN STR',	'OUT STR',	'VAR STR',	'FIX STR',
	'EOLN',		'EOF',		'EOPAGE',	'CURSOR',
	'FILESIZ',	'IOSTAT',

	'RANDOM',	'SET',
	'EXTSTAT',	'DATE',		'TIME',		'RUNTIME',
	'OPEN',		'RESET',	'REWRITE',	'UPDATE',

	'SIZE',

	'FCALL',	'FSIGNAL',

	'CALL',		'SIGNAL',

	'BLOCK',	'END',		'STMT',		'WITH',
	'ENDWITH',	'COND',		'ENDCOND',	'EVAL',
	'ASSIGN',	'JUMP',		'RETJUMP',	'JUMP',
	'JUMP',		'JUMP',		'*JUMP',	'CASE',
	'LABEL',	'STOP',		'RETURN',	'ABORT',
	'CASEABT',	'GOTO',		'DISPOSE',	'NOP',

	'SUBRNGE',	'STRRNGE',	'VALRNGE',	'PTRCHK',
	'FILECHK',	'FLDCHK',	'COMPAT',	'RST STK',

	'STARTIO',	'END IO',
	'GET',		'PUT',		'READLN',	'WRITELN',
	'PAGE',		'CLEAR',	'BREAK',	'EMPTY',
	'CLOSE',	'SCRATCH',	'READ',
	'WRITE',	'SEEK',		'CLOSEIO'					);
$PAGE prt_node - in dmptuples

(*  PrtNode prints the initial information that is common to all tuple dump
    lines:  the node id number and the kind of node.  The node id number is
    printed in columns 1-6.  For action nodes, the action name is printed in
    columns 8-14.  For expression nodes, '=' is printed in column 8, and the
    action kind is printed in columns 16-22.  The last column printed in the
    dump line is saved as the continuation column.  *)


procedure prt_node ( t: tuple );

type eq_syms = array [boolean, boolean] of packed array [1..8] of char;

const eq_text: eq_syms =
     (  '=', (* not copy_tuple, not blk_input_tuple *)
	':=', (* copy_tuple, not blk_input_tuple *)
	'=>', (* not copy_tuple, blk_input_tuple *)
	':=>'  ); (* copy_tuple, blk_input_tuple *)

begin
  with t^ do begin
    if (opcode = label_node) or (opcode = start_stmt) then
      fio_skip (dumpfb);
    fio_write (dumpfb, cvf_int(nodeid,6) || ' ');
    if opcode = label_node
      then fio_write (dumpfb, cvf_int(block_order_no,4) || ' ')
      else fio_write (dumpfb, '     ');
    if (opcode >= first_expr) and (opcode <= last_expr) then
      fio_write (dumpfb, eq_text [blk_input_tuple, copy_tuple]);
    fio_write (dumpfb, tuple_names[opcode] || ' ');
    dumpfb.c_column := dumpfb.column;
  end (* with t^ *);
end (* prt_node *);
$PAGE prt_args - in dmptuples

(*  PrtArgs prints out any arguments or qualifiers of a tuple.  *)


procedure prt_args ( t: tuple );

type rw_mode_syms = array [rd_wr_mode] of string [8];

const rw_mode_text: rw_mode_syms =
     (  'BINARY ', 'IMAGE ', 'BOOLEAN ', 'DECIMAL ', 'OCTAL ', 'HEX ',
	'REAL ', 'FIXED ', 'FLOAT ', 'LEFT ', 'RIGHT '  );

type fil_mode_syms = array [file_modes] of string [6];

const fil_mode_text: fil_mode_syms =
     (  'TEXT', 'TYPED', 'BINARY', 'ANY'  );

var i: int_type;

begin
  with t^ do begin
    case opcode of

      start_block:
	fio_write (dumpfb, block_id (block));

      end_block:
	(* no text *);

      start_stmt:
	fio_write (dumpfb, cv_int (stmt_index) ||  ': ' || symbol_names [stmt_kind] ||
		   ' ON LINE ' || cv_source_id (stmt_source));

      start_with,
      end_with:
	prt_expr (with_rec);

      start_cond,
      end_cond:
	fio_write (dumpfb, block_id (handler));

      assign_op,
      eval_op:
	begin
	  prt_expr (lhs);
	  fio_write (dumpfb, ' := ');
	  prt_expr (rhs);
	  if overlaps then
	    fio_write (dumpfb, ' (OVL)');
	  if lrecursive then
	    fio_write (dumpfb, ' (LREC)');
	  if rrecursive then
	    fio_write (dumpfb, ' (RREC)');
	end;

      label_node:
	if (label_sym <> nil) andif (label_sym^.name <> nil) then
	  fio_write (dumpfb, sym_text (label_sym));

      goto_op:
	begin
	  fio_write (dumpfb, sym_text (target_label));
	  if target_frame <> nil then begin
	    fio_write (dumpfb, ' IN FRAME ');
	    prt_expr (target_frame);
	  end;
	end;

      jump_op,
      retjump_op,
      jump_t_op,
      jump_f_op,
      jump_in_op,
      gen_jump_op:
	begin
	  fio_write (dumpfb, 'TO ' || cv_int (jump_to^.block_order_no) ||
		    ' @ ' || cv_int (jump_to^.nodeid) );
	  if (opcode <> jump_op) and (opcode <> retjump_op) and (opcode <> gen_jump_op) then begin
	    fio_write (dumpfb, ' IF ');
	    if opcode = jump_f_op then
	      fio_write (dumpfb, 'NOT ');
	    prt_expr (cond);
	    if opcode = jump_in_op then
	      fio_write (dumpfb,  ' IN ' || cv_int (low_cntrl) || ' .. ' ||
			 cv_int (high_cntrl) );
	  end;
	end;

      case_jump_op:
	begin
	  prt_expr (cond);
	  fio_write (dumpfb, ' IN ' || cv_int (low_cntrl) || ' .. ' ||
		    cv_int (high_cntrl) || ' ELSE JUMP TO ' ||
		    cv_int (jump_to^.block_order_no) || ' @ ' ||
		    cv_int (jump_to^.nodeid) );
	end;

      dispose_op:
	prt_expr (dptrarg);

      stop_op,
      return_op,
      abort_op,
      reset_stk_op,
      nop:
	(* no text *);

      first_io_stmt..last_io_stmt:
	begin
	  if old_file then
	    fio_write (dumpfb, 'REMEMBERED ');
	  prt_expr (file_arg);
	end;

      read_op,
      write_op:
	begin
	  fio_write (dumpfb, rw_mode_text [rw_mode]);
	  prt_expr (rw_item);
	  if (rw_width <> nil) or (rw_precision <> nil) then begin
	    fio_write (dumpfb, ': ');
	    prt_expr (rw_width);
	    if rw_precision <> nil then begin
	      fio_write (dumpfb, ': ');
	      prt_expr (rw_precision);
	    end;
	  end;
	  if opcode = read_op
	    then fio_write (dumpfb, ' FROM ')
	    else fio_write (dumpfb, ' TO ');
	  if rw_old_file then
	    fio_write (dumpfb, 'REMEMBERED ');
	  prt_expr (rw_file);
	end;

      seek_op:
	begin
	  prt_expr (seek_index);
	  fio_write (dumpfb, ' IN ');
	  prt_expr (seek_file);
	end;

      ident_ref:
	fio_write (dumpfb, sym_text (id_sym));

      cst_ref:
	fio_write (dumpfb, simple_cst (cst_val));

 field_ref:
	begin
	  fio_write (dumpfb, sym_text (field_sym));
	  fio_write (dumpfb, ' OF ');
	  prt_expr (base_rec);
	end;

      ptr_ref:
	prt_expr (base_ptr);

      array_ref:
	begin
	  prt_expr (base_array);
	  fio_write (dumpfb, ' [');
	  prt_expr (index_val);
	  fio_write (dumpfb, ']');
	end;

      substr_ref:
	begin
	  prt_expr (base_string);
	  fio_write (dumpfb, ' [');
	  prt_expr (substr_index);
	  fio_write (dumpfb, ' : ');
	  prt_expr (substr_length);
	  fio_write (dumpfb, ']');
	end;

      buffer_ref:
	prt_expr (base_file);

      display_op:
	fio_write (dumpfb,  cv_int (nlevels) );

      mem_ref,
      addr_ref,
      immed_ref:
	with item do begin
	  if class = constant_sc then begin
	    fio_write (dumpfb, '=');
	    with cstref^ do
	      case kind of
		scalar_cst:
		  fio_write (dumpfb,  cv_int (scalar_val) );
		real_cst:
		  fio_write (dumpfb, cv_real (real_val));
		string_cst:
		  fio_write (dumpfb, '''' || substr (str_val, 1, str_len) || '''');
		ptr_cst:
		  fio_write (dumpfb, 'NIL');
		others:
		  fio_write (dumpfb, val_kind_id [kind])
	      end (* case kind *);
	  end
	  else if sym_name <> nil then
	    fio_write (dumpfb, sym_text (sym_name));
	if (offset <> 0) or (sym_name = nil) then begin
	    if ((class = constant_sc) orif (sym_name <> nil))
	      andif (offset > 0) then
		fio_write (dumpfb, '+');
	    if pack
	      then begin
		if offset >= 0 then begin
		  fio_write (dumpfb,  cv_int (offset div bits_per_unit) || '.' ||
			     cv_int (offset mod bits_per_unit)		 );
		end
		else begin
		  fio_write (dumpfb,  cv_int ((offset - (bits_per_unit - 1)) div bits_per_unit)
				|| '.' || cv_int (bits_per_unit - 1 + ((offset+1) mod bits_per_unit)) );
		end;
	      end
	    else if offset = 377777B
	      then fio_write (dumpfb, 'NIL')
	    else fio_write (dumpfb,  cv_int (offset) );
	  end;
	  if index <> nil then begin
	    fio_write (dumpfb, '[');
	    prt_expr (index);
	    fio_write (dumpfb, ']');
	  end;
	  if base <> nil then begin
	    fio_write (dumpfb, '(');
	    prt_expr (base);
	    fio_write (dumpfb, ')');
	  end;
	  if pack then fio_write (dumpfb, ' BYTE ' || cv_int (size));
	  end;



      call_op,
      signal_op,
      func_call_op,
      func_signal_op:
	begin
	  prt_expr (subr);
	  if nargs <> 0 then
	    fio_write (dumpfb, ' ( ');
	  for i := 1 to nargs do begin
	    prt_expr (arglist[i]);
	    if i = nargs
	      then fio_write (dumpfb, ' )')
	      else fio_write (dumpfb, ', ');
	  end;
	end;

      first_nary_op .. last_nary_op,
      first_chk_op..last_chk_op:
	begin
	  for i := 1 to nopers do begin
	    prt_expr (operand[i]);
	    if i <> nopers then
	      fio_write (dumpfb, ', ');
	  end;
	end;

      size_op:
	begin
	  fio_write (dumpfb, type_name (type_ref));
	  if type_parm <> nil then begin
	    fio_write (dumpfb, ' (');
	    prt_expr (type_parm);
	    fio_write (dumpfb, ')');
	  end;
	end;

      others:
	fio_write (dumpfb, '**  UNPROCESSED  **')

    end (* case opcode *);
  end (* with t^ *);
end (* prt_args *);
$PAGE prt_desc - in dmptuples

(*  PrtDesc prints out the descriptor information for an expression tuple.  *)


procedure prt_desc ( t: tuple );

begin
  with t^ do begin
    if ref_fre <> 0 then begin
      fio_tab (dumpfb, 48);
      fio_write (dumpfb, '#' || cv_int(ref_fre));
    end;
      fio_tab (dumpfb, 53);
      fio_write (dumpfb, cvf_int(usage_count,4) || ' ');
    if (desc.base <> nil) and
      (desc.base <> type_int) and
      (desc.base <> type_bool) and
      (desc.base <> type_char) and
      (desc.base <> type_real) then begin
	fio_write (dumpfb, type_name (desc.base));
	fio_write (dumpfb, ': ');
    end;
    fio_write (dumpfb, tp_kind_id[desc.kind]);
    if desc.kind in [ints, bools, chars, scalars, pointers, files] then begin
      fio_write (dumpfb, ' ' || cv_int (desc.int_prec));
      if desc.signed then
	fio_write (dumpfb, 'S');
    end
    else if desc.kind = strings then begin
      fio_write (dumpfb, ' ');
      if desc.str_flex
        then fio_write (dumpfb, '*');
      if desc.str_length <> maximum (char_range)
	then fio_write (dumpfb, cv_int (desc.str_length));
      if desc.str_kind = varying then
	fio_write (dumpfb, 'V');
    end
    else if desc.kind = sets then begin
      fio_write (dumpfb, ' ');
      if not desc.set_cst_lwb
        then fio_write (dumpfb, '*');
      fio_write (dumpfb, cv_int (desc.set_lwb));
      fio_write (dumpfb, ',');
      if not desc.set_cst_len
        then fio_write (dumpfb, '*');
      if desc.set_cst_len or (desc.set_length <> max_set_length)
        then fio_write (dumpfb, cv_int (desc.set_length));
    end
    else if desc.kind = reals then
      fio_write (dumpfb, ' ' || cv_int (desc.precision));
  end (* with t^ *);
end (* prt_desc *);
$PAGE dmptuples - main routine

var
    i: int_type;
    t: tuple;

begin
  prt_title (title);

  t := t_chain;
  while t <> nil do begin
    prt_node (t);
    prt_args (t);
    if (t^.opcode >= first_expr) and (t^.opcode <= last_expr) then
      prt_desc (t);
    fio_skip (dumpfb);
    t := t^.next;
  end;
end (* dmptuples *);
(*$y6 (* unused in quick pass *)
$PAGE prt_set

(*  PRT SET will print a single set from a set vector.  *)


procedure prt_set ( s: svector; i: set_number; n_elems: elem_number );

var
    j, j1: elem_number;
    first: boolean;

begin
  fio_write (dumpfb, '[');
  dumpfb.c_column := dumpfb.column;
  first := true;
  j := 0;
  loop
    while (j <= n_elems) andif not in_set (s, i, j) do
      j := j + 1;
  exit if j > n_elems;
    j1 := j + 1;
    while (j1 <= n_elems) andif in_set (s, i, j1) do
      j1 := j1 + 1;
    if not first then
      fio_write (dumpfb, ', ');
    first := false;
    if j1 = j + 1
      then fio_write (dumpfb, cv_int(j))
      else fio_write (dumpfb, cv_int(j) || '..' || cv_int(j1-1));
    j := j1;
  end;
  fio_line (dumpfb, ']');
end (* prt_set *);
$PAGE dmpset

(*  DMPSET will dump the contents of a set vector.  *)


public procedure dmpset ( s: svector; n_elems: elem_number; title: line_string );

var i: set_number;

begin
  prt_title (title);

  for i := 0 to s.n_sets do begin
    fio_write (dumpfb, cvf_int (i, 5) || ': ');
    prt_set (s, i, n_elems);
  end;
end (* dmpset *);
$PAGE dmpcgraph

(*  DMPCGRAPH dumps all the blocks of the module, with their lexical and
    calling relations specified.  *)

public procedure dmpcgraph;

var
    ind: int_type;
    b: blk;
    c: call_link;

begin
  prt_title ('BLOCK STRUCTURE AND CALL GRAPH');

  (*  This is the same non-recursive lexical block tree traversal which is
      used in lex_ordering in PASCGR.  *)

  b := root_block;
  loop
    loop

      ind := b^.level*2 + 1;
      dumpfb.c_column := ind + 6;
      fio_tab (dumpfb, ind);
      fio_line (dumpfb, 'BLOCK ' || block_id (b));
      if b^.kind = subr_blk then begin
	fio_tab (dumpfb, ind);
	fio_line (dumpfb, 'OWNER = ' || cv_int (b^.owner^.number));
      end;
      c := b^.calls;
      if c <> nil then begin
	fio_tab (dumpfb, ind);
	fio_write (dumpfb, 'CALLS:  ');
	dumpfb.c_column := dumpfb.column;
	while c <> nil do begin
	  fio_write (dumpfb,  cv_int(c^.called_subr^.number));
	  c := c^.rlink;
	  if c <> nil then
	    fio_write (dumpfb,  ', ');
	end;
	fio_skip (dumpfb);
      end (* if c <> nil *);
      fio_skip (dumpfb);

    exit if b^.children = nil;
      b := b^.children;
    end;
    while (b^.peer = nil) and (b^.parent <> nil) do
      b := b^.parent;
  exit if b^.peer = nil;
    b := b^.peer;
  end;
end (* dmpcgraph *);
$PAGE dmpfgraph

(*  DMPFGRAPH will dump the flow graph of the current module.  The flow graph
    is dumped as the set of successor blocks of each basic block.  *)


public procedure dmpfgraph ( blocks: tpl_vector; n_blocks: index_range );

type
    block_pred = function ( index_range; index_range ): boolean;

var
    forward_limit, backward_limit, forward_depth, backward_depth, index: index_vector;
    successors, fmarks, bmarks: svector;
    f_depth, b_depth, fpdepth, bpdepth: index_range;
    str: packed array [1..100] of char;


function f_contains ( i, j: index_range ): boolean;
begin
  f_contains := (i < j) andif (forward_limit^[i] >= forward_limit^[j]);
end;

function f_conflicts ( i, j: index_range ): boolean;
begin
  f_conflicts :=
    (i < j) and (j < forward_limit^[i]) and (forward_limit^[i] < forward_limit^[j]) or
    (j < i) and (i < forward_limit^[j]) and (forward_limit^[j] < forward_limit^[i]);
end;

function b_contains (i, j: index_range ): boolean;
begin
  b_contains := (i > j) andif (backward_limit^[i] <= backward_limit^[j]);
end;

function b_conflicts (i, j: index_range ): boolean;
begin
  b_conflicts :=
    (i > j) and (j >= backward_limit^[i]) and (backward_limit^[i] > backward_limit^[j]) or
    (j > i) and (i >= backward_limit^[j]) and (backward_limit^[j] > backward_limit^[i]);
end;
$PAGE compute_depths - in dmpfgraph

procedure compute_depths ( var index: index_block;
			       limit: index_block;
			   var depth: index_block;
			       contains: block_pred;
			       conflicts: block_pred;
			   var max_depth: index_range );

var i, j, k, t,
    next_free, last_possible, selected,
    crosses, min_crosses: index_range;

begin
  next_free := 1;
  max_depth := 0;
  for i := 1 to n_blocks do begin
    index [i] := i;
    if limit [i] = i + 1 then begin
      depth [i] := 0;
      index [i] := index [next_free];
      index [next_free] := i;
      next_free := next_free + 1;
    end;
  end;

  while next_free <= n_blocks do begin
    max_depth := max_depth + 1;
    last_possible := n_blocks;
    for i := n_blocks downto next_free do
      for j := last_possible downto next_free do
	exit if contains (index [i], index [j]) do begin
	  t := index [i];
	  index [i] := index [last_possible];
	  index [last_possible] := t;
	  last_possible := last_possible - 1;
	end;
    selected := next_free - 1;
    while selected <> last_possible do begin
      min_crosses := maximum (min_crosses);
      for j := selected + 1 to last_possible do begin
	crosses := 0;
	for k := next_free to n_blocks do
	  if conflicts (index [j], index [k]) then
	    crosses := crosses + 1;
	if crosses < min_crosses then begin
	  i := j;
	  min_crosses := crosses;
	end;
      end (* for j *);
      selected := selected + 1;
      t := index [i];
      index [i] := index [selected];
      index [selected] := t;
      depth [t] := max_depth;
      for j := last_possible downto selected + 1 do
	if conflicts (index [selected], index [j]) then begin
	  t := index [j];
	  index [j] := index [last_possible];
	  index [last_possible] := t;
	  last_possible := last_possible - 1;
	end;
    end (* while selected <> last_possible *);
    next_free := selected + 1;
  end (* while next_free <= n_blocks *);
end (* compute_depths *);
$PAGE mark_fstr, mark_fedge, mark_bstr & mark_bedge - in dmpfgraph

procedure mark_fstr;
var d: index_range;
begin
  str := '';
  for d := 1 to f_depth do
    if in_set (fmarks, 0, d) then
      str [d * 2 + 1] := '|';
end;

procedure mark_bstr;
var d: index_range;
begin
  str := '';
  for d := 1 to b_depth do
    if in_set (bmarks, 0, d) then
      str [(b_depth - d) * 2 + 1] := '|';
end;

procedure mark_fedge ( depth: index_range; angle, ptr: char );
var d: index_range;
begin
  if depth <> 0 then begin
    str [1] := ptr;
    str [depth * 2 + 1] := angle;
    for d := 2 to depth * 2 do
      str [d] := '-';
  end;
end;

procedure mark_bedge ( depth: index_range; angle, ptr: char );
var d: index_range;
begin
  if depth <> 0 then begin
    str [b_depth * 2 + 1] := ptr;
    str [(b_depth - depth) * 2 + 1] := angle;
    for d := (b_depth - depth) * 2 + 2 to b_depth * 2 do
      str [d] := '-';
  end;
end;
$PAGE dmpfgraph - main routine

var i, j: index_range;
    t: tuple;
    depth: index_range;
    idstr: string [5];

begin
  new (forward_limit: n_blocks);
  new (backward_limit: n_blocks);
  new (forward_depth: n_blocks);
  new (backward_depth: n_blocks);
  new (index: n_blocks);
  successors := new_svector (n_blocks, n_blocks);
  for i := 1 to n_blocks do begin
    forward_limit^ [i] := i + 1;
    backward_limit^ [i] := i + 1;
    clr_set (successors, i);
    t := blocks^[i]^.outward_jumps;
    if t <> nil then begin
      while (t^.opcode >= first_jump_op) and (t^.opcode <= last_jump_op) do begin
	j := t^.jump_to^.block_order_no;
	add_elem (successors, i, j);
	forward_limit^ [i] := max (forward_limit^ [i], j);
	backward_limit^ [i] := min (backward_limit^ [i], j);
	t := t^.next;
      end;
    end;
  end (* for i *);

  compute_depths (index^, forward_limit^, forward_depth^, f_contains, f_conflicts, f_depth);
  compute_depths (index^, backward_limit^, backward_depth^, b_contains, b_conflicts, b_depth);

  prt_title ('FLOW GRAPH FOR BLOCK $');

  fmarks := new_svector (0, f_depth);
  bmarks := new_svector (0, b_depth);
  clr_set (fmarks, 0);
  clr_set (bmarks, 0);
  for i := 1 to n_blocks do begin
    fpdepth := 0;
    bpdepth := 0;
    for j := 1 to i - 1 do begin
      depth := forward_depth^ [j];
      if in_set (successors, j, i) and (i <> j + 1) then
	fpdepth := max (fpdepth, depth);
      if i = forward_limit^ [j] then
	del_elem (fmarks, 0, depth);
    end;
    for j := i to n_blocks do begin
      depth := backward_depth^ [j];
      if in_set (successors, j, i) then
	bpdepth := max (bpdepth, depth);
      if i = backward_limit^ [j] then
	add_elem (bmarks, 0, depth);
    end;

    mark_bstr;
    mark_bedge (bpdepth, '.', '>');
    fio_write (dumpfb, substr (str, 1, b_depth * 2 + 1));
    fio_write (dumpfb, '*******');
    mark_fstr;
    mark_fedge (fpdepth, '''', '<');
    fio_line (dumpfb, substr (str, 1, f_depth * 2 + 1));

    mark_bstr;
    fio_write (dumpfb, substr (str, 1, b_depth * 2 + 1));
    fio_write (dumpfb, '*');
    idstr := cv_int (i);
    fio_space (dumpfb, (5 - length (idstr)) div 2);
    fio_write (dumpfb, idstr);
    fio_space (dumpfb, (5 - length (idstr) + 1) div 2);
    fio_write (dumpfb, '*');
    mark_fstr;
    fio_line (dumpfb, substr (str, 1, f_depth * 2 + 1));

    add_elem (fmarks, 0, forward_depth^ [i]);
    del_elem (bmarks, 0, backward_depth^ [i]);

    mark_bstr;
    mark_bedge (backward_depth^ [i], '`', '-');
    fio_write (dumpfb, substr (str, 1, b_depth * 2 + 1));
    fio_write (dumpfb, '*******');
    mark_fstr;
    mark_fedge (forward_depth^ [i], '.', '-');
    fio_line (dumpfb, substr (str, 1, f_depth * 2 + 1));

    if i <> n_blocks then begin
      mark_bstr;
      fio_write (dumpfb, substr (str, 1, b_depth * 2 + 1));
      if in_set (successors, i, i + 1)
	then fio_write (dumpfb, '   |   ')
	else fio_space (dumpfb, 7);
      mark_fstr;
      fio_line (dumpfb, substr (str, 1, f_depth * 2 + 1));
    end;
  end (* for i *);

  dispose (forward_limit);
  dispose (backward_limit);
  dispose (forward_depth);
  dispose (backward_depth);
  dispose (index);
  del_svector (successors);
  del_svector (fmarks);
  del_svector (bmarks);
end (* dmpfgraph *);
$PAGE dmpvlsyms

(*  DMPVLSYMS will dump all the symbols in the VL list.  Since the list is
    in reverse creation order, DMPVLSYMS will walk the list to its end,
    reversing it on the way, and then walk it back to the beginning, dumping
    the symbols and putting it back in its original order.  *)


public procedure dmpvlsyms;

var v, v1, v2: vl_link;

begin
  prt_title ('LABEL AND VARIABLE SYMBOLS');

  v := vl_list;
  v1 := nil;
  while v <> nil do begin
    v2 := v^.last;
    v^.last := v1;
    v1 := v;
    v := v2;
  end;

  v := v1;
  v1 := nil;
  while v <> nil do
    with v^ do begin
      fio_write (dumpfb, cvf_int (symbol^.id_number, 5) || ': ' || sym_text (symbol));
      fio_tab (dumpfb, 41);
      if symbol^.block = nil then
	fio_skip (dumpfb)
      else
	with symbol^.block^ do
	  case kind of
	    root_blk: fio_line (dumpfb, 'IN <PASCAL>');
	    program_blk,
	    module_blk,
	    data_blk: fio_line (dumpfb, 'IN '  || substr (id^.text, 1, id^.len));
	    subr_blk: fio_line (dumpfb, 'IN ' || sym_text (subr_sym));
	    others: fio_skip (dumpfb)
	  end;
	v2 := v^.last;
      v^.last := v1;
      v1 := v;
      v := v2;
    end;

end (* dmpvlsyms *);
$PAGE dmpfrlist

(*  DMPFRLIST will dump the Formal Reference Expression list for a subroutine.  *)


public procedure dmpfrtable ( f: fre_vector; n_fre: fre );

var i: fre;

begin
  prt_title ('FRE TABLE FOR BLOCK $');
  dumpfb.c_column := 9;
  for i := 1 to n_fre do
    with f^ [i] do begin
      fio_write (dumpfb, cvf_int (i, 6) || ': ');
      case kind of
	sym_fre:
	  fio_line (dumpfb, sym_text (id_sym));
	elem_fre:
	  fio_line (dumpfb, 'ELEMENT OF FRE #' || cv_int (base_fre));
	field_fre:
	  fio_line (dumpfb, 'FIELD ' || sym_text (field_sym) || ' OF FRE #' ||
			    cv_int (record_fre))
      end (* case *);
    end (* for i *);
end (* dmpfrtable *);
$PAGE dmpdominators

(*  DMPDOMINATORS will print the dominator tree of a flow graph.  *)


public procedure dmpdominators ( idom, dom_son, dom_brother: index_vector;
				 reducible: boolean );

var
    b: index_range;
    level: int_type;
    i: int_type;

begin
  prt_title ('DOMINATOR TREE FOR BLOCK $');

  (*  This is the same old non-recursive tree walk--other examples are in
      the quick block computation in PA2CGR.  *)

  level := 0;
  b := 1;
  loop
    loop
      for i := 1 to level do
	fio_write (dumpfb, '| ');
      fio_line (dumpfb, cv_int (b));
    exit if dom_son^ [b] = 0;
      b := dom_son^ [b];
      level := level + 1;
    end;
    while (dom_brother^ [b] = 0) and (idom^ [b] <> 0) do begin
      b := idom^ [b];
      level := level - 1;
    end;
  exit if dom_brother^ [b] = 0;
    b := dom_brother^ [b];
  end;
  fio_skip (dumpfb);
  if reducible
    then fio_line (dumpfb, 'FLOW GRAPH IS REDUCIBLE')
    else fio_line (dumpfb, 'FLOW GRAPH IS NOT REDUCIBLE');
end (* dmpdominators *);
$PAGE dmpwenv

(*  DMPWENV dumps the weak environment for a subroutine.  *)


public procedure dmpwenv ( input_lists: tpl_list_vec; n_blocks: index_range );

var
    i: index_range;
    t: tpl_list;

type op_sym_list = array [first_io_func..last_io_func] of string [8];

const op_sym: op_sym_list =
     (  'EOLN', 'EOF', 'EOPAGE', 'CURSOR', 'FILESIZE'  );

begin
  prt_title ('WEAK ENVIRONMENT FUNCTION FOR BLOCK $');
  for i := 1 to n_blocks do begin
    t := input_lists^ [i];
    if t <> nil then begin
      fio_skip (dumpfb);
      fio_line (dumpfb, 'FOR BASIC BLOCK ' || cv_int (i) || ':');
      fio_skip (dumpfb);
      while t <> nil do begin
	with t^.tpl^ do begin
	  fio_write (dumpfb, cvf_int (nodeid, 6) || ' ');

	  case opcode of
	    ident_ref:
	      fio_write (dumpfb, sym_text (id_sym));
	    ptr_ref:  begin
	      prt_expr (base_ptr);
	      fio_write (dumpfb, ' ^');
	    end;
	    buffer_ref:  begin
	      prt_expr (base_file);
	      fio_write (dumpfb, ' ^');
	    end;
	    array_ref:  begin
	      prt_expr (base_array);
	      fio_write (dumpfb, ' [');
	      prt_expr (index_val);
	      fio_write (dumpfb, ']');
	    end;
	    substr_ref:  begin
	      fio_write (dumpfb, 'SUBSTR (');
	      prt_expr (base_string);
	      fio_write (dumpfb, ', ');
	      prt_expr (substr_index);
	      fio_write (dumpfb, ', ');
	      prt_expr (substr_length);
	      fio_write (dumpfb, ')');
	    end;
	    field_ref:  begin
	      prt_expr (base_rec);
	      fio_write (dumpfb, ' . ' || sym_text (field_sym));
	    end;
	    others (* i/o functions *):  begin
	      fio_write (dumpfb, op_sym [opcode] || ' (');
	      prt_expr (operand[1]);
	      fio_write (dumpfb, ')');
	    end
	  end (* case opcode *);

	  fio_line (dumpfb, ' -> BLOCK ' || cv_int (result^.block_order_no));
	end;
	t := t^.next;
      end;
    end;
  end;
end (* dmpwenv *);
$PAGE dmprefcounts

(*  DMPREFCOUNTS dumps the reference allocation counts for each loop header
    block.  *)


public procedure dmprefcounts ( trees: ref_c_vector; n_blocks: index_range );

var
    i: index_range;
    tr: ref_c_tree;

begin
  prt_title ('REFERENCE ALLOCATION COUNTS FOR BLOCK $');

  for i := 1 to n_blocks do
    if trees^ [i] <> nil then begin
      fio_skip (dumpfb);
      fio_line (dumpfb, 'FOR LOOP BEGINNING WITH BASIC BLOCK ' || cv_int (i) || ':');
      fio_skip (dumpfb);
      tr := trees^ [i];
      while tr <> nil do begin
	fio_line (dumpfb, '    TUPLE' || cvf_int (tr^.tpl^.nodeid, 6) ||
			  ' REFERENCED ' || cvf_int (tr^.ref_count, 6) || ' TIMES');
	tr := tr^.right;
      end;
    end;
end (* dmprefcounts *); *)
$PAGE dmpstorage

(*  DMPSTORAGE will print the allocated storage addresses for all the variable
    and parameter symbols in the program.  *)


public procedure dmpstorage;

var
    b: blk;
    symbols: sym;
    title_printed: boolean;
    class: storage_class;
    printed_classes: set of storage_class;


  procedure prt_address ( name: line_string; addr: unit_range; class: storage_class );
  type class_codes = array [local_sc..static_sc] of char;
  const code: class_codes = ( '+', '*', ' ' );
  begin
    if not title_printed then begin
      fio_skip (dumpfb);
      fio_line (dumpfb, 'FOR BLOCK ' || block_id (b) || '');
      fio_skip (dumpfb);
      title_printed := true;
    end;
    fio_line (dumpfb, cv_oct (addr, 6) || code [class] || ' ' || name);
    printed_classes := printed_classes + [class];
  end (* prt_address *);


begin
  prt_title ('SYMBOLIC STORAGE ALLOCATION');
  b := root_block;
  loop
    loop
      title_printed := false;
      printed_classes := [ ];
      if b^.return_sym <> nil then
	prt_address ('<RETURN SYM>', b^.return_sym^.item_addr, local_sc);
      if b^.kind = subr_blk then begin
	if b^.subr_sym^.type_desc^.parmlist_size > 6 then begin
	  prt_address ('<PARM LIST POINTER>', b^.parm_list_base, local_sc);
	  class := parameter_sc;
	end
	else
	  class := local_sc;
	symbols := b^.parm_list.first;
	while symbols <> nil do begin
	  prt_address (sym_text (symbols), symbols^.item_addr, class);
	  symbols := symbols^.next;
	end;
      end;
      symbols := b^.id_list.first;
      while symbols <> nil do  with symbols^ do begin
	if (kind in [vars, values]) andif (dcl_class in [local_sc, static_sc]) then
	  prt_address (sym_text (symbols), item_addr, dcl_class);
	symbols := next;
      end;
      if local_sc in printed_classes then begin
	fio_skip (dumpfb);
	fio_tab (dumpfb, 9);
	fio_line (dumpfb, '+ = STACK FRAME OF BLOCK ' || block_id (b^.owner));
      end;
      if parameter_sc in printed_classes then begin
	fio_skip (dumpfb);
	fio_tab (dumpfb, 9);
	fio_line (dumpfb, '* = PARAMETER LIST AREA');
      end;
    exit if b^.children = nil;
      b := b^.children;
    end;
    while (b^.peer = nil) and (b^.parent <> nil) do
      b := b^.parent;
  exit if b^.peer = nil;
    b := b^.peer;
  end;
end (* dmpstorage *);
$PAGE dmp_close

(*  DMP_CLOSE will close the dump file, if it is open, and change its status
    from "now open" to "previously opened".  *)

$OPTIONS list

public procedure dmp_close;

begin
  if df_status = now_open then begin
    fio_close (dumpfb);
    df_status := prev_opened;
  end;
end.
*N{Võ