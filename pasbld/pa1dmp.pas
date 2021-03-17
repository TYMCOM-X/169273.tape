$TITLE pa1dmp -- symbol table dump routines
$LENGTH 42

(*   +--------------------------------------------------------------+
     |                                                              |
     |                P A 1 D M P   -   P a s s   1                 |
     |                - - - - - - - - - - - - - - -                 |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  This is the  debugging  dump  module.  It  contains  a
        collection  of  procedures  to  dump  portions  of the symbol
        table, intermediate form code, etc., to a .DMP file.
     
     ENTRY POINTS:
     
        dump_name_table
                    prints out all the names  in  the  compiler  name
                    table,   in  alphabetical  order.  Each  name  is
                    printed with the address of its name table  node.
                    Statistics about the name table are also printed.
     
        dmpsymbol   produces a formatted dump of a single symbol node
                    (a  SYM  node).  This  dump  includes  the   node
                    address  and  the  symbol  name,  type, and kind;
                    where the symbol was declared; the identification
                    of the containing block; and information specific
                    to this particular kind of symbol.
     
        dmpstable   invokes  dmpsymbol  to  dump  all   the   symbols
                    declared  in  the current block.  The symbols are
                    dumped in alphabetical order.
     
        dmptype     produces a formatted dump of a single  type  node
                    (a   TYP  node).  This  dump  includes  the  node
                    address;  the  type  name  and  kind;  its  size,
                    alignment,    packed,   flexible,   and   generic
                    attributes;  and  information  specific  to  this
                    particular kind of type.
     
        dmpconst    produces  a  formatted  dump of a single constant
                    node (a VAL node).  The dump indicates  the  kind
                    of constant, and the constant value itself.
     
        dmpblock    produces  a formatted dump of a single block node
                    (a BLK node).  The dump identifies the block  and
                    includes all the pointers and miscellaneous items
                    of information that are included in scope block.
     
        dmp_ptree   produces a formatted dump of a  parse  tree.  The
                    dump  contains one line for each parse tree node,
                    with tree structure indicated by indentation  and
                    vertical lines.
     
        dmp_close   will close the dump file, if it is open, and will
                    reset its status flag.
     
     ---------------------------------------------------------------- *)

$PAGE includes, externals, forwards

$SYSTEM pascal.inc
$SYSTEM pasfil.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM paspt.typ
$SYSTEM passw.inc
$SYSTEM pascv.inc
$SYSTEM paslex.inc
$SYSTEM passpf.nam
$SYSTEM pasbnf.nam
$SYSTEM pasdmp.inc



public procedure dmptype ( node: typ );
forward;

public procedure dmpconst ( con: val );
forward;
$PAGE prt_type
(*  prt_type prints out a message of the form:

	<msg><name>(<address>)

    if the type node has a name, or a message of the form:

	<msg><address>

    if the type node doesn't have a name.  if the eol flag is true,
    it will all be terminated by a new line.  *)

type msg_parm = string [40];

procedure prt_type
    (	msg: msg_parm;
	node: typ;
	eol:  boolean  );

begin
  fio_write (dumpfb, msg);
  if node = nil then
    fio_write (dumpfb, '*NIL*')
  else
    with node^ do
      if (type_id <> nil) andif (type_id^.name <> nil) then
	with type_id^ do begin
	  if (kind <> types) then
	    fio_write (dumpfb, 'TYPE OF ');
	  fio_write (dumpfb, name^.text || '(' ||
			    cv_ptr (ord (node)) || ')' );
	end
      else
	fio_write (dumpfb, cv_ptr (ord (node)));
  if eol then
    fio_skip (dumpfb);
end (* prt_type *);
$PAGE prt_blk_num:  print a block number and level
(*  prt_blk_num will print the block number, block level, and
    block node address for a given block.  *)

procedure prt_blk_num ( block: blk );
begin
  if block = nil
    then fio_line (dumpfb,  '*NIL BLOCK*')
    else with block^ do begin
	fio_write (dumpfb, 'BLOCK ' || cv_int(number) || ' AT LEVEL ' ||
			   cv_int(level) || ' (' || cv_ptr (ord (block)) ||
			   '): ');
	case kind of
	    root_blk:
		fio_line (dumpfb, '<ROOT>');
	    program_blk:
		if id = nil
		    then fio_line (dumpfb, '<PROGRAM>')
		    else fio_line (dumpfb, 'PROGRAM ' || id^.text);
	    module_blk:
		if id = nil
		    then fio_line (dumpfb, '<MODULE>')
		    else fio_line (dumpfb, 'MODULE ' || id^.text);
	    data_blk:
		if id = nil
		    then fio_line (dumpfb,  '<DATA MODULE>')
		    else fio_line (dumpfb,  'DATA MODULE ' ||  id^.text);
	    subr_blk:
		if (subr_sym = nil) orif (subr_sym^.name = nil)
		    then fio_line (dumpfb, '<SUBR>')
		    else fio_line (dumpfb, subr_sym^.name^.text);
	    class_blk:
		prt_type ('CLASS OF ', class_type, true);
	    extern_blk:
		fio_line (dumpfb, '<EXTERNAL>')
	end (* case kind *);
    end (* with block *);
end (* prt_blk_num *);
$PAGE prt_sym
(*  prt_sym will skip the specified number of spaces in the output
    file, and then print a message of the form:

	<address>  <msg><name>/<number>  *)

procedure prt_sym
    (	skip: line_index;
	msg: parm_string;
	symbol: sym;
	eol: boolean  );

begin
    fio_tab (dumpfb, skip);
    fio_write (dumpfb, cv_ptr (ord (symbol)) || '  ' || msg);
    if symbol <> nil then begin
      if symbol^.name = nil
	then fio_write (dumpfb, '<NO NAME>')
	else fio_write (dumpfb, symbol^.name^.text);
      fio_write (dumpfb, '/' || cv_int (symbol^.id_number));
    end;
    if eol then fio_skip (dumpfb);
end (* prt_sym *);
$PAGE prt_const
(*  prt_const prints a value node.  *)

type vki_type = array[real_cst..record_cst] of string[10];
const val_kind_id: vki_type :=
      ( 'REAL', 'STRING', 'SET', 'POINTER', 'ARRAY', 'RECORD' );

procedure prt_const ( con: val );

var
    ind: int_type;

begin
  if con.kind = scalar_cst then
    fio_line (dumpfb, cv_int(con.ival))
  else if con.kind = no_value then
    fio_line (dumpfb, '*NO VALUE*')
  else if con.kind = subr_cst then
    prt_blk_num (con.blkp)
  else begin
    fio_write (dumpfb, val_kind_id[con.kind] || ' @ ' || cv_ptr (ord (con.valp)) || ' = ');
    with con.valp^ do
      case con.kind of
	real_cst:
	    fio_line (dumpfb, ' ' || cv_real(real_val) ||  ' prec ' ||  cvf_int(real_prec,2));
	string_cst:
	  begin
	    if str_varying_ref
	      then fio_write (dumpfb, ' VAR');
	    fio_line (dumpfb, ' ''' || str_val || '''');
	  end;
	set_cst:
	  begin
	    fio_write (dumpfb, cv_int(set_origin) || ':[');
	    for ind := 0 to upperbound (set_val) do
	      if set_val[ind]
		then fio_write (dumpfb, '1')
		else fio_write (dumpfb, '0');
	    fio_line (dumpfb, ']');
	  end;
	ptr_cst:
	    fio_line (dumpfb, 'NIL');
	array_cst,
	record_cst:
	  begin
	    fio_skip (dumpfb);
	    for ind := 1 to upperbound (elem_vals) do
	      dmpconst (elem_vals[ind]);
	    fio_tab (dumpfb, 9);
	    fio_line (dumpfb, 'END ' || val_kind_id[con.kind] || ' @ ' || cv_ptr (ord (con.valp)));
	  end
      end (* case *);
    end;
end (* prt_const *);
$PAGE dump_name_table
(*  dump_name_table will perform a post-order traversal of the
    name tree, printing, for each name in the table, the name itself
    and its address in the table.  *)

public procedure dump_name_table;

type
    cptr = ^ cnode;
    cnode = record  num: int_type; next: cptr end;

var
    count: cptr;
    c: cptr;
    depth_sum: int_type;
    index: int_type;
    name_count: int_type;

    procedure dump_names ( root: nam;		(* recursive tree traversal *)
			   depth: int_type;
			   count: cptr );
    begin
	with root^, count^ do begin
	    if next = nil then begin
		new (next);
		next^.num := 0;
		next^.next := nil;
	    end;
	    if alink <> nil then dump_names (alink,depth+1,next);
	    fio_line (dumpfb, cv_ptr (ord (root)) || ': ' || cvf_int(depth,5) ||
			      cv_ptr (ord (alink)) || ' ' || cv_ptr (ord (zlink)) ||
			      ' ' || text );
	    num := num + 1;
	    name_count := name_count + 1;
	    depth_sum := depth_sum + depth;
	    if zlink <> nil then dump_names (zlink,depth+1,next);
	end;
    end (* dump_names *);

begin
    dmp_open (true);
    fio_line (dumpfb, 'NAME TABLE');
    fio_line (dumpfb, '---- -----');
    fio_skip (dumpfb);
    fio_skip (dumpfb);
    fio_line (dumpfb, 'ADDRESS DEPTH  ALINK  ZLINK NAME');
    fio_line (dumpfb, '------- -----  -----  ----- ----');
    fio_skip (dumpfb);
    new (count);
    count^.num := 0;
    count^.next := nil;
    depth_sum := 0;
    name_count := 0;
    dump_names (root_name,0,count);
    fio_skip (dumpfb);
    fio_line (dumpfb, cv_int(name_count) || ' NAMES');
    index := 0;
    while count <> nil do begin
	fio_line (dumpfb, cv_int(count^.num) || ' NAMES AT DEPTH ' ||
		  cv_int (index) || ', SATURATION = ' ||
		  cv_real ( count^.num / (2**index)));
	index := index + 1;
	c := count;
	count := count^.next;
	dispose (c);
    end;
    fio_line (dumpfb, 'AVERAGE DEPTH = ' || cv_real( depth_sum / name_count));
end (* dump_name_table *);
$PAGE dmpsymbol:  dump a symbol table node
(*  dmpsymbol will produce a formatted dump of a symbol node.  the
    dump will always include the node address, symbol name, declaration,
    containing block, type, and kind of symbol.  information specific
    to the symbol kind will also be printed.  *)

public procedure dmpsymbol ( symbol: sym );

type ski_type = array [sym_kind] of string [20];
const sym_kind_id: ski_type :=
     (  'LABEL', 'FIELD', 'TYPE', 'CONSTANT', 'VARIABLE', 'VALUE',
	'FOR LOOP INDEX', 'STANDARD PROCEDURE', 'STANDARD FUNCTION',
	'CONDITION', 'BLOCK'  );

type dci_type = array [storage_class] of string [11];
const dcl_class_id: dci_type :=
     (  'LOCAL', 'PARAMETER', 'STATIC', 'CONSTANT', 'EXTERNAL', 'DYNAMIC',
	'FILE BLOCK', 'OPT', 'CODE', 'ABSOLUTE', 'TEMP', 'REGISTER', 'RUNTIME',
	'DEF', 'SELF', 'UNALLOCATED'  );

begin
  dmp_open (false);
  fio_skip (dumpfb);
  if symbol = nil then begin
    fio_line (dumpfb, '********  NIL SYMBOL');
    return;
  end;
  with symbol^ do begin
    prt_sym (1,'',symbol,false);
    fio_write (dumpfb, ': ' || sym_kind_id[kind] ||  ', IN ');
    prt_blk_num (block);
    prt_type ('        TYPE = ',type_desc,false);
    fio_line (dumpfb, ', NEXT = ' || cv_ptr (ord (next)) ||
		      ', SCOPECHAIN = ' || cv_ptr (ord (scopechain)));
    case kind of

      labels:
	begin
	  fio_tab (dumpfb, 9);
	  fio_write (dumpfb, 'DECLARED AT LINE ' || cv_source_id(lab_declaration));
	  if lab_defined
	    then fio_line (dumpfb, ', DEFINED')
	    else fio_skip (dumpfb);
	end;

      fields:
	begin
	  prt_type ('        FIELD '||cv_int(fld_number)||' OF RECORD ',  fld_record,false);
	  prt_type (', IN VARIANT ', fld_variant, true);
	  fio_line (dumpfb, '        OFFSET= ' || cv_int(fld_offset div byte_size) ||
			    '+' || cv_int(fld_offset mod byte_size) ||
			    ', WIDTH= ' || cv_int(fld_width));
       end;

      consts,
      vars,
      values,
      for_inds,
      conditions:
	begin
	  fio_tab (dumpfb, 9);
	  if public_dcl then
	    fio_write (dumpfb, 'PUBLIC, ');
	  fio_write (dumpfb, 'STORAGE CLASS = ' || dcl_class_id [dcl_class]);
	  if standard then
	    fio_write (dumpfb, ', STANDARD');
	  if maskable then
	    fio_write (dumpfb, ', MASKABLE');
	  fio_write (dumpfb, ', VALUE = ');
	  prt_const (init_value);
	  fio_tab (dumpfb, 9);
	  if abnormal_use then
	    fio_write (dumpfb, 'ABNORMAL, ');
	  if allocated then
	    fio_write (dumpfb, 'ALLOCATED, ');
	  fio_line (dumpfb, 'ADDR = ' || cv_int(item_addr));
	end;

      std_procs,
      std_funcs:
	begin
	  fio_tab (dumpfb, 9);
	  fio_line (dumpfb, sym_kind_id [kind] || ' ' || std_pf_id [std_pf_code]);
	end

    end (* case kind *);
    if (type_desc <> nil) andif
       ((type_desc^.type_id = nil) or (type_desc^.type_id = symbol)) then
	  dmptype (type_desc);
  end (* with symbol *);
end (* dmpsymbol *);
$PAGE dmpstable:  dump all the symbols in a block
(*  dmpstable will traverse the name table, and for each name,
    will check if there is a symbol with that name in the specified
    block, and if so, will dump it with dmpsymbol.  *)

public procedure dmpstable ( block: blk );

    procedure traverse (rt:nam);		(* do the name tree traversal *)
    var
	root: nam;
	symbol: sym;
    begin
	root := rt;
	while root <> nil do
	    with root^ do begin
		if alink <> nil then traverse (alink);
		symbol := scopechain;
		while (symbol <> nil) andif (symbol^.block <> block) do
		    symbol := symbol^.scopechain;
		if (symbol <> nil) andif (symbol^.block = block) then
		    dmpsymbol (symbol);
		root := zlink;
	    end;
    end (* traverse *);

begin
    dmp_open (true);
    fio_write (dumpfb, 'SYMBOL TABLE FOR ');
    prt_blk_num (block);
    fio_line (dumpfb, '------ -----');
    fio_skip (dumpfb);
    traverse (root_name);
end (* dmpstable *);
$PAGE dmptype:  display a type node
(*  dmptype will produce a formatted dump of a type node.  the dump will
    always include the node address, packed and flexible attributes, and
    kind of type node.  information specific to the type node kind will
    also be printed.  *)

type tpki_type = array [type_kind] of string [10];
const tp_kind_id: tpki_type :=
      ( 'SCALAR', 'BOOLEAN', 'CHAR', 'INTEGER', 'REAL', 'SET', 'POINTER',
	'FILE', 'STRING', 'ARRAY', 'RECORD', 'VARIANT', 'TAG',
	'PROCEDURE', 'FUNCTION', '<UNKNOWN>', '<INDIRECT>' );

type mode_name_type = array [file_modes] of string [10];
const mode_name: mode_name_type :=
      ( 'TEXT', 'TYPED', 'BINARY', 'ANY' );

public procedure dmptype (* node: typ *);

var
    csym: sym;
    var_ptr: typ;  last_var: typ;
    i: parm_range;


  procedure dmpfields ( rv: typ );
   var csym: sym;
   begin
    with rv^ do begin
      csym := field_list;
      while (csym <> nil) andif (csym^.fld_variant = rv) do begin
	dmpsymbol (csym);
	csym := csym^.next;
      end;
      if variant_tag <> nil then dmptype (variant_tag);
      fio_skip (dumpfb);
      fio_line (dumpfb, cv_ptr (ord (rv)) || ': END ' || tp_kind_id [kind]);
    end
   end;
begin
  dmp_open (false);
  fio_skip (dumpfb);
  if node = nil then begin
    fio_line (dumpfb, '********   NIL TYPE');
    return;
  end;
  with node^ do begin
    fio_write (dumpfb, cv_ptr (ord (node)) || '  ');
    if (type_id <> nil) andif (type_id^.name <> nil) then
	fio_write (dumpfb, type_id^.name^.text || ': ');
    fio_write (dumpfb, tp_kind_id[kind] || ' TYPE,' ||
		       ' SIZE= ' || cv_int(base_size div byte_size) ||
		       '+' || cv_int(base_size mod byte_size));
    if packable then
      fio_write (dumpfb, ', PACKED');
    if flexible then
      fio_write (dumpfb, ', FLEXIBLE');
    if generic then
      fio_write (dumpfb, ', GENERIC');
    fio_skip (dumpfb);
    fio_tab (dumpfb, 9);
    fio_write (dumpfb, tp_kind_id [kind] || ' ');
    case kind of

      bools,
      ints,
      chars,
      scalars:
	begin
	  prt_type (cv_int(minval)||' .. '||cv_int(maxval)||
		    ' OF BASE TYPE ',base_type,false);
	  if (kind in [bools,scalars]) and (base_type = node) then begin
	    fio_line (dumpfb, ' WITH ELEMENTS:');
	    csym := cst_list.first;
	    while csym <> nil do begin
	      prt_sym (10,'',csym,true);
	    exit if csym = cst_list.last;
	      csym := csym^.next;
	    end;
	  end
	  else
	    fio_skip (dumpfb);
	end;

      reals:
	begin
	  fio_write (dumpfb, cv_real(rminval) || ' .. ' || cv_real(rmaxval));
	  fio_line (dumpfb, ' PREC ' || cv_int(precision));
	end;

      sets:
	begin
	  prt_type ('OF TYPE ',set_element_type,true);
	  if (set_element_type <> nil) andif (set_element_type^.type_id = nil) then
	    dmptype (set_element_type);
	end;

      pointers:
	begin
	  prt_type ('TO TYPE ',target_type,true);
	  if (target_type <> nil) andif (target_type^.type_id = nil) then
	    dmptype (target_type);
	end;

      arrays:
	begin
	  prt_type ('[TYPE ',index_type,false);
	  prt_type ('] OF TYPE ',element_type,false);
	  fio_line (dumpfb, ', ELEMENT SIZE = ' || cv_int (element_size));
	  if (index_type <> nil) andif (index_type^.type_id = nil) then
	    dmptype (index_type);
	  if (element_type <> nil) andif (element_type^.type_id = nil) then
	    dmptype (element_type);
	end;

      files:
	begin
	  fio_write (dumpfb, '[' || mode_name [file_kind] || '] ');
	  prt_type ('OF TYPE ',component_type,true);
	  if (component_type <> nil) andif (component_type^.type_id = nil) then
	    dmptype (component_type);
	end;

      strings:
	  if str_kind = varying
	      then fio_line (dumpfb, '[' || cv_int(str_length) || '] VARYING')
	      else fio_line (dumpfb, '[' || cv_int(str_length) || '] NONVARYING');

      records:
	begin
	  fio_line (dumpfb, ', WITH FIELDS:');
	  dmpfields (node);
	end;

      variants:
	begin
	  if others_var
	    then fio_write (dumpfb, 'OTHERS')
	    else fio_write (dumpfb, cv_int (minlab) || ' .. ' || cv_int (maxlab));
	  fio_write (dumpfb, ' OF TAG ' || cv_ptr (ord (tag)));
	end;


      tags:
	begin
	  prt_type ('TYPE ',tag_type,false);
	  prt_type (' IN RECORD ',tag_recvar,true);
	  prt_sym (10,'TAG NAME ',tag_field,true);
	  var_ptr := first_variant;
	  while var_ptr <> nil do begin
	    last_var := var_ptr;
	    loop
	      dmptype (var_ptr);
	      var_ptr := var_ptr^.next_variant;
	    exit if (var_ptr = nil) orif (var_ptr^.field_list <> last_var^.field_list);
	      fio_skip (dumpfb);
	    end;
	    fio_line (dumpfb, ', WITH FIELDS:');
	    dmpfields (lar);
	  end;
	end;

      procs,
      funcs:
	begin
	  if fortran_call then fio_write (dumpfb, '(FORTRAN) ');
	  if kind = funcs then begin
	    prt_type ('RETURNS TYPE ',return_type,false);
	    fio_write (dumpfb, ' ');
	  end;
	  fio_line (dumpfb, 'WITH ' || cv_int(upperbound (params)) || ' PARAMETERS:');
	  for i := 1 to upperbound (params) do
	    if params[i].parm_kind = values
	      then prt_type ('VALUE ', params[i].parm_type, true)
	      else prt_type ('VAR   ', params[i].parm_type, true);
	end;

      indirect_type:
	begin
	  prt_type ('TO ACTUAL TYPE ',actual_type,true);
	  if (actual_type <> nil) andif
	    ((actual_type^.type_id = nil) or (actual_type^.type_id = type_id))
	      then dmptype (actual_type);
	end;

      others:
	  fio_skip (dumpfb)

    end (* case *);
  end (* with node *);
end (* dmptype *);
$PAGE dmpconst:  dump a constant table node
public procedure dmpconst (* con: val *);

begin
  dmp_open (false);
  fio_tab (dumpfb, 9);
  prt_const (con);
end (* dmp_const *);
$PAGE dmpblock: dump a block node
public procedure dmpblock ( block: blk );

    procedure dump_list (list:sym_list;description:parm_string);
    var symbol: sym;
    begin
	fio_skip (dumpfb);
	fio_tab (dumpfb, 9);
	fio_line (dumpfb, description || ' (' || cv_ptr (ord (list.first)) ||
			  ', ' || cv_ptr (ord (list.last)) || '):');
	symbol := list.first;
	while symbol <> nil do begin
	  prt_sym (10,'',symbol,true);
	exit if symbol = list.last;
	  symbol := symbol^.next;
	end;
    end (* dump_list *);

type on_type = array [optionlist] of string [10];
const opt_names: on_type :=
      ( 'CHECK ASS', 'CHECK CAS', 'CHECK COM', 'CHECK FIE', 'CHECK FIL',
        'CHECK INP', 'CHECK POI', 'CHECK STR', 'CHECK SUB', 'CHECK VAL',
	'CHECK STK', 'MAP', 'SYMBOLS', 'CALLS',
	'ASSEMBLY', 'XREF', 'SPEC COE', 'SPEC PTR', 'SPEC WOR', 'TRACE',
	'QBLOCKS', 'OPTIMIZE' );

var
    any: boolean;
    opt: optionlist;
    sw: switch_ptr;

begin
    dmp_open (true);
    with block^ do begin
	prt_blk_num (block);
	fio_line (dumpfb,  '-----');
	fio_skip (dumpfb);
	fio_tab (dumpfb, 9);
	fio_line (dumpfb, 'PARENT = ' || cv_ptr (ord (parent)) ||
			  ', PEER = ' || cv_ptr (ord (peer)) ||
			  ', CHILD = ' || cv_ptr (ord (children)));
	if (kind = subr_blk) andif forward_dcl then begin
	  fio_tab (dumpfb, 9);
	  fio_line (dumpfb, 'FORWARD');
	end;
	fio_tab (dumpfb, 9);
	fio_line (dumpfb, 'RETURN SYMBOL =' || cv_ptr (ord (return_sym)));
	if return_sym <> nil then dmpsymbol (return_sym);
	dump_list (parm_list,'PARAMETERS');
	dump_list (label_list,'LABELS');
	dump_list (type_list,'TYPE SYMBOLS');
	dump_list (id_list,'IDENTIFIERS');
	fio_skip (dumpfb);
	fio_tab (dumpfb, 9);
	fio_write (dumpfb, 'OPTIONS = [');
	any := false;
	for opt := minimum (optionlist) to maximum (optionlist) do
	    if opt in semantic_options then begin
		if any then fio_write (dumpfb, ', ');
		any := true;
		fio_write (dumpfb, opt_names[opt]);
	    end;
	if not any then fio_write (dumpfb, ' ');
	fio_line (dumpfb, ']');
	fio_write (dumpfb, 'DUMP = (');
	sw := dump_switches;
	while sw <> nil do begin
	    if sw <> dump_switches then fio_write (dumpfb, ', ');
	    fio_write (dumpfb, sw^.name);
	    if sw^.enabled then fio_write (dumpfb, '!');
	    sw := sw^.next_switch;
	end;
	if dump_switches = nil then fio_write (dumpfb, ' ');
	fio_line (dumpfb, ')');
    end;
end (* dmpblock *);
$PAGE dmp_ptree
type op_name_vec = array [operators] of string [10];

const op_names: op_name_vec =
	( '*', '/', 'DIV', '**', 'MOD', 'AND', 'ANDIF', '+', '-', 'OR',
	  'ORIF', '||', '<=', '<', '>', '>=', '=', '<>', 'IN',
	  'READ', 'WRITE', 'READLN', 'WRITELN', 'READRN', 'WRITERN',
	  'GETSTRING', 'PUTSTRING', '*NOP*' );


public procedure dmp_ptree ( node: parse_node );

  procedure do_dump ( node: parse_node; level: line_index );

    procedure indent;
    var i: line_index;
    begin
      for i := 1 to level do fio_write (dumpfb, '| ');
    end;

    procedure display_node;
    begin
      with node^ do begin
	fio_write (dumpfb, symbol_names [sym]);
	if dummy then fio_write (dumpfb, ' (dummy)')
	else begin
	  fio_write (dumpfb, ' (on line ' || cv_source_id (source));
	  fio_write (dumpfb, ' || col ' || cv_int (column) || ')');
	end;
	if sym in [ident, intconst, realconst, stringconst,
		   notsy, powerop, mulop, addop, relop, iosy] then
	  fio_write (dumpfb, ': ');
	case sym of
	  ident:
	    if name = nil then
	      fio_write (dumpfb, 'NO NAME')
	    else
	      fio_write (dumpfb, 'NAME = ' || name^.text);
	  intconst:
	    fio_write (dumpfb, 'VALUE = ' || cv_int(value.ival));
	  realconst:
	    with value.valp^ do
	      fio_write (dumpfb, 'VALUE = ' || cv_real(real_val) ||
				 ' PREC ' || cv_int(real_prec));
	  stringconst:
	    if value.kind = scalar_cst then
	      fio_write (dumpfb, 'VALUE = CHAR ''' || chr(value.ival) || '''')
	    else
	      fio_write (dumpfb, 'VALUE = ''' || value.valp^.str_val || '''');
	  notsy,
	  powerop,
	  mulop,
	  addop,
	  relop,
	  iosy:
	    fio_write (dumpfb, 'OPERATOR = ' || op_names[op])
	end (* case *);
      end (* with node^ *);
      fio_skip (dumpfb);
    end (* display_node *);

  var def: parse_node;

  begin (* do_dump *);
    indent;
    fio_skip (dumpfb);
    indent;
    display_node;
    def := node^.defn;
    while def <> nil do begin
      do_dump (def,level+1);
      def := def^.next;
    end;
  end (* do_dump *);

begin (* dump_parse_table *);
  if node = nil then return;
  dmp_open (true);
  do_dump (node,0);
end (* dump_parse_table *);
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
    if addr >= 0
      then fio_write (dumpfb, ' ')
      else fio_write (dumpfb, '-');
    fio_line (dumpfb, cv_radix (abs (addr), adr_width) || code [class] || ' ' || name);
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
end (* dmpstorage *).
 }3h<