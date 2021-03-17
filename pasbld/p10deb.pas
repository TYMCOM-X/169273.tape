$title P10DEB - symbol table dump for DEBUG
module p10deb;
(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P 1 0 D E B                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  Emission of DEBUG and TRACE supports.
     
     ENTRY POINTS:
     
        DEB_INIT    initializes  the  data  structures  and  reserves
                    locations for the program block.
     
        TRACE_BLOCK emits the trace block.
     
        PROG_BLOCK  emits the program block.
     
        STMT_BLOCK  emits  statement  blocks  and sets up definitions
                    for page and file blocks.
     
        BLK_END     insures  that  forward  statement  pointers  from
                    trace blocks are resolved.
     
        FP_BLOCKS   dumps the page and file blocks.
     
        DEB_STABLE  directs  the  symbol  table  dump  for the PASCAL
                    debugger.
     
     EFFECTS:  Since some symbol table entries  are  modified  during
        the dump P10DEB must be called after code emission is PASS4.
     
     ALGORITHM:   The compiler symbol table is a complex structure on
        the heap comprised of block, symbol,  type,  value  and  name
        nodes.  This  structure  is  walked in three passes, starting
        from the root block and following all relevent pointers.  The
        nodes are visited in the same order in all three passes.  The
        first or initialization pass sets the "self" pointer of  each
        node  to NIL and its Boolean "visited" flag to false.  In the
        second pass each node is assigned an address offset from zero
        which  will  be its word offset in the symbol table file when
        it is dumped.  This address is stored in its self pointer and
        the  visited  flag  is  set true in preparation for the third
        pass.  Finally, each node is copied field  by  field  into  a
        simplified  version of the record for the debugger, replacing
        all pointers by their self  addresses,  and  written  to  the
        symbol  table  file.  The  visied  flag  is then set false to
        indicate that the node has been processed.
     
     NOTES:  Not all symbols are emitted.  In particular named types,
        indirect  types,  and  standard  procedures and functions are
        deleted from the "sym_list"s  during  the  first  pass.  Name
        nodes  which  are  not  required (not "visited" in the second
        pass) are deleted from the name tree prior to the third pass.
     
        In emission of file and page  blocks,  three  fields  in  the
        PAGE_ID records are redefined since they are otherwise unused
        in the fourth pass.  The  "following_page"  pointer  for  the
        first  page  of a file holds the "def" pointer for the file's
        "file_block".  Each page for which  a  page_block  is  to  be
        emitted  has  the  "def" pointer for this block stored in the
        "left_page"  field  of  its   page_id   record,   while   the
        "right_page"  field  holds the def pointer of the last "stmt"
        block emitted for the page.  These are initialized to NIL  by
        DEB_INIT, assigned by STMT_BLOCK, and resolved by FP_BLOCKS.
     
     ---------------------------------------------------------------- *)
$PAGE includes
$system pascal.inc
$system pasist.inc
$system paspt.typ
$system pasif.typ
$system pasfil.inc
$system pascv.inc
$system passw.inc
$system p10cg.typ
$system p10gen.inc
$system p10cgu.inc
$system p10exp.inc
$system p10opc.inc
$system p10mac.inc
$system p10rel.inc
$include p10deb.typ
$PAGE declarations - static area
static var
    prg_blk_def: def;   (* def record for program block *)

    deb_file: file of int_type; (* .DEB file for debugger *)

    first_stmt: def;    (* for resolution of first stmt in block *)

    file_def: def;      (* def of last file block emitted *)
$PAGE deb_init
  (* DEB_INIT initializes the structures used in support of the debugger
     and reserves storage for the program block.        *)

  public procedure deb_init;

  const
      prg_blk_size := 6;        (* module word + 5 words data *)

  var fil: source_ptr;
      page: page_ptr;

  begin

    (* emit the radix50 module name for the debugger at the start of the module *)

    gen_word (code_area, radix50 (0, root_block^.children^.id^.text), fullword);

    (* the program block comes next, or a zero word for a non-debug compilation *)

    prg_blk_def := make_def (code_def);
    mark_def (code_area, prg_blk_def);

    if not prog_options.debug_opt then
      gen_xwd (0, none, 0, none)

    else begin
      gen_origin (code_area,
                  loc_code + prg_blk_size + 1 + (length (rel_file) + 4) div 5);

      fil := file_list; (* first in list of source files *)
      while fil <> nil do with fil^ do begin
        page := pages;  (* first page in file *)
        page^.following_page := nil;    (* def record for file block *)
        while page <> nil do with page^ do begin
          left_page := nil;     (* def record for page block, if emitted *)
          right_page := nil;    (* def record for last stmt block in page *)
          page := next_page;
        end;
        fil := next_file;
      end;
    end;
  end;
$PAGE locate_file, locate_page
  (* LOCATE_FILE accepts a file number and returns a pointer to the
     src_id record for that file *)

  function locate_file (f_no: file_range): source_ptr;

  begin
    locate_file := file_list;   (* first file in list *)
    while locate_file^.file_no <> f_no do
      locate_file := locate_file^.next_file;
  end;

  (* LOCATE_PAGE accepts a pointer to a file record and a page number within
     the file, returning a pointer to the page_id record for that page *)

  function locate_page (fil: source_ptr; page: page_range): page_ptr;

  begin
    locate_page := fil^.pages;  (* first page in file *)
    while locate_page^.page_number <> page do
      locate_page := locate_page^.next_page;
  end;
$PAGE stmt_block
  (* STMT_BLOCK determines if a statement block must be emitted for
     the current source statement. Assumes it is the first statement
     on the line.       *)

  public procedure stmt_block (stmt: source_id; stmt_type: symbols)  options special(coercions);

  var
    fil: source_ptr;    (* file containing stmt *)
    page: page_ptr;     (* page containing stmt *)
    d: def;
    kind: int_type;

  begin
    case stmt_type of
        simple_stmt:    kind := 0;
        if_stmt:        kind := 1;
        for_stmt:       kind := 2;
        loopsy:         kind := 3;
        while_stmt:     kind := 4;
        case_stmt:      kind := 5;
        with_stmt:      kind := 6;
        goto_stmt:      kind := 7;
        io_stmt:        kind := 8;
        return_stmt:    kind := 9;
        stop_stmt:      kind := 10;
        exit_clause:    kind := 11;
        untilsy:        kind := 12;
        repeatsy:       kind := 13;
        endsy:          kind := 14;
        func_qualifier: kind := 15;
        others:         return  (* ignore *)
    end;
    clr_rv;     (* protect function values *)
    fil := locate_file (stmt.file_no);  (* get file record *)
    page := locate_page (fil, stmt.page_no);    (* and page *)
    if fil^.pages^.following_page = nil then    (* no file block yet *)
      fil^.pages^.following_page := page_ptr (make_def (code_def));    (* for fwd refs *)
    if page^.left_page = nil then       (* no page block yet *)
      page^.left_page := ptr (ord (make_def (code_def)));
    gen_rt (jsp, 1, rt_stmt);   (* interface to debugger *)
    kind := kind * 40000b + stmt.line_no;
    d := make_def (code_def);
    mark_def (code_area, d);    (* mark location of stmt block *)
    if first_stmt <> nil then begin     (* resolve ptr from trace block *)
      mark_def (code_area, first_stmt); (* resolve *)
      first_stmt := nil;                (* once only *)
    end;
    if page^.right_page = nil then      (* first stmt emitted, backlink to page block *)
      gen_xwd (0, reldef (ptr (ord (page^.left_page))), kind, none)
    else gen_xwd (0, reldef (ptr (ord (page^.right_page))), kind, none);
    page^.right_page := ptr (ord (d));  (* backlink to this stmt next time *)
  end (* stmt_block *);
$PAGE trace_block
(* TRACE BLOCK generates the trace control block for a procedure or main program
   given the "name" of the block.  A definition for the start of the block is
   generated and returned. *)

public function trace_block (block: blk): def;
  var
    name: nam;

begin
  trace_block := make_def (code_def);
  mark_def (code_area, trace_block);

  if prog_options.debug_opt then begin

     (* There are two unresolved fields in the trace block in debug
        mode. The symbol table index for the block is not supplied until
        the symbol table is dumped, so a definition record of DEB_SYM_REF
        type is created here and resolved on dumping the block node
        in SCAN_BLK.  Another def record is created for the first statement
        in the block. The def pointer is maintained in a static pointer
        FIRST_STMT until it is resolved by STMT_BLOCK (as the first stmt)
        or by BLK_END (as NIL). *)

    gen_xwd (block^.level, none, 0, reldef (get_def (deb_sym_def, block^.number)));
    first_stmt := make_def (code_def);
    gen_xwd (0, reldef (prg_blk_def), 0, reldef (first_stmt));
  end (* if debug on *)
  else begin
    gen_xwd (block^.level, none, 0, none);
    gen_xwd (0, reldef (prg_blk_def), 0, none);
  end;

  case block^.kind of
    program_blk, module_blk, data_blk:
      name := block^.id;
    subr_blk:
      name := block^.subr_sym^.name
  end;
  gen_string (name^.text);
end;
$PAGE prog_block
(* PROG_BLOCK emits the program block after the symbol table has been dumped *)

public procedure prog_block;

const
    loc_prog_block := 400001b;  (* follows module word *)

var
    location: code_address;

var word: pdp10word;

begin
  gen_origin (code_area, loc_prog_block);
  if file_def = nil then        (* no stmt blocks ? *)
    gen_xwd (int_nil, none, 0, none)
  else gen_xwd (0, reldef (file_def), 0, none);
  word.dtime_value := root_block^.children^.comp_dtime;       (* comp date and time *)
  gen_word (code_area, word, fullword);
  gen_xwd (0, reldef (high_base), 0, reldef (low_base));
  gen_xwd (type_int^.file_loc, none, type_real^.file_loc, none);
  gen_xwd (type_char^.file_loc, none, type_bool^.file_loc, none);
  gen_string (rel_file);
  location := loc_prog_block;   (* space already reserved *)
  emit_code (code_area, location, prog_options.semantic_options);
end;
$PAGE blk_end
  (* BLK_END is called from COMPILE_BODY at the end of a block to
     assure the stmt pointer in the trace block has been resolved. *)

public procedure blk_end;

begin
  if first_stmt <> nil then     (* no stmt blocks emitted for block *)
    def_value (first_stmt, int_nil, false);     (* resolve as nil *)
end;
$PAGE fp_blocks
  (* FP_BLOCKS emits page and file blocks for the debugger *)

  public procedure fp_blocks  options special(coercions);

  var 
    fil: source_ptr;    (* current file *)
    page: page_ptr;     (* current page in fil *)
    def_page, def_file: def;    (* for backlinking through blocks *)
    start_ix, len: int_type;

  begin
    fil := file_list;   (* first file *)
    def_file := nil;    (* no previous file before first *)
    while fil <> nil do with fil^ do begin
      if pages^.following_page <> nil then begin        (* stmt blocks emitted for file *)
        def_page := def (pages^.following_page);  (* first page links to file block *)
        page := pages;  (* first page in file *)
        while page <> nil do with page^ do begin
          if left_page <> nil then begin        (* stmts in page *)
            mark_def (code_area, ptr (ord (left_page)));        (* define addr of page block *)
            gen_xwd (0, reldef (def_page), 0, none);    (* link to prev page or file block *)
            gen_xwd (0, reldef (ptr (ord (right_page))), page_number, none);    (* link to last stmt in page *)

            (* emit only initial alphanumeric portion of subtitle *)

            len := verify (uppercase (subtitle), ['A'..'Z','0'..'9','_','$'], length (subtitle) + 1) - 1;
            gen_string (substr (subtitle, 1, len));
            def_page := ptr (ord (left_page));  (* backlink to this block next *)
          end;
          page := next_page;
        end (* while *);
        mark_def (code_area, ptr (ord (pages^.following_page)));        (* define addr of file block *)
        if def_file = nil then  (* no previous file block *)
          gen_xwd (int_nil, none, 0, reldef (def_page))
        else gen_xwd (0, reldef (def_file), 0, reldef (def_page));
        gen_xwd (0, reldef (prg_blk_def), file_no, none);
        start_ix := search (file_name, [':']) + 1;      (* strip device *)
        len := search (substr (file_name, start_ix), ['.','[']) - 1;
        gen_string (substr (file_name, start_ix, len));    (* generate filename *)
        def_file := ptr (ord (pages^.following_page));
      end;
      fil := next_file;
    end (* while *);
    file_def := def_file;       (* last file def for program block *)
  end (* fp_blocks *);
$PAGE deb_stable
public procedure deb_stable  options special (word);

const
  disk_block := 128;    (* words in a disk block (for alignment) *)

var
  relocating: boolean;			(* true => relocation pass, false => writing pass *)
  deb_offset: deb_file_index;         (* words allocated within symbol file *)
  deb_index: deb_file_index;          (* words written to symbol file *)

  deb_record: record            (* for emission of various records *)
    case int_type of
      0: (overlay: array[1..disk_block] of machine_word);   (* overlays other records *)
      1: (deb_value: deb_value_node);
      2: (deb_nam: deb_name_node);
      3: (deb_sym: deb_symbol_node);
      4: (deb_typ: deb_type_node);
      5: (deb_blk: deb_block_node)
    end;
$PAGE init_deb_file, close_deb_file
  (* INIT_DEB_FILE opens the debugger symbol table file, initializing required counters *)

  procedure init_deb_file;
  var i: int_type;
  begin
    rewrite (deb_file, rel_file || '.DEB');
    with deb_record do
      for i := 1 to disk_block do
        overlay[i] := 0;
    deb_offset := 0;    (* no addresses assigned *)
    deb_index := 0;     (* no entries written *)
  end;

  (* CLOSE_DEB_FILE fills out the symbol table file to a disk block
     boundary before closing the file.  *)

  procedure close_deb_file;
  var i: int_type;
  begin
    if deb_index mod disk_block <> 0 then begin
      for i := deb_index mod disk_block to disk_block - 1 do begin      (* fill out *)
        deb_file^ := 0;
        put (deb_file);
      end;
    end;
    close (deb_file);
  end;
$PAGE reserve
  (* RESERVE essentially assigns offset addresses within the deb file,
     assuring that no block will cross a disk block boundary.   *)

  function reserve (size_required: int_type): deb_file_index;
  begin
    if (deb_offset mod disk_block) + size_required > disk_block then begin

      (* overflow--skip to next disk block *)

      deb_offset := ((deb_offset + disk_block - 1) div disk_block) * disk_block;
    end;
    reserve := deb_offset;
    deb_offset := deb_offset + size_required;
  end (* reserve *);
$PAGE place_entry
  (* PLACE_ENTRY puts a node of any type to the .DEB file, given its size
     in words and its "self" pointer within the file. The file is padded
     up to the specified offset. The record to be written must already
     have been stored in "deb_record". *)

  procedure place_entry (size: int_type; offset: deb_file_index);

  var i: deb_file_index;

  begin
    for i := deb_index to offset - 1 do begin   (* pad out if necessary *)
      deb_file^ := 0;
      put (deb_file);
    end;
    deb_index := offset + size;
    with deb_record do begin
      for i := 1 to size do begin
        deb_file^ := overlay[i];
        put (deb_file);
        overlay[i] := 0;
      end;
    end;
  end (* place_entry *);
$PAGE trans_nam, trans_sym, trans_blk, trans_typ, trans_val
(* These routines merely return the translated "self" pointer 
   or NIL, as appropriate.    *)

function trans_nam (n: nam): nam  options special (coercions);
begin
  if n = nil then trans_nam := nil
  else trans_nam := nam (n^.file_loc);
end;

function trans_sym (s: sym): sym  options special (coercions);
begin
  if s = nil then trans_sym := nil
  else trans_sym := sym (s^.file_loc);
end;

function trans_blk (b: blk): blk  options special (coercions);
begin
  if b = nil then trans_blk := nil
  else trans_blk := blk (b^.file_loc);
end;

function trans_typ (t: typ): typ  options special (coercions);
begin
  if t = nil then trans_typ := nil
  else if t^.kind = indirect_type then  (* eliminate indirect types *)
    trans_typ := trans_typ (t^.actual_type)
  else trans_typ := typ (t^.file_loc);
end;

(* TRANS_VAL accepts a VAL record and returns a VAL record with translated pointers *)

function trans_val (valu: val): val;
begin
  with valu do begin
    trans_val.kind := kind;
    case kind of
      scalar_cst: trans_val.ival := ival;
      subr_cst: trans_val.blkp := trans_blk (blkp);
      no_value: ;
      others: assert (false);
    end;
  end;
end;
$PAGE sym_size, value_size, typ_size, blk_size, nam_size
  (* Routines to determine the size of various nodes *)

  function nam_size (n: nam): int_type;

  begin
    nam_size := 2 + (length (n^.text) + 4) div 5;
  end;

  function sym_size (s: sym): int_type;
  begin
    with s^ do
      case kind of
	fields:		sym_size := size (deb_symbde, fields);
	types:		sym_size := size (deb_symbol_node, types);
	consts:		if allocated then sym_size := size (deb_symbol_node, consts, true)
				       else sym_size := size (deb_symbol_node, consts, false);
	vars:		if allocated then sym_size := size (deb_symbol_node, vars, true)
				       else sym_size := size (deb_symbol_node, vars, false);
	values:		if allocated then sym_size := size (deb_symbol_node, values, true)
				       else sym_size := size (deb_symbol_node, values, false);
	for_inds:		if allocated then sym_size := size (deb_symbol_node, for_inds, true)
				       else sym_size := size (deb_symbol_node, for_inds, false);
	conditions:	if allocated then sym_size := size (deb_symbol_node, conditions, true)
				       else sym_size := size (deb_symbol_node, conditions, false);
	blocks:		sym_size := size (deb_symbol_node, blocks);
	others:
	  assert (false)
      end;
  end (* sym_size *);

  function typ_size (t: typ): int_type;
  begin
    case t^.kind of
      bools:
	typ_size := size (deb_type_node, bools, bools);
      scalars:
	typ_size := size (deb_type_node, scalars, scalars);
      chars:
	typ_size := size (deb_type_node, chars, chars);
      ints:
	typ_size := size (deb_type_node, ints, ints);
      reals:
	typ_size := size (deb_type_node, reals);
      sets:
	typ_size := size (deb_type_node, sets);
      pointers:
	typ_size := size (deb_type_node, pointers);
      files:
	typ_size := size (deb_type_node, files);
      strings:
	typ_size := size (deb_type_node, strings);
      arrays:
	typ_size := size (deb_type_node, arrays);
      records:
	typ_size := size (deb_type_node, records, records);
      variants:
	typ_size := size (deb_type_node, variants, variants);
      tags:
	typ_size := size (deb_type_node, tags);
      procs:
	typ_size := size (deb_type_node, procs);
      funcs:
	typ_size := size (deb_type_node, funcs);
      unknown_type:
	typ_size := size (deb_type_node, unknown_type);
      indirect_type:
	typ_size := size (deb_type_node, indirect_type);
      others:
	assert (false)
    end;
  end (* typ_size *);

  function blk_size (b: blk): int_type;
  begin
    case b^.kind of
      root_blk:
	blk_size := size (deb_block_node, root_blk);
      program_blk:
	blk_size := size (deb_block_node, program_blk);
      module_blk:
	blk_size := size (deb_block_node, module_blk);
      data_blk:
	blk_size := size (deb_block_node, data_blk);
      subr_blk:
	blk_size := size (deb_block_node, subr_blk);
      others:
	assert (false)
    end
  end (* blk_size *);
$PAGE rebush_nam_tree
  (* REBUSH_NAM_TREE prunes and rebushes the nam tree, eliminating
     nodes not marked as "visited", and more or less balances the tree. *)

  procedure rebush_nam_tree;

  var index, limit: int_type;
      rebush_tree: ^ packed array [0..*] of nam;

    function count_nodes (name: nam): int_type;

    (* Counts the nodes in a subtree *)

    begin
      if name = nil then count_nodes := 0
      else count_nodes := count_nodes (name^.alink) + count_nodes (name^.zlink)
                                + ord (name^.visited);
    end;

    procedure enter_nam (name: nam);

    (* Enters "visited" name nodes in the allocated rebush array in
        alphabetical order.     *)

    begin
      if name <> nil then begin
        enter_nam (name^.alink);        (* enter lower names first *)
        if name^.visited then begin
          rebush_tree^[index] := name;  (* enter *)
          index := index + 1;
        end;
        enter_nam (name^.zlink);
      end;
    end;

    function rebush (left, right: int_type): nam;

    (* REBUSH balances a subtree, given left and right limits within
       the rebush array. The returned pointer is the "center" of this
       sequence of nodes (or nil) *)

    var center: int_type;

    begin
      if left > right then rebush := nil
      else if left = right then (* one node only *) begin
        rebush := rebush_tree^[left];
        rebush^.alink := nil;
        rebush^.zlink := nil
      end
      else begin
        center := left + (right - left) div 2;  (* center of subrange *)
        rebush := rebush_tree^[center];
        rebush^.alink := rebush (left, center - 1);
        rebush^.zlink := rebush (center + 1, right);
      end;
    end;

  begin (* rebush_nam_tree *)
    limit := count_nodes (root_name) - 1;       (* number of nodes in pruned tree *)
    new (rebush_tree, limit);   (* allocate rebush array *)
    index := 0;                 (* start at ^[0] *)
    enter_nam (root_name);              (* fill array *)
    root_name := rebush (0, limit);     (* rebush it *)
    dispose (rebush_tree);      (* free working space *)
  end;
$PAGE scan procedures - forward declarations
procedure scan_nam ( n: nam );  forward;

procedure scan_sym ( first_sym, last_sym: sym )  options special (coercions);  forward;

procedure scan_typ ( t: typ );  forward;

procedure scan_blk ( b : blk );  forward;
$PAGE scan_nam
  (* SCAN_NAM initializes, locates, and dumps NAME_NODEs.  
     Since the nam tree is pruned and rebushed after the second pass
     left and right links are not followed here.  *)

  procedure scan_nam (* forward *);
  var size: int_type;
  begin
    if n <> nil then with n^ do begin
      if visited <> relocating then begin
	size := nam_size (n);
	if relocating then begin
	  file_loc := reserve (size);
	  visited := true;
	end
	else (* writing *) with deb_record do begin
	  deb_nam.alink := trans_nam (alink);
	  deb_nam.zlink := trans_nam (zlink);
	  deb_nam.len := length (text);
	  deb_nam.text[1:length (text)] := text;
	  place_entry (size, file_loc);
	  visited := false;
	end;
      end;
    end (* with *);
  end (* scan_nam *);
$PAGE scan_sym
  (* SCAN_SYM relocates pointers in a "symbol_node" *)

  procedure scan_sym (* forward *);
  var size: int_type;
      s: sym;
      df: def;
  begin
    s := first_sym;     (* chase next chain to prevent pdl overflows *)
    while s <> nil do with s^ do begin
      if visited <> relocating then begin
	size := sym_size (s);
	if relocating then begin
	  file_loc := reserve (size);   (* loc in deb file *)
	  visited := true;
	end
	else (* writing *) with deb_record do begin 
	  deb_sym.name := trans_nam (name);
	  deb_sym.block := trans_blk (block);
	  deb_sym.next := trans_sym (next);
	  deb_sym.type_desc := trans_typ (type_desc);
	  deb_sym.kind := kind;
	  case kind of
	    fields: begin
	      deb_sym.fld_record := trans_typ (fld_record);
	      deb_sym.fld_variant := trans_typ (fld_variant);
	      deb_sym.fld_offset := fld_offset;
	      deb_sym.fld_width := fld_width;
	      deb_sym.fld_number := fld_number;
	    end;
	    consts, vars, values, conditions: begin
	      deb_sym.dcl_class := dcl_class;
	      deb_sym.public_dcl := public_dcl;
	      deb_sym.standard := standard;
	      deb_sym.maskable := maskable;
	      if (dcl_class = constant_sc) and (init_value.kind = alloc_cst) then begin
		(* supply address of constant *)
		deb_sym.allocated := true;
		df := def (init_value.defp);    (* get definition pointer *)
		deb_sym.item_addr := df^.addr;
	      end
	      else begin
		deb_sym.allocated := allocated;
		if dcl_class <> static_sc then
		  deb_sym.item_addr := item_addr
		else if kind = conditions then
		  deb_sym.item_addr := item_addr + size_init + size_uninit
		else begin
		  assert (kind = vars);
		  if init_value.kind = no_value then
		    deb_sym.item_addr := item_addr + size_init
		  else
		    deb_sym.item_addr := item_addr
		end
	      end;
	      if (kind = consts) and (not deb_sym.allocated) and (dcl_class <> external_sc) then
		deb_sym.init_value := trans_val (init_value);
	    end
	  end (* case *);
	  place_entry (size, file_loc);
	  visited := false;
	end (* writing *);
        scan_nam (name);
        scan_blk (block);
        scan_typ (type_desc);
        if kind = fields then begin
          scan_typ (fld_record);
          scan_typ (fld_variant);
        end;
      end;
    exit if s = last_sym;
      s := next;
    end (* with *)
  end (* scan_sym *);
$PAGE prune_sym_list
  (* PRUNE_SYM_LIST is called only in the first pass to filter
     unrequired sym records (such as named types) from a sym list.      *)

  procedure prune_sym_list (var s_list: sym_list);

  var new_first, new_last, current: sym;

  begin
    new_first := nil;
    new_last := nil;
    current := s_list.first;
    while current <> nil do with current^ do begin
      if kind in [fields, consts, vars, values, conditions, blocks] then begin
        if new_first = nil then new_first := current
        else new_last^.next := current;
        new_last := current;
      end;
    exit if current = s_list.last;
      current := current^.next;
    end;
    if new_last <> nil then
      if new_last <> s_list.last then
        new_last^.next := s_list.last^.next;
    s_list.first := new_first;
    s_list.last := new_last;
  end (* prune_sym_list *);
$PAGE scan_typ
  (* SCAN_TYP relocates pointers within a "type_node" *)

  procedure scan_typ (* forward *);

  var t_size: int_type;

  begin
    if t <> nil then with t^ do begin
      if visited <> relocating then begin
        if kind = indirect_type then
	  visited := relocating (* always "seen", never processed *)
        else begin
          t_size := typ_size (t);
          if relocating then begin
	    file_loc := reserve (t_size);
            visited := true;
          end
          else (* writing *) with deb_record do begin (* pass 3 *)
            deb_typ.size := base_size;
            deb_typ.packable := packable;
            deb_typ.flexible := flexible;
            deb_typ.generic := generic;
            deb_typ.kind := kind;
            case kind of
              bools, ints, chars, scalars: begin
                deb_typ.base_type := trans_typ (base_type);
                deb_typ.minval := minval;
                deb_typ.maxval := maxval;
                if kind in [bools, scalars] then begin
                  deb_typ.cst_list.first := trans_sym (cst_list.first);
                  deb_typ.cst_list.last := trans_sym (cst_list.last);
                end;
              end;
              reals: begin
                deb_typ.rminval := rminval;
                deb_typ.rmaxval := rmaxval;
                deb_typ.precision := precision;
              end;
              sets:
                deb_typ.set_element_type := trans_typ (set_element_type);
              pointers:
                deb_typ.target_type := trans_typ (target_type);
              arrays:  begin
                deb_typ.element_size := element_size;
                deb_typ.element_type := trans_typ (element_type);
                deb_typ.index_type := trans_typ (index_type);
              end;
              files:
                deb_typ.component_type := trans_typ (component_type);
              strings: begin
                deb_typ.str_kind := str_kind;
                deb_typ.str_length := str_length;
              end;
              records, variants: begin
                deb_typ.field_list := trans_sym (field_list);
                deb_typ.variant_tag := trans_typ (variant_tag);
                if kind = variants then begin
                  deb_typ.tag := trans_typ (tag);
                  deb_typ.next_variant := trans_typ (next_variant);
                  deb_typ.others_var := others_var;
                  deb_typ.minlab := minlab;
                  deb_typ.maxlab := maxlab;
                end;
              end;
              tags: begin
                deb_typ.tag_field := trans_sym (tag_field);
                deb_typ.tag_type := trans_typ (tag_type);
                deb_typ.tag_recvar := trans_typ (tag_recvar);
                deb_typ.first_variant := trans_typ (first_variant);
              end;
              procs, funcs: begin
                deb_typ.return_type := trans_typ (return_type);
                deb_typ.nparms := upperbound (params);
                deb_typ.fortran_call := fortran_call;
                deb_typ.parmlist_size := parmlist_size;
              end
            end (* case *);
            place_entry (t_size, file_loc);
            visited := false;
          end (* writing *);
	end (* not indirect_type *);

        case kind of
	  ints, chars:
	    scan_typ (base_type);
          bools, scalars: begin
            scan_typ (base_type);
	    if relocating then
	      prune_sym_list (cst_list);
	    scan_sym (cst_list.first, cst_list.last);
          end;
          sets: 
            scan_typ (set_element_type);
          pointers:
            scan_typ (target_type);
          arrays: begin
            scan_typ (element_type);
            scan_typ (index_type);
          end;
          files:
            scan_typ (component_type);
          records, variants: begin
            scan_sym (field_list, nil);
            scan_typ (variant_tag);
            if kind = variants then begin
              scan_typ (tag);
              scan_typ (next_variant);
            end;
          end;
          tags: begin
            scan_sym (tag_field, tag_field);
            scan_typ (tag_type);
            scan_typ (tag_recvar);
            scan_typ (first_variant);
          end;
          procs, funcs:
            scan_typ (return_type);
          indirect_type:
            scan_typ (actual_type)
        end (* case *);
      end;
    end (* with *)
  end (* scan_typ *);
$PAGE scan_blk
  (* SCAN_BLK relocates pointers within a "block_node" *)

  procedure scan_blk (* forward *);

  var size: int_type;

  begin
    if b <> nil then with b^ do begin
      if visited <> relocating then begin
	size := blk_size (b);
	if relocating then begin
	  file_loc := reserve (size);
	  visited := true;
          prune_sym_list (parm_list);
          prune_sym_list (id_list);
	end
	else (* writing *) with deb_record do begin
	  deb_blk.parent := trans_blk (parent);
	  deb_blk.peer := trans_blk (peer);
	  deb_blk.children := trans_blk (children);
	  deb_blk.level := level;
	  deb_blk.return_sym := trans_sym (return_sym);
	  deb_blk.parm_list.first := trans_sym (parm_list.first);
	  deb_blk.parm_list.last := trans_sym (parm_list.last);
	  deb_blk.id_list.first := trans_sym (id_list.first);
	  deb_blk.id_list.last := trans_sym (id_list.last);
	  deb_blk.semantic_options := semantic_options;
	  deb_blk.parm_list_base := parm_list_base;
	  deb_blk.kind := kind;
	  case kind of
	    program_blk, module_blk, data_blk: begin
	      deb_blk.id := trans_nam (id);
	      deb_blk.start_addr := start_addr;
	      deb_blk.comp_dtime := comp_dtime;
	    end;
	    subr_blk:  begin
	      deb_blk.subr_sym := trans_sym (subr_sym);
	    end
	  end (* case *);
	  if kind in [program_blk, module_blk, data_blk, subr_blk] then
	    def_value (get_def (deb_sym_def, number), file_loc, false);
	  place_entry (size, file_loc);
	  visited := false;
	end (* writing *);

        scan_sym (parm_list.first, parm_list.last);
        scan_sym (id_list.first, id_list.last);
        if kind = root_blk then
          scan_sym (type_list.first, type_list.last);
        scan_blk (peer);        (* no need to scan parent *)
        scan_blk (children);
        scan_sym (return_sym, return_sym);
        case kind of
          program_blk, module_blk, data_blk:
            scan_nam (id);
          subr_blk: 
            scan_sym (subr_sym, subr_sym)
        end (* case *);
      end;
    end (* with *);
  end (* scan_blk *);
$PAGE filter_blocks
(* FILTER_BLOCKS removes unwanted block_nodes from the various block chains *)

function filter_blocks (b: blk): blk;

begin
  filter_blocks := b;   (* default *)
  if b <> nil then with b^ do begin
    case kind of
      root_blk, program_blk, module_blk, data_blk, subr_blk: begin

        (* filter chains from these blocks *)

        peer := filter_blocks (peer);
        children := filter_blocks (children);
      end;
      others: begin
        filter_blocks := filter_blocks (peer)   (* remove from linkage *)
      end
    end;
  end;
end;
$PAGE deb_stable - body
begin
  if rel_file = '' then return; (* <---- no rel file, no debug file *)
  init_deb_file;

  root_block := filter_blocks (root_block);     (* remove externs, etc. *)
  relocating := true;
  scan_blk (root_block);
  rebush_nam_tree;
  relocating := false;
  scan_blk (root_block);

  close_deb_file;
end.
@PR