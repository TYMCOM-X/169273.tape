$TITLE PASALC - Pascal Compiler Storage Allocation

module pasalc;

$HEADER pasalc.hdr
$PAGE include files
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM pasdat.inc
$SYSTEM pastal.inc
$SYSTEM pasmth.inc
$SYSTEM passw.inc

external procedure dmpstorage;
$PAGE parameters
$INCLUDE pasalc.prm
$PAGE global variables
var counter: array [area_class] of unit_range;
$PAGE alc_init
(*  ALC INIT initializes the storage allocation tables by copying them from
    their resting place on the heap.  *)

external var alc_tables: save_pointer;

public procedure alc_init  options special(coercions);

begin
  hp_get (ord (address (alc_tbl_start)), ord (address (alc_tbl_end)), alc_tables);
end;
$PAGE alc_space
(*  ALC SPACE will allocate a specified number of bytes with a specified
    alignment in a specified area, adjusting the area's location counter,
    and returning the address of the allocated space.  *)

function alc_space (area: area_class; bytes: unit_range; align: align_range): unit_range;

var c: bit_range;

begin
  c := counter [area];

  if alc_area_descs [area].pos_allocation then begin
    c := ngm (c, align);
    alc_space := c;
    c := c + bytes;
  end

  else (* neg_allocation *) begin
    c := ngm (c + bytes, align);
    alc_space := - c;
  end;

  counter [area] := c;
end (* alc_space *);
$PAGE alloc_symbol
(*  ALLOC SYMBOL will perform the first pass storage allocation for a
    single symbol.  *)

procedure alloc_symbol ( s: sym );

var bits: bit_range;
    bytes: unit_range;
    align: align_range;

begin
  with s^ do begin
    case kind of

      vars, values:
	begin
	  alc_data (type_desc, bits, align);
	  bytes := bits div byte_size;
	  align := align div byte_size;
	  case dcl_class of
	    local_sc, parameter_sc:
	      item_addr := alc_space (loc_area, bytes, align);
	    static_sc:
	      if init_value.kind = no_value
		then item_addr := alc_space (uninit_area, bytes, align)
		else item_addr := alc_space (init_area, bytes, align);
	    others: 
	  end (* case dcl_class *);
	end;

      conditions:
	if dcl_class <> external_sc then
	  item_addr := alc_space (cond_area, cond_size, cond_alignment);

      others:
	(* no allocation for other symbols *)

    end (* case kind *);
  end (* with s^ *);
end (* alloc_symbol *);
$PAGE offset_symbols
(*  OFFSET SYMBOLS will add a specified offset to the ItemAddr field of each
    local or parameter symbol in a symbol list.  *)

procedure offset_symbols (symbols: sym; offset: unit_range);

var s: sym;

begin
  s := symbols;
  while s <> nil do begin
    with s^ do begin
      if (kind in [vars, values]) andif (dcl_class in [local_sc, parameter_sc]) then
	item_addr := item_addr + offset;
      s := next;
    end;
  end;
end (* offset_symbols *);
$PAGE allocate_all_symbols
(*  ALLOCATE ALL SYMBOLS performs a complete preliminary storage
    allocation.  It assigns the correct final location to all conditions
    and all static variables.  Local variables and parameters are assigned
    stack frame offsets which will be correct if their block is its own
    owner (i.e., is non-quick).  Local variables in quick blocks will later
    have to be relocated by the offset of their block's stack area in its
    owner's stack frame.  If quick block analysis is not in effect, then
    the storage locations assigned by this routine will be final.  *)

procedure allocate_all_symbols;

type
    syms_of_interest = (rvsym, plsym, varsym, nosym);
    syms_order = array [1..7] of syms_of_interest;

const
    init_order: syms_order =
      ( nosym, nosym, nosym, varsym, nosym, nosym, nosym );

var block: blk;
    rv_by_address, rv_in_parmlist, pl_in_stackframe: boolean;
    parmlist_size, pl_size: unit_range;
    pl_area: area_class;
    loc: 1 .. 7;
    alc_order: syms_order;
    symbols: sym;

begin
  block := root_block;
  while block <> nil do begin
    with block^ do begin
      if owner = block then begin
	counter [pos_stack_area] := alc_area_descs [pos_stack_area].init_offset;
	counter [neg_stack_area] := alc_area_descs [neg_stack_area].init_offset;
      end
      else begin
	counter [pos_stack_area] := 0;
	counter [neg_stack_area] := 0;
      end;
      pos_stk_begin := counter [pos_stack_area];
      neg_stk_begin := - counter [neg_stack_area];

      alc_order := init_order;

      if return_sym <> nil then begin
	rv_by_address := passed_by_address (return_sym);
	with retsym_loc [rv_by_address] do begin
	  rv_in_parmlist := in_parmlist;
	  if not in_parmlist then begin
	    if at_start then begin
	      if before_parmlist
		then loc := 1
		else loc := 3;
	    end
	    else begin
	      if before_parmlist
		then loc := 5
		else loc := 7;
	    end;
	    alc_order [loc] := rvsym;
	  end;
	end (* with retsym_loc *);
      end
      else
	rv_in_parmlist := false;

      if kind = subr_blk then begin
	parmlist_size := subr_sym^.type_desc^.parmlist_size;
	pl_in_stackframe := (parmlist_size <= pl_size_limit);
	if pl_in_stackframe
	  then pl_size := parmlist_size
	  else pl_size := parm_ptr_size;
	with parmlist_loc [pl_in_stackframe] do begin
	  pl_area := area;
	  if (area <> loc_area) or at_start
	    then loc := 2
	    else loc := 6;
	end;
	alc_order [loc] := plsym;
      end
      else
	pl_in_stackframe := false;

      for loc := 1 to 7 do begin
	case alc_order [loc] of

	  rvsym:
	    if rv_by_address
	      then return_sym^.item_addr := alc_space (loc_area, parm_ptr_size, basic_alignment)
	      else alloc_symbol (return_sym);

	  plsym:
	    parm_list_base := alc_space (pl_area, pl_size, basic_alignment);

	  varsym:
	    begin
	      symbols := id_list.first;
	      while symbols <> nil do begin
		alloc_symbol (symbols);
		symbols := symbols^.next;
	      end;
	    end;

	  nosym:
	    (* no allocation *)

	end;
      end (* for loc *);

      if pl_in_stackframe then begin
	offset_symbols (parm_list.first, parm_list_base);
	if rv_in_parmlist then
	  return_sym^.item_addr := return_sym^.item_addr + parm_list_base;
      end;

      pos_stk_end := ngm (counter [pos_stack_area], basic_alignment);
      neg_stk_end := - ngm (counter [neg_stack_area], basic_alignment);
      pos_local_size := pos_stk_end - pos_stk_begin;
      neg_local_size := neg_stk_begin - neg_stk_end;

      block := downward_call_thread;
    end (* with block^ *);
  end (* while block <> nil *);
end (* allocate_all_symbols *);
$PAGE compute_block_offsets
(*  COMPUTE BLOCK OFFSETS computes StackBegin and StackEnd (both positive
    and negative) for each program and subroutine block.  StackBegin is the
    offset of a block's stack area from the start of its owner's stack
    frame.  StackEnd is the maximum, for a block and for any blocks with
    the same owner which can be called by it (directly or indirectly), of
    the sums StackBegin + LocalSize.  Local and parameter symbols within
    quick blocks are relocated by the offsets of their blocks' stack frames
    within their owner blocks.  *)

procedure compute_block_offsets;

var block, last_block: blk;
    pl_in_local, rv_in_parmlist: boolean;
    call_list: call_link;

begin

  (*  Compute StackBegin for each block.  Relocate the local symbols and
      parameters in quick blocks, according to StackBegin.  Set StackEnd
      to StackBegin + LocalSize.  *)

  block := root_block;
  while block <> nil do begin
    with block^ do begin
      if kind in [program_blk, subr_blk] then begin

	if block <> owner then begin
	  counter [pos_stack_area] := pos_stk_begin;
	  counter [neg_stack_area] := neg_stk_begin;
	  offset_symbols (id_list.first, counter [loc_area]);
	  if kind = subr_blk then begin
	    pl_in_local := (subr_sym^.type_desc^.parmlist_size <= pl_size_limit);
	    parm_list_base := parm_list_base + counter [parmlist_loc [pl_in_local].area];
	    if pl_in_local then
	      offset_symbols (parm_list.first, counter [parmlist_loc [true].area]);
	    if return_sym <> nil then begin
	      rv_in_parmlist := retsym_loc [passed_by_address (return_sym)].in_parmlist;
	      if rv_in_parmlist and pl_in_local then
		return_sym^.item_addr :=
		  return_sym^.item_addr + counter [parmlist_loc [true].area]
	      else if not rv_in_parmlist then
		return_sym^.item_addr := return_sym^.item_addr + counter [loc_area];
	    end;
	  end;
	end (* if owner <> block *);

	pos_stk_end := pos_stk_begin + pos_local_size;
	neg_stk_end := neg_stk_begin - neg_local_size;

	call_list := calls;
	while call_list <> nil do begin
	  with call_list^ do begin
	    if (called_subr^.owner = owner) and
	       (called_subr <> owner) and
	       (called_subr <> block) then begin
	      called_subr^.pos_stk_begin := max (called_subr^.pos_stk_begin, pos_stk_end);
	      called_subr^.neg_stk_begin := min (called_subr^.neg_stk_begin, neg_stk_end);
	    end;
	    call_list := rlink;
	  end;
	end (* while call_list <> nil *);

      end (* if kind in [program_blk, subr_blk] *);
      last_block := block;
      block := downward_call_thread;
    end;
  end (* while block <> nil *);

  (*  Compute StackEnd for each block.  *)

  block := last_block;
  while block <> nil do begin
    with block^ do begin
      if kind in [program_blk, subr_blk] then begin

	call_list := calls;
	while call_list <> nil do begin
	  with call_list^ do begin
	    if called_subr^.owner = owner then begin
	      pos_stk_end := max (pos_stk_end, called_subr^.pos_stk_end);
	      neg_stk_end := min (neg_stk_end, called_subr^.neg_stk_end);
	    end;
	    call_list := rlink;
	  end;
	end (* while call_list <> nil *);

      end (* if kind in [program_blk, subr_blk] *);
      block := upward_call_thread;
    end;
  end (* while block <> nil *);

end (* compute_block_offsets *);
$PAGE allocate_storage
(*  ALLOCATE STORAGE assigns an address to each condition, variable, and
    parameter symbol in the compilation.  Conditions are allocated relative
    to the start of the condition area; static variables are allocated
    relative to the start of the initialized or uninitialized static areas;
    and local variables and parameters are allocated relative to the starts
    of the stack frames for their blocks' owner blocks.  *)

public procedure allocate_storage;

var area: static_areas;

begin
  for area := minimum (static_areas) to maximum (static_areas) do
    counter [area] := alc_area_descs [area].init_offset;

  allocate_all_symbols;

  (*  Allocation is now complete, unless quick block analysis is in effect.  *)

  if qbl_allowed and (qblocks_opt in all_opts) then
    compute_block_offsets;

  size_init := ngm (counter [init_area], basic_alignment);
  size_uninit := ngm (counter [uninit_area], basic_alignment);
  size_cond := ngm (counter [cond_area], basic_alignment);

  if switch (prog_options.dump_switches, 'STORAGE') then
    dmpstorage;
end (* alloc_symbols *).
