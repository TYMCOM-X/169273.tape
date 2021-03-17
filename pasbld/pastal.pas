$TITLE PASTAL.PAS, last modified 5/14/84, zw
MODULE pastal options special(word);
(*TYM-Pascal compiler Type Allocation Module*)
$HEADER pastal.hdr
$PAGE includes
$SYSTEM ENVUTL.INC
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM pasmth.inc
$PAGE All About Type Allocation
$HEADER pastal.not
$PAGE parameters
$INCLUDE pastal.prm
$PAGE tal_init
(*  TAL INIT initializes the type allocation tables by copying them from
    their resting place on the heap.  *)
external var tal_tables: environment;
public procedure tal_init  options special(coercions);
begin
  getenv(ord (address (tal_tbl_start)), ord (address (tal_tbl_end)), tal_tables);
end;
$PAGE pack_in_context
(* PACK IN CONTEXT computes the width and alignment of an item of a particular
   type given the context for which it is to be packed.  *)
procedure pack_in_context
      ( type_desc: typ; (* type of datum *)
        context: packing_contexts;
        var datum_width: bit_range;     (* output *)
        var datum_alignment: align_range  );
var r: rule;
    min_alignment: align_range;
begin
  if type_desc = nil then begin (* return dummy values for bad type *)
    datum_width := efw;
    datum_alignment := efa;
    return; (* <---- return with dummy values *)
  end;
  with type_desc^ do begin
    (*  In general, there is a single packing rule list for each type kind and
	context, and there are no a priori alignment restrictions.  However,
	there are special rule lists for signed integers and varying strings,
	and there are a priori alignment constraints for records and arrays,
	based on the alignment constraints for their components.  *)
    r := allocation_tables [kind, context];
    min_alignment := 1;
    case kind of
      scalars, bools, chars:
	if packable then
	  r := packed_scalar_rules [kind, context];
      ints:
	r := integer_rules [(minval < 0), packable, context];
      strings:
	if str_kind = varying then
	  r := var_string_rules [context];
      records:
	min_alignment := record_alignment;
      arrays:
	min_alignment := element_alignment;
      others:
    end (* case kind *);
    (*  Now that we have a rule list for this type kind, we must find the
	rule that applies to a type with this particular base size.  *)
    while r^.limit < base_size do
      r := r^.next;
    (*  Use the rule and the type-specific information to allocate the type.  *)
    datum_width := ngm (base_size, r^.width_factor);
    datum_alignment := max (min_alignment, r^.alignment);
  end (* with type_desc^ *);
end (* pack_in_context *);
$PAGE alc_data
(* ALC DATA is a public wrapper for PackInContext.  It does not take a context
   parameter, always assuming NoPacking instead. *)
public procedure alc_data ( type_desc: typ;
                            var datum_width: bit_range;
                            var datum_alignment: align_range );
begin
  pack_in_context (type_desc, no_packing, datum_width, datum_alignment);
end;
$PAGE size_of
(*  SIZE OF is used to determine the elements for a runtime computation of the
    size of a variable of a specified type.  *)
public procedure size_of ( data_type: typ; alloc_size: boolean;
			   var a, b, c, d: integer; var gruesome: boolean );
var flex_type: typ;
    field: sym;
    elem_size: bit_range;
    adjustment: bit_range;
    const_part: bit_range;
    dummy_alignment: align_range;
    g: integer;
begin
  gruesome := false;	(* We hope! *)
  d := 0;
  if not data_type^.flexible then begin
    (*  For a non-flexible type, Size = BaseSize.  *)
    pack_in_context (data_type, no_packing, const_part, dummy_alignment);
    a := 0;
    b := (const_part + byte_size - 1) div byte_size;
    c := 1;
  end
  else begin
    (*  Find the element size and adjustment for the flexible array or string.
	The adjustment is the amount by which the X parameter in the runtime
	size computation must be decremented to give the actual number of
	elements.  For a string, this is zero, since X is the length.  For
	a flexible array, it is one less than the lower bound, since X is
	the upper bound, and the number of elements is (upb-lwb+1).  For
	a generic array, X is (upb-lwb), so the adjustment is -1.
        Arrays and strings can always be passed as var parameters, even when
	they occur in packed records.  Therefore, they must always be byte
	aligned.  Since the offset of a flexible array or string in a record
	or variant is simply the BaseSize of the record or variant, we can
	assume that BaseSize is an integral number of bytes.  *)
    flex_type := data_type;
    case flex_type^.kind of
      arrays:
      	if alloc_size
	  then const_part := ngm (arr_desc_rules [no_packing]^.width_factor,
				  max (flex_type^.element_alignment,
				       allocation_tables [arrays, no_packing]^.alignment))
	  else const_part := 0;
      strings:
	begin
	  if alloc_size
	    then const_part := str_desc_rules [no_packing]^.width_factor
	    else const_part := 0;
	  if flex_type^.str_kind = varying then
	    const_part := const_part + str_lw_width;
	end;
      records,
      variants:
	begin
	  const_part := ngm (data_type^.base_size,
			     allocation_tables[records,no_packing]^.width_factor);
	  assert (const_part mod byte_size = 0);
	  field := data_type^.field_list;	(* Find the last (flexible) field. *)
	  while (field^.next <> nil) andif (field^.next^.fld_variant = flex_type) do
	    field := field^.next;
	  flex_type := field^.type_desc;
	end;
      others:
	assert (false)
    end (* case flex_type^.kind *);
    if flex_type^.kind = strings then begin
      elem_size := str_char_size;
      adjustment := 0;
    end
    else begin
      elem_size := flex_type^.element_size;
      if flex_type^.generic
	then adjustment := -1
	else adjustment := flex_type^.index_type^.minval - 1;
    end;
    if (elem_size >= efw) or (efw mod elem_size = 0) or (efa < efw) then begin
      (*  If an element is larger than an extraction field (and therefore,
	  necessarily, a multiple of EFA, which is a multiple of ByteSize),
	  or elements pack neatly into EF's, or EF boundaries don't matter,
	  then Size = (X * ElemSize) + BaseSize.  *)
      a := elem_size;
      b := (const_part - elem_size * adjustment) + byte_size - 1;
      c := byte_size;
    end
    else (* (elem_size < efw) and (efw mod elem_size <> 0) and (efa = efw) *) begin
      (*  If elements do not pack neatly into EF's, and may not cross EF
	  boundaries, then the computation is more complex.  *)
      if efw = byte_size then begin
	(*  If bytes and extraction fields are the same, then life still
	    isn't too bad.  Let Epb (Elements per Byte) be ByteSize div
	    ElemSize.  The base requires (BaseSize div ByteSize) bytes;
	    the array requires ((X + EpB - 1) div EpB) bytes.  The total
	    size can therefore be expressed as
	      [X + (BaseSize div ByteSize) * EpB + EpB - 1] div EpB.  *)
	a := 1;
	c := byte_size div elem_size;
	b := (const_part div byte_size) * c - adjustment + c - 1;
      end
      else begin
	(*  This is the gruesome case.  Let EpF (Elements per Field) be
	    Efw div ElemSize.  The total space required will be:  (1) The
	    number of full EF's needed, (X' div EpF), times the number of
	    bytes in an EF; plus (2) the number of elements left over after
	    the last full EF, (X' mod EpF), times ElemSize, divided by
	    ByteSize; plus (3) the base size, in bytes.  Believe it or not,
	    this whole mess can be expressed as
	      ( ((X - Adjustment) div (Efw div ElemSize)) * Efw) +
		((X - Adjustment) mod (Efw div ElemSize)) * ElemSize) +
		BaseSize + ByteSize - 1 )   div ByteSize  *)
	gruesome := true;
	a := efw;
	b := elem_size;
	c := const_part + byte_size - 1;
	d := adjustment;
      end;
    end;
    (*  Scale the constants down by the greatest common divisor of the
	multiplier term (A) and the divisor term (C).  *)
    if not gruesome then begin
      if a = 0
	then g := c
	else g := gcd (a, c);
      a := a div g;
      b := b div g;
      c := c div g;
    end;
  end (* if data_type^.flexible *);
end (* size_of *);
$PAGE map_array
(* MAP ARRAY determines the space that should be allocated for each element of
   an array, and the total space that will be occupied by the array.  For flexible
   arrays, whose length is not known at compile time, MapArray may be called
   with Nelems = Efw + 1, to guarantee that the elements will be allocated for
   the general case (in which they will not all fit in a single extraction
   field. *)
procedure map_array
      ( elem_type: typ;                 (* type of elements of array *)
        packing: packing_contexts;       (* either minimal or field packing *)
        nelems: int_type;               (* number of elements in array *)
        var elem_width: bit_range;              (* size in which to store elements *)
        var array_width: bit_range;              (* width of array as allocated *)
	var elem_alignment: align_range );	(* intrinsic element alignment in context *)
var
  elems_per_ef: int_type;                     (* number of elements per extraction field *)
  trial_width: bit_range; (* used when relaxing the packing context *)
  trial_alignment: align_range;
  trial_context: packing_contexts;
begin
  (* Get the width of the array elements to be mapped.  We round this width up
     to the alignment of the element data, as each successive element must be
     aligned on such a boundary. *)
  pack_in_context (elem_type, packing, elem_width, elem_alignment);
  elem_width := ngm (elem_width, elem_alignment);
  (* If the width of the element data is greater than the largest packed field,
     then there is no hope of packing the data, even if the array is specified
     as being packed.  Therefore, the array might as well be allocated with
     each element beginning on an extraction field boundary. *)
  if elem_width >= efw then begin
    elem_width := ngm (elem_width, efa);
    array_width := elem_width * nelems;
    elem_alignment := max (elem_alignment, efa);
  end
  (* Otherwise, the fields are small enough to be packed.  If efa < efw then we
     can access an element regardless of the machine boundaries, which means we
     can simply pack everything together with no slack space.  Alternatively, if
     the entire array can be packed into a single extraction field, then there
     is no need to worry about slack space. *)
  else if (efa < efw) or (nelems * elem_width <= efw) then
    array_width := nelems * elem_width
  (* An individual array element will fit in less than an extraction field,
     but the array as a whole requires more than one extraction field.  Thus,
     it is necessary to determine how many array elements will fit in each
     extraction field, and then allocate as many extraction fields as required
     for the array.  If the context is minimal packing, then it may be profitable
     to see if we can get the same number of elements in an extraction field
     using slack packing, or even field packing. *)
  else begin
    elems_per_ef := efw div elem_width;                       (* number of elements packable in one ef *)
    if packing = minimal_packing then begin
      for trial_context := field_packing downto slack_packing do begin
	pack_in_context (elem_type, trial_context, trial_width, trial_alignment);
	trial_width := ngm (trial_width, trial_alignment);
      exit if elems_per_ef * trial_width <= efw
	do begin
	  elem_width := trial_width;
	  elem_alignment := trial_alignment;
	end;
      end;
    end;
    array_width := efw * (nelems div elems_per_ef) + elem_width * (nelems mod elems_per_ef);
    elem_alignment := max (elem_alignment, efa);
  end;
end (* map_array *);
$PAGE map_record
(*  MAP RECORD determines the "fld_width" and "fld_offset" for each field in
    a record.  It also determines sizes for the record, tag, and variant nodes
    in the record.  The size of the record is the maximum size for any variant;
    the size of a variant is the size for that particular variant of the record
    type; the size field in a tag node specifies the size for unspecified
    variants (those with null field lists).
    MapRecord is simply a wrapper, which applies the general record/variant
    mapping routine MapRecVariant to the record, with starting offset 0.  *)
procedure map_record ( rec_type: typ; packing: packing_contexts );
$PAGE map_rec_variant - in map_record
(*  MAP REC VARIANT is the working routine for MapRecord.  It performs the
    mapping for a single record or variant, invoking itself recursively (via
    AllocVariants) to map any sub-variants.
    Parent is the record or variant to be mapped by the call.  Packing is
    the appropriate packing context for the fields in the record -- either
    FieldPacking, for an unpacked record, or MinimalPacking, for a packed
    record.  InitRo is the offset, relative to the start of the record, at
    which allocation for fields of this particular variant should begin (for
    the record itself, InitRo will be 0).  On return, FirstFldOffset will be
    the offset, relative to the start of the record, of the first field in
    this record or variant ("maximum (bit_range)" if there are no fields in
    it).  Ra will be the maximum alignment requirement of any field in the
    record or variant:  a record must be aligned at least as strictly as the
    strictest alignment of any field within it, since otherwise the align-
    ments within it could not be guaranteed.
    The variable Ro is used to keep track of the offsets assigned to fields
    within this record or variant.  At any point during the routine, Ro is the
    bit address of the first unassigned bit within this record or variant; at
    the conclusion of the routine, Ro may therefore be interpreted as the total
    size, in bits, of this record or variant, or its largest sub-variant.  *)
procedure map_rec_variant
      ( parent: typ;
        init_ro: bit_range;
        var first_fld_offset: bit_range;
        var ra: align_range  );
var ro: bit_range;
$PAGE alloc_variants - in map_rec_variant
(*  ALLOC VARIANTS processes all the variant parts of a record case.  The
    sub field lists are allocated starting at the offset of the end of the
    preceding fixed part.  This also keeps track of the alignment requirements
    of each variant, and the minimum offset of the first fields in the variants.
    The latter is returned via NextOffset so ReallocField can perform slack
    allocation on the previous field.  *)
procedure alloc_variants ( tag: typ; var next_offset: bit_range );
var variant: typ;                             (* variant being processed *)
    syn_variant: typ;                         (* variant(s) with same field list as the above *)
    max_width: bit_range;                           (* maximum variant width (size) *)
    vo: bit_range; (* first field offset for a variant *)
    va: bit_range; (* required alignment for a variant *)
begin
  max_width := ro;                                 (* minimum possible *)
  next_offset := maximum (bit_range);        (* default if no fields seen *)
  variant := tag^.first_variant;                  (* process all variants at the tag *)
  while variant <> nil do begin
    map_rec_variant (variant, ro, vo, va);
    ra := max (ra, va);     (* adjust record alignment for fields in variant *)
    max_width := max (max_width, variant^.base_size);        (* get max size of any variant *)
    next_offset := min (next_offset, vo);  (* closest field in any variant *)
    (*  Several variants may share the same field list, so for each such
	"synonym", copy the size information.  *)
    syn_variant := variant^.next_variant;
    while (syn_variant <> nil) andif
	  (syn_variant^.field_list = variant^.field_list) do begin
      syn_variant^.base_size := variant^.base_size;
      syn_variant := syn_variant^.next_variant;
    end;
    variant := syn_variant;                  (* process next non-synonym on next iteration *)
  end (* while variant <> nil *);
  ro := max_width;                                   (* size of record will be max of all variant widths *)
end (* alloc_variants *);
$PAGE alloc_field - in map_rec_variant
(*  ALLOC FIELD performs the preliminary allocation for a single field in a
    record or variant.  This preliminary allocation is based only on the type
    of the field and the context.  Later, if there is slack space available
    following the field, REALLOC FIELD may adjust its allocation.  The global
    offset counter RO is adjusted past the end of the field.  The allocation
    counter RA is increased, if necessary, to guarantee that the record as a
    whole will be aligned so as to force the correct alignment of the field.  *)
procedure alloc_field ( field: sym );
var fw: bit_range;                            (* field width in bits *)
    fa: align_range;                          (* field alignment *)
    desc_rule: rule;				(* flex array/string descriptor  rule *)
begin
  with field^ do begin
    (*  Get the necessary alignment and width of the field to be allocated.  *)
    pack_in_context (type_desc, packing, fw, fa);
    (*  If the field is a flexible array or string, then allocate it with room
	for its descriptor (the "hidden length word").  Align the field so that
	the descriptor and the data are contiguous, and the alignment require-
	ments of both the descriptor and the data are satisfied.  Assume that
	the descriptor alignments are multiples of EFA, so there is no boundary
	problem.  (See the footnote below for the justification of the offset
	formula.)  *)
    if type_desc^.flexible and (type_desc^.kind in [arrays, strings]) then begin
      if type_desc^.kind = arrays
	then desc_rule := arr_desc_rules [packing]
	else desc_rule := str_desc_rules [packing];
      with desc_rule^ do begin
	ro := ngm (ro + width_factor, max (fa, alignment)) - width_factor;
	ra := max (ra, fa, alignment);
	fw := fw + width_factor;
      end;
    end
    (*  The field is not a flexible array or string, so the size we have for it
	is valid.  If EFA < EFW, txtraction field boundaries may be disre-
	garded; otherwise, it may be necessary to align the field to the start
	of an extraction field, to keep it from falling across a boundary.  *)
    else begin
      ro := ngm (ro, fa);                                (* get offset satisfying field alignment *)
      ra := max (ra, fa); (* force required record alignment *)
      if (efw = efa) and ((ro mod efw) + fw > efw) then begin
	ro := ngm (ro, efa);
	ra := max (ra, efa);
      end;
    end;
    (*  Allocate the field.  *)
    fld_offset := ro;
    fld_width := fw;
    ro := ro + fw;
  end (* with field^ *);
end (* alloc_field *);
(*--------------------------------------
    Justification of the flexible field offset formula.
    Let DW and DA be the descriptor width and required alignment.  Let FA be the
    required alignment for the field, disregarding the length word.  Let RO be
    the current offset in the record.  We want to compute an offset RO' for the
    descriptor, where RO' is the smallest number such that RO' >= RO, RO' is a
    multiple of DA, and (RO'+DW) is a multiple of FA.  This will not be possible
    if DW is not a multiple of DA, so assume that it is.  There are two cases
    to consider:
    (1)  DA >= FA.
    Then DA is a multiple of FA.  Let
	RO' = NGM (RO, DA).
    Then RO' >= RO and RO' is a multiple of DA.  since DA and DW are mul-
    tiples of FA, (RO'+DW) must be a multiple of FA.
    (2)  DA < FA.
    Then FA is a multiple of DA.  Let
	RO' = NGM (RO + DW, FA) - DW.
    Then RO' >= RO and (RO'+DW) is a multiple of FA.  Since FA and DW are mul-
    tiples of DA, RO' must also be a multiple of DA.
    Consider formula (1):  RO' = NGM (RO, DA).  Since DW is a multiple of DA,
    it follows from the definition of the NGM function that
	RO' = NGM (RO + DW, DA) - DW.
    But in the preceding formula, DA = MAX (DA, FA); and in formula (2),
    FA = MAX (DA, FA).  Therefore, the two cases collapse to the single
    formula:
	RO' = NGM (RO + DW, MAX (DA, FA)) - DW.			*)
$PAGE realloc_field - in map_rec_variant
(*  REALLOC FIELD is called with a field symbol and the offset of the next
    meaningful information following the field.  The packing context is
    MinimalPacking.  ReallocField considers the tentative size and offset
    which have been assigned to the field by AllocField, and determines
    whether there is sufficient slack space following the field for it to
    be allocated with slack or field packing, rather than minimal packing.
    If possible, this should be advantageous, since slack and field packed
    data are presumed to be more efficiently accessible than minimally
    packed data.  Since flexible fields have a whole collection of additional
    constraints, and only occur at the end of a field list, this  procedure
    is never applied to flexible fields.  *)
procedure realloc_field ( field: sym; next_offset: bit_range );
var trial_context: packing_contexts;
    trial_width: bit_range;
    trial_alignment: align_range;
    trial_offset: bit_range;
begin
  with field^ do begin
    for trial_context := field_packing downto slack_packing do begin
      pack_in_context (type_desc, trial_context, trial_width, trial_alignment);
      trial_offset := ngm (fld_offset, trial_alignment);
    exit if (trial_offset + trial_width <= next_offset) and (trial_alignment <= ra)
      do begin
	fld_offset := trial_offset;
	fld_width := trial_width;
	ro := max (ro, fld_offset + fld_width);
      end;
    end;
  end (* with field^ *);
end (* realloc_field *);
$PAGE map_rec_variant - main routine
var
  field, last_field, next_field, final_field: sym;
  next_offset: bit_range;
begin
  (*  To begin with, we walk the list of field symbols in the fixed part of
      this record or variant, assigning an initial size and offset in the
      record to each.  At the same time, we reverse the "next" pointers in
      the field symbols, so that we will be able to process the same list
      in the opposite order later.  At the conclusion of this loop, LastField
      will point to the last field symbol in the fixed part (Nil if there are
      none), and the symbols in the fixed part will be chained in reverse
      order on their "next" pointers, with the first field symbol having a
      "next" pointer of Nil.  *)
  ro := init_ro;
  ra := allocation_tables [records, minimal_packing]^.alignment; (* minimal *)
  last_field := nil;
  field := parent^.field_list;
  while (field <> nil) andif (field^.fld_variant = parent) do begin
    alloc_field (field);  (* Assign the width and offset for Field. *)
    next_field := field^.next; (* Rearrange the pointers. *)
    field^.next := last_field;
    last_field := field;
    field := next_field;
  end;
  (*  Okay, we've finished our preliminary allocation of the fixed field list.
      Now let's allocate the variants, if there are any.  When we're done, we
      want NextOffset to be the lowest starting offset of any variant, if there
      are any variants, and to be the start of the next "word" after the last
      fixed field, otherwise.  *)
  if parent^.variant_tag <> nil then
    alloc_variants (parent^.variant_tag, next_offset)
  else
    next_offset := ngm (ro, allocation_tables [records, no_packing]^.width_factor);
  final_field := last_field;
  (*  Finally, we will go through the fixed field list again, backwards this
      time, determining the final allocations of all the fixed fields (which
      may involve expanding the previously computed sizes or adjusting the
      previously computed offsets to improve field accessing), and restoring
      the original ordering of the "next" pointers.  *)
  next_field := last_field;
  last_field := field;
  field := next_field;
  while field <> nil do begin
    if (packing = minimal_packing) and (not field^.type_desc^.flexible) then
      realloc_field (field, next_offset);
    next_offset := field^.fld_offset;
    next_field := field^.next;
    field^.next := last_field;
    last_field := field;
    field := next_field;
  end;
  (*  Set the size of the entire record or variant, and the size of the
      subvariant which has no fields.  Return the offset of the first field
      in the record or variant.  *)
  parent^.base_size := ro;
  if parent^.variant_tag <> nil then begin
    if final_field = nil
      then parent^.variant_tag^.base_size := 0
      else parent^.variant_tag^.base_size := final_field^.fld_offset + final_field^.fld_width;
  end;
  first_fld_offset := next_offset;
end (* map_rec_variant *);
$PAGE map_record - main routine
var toff: bit_range;
    talign: align_range;
begin
  map_rec_variant (rec_type, 0, toff, talign);
  rec_type^.record_alignment := talign;
end;
$PAGE p_b_address
(*  P B ADDRESS determines if all parameters of a specified type must be passed
    by address instead of by value.  *)
public function p_b_address ( parm_type: typ ): boolean;
begin
  p_b_address :=
    (parm_type <> nil) andif
    ( (parm_type^.kind in pba_types) or
      (parm_type^.base_size > pbv_limit) or
      (parm_type^.flexible) );
end (* p_b_address *);
$PAGE passed_by_address
(*  PASSED BY ADDRESS determines if a parameter is one which is to be passed by
    address instead of by value. *)
public function passed_by_address ( parm: sym ): boolean;
begin
  passed_by_address :=
    ( (parm^.kind = vars) and
      ( (parm <> parm^.block^.return_sym) or pba_retsym) ) or
    p_b_address (parm^.type_desc);
end (* passed_by_address *);
$PAGE map_parm_list
(*  MAP PARM LIST determines the total space that must be allocated for the
    parameter list of a procedure or function type.  Basically, this just
    means allocating space for each parameter, using the ParmPacking context.
    In addition, if the type being allocated is the type of an actual routine,
    then its list of parameter symbols will also be passed, and the ItemAddr
    field of each symbol must be set to the location of the corresponding
    parameter in the list.  For some target machines, it will also be necessary
    to allocate space for the return value of a function somewhere in the list.
    The space allocated for a parameter is either true size of the parameter or,
    if the parameter will be passed by address, the space required for a pointer
    to the parameter.  Flexible (and generic) parameters have additional space
    allocated for their descriptor words; the descriptor word may be found by
    taking the location of the parameter, and subtracting the size of the
    descriptor.  *)
procedure map_parm_list ( subr_type: typ; params, retsym: sym );
var parm: sym;
    ploffset: bit_range;                 (* parameter list size in bits *)
    psize: bit_range;                    (* parameter size *)
    palign: align_range;                 (* parameter alignment *)
    rv_loc: rv_loc_type;		(* where the return value goes *)
    rv_size: bit_range;		(* the return value size *)
    rv_align: align_range;		(* the return value alignment *)
    desc_rule: rule;			(* flexible parameter allocation rule *)
    desc_size: bit_range;		(* the size from DescRule *)
    i: parm_range;
begin
  ploffset := pl_base;
  parm := params;
  with subr_type^ do begin
    (*  If this is a function, and the return value (or its address) must be
	allocated in the parameter list, then compute its size and alignment.  *)
    if return_type = nil then
      rv_loc := rv_nowhere
    else begin
      if p_b_address (return_type) or pba_retsym then begin
	rv_loc := rv_addr_loc;
	if rv_loc <> rv_nowhere then begin
	  rv_size := adr_prm_size;
	  rv_align := adr_prm_alignment;
	end;
      end
      else (* not passed by address *) begin
	rv_loc := rv_value_loc;
	if rv_loc <> rv_nowhere then
	  pack_in_context (return_type, parm_packing, rv_size, rv_align);
      end;
    end;
    (*  If we have a return value to allocate at the start of the parameter
	list, then allocate it.  *)
    if rv_loc = rv_at_start then begin
      ploffset := ngm (ploffset, rv_align);
      if retsym <> nil then
	retsym^.item_addr := ploffset div byte_size;
      ploffset := ploffset + rv_size;
    end;
    (*  Allocate the parameters.  *)
    for i := 1 to upperbound (params) do begin
      with params [i] do begin
	(*  Get the size and alignment for the parameter.  *)
	if (parm_kind = vars) or p_b_address (parm_type) then begin
	  psize := adr_prm_size;
	  palign := adr_prm_alignment;
	end
	else
	  pack_in_context (parm_type, parm_packing, psize, palign);
	(*  Adjust the parameter list offset to align the parameter.  If it
	    is flexible, allow for its length word, too.  (See the footnote
	    in AllocField.)  *)
	if parm_type^.flexible then begin
	  if parm_type^.kind = arrays
	    then desc_rule := arr_desc_rules [parm_packing]
	    else desc_rule := str_desc_rules [parm_packing];
	  with desc_rule^ do begin
	    if parm_type^.generic
	      then desc_size := 2 * width_factor
	      else desc_size := width_factor;
	    ploffset := ngm (ploffset + desc_size, max (palign, alignment));
	  end;
	end
	else
	  ploffset := ngm (ploffset, palign);
	(*  If we have a list of parameter symbols, then allocate the
	    symbol for this parameter.  *)
	if parm <> nil then begin (* Allocate the parameter symbol. *)
	  parm^.item_addr := ploffset div byte_size;
	  parm := parm^.next;
	end;
	(*  Allow space for the parameter.  *)
	ploffset := ploffset + psize;
      end (* with params [i] *);
    end (* for i *);
    (*  If we have a return value to allocate at the end of the parameter
	list, then allocate it.  *)
    if rv_loc = rv_at_end then begin
      ploffset := ngm (ploffset, rv_align);
      if retsym <> nil then
	retsym^.item_addr := ploffset div byte_size;
      ploffset := ploffset + rv_size;
    end;
    parmlist_size := (ploffset + byte_size - 1) div byte_size;
  end (* with subr_type^ *);
end (* map_parm_list *);
$PAGE alloc_type
(*  ALLOC TYPE fills in the size information for type nodes.  The interpretation
    of this field varies from kind to kind:
    (1) For the simple scalar types, the width is the minimum possible bit size.
	This is a lower bound for packing -- more bits may be allocated when it
	seems reasonable.  Note that specification of a preferred bit size in a
	packing clause (PACKED [10] 0..21) will replace the minimum bit size
	computed by AllocType, and is therefore interpreted as a lower bound in
	the same way as a computed size.
    (2) For data types represented by fixed sizes (pointers, procs, funcs),
	the actual width is determined only by context.  The values set here
	are minimal widths, applicable in minimal packing contexts.
    (3) For sets, the width is the minimum number of bits needed to represent
	the set.  Again, a wider field may be used in any particular context.
    (4) For arrays, records, and strings, the width is independent of context,
	due to the language requirement that any aggregate may be passed as a
	var parameter, even if it occurs in a packed record.  *)
public procedure alloc_type (type_desc: typ);
var twidth: bit_range;
    nelems: integer;
    elem_size: bit_range;
    elem_alignment: align_range;
    chars_per_ef: integer;
static var
    packing_choice: array [boolean] of packing_contexts :=
      ( field_packing, minimal_packing );
begin
  if type_desc = nil then
    return;
  with type_desc^ do begin
    if base_size <> 0 then return;                   (* <---- already allocated *)
    case kind of
      bools, ints, chars, scalars:
        begin
	  if minval >= 0 then
	    base_size := int_bits (maxval)
	  else if maxval < 0 then
	    base_size := int_bits (minval)
	  else
	    base_size := max (int_bits (minval), 1 + int_bits (maxval));
        end;
      reals:
	base_size := real_base_size [precision];
      sets:
	if set_element_type <> nil then begin
	  with set_element_type^ do      (* one bit for each member *)
	    if maxval >= minval then
	      type_desc^.base_size :=
		max (ngm (maxval + 1, set_uquantum) - 1, set_ubase) -
		  min (nlm (minval, set_lquantum), set_lbase) + 1
	    else
	      type_desc^.base_size := 0;
	end;
      pointers:
	base_size := pointer_base_size;
      files:
        begin
	  base_size := file_base_size;
          if (file_kind = typedfile) and (component_type <> nil) then begin
	    map_array (component_type, packing_choice[packable], efw + 1,
		       elem_size, twidth, elem_alignment);
            comp_size := elem_size;
          end
          else
            comp_size := 0;
        end;
      procs, funcs:
        begin
	  base_size := subr_base_size;
          map_parm_list (type_desc, nil, nil);       (* determine sizes of parameters *)
        end;
      strings:
        begin
	  if flexible then
	    base_size := 0	(* no fixed part *)
	  else begin (* formulas inspired by SizeOf and MapArray *)
	    if (str_char_size >= efw) or (efa < efw) then
	      base_size := str_char_size * str_length
	    else begin
	      chars_per_ef := efw div str_char_size;
	      base_size := efw * (str_length div chars_per_ef) +
			   str_char_size * (str_length mod chars_per_ef);
	    end;
	  end;
          if str_kind = varying then
            base_size := base_size + str_lw_width; (* length word width *)
        end;
      arrays:
	if (element_type <> nil) and (index_type <> nil) then begin
	  if flexible        (* get number of elements *)
	    then nelems := efw + 1    (* at least more than fit in one ef *)
	    else nelems := index_type^.maxval - index_type^.minval + 1;
	  map_array (element_type, packing_choice[packable], nelems,
		     elem_size, twidth, elem_alignment);
	  element_size := elem_size;          (* record width of elements of array *)
	  element_alignment := elem_alignment;
	  if flexible
	    then base_size := 0 (* don't count the hidden size word *)
	    else base_size := twidth; (* computed array size *)
	end;
      records:
	map_record (type_desc, packing_choice[packable])
    end (* case kind *) ;
  end
end (* alloc_type *);
$PAGE alc_subr
(* ALC SUBR is equivalent to ALLOC TYPE, except that it is always called
   with a subroutine type and the corresponding parameter list, and will
   not only compute the total parameter list size, but will also allocate
   the individual parameters within the list. *)
public procedure alc_subr (type_desc: typ; parm_list, return_sym: sym);
begin
  with type_desc^ do begin
    if base_size <> 0 then return;           (* <---- already allocated *)
    base_size := subr_base_size;
    map_parm_list (type_desc, parm_list, return_sym);
  end;
end (* alc_subr *).
   5@	js