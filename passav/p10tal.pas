$TITLE P10TAL -- PDP-10 Type Allocator

module p10tal;

$HEADER ptmtal.hdr
$PAGE includes
$include pascal.inc
$include pasist.inc
$INCLUDE pasmth.inc
$INCLUDE ptmprm.inc
$PAGE parameters
type
  packing_context =
     (  minimal_packing,        (* pack in least number of bits *)
        semi_packing,           (* for slack allocation of fields *)
        class_packing,          (* pack in minimal units *)
        no_packing  );          (* pack in canonical width for type *)

  packing_values = 
    array [packing_context] of
      packed record
        width: bit_range;                       (* width of item in bits *)
        alignment: align_range                  (* alignment of item *)
      end;

  packing_array = array[1..36] of packing_values;

  rsa_type = array [prec_type] of bit_range;
  raa_type = array [prec_type] of align_range;

const
  efw = 36;                     (* extraction field width -- max size field
                                   that can be removed from memory *)
  efa = 36;                     (* alignment of extraction field *)

  bool_packing: packing_values :=
     (  (1,1),    (18,18),  (36,36),  (36,36)  );

  char_packing: packing_values :=
     (  (7,1),    (18,18),  (36,36),  (36,36)  );

  scalar_packing: packing_array :=
    ( ((1,1),   (18,18),  (36,36),  (36,36)),
      ((2,1),   (18,18),  (36,36),  (36,36)),
      ((3,1),   (18,18),  (36,36),  (36,36)),
      ((4,1),   (18,18),  (36,36),  (36,36)),
      ((5,1),   (18,18),  (36,36),  (36,36)),
      ((6,1),   (18,18),  (36,36),  (36,36)),
      ((7,1),   (18,18),  (36,36),  (36,36)),
      ((8,1),   (18,18),  (36,36),  (36,36)),
      ((9,1),   (18,18),  (36,36),  (36,36)),
      ((10,1),  (18,18),  (36,36),  (36,36)),
      ((11,1),  (18,18),  (36,36),  (36,36)),
      ((12,1),  (18,18),  (36,36),  (36,36)),
      ((13,1),  (18,18),  (36,36),  (36,36)),
      ((14,1),  (18,18),  (36,36),  (36,36)),
      ((15,1),  (18,18),  (36,36),  (36,36)),
      ((16,1),  (18,18),  (36,36),  (36,36)),
      ((17,1),  (18,18),  (36,36),  (36,36)),
      ((18,1),  (18,18),  (36,36),  (36,36)),
      ((19,1),  (36,36),  (36,36),  (36,36)),
      ((20,1),  (36,36),  (36,36),  (36,36)),
      ((21,1),  (36,36),  (36,36),  (36,36)),
      ((22,1),  (36,36),  (36,36),  (36,36)),
      ((23,1),  (36,36),  (36,36),  (36,36)),
      ((24,1),  (36,36),  (36,36),  (36,36)),
      ((25,1),  (36,36),  (36,36),  (36,36)),
      ((26,1),  (36,36),  (36,36),  (36,36)),
      ((27,1),  (36,36),  (36,36),  (36,36)),
      ((28,1),  (36,36),  (36,36),  (36,36)),
      ((29,1),  (36,36),  (36,36),  (36,36)),
      ((30,1),  (36,36),  (36,36),  (36,36)),
      ((31,1),  (36,36),  (36,36),  (36,36)),
      ((32,1),  (36,36),  (36,36),  (36,36)),
      ((33,1),  (36,36),  (36,36),  (36,36)),
      ((34,1),  (36,36),  (36,36),  (36,36)),
      ((35,1),  (36,36),  (36,36),  (36,36)),
      ((36,1),  (36,36),  (36,36),  (36,36))  );

  neg_int_packing: packing_array :=
    ( ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((18,18),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36)),
      ((36,36),  (36,36),  (36,36),  (36,36))  );

  pointer_packing: packing_values :=
    (  (18,1),   (18,18),  (36,36),  (36,36)  );

  subr_packing: packing_values :=
    (  (36,36),  (36,36),  (36,36),  (36,36)  );

  real_size: rsa_type :=
    ( 36, 36, 36, 36, 36, 36, 36, 72, 72, 72, 72, 72, 72, 72, 72, 72 );
  real_alignment: raa_type :=
    ( 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36 );
$PAGE pack_in_context
(* PACK IN CONTEXT computes the width and alignment of an item of a particular
   type given the context for which it is to be packed.  In the case of scalar
   items, this relies on the fact that the size stored in the type node is the
   minimum bit size, which is used to index the packing tables to retrieve the
   approved packing. *)

procedure pack_in_context
      ( type_desc: typ; (* type of datum *)
        suggested_context: packing_context;
        var datum_width: bit_range;     (* output *)
        var datum_alignment: align_range  );

 var context: packing_context;
 begin
  if type_desc = nil then begin (* return dummy values for bad type *)
    datum_width := bits_per_unit;
    datum_alignment := bits_per_unit;
    return; (* <---- return with dummy values *)
  end;

  with type_desc^ do begin
    context := suggested_context;
    if packable                 (* alloc packed simple type values in fewest units *)
      then if context > class_packing   (* except if tighter packing specified *)
        then context := class_packing;

    case kind of

      bools:
        with bool_packing[context] do begin
          datum_width := width;
          datum_alignment := alignment;
        end;

      chars:
        with char_packing[context] do begin
          datum_width := width;
          datum_alignment := alignment;
        end;

      ints, scalars:
        begin
          if minval < 0 
            then with neg_int_packing [size, context] do begin
              datum_width := width;
              datum_alignment := alignment
            end
            else with scalar_packing [size, context] do begin
              datum_width := width;
              datum_alignment := alignment
            end
        end;

      sets:
        begin
          if size > bits_per_unit then begin
            datum_width := ngm (size, bits_per_unit);   (* pad items > 1 unit long *)
            datum_alignment := bits_per_unit;   (* must be aligned *)
          end
          else with scalar_packing[size, context] do begin

            (* in some contexts short sets may be slack allocated *)

            datum_width := width;
            datum_alignment := alignment;
          end
        end;

      pointers, files:
        with pointer_packing[context] do begin
          datum_width := width;
          datum_alignment := alignment;
        end;

      procs, funcs:
        with subr_packing[context] do begin
          datum_width := width;
          datum_alignment := alignment;
        end;

      others:           (* info directly in type node *)
        begin
          datum_width := ngm (size, bits_per_unit);     (* pad items > 1 unit long *)
          if context in [class_packing, no_packing]
            then datum_alignment := bits_per_unit       (* these packing types are aligned *)
            else datum_alignment := alignment;  (* use intrinsic alignment for the type *)
        end

    end (* case kind *) ;
  end (* with *) ;
 end;
$PAGE alc_data
(* ALC DATA is a public wrapper for PackInContext.  It does not take a context
   parameter, always assuming NoPacking instead. *)

public procedure alc_data ( type_desc: typ;
                            var datum_width: bit_range;
                            var datum_alignment: align_range );

 begin
  pack_in_context (type_desc, no_packing, datum_width, datum_alignment);
 end;
$PAGE map_array
(* MAP ARRAY determines the total width and alignment requirements for an array
   of arbitrary type.  This is used both in the mapping of arrays and of strings.
   In the latter case, it is used to get the appropriate width for n characters. *)

procedure map_array
      ( elem_type: typ;                 (* type of elements of array *)
        packing: packing_context;       (* either minimal or no packing *)
        nelems: int_type;               (* number of elements in array *)
        var ew: bit_range;              (* size in which to store elements *)
        var aw: bit_range;              (* width of array as allocated *)
        var aa: align_range       );    (* derived alignment *)

 var
  ea: align_range;                      (* element alignment *)
  ngroup: int_type;                     (* number of elements per extraction field *)

 begin
  (* Get the width of the array elements to be mapped.  We round this width up
     to the alignment of the element data, as each successive element must be
     aligned on such a boundary. *)

  pack_in_context (elem_type, packing, ew, ea);
  ew := ngm (ew, ea);

  (* If the width of the element data is greater than the largest packed field,
     then there is no hope of packing the data, even if the array is specified
     as being packed. *)

  if ew > efw then begin
    aa := lcm (ea, efa);        (* each element muse start on ef boundary *)
    ew := ngm (ew, efa);
    aw := ew * nelems;
    return;
  end;

  (* Otherwise, the fields are small enough to be packed.  If efa < efw then we
     can access an element regardless of the machine boundaries, which implies
     that any element may cross a machine boundary. *)

  if efa < efw then begin
    aw := nelems * ew;
    aa := ea;
    return;
  end;

  (* Here we try to pack the fields of an array, *but* we must insure that
     no field crosses the machine's extraction boundary.  There are two cases:
     (1) the entire array fits into the extraction field width, which means
     that no element will cross a boundary;  (2) the array is long enough to
     cross a boundary, which means that the individual elements must be grouped
     into units which fit in an extraction field. *)

  if (nelems * ew) <= efw then begin
    aw := nelems * ew;
    aa := ea;
    return;
  end

  else begin
    ngroup := efw div ew;                       (* number of elements packing in one ef *)
    aw := (efw * (nelems div ngroup)) + (ew * (nelems mod ngroup));
    aa := lcm (efa, ea);                        (* to achieve n per ef packing must assure that
                                                   array starts at least on efa boundary *)
  end;
 end;
$PAGE map_record
(* MAP RECORD determines the fld_width and fld_offset for each field in the 
   record.  It also determines sizes for the record, tag, and variant nodes
   in the record.  The size of the record or variant is the maximum size
   including all sub-variants; the size given with the tag, is the size of
   unspecified variants (i.e. that have null field lists).

   This routine is called with both record and variant nodes.  In the latter
   case, it is simply a convenient recursion point.  The alignment field of
   the variant is set to return the alignment requirements of the variant, but
   is never again used.  The offset of the first field is also returned. *)

procedure map_record
      ( parent: typ;            (* record or variant being mapped *)
        packing: packing_context;(* either minimal or no, depending on packable bit *)
        init_ro: bit_range;     (* minimum offset of first field *)
        init_ra: align_range;   (* prima facia alignment *)
        var first_fld_offset: bit_range  );     (* offset of first field in record/variant or
                                           maximum (bit_range) if there are none *)

  var ro: bit_range;                            (* current offset in record *)
      ra: align_range;                          (* derived alignment requirements *)
$PAGE alloc_variant - in map_record

 (* ALLOC VARIANTS processes all the variant parts of a record case.  The
    sub field lists are allocated starting at the offset of the end of the
    preceding fixed part.  This also keeps track of the alignment requirements
    of each variant, and the minimum offset of the first fields in the variant.
    The latter is returned via next_offset to alloc_field to allow slack
    allocation of the previous field. *)

 procedure alloc_variants ( tag: typ; var next_offset: bit_range );

  var variant: typ;                             (* variant being processed *)
      syn_variant: typ;                         (* variant(s) with same field list as the above *)
      mvw: bit_range;                           (* maximum variant width (size) *)
      no: bit_range;

  begin
   with tag^ do begin
     size := ro;                                (* tag size is minimum offset so far *)
     mvw := ro;                                 (* minimum possible *)
     next_offset := maximum (bit_range);        (* default if no fields seen *)

     variant := first_variant;                  (* process all variants at the tag *)
     while variant <> nil do begin
       with variant^ do begin
         map_record (variant, packing, ro, ra, no);
         ra := lcm (ra, alignment);     (* adjust for fields in variant *)
         mvw := max (mvw, size);        (* get max size of any variant *)
         next_offset := min (next_offset, no);  (* closest field in any variant *)
       end;

       (* Several variants may share the same field list, so for each such
          "synonym", copy the size information. *)

       syn_variant := variant^.next_variant;
       while (syn_variant <> nil) andif (syn_variant^.field_list = variant^.field_list) do begin
         syn_variant^.size := variant^.size;
         syn_variant := syn_variant^.next_variant;
       end;
       variant := syn_variant;                  (* process next non-synonym on next iteration *)
     end (* while *) ;
   end (* with tag *) ;
   ro := mvw;                                   (* size of record will be max of all variant widths *)
  end;
$PAGE alloc_field - in map_record

 (* ALLOC FIELD uses tail recursion to scan the list of fields in the fixed
    part of a record or variant.  We first allocate the field using minimal
    packing. Then allocate the following field.  If there are unused bits between
    the current and following fields due to the alignment requirements of the
    following field, we reallocate the field using semi packing or class packing
    if it fits.  This presumably gives speedier access, at no increase in 
    storage usage.   At the end of the fixed part of the record or variant,
    alloc_variants is called to process the record case. *)

 procedure alloc_field ( field: sym );

  var fw: bit_range;                            (* field width in bits *)
      fa: align_range;                          (* field alignment *)
      next_offset: bit_range;                           (* offset of trailing field *)
      test_offset: bit_range;
      tfw: bit_range;  tfa: align_range;        (* test fw, fa *)
      context: packing_context;

  begin
   with field^ do begin
     pack_in_context (type_desc, packing, fw, fa);      (* get width and alignment for field *)
     if (type_desc^.kind in [arrays, strings]) and type_desc^.flexible then
       fw := fw + flex_arr_desc_size; (* allow for the array size word *)
     ro := ngm (ro, fa);                                (* get offset satisfying field alignment *)
     if efw <= efa then begin                           (* must worry about machine boundaries, too *)
       if (ro div efw) <> ((ro+fw-1) div efw) then begin                (* if crosses extraction boundary *)
         ro := ngm (ro, efa);                   (* assume efa, fa compatible *)
         ra := lcm (ra, efa);                   (* must insure this alignment *)
       end;
     end;
     fld_offset := ro;
     fld_width := fw;
     ro := ro + fw;                                     (* alloc space for field *)

     (* Process the following field, or case *)

     if (next = nil) orif (next^.fld_variant <> fld_variant)
       then begin                                       (* followed by case *)
         if parent^.variant_tag <> nil
           then alloc_variants (parent^.variant_tag, next_offset)
           else next_offset := ngm (ro, bits_per_unit); (* rest of unit is free *)
       end
       else begin                                       (* followed by field *)
         alloc_field (next);
         next_offset := next^.fld_offset;
       end;

     (* See if the field make use of a wider space if available *)

     if packing = minimal_packing then begin    (* don't slack alloc if record unpacked *)
       for context := class_packing downto semi_packing do begin
         pack_in_context (type_desc, context, tfw, tfa);        (* get less restrictive allocation *)
         test_offset := ngm (fld_offset, tfa);          (* alignment must be at least as restrictive *)
       exit if (test_offset + tfw) <= next_offset do
           begin                                                (* this allocation possible *)
             fld_offset := test_offset;
             fld_width := tfw;
             fa := tfa;
           end
       end;
     end (* if *) ;

     ra := lcm (ra, fa);        (* update record alignment by field ment
                                   decided on *)
   end (* with field *) ;
  end;
$PAGE map_record - main routine
 begin
  with parent^ do begin
    ro := init_ro;                              (* start off as told *)
    ra := init_ra;

    if (field_list <> nil) andif (field_list^.fld_variant = parent)
      then begin
        alloc_field (field_list);
        first_fld_offset := field_list^.fld_offset;
      end
    else if variant_tag <> nil
      then alloc_variants (variant_tag, first_fld_offset);

    size := ro;                                 (* maximum size of record or variant *)
    alignment := ra;                            (* lcm ( all field alignments ) *)
  end (* with parent *) ;
 end;
$PAGE map_parm_list
(* MAP PARM LIST determines the item_addr for each parameter for a specified
   "subr_type".  The argument list is mapped as a contiquous record with
   special fields allocated to serve as the descriptors of parameters of
   flexible types.  This procedure, along with p10sel embodies the rules for
   determining when to pass parameters by address rather than value. *)

procedure map_parm_list ( subr_type: typ; params: sym );

 var
   parm: sym;
   ploffset: bit_range;                 (* parameter list size in bits *)
   psize: bit_range;                    (* parameter size *)
   palign: align_range;                 (* parameter alignment *)
   i: parm_range;

 begin
  ploffset := 0;
  parm := params;

  for i := 1 to subr_type^.nparms do begin
    with subr_type^.params[i] do begin
      case parm_type^.kind of           (* allocate descriptors *)

        arrays:
          if parm_type^.flexible then begin
            ploffset := ngm (ploffset, array_alignment);
            ploffset := ploffset + flex_arr_desc_size;
            if parm_type^.generic
              then ploffset := ploffset + flex_arr_desc_size;
          end;

        strings:
          if parm_type^.flexible then begin
            ploffset := ngm (ploffset, string_alignment);
            ploffset := ploffset + flex_arr_desc_size;
          end
 
      end (* case *) ;

      if (parm_kind = vars) or p_b_address (parm_type) then begin
        ploffset := ngm (ploffset, pointer_packing[no_packing].alignment);
        if parm <> nil then
          parm^.item_addr := ploffset div bits_per_unit;
        ploffset := ploffset + pointer_packing[no_packing].width;
      end
      else (* passed by value *) begin
        pack_in_context (parm_type, no_packing, psize, palign); (* get size of parm *)
        ploffset := ngm (ploffset, palign);
        if parm <> nil then
          parm^.item_addr := ploffset div bits_per_unit;
        ploffset := ploffset + psize;
      end;
    end (* with subr_type^.params[i] *);
    if parm <> nil then
      parm := parm^.next;
  end (* for i *);

  subr_type^.parmlist_size := ploffset div bits_per_unit;
 end;
$PAGE alloc_type
(* ALLOC TYPE fills in the width and aligment for type nodes.  The interpretation
   of these fields varies from kind to kind.  (1) For the simple scalar types,
   the width is the minimum possible bit size, and the alignment the minimum
   permissible alignment.  These values are only lower bounds; in packing, wider
   or less restrictive allocation may be used as context suggests.  Note that
   this minimum width may be superseded by specification of a preferred bit size,
   which replaces the actual minimum width.  Thus a preferred bit size  is also
   a lower bound which may be adjusted according to context.  (2) For data
   types represented by fixed sizes (pointers, procs, funcs), the width and 
   alignment are determined solely from context.  The values set here are
   sizes for unpacked contexts, so the standard function SIZE works right.
   (3) For sets, the width is the minimum width required to represent
   the set; alignment gives the preferred set alignment for the machine. (However
   unlike scalars, the alignment is likely to be > 1.)  Again in packing, these
   can be relaxed.  (4) For arrays and records, the width and alignment are
   independent of context.  The reason is that we cannot map the type separately
   for each context because all data of the type share the same node. *)

public procedure alloc_type (type_desc: typ);
 var msize: bit_range;
     twidth: bit_range;
     talign: align_range;
     toff: bit_range;
     nelems: int_type;
     elem_size: bit_range;

 begin
  with type_desc^ do begin
    if size <> 0 then return;                   (* already allocated *)
    case kind of

      bools, ints, scalars:
        begin
          if minval >= 0 then
            size := int_bits (maxval)
          else if maxval < 0 then
            size := int_bits (minval)
          else
            size := max (int_bits (minval), int_bits (-maxval));
          alignment := 1;                               (* packing contexts enforce machine dependencies *)
        end;

      reals:
        begin                           (* size determined from precision of mantissa *)
          size := real_size (precision);
          alignment := real_alignment (precision);
        end;

      sets:
        begin
          if set_element_type <> nil
            then with set_element_type^ do      (* one bit for each member *)
              type_desc^.size := maxval - minval + 1;
          if size > bits_per_unit
            then alignment := bits_per_unit
            else alignment := set_alignment;
        end;

      chars:
        begin                                   (* size determined solely by context, except
                                                   that minimum size is needed to check a bit size *)
          size := char_packing [minimal_packing].width;
          alignment := char_packing [minimal_packing].alignment;
        end;

      pointers:
        begin                           (* size determined by context, but save minimum *)
          size := pointer_packing [minimal_packing].width;
          alignment := pointer_packing [no_packing].alignment;
        end;

      files:
        begin
          size := pointer_packing [minimal_packing].width; (* just like for pointers *)
          alignment := pointer_packing [no_packing].alignment;
          if (file_kind = typedfile) and (component_type <> nil) then begin
            if packable (* determine component size in context *)
              then map_array (component_type, minimal_packing, efw + 1, elem_size, twidth, talign)
              else map_array (component_type, no_packing, efw + 1, elem_size, twidth, talign);
            comp_size := elem_size;
          end
          else
            comp_size := 0;
        end;

      procs, funcs, con_procs, con_funcs:
        begin
          map_parm_list (type_desc, nil);       (* determine sizes of parameters *)
          size := subr_packing [minimal_packing].width;
          alignment := subr_packing[no_packing].alignment;
        end;

      strings:
        begin
          alignment := string_alignment;        (* assume that with width, 1st char comes out okay *)
          if flexible (* map like a packed array of char's *)
            then twidth := 0
            else map_array (type_char, minimal_packing, str_length, elem_size, twidth, talign);
          size := twidth;
          if str_kind = varying then
            size := size + str_lw_width; (* length word width *)
        end;

      arrays:
        begin
          if (element_type <> nil) and (index_type <> nil) then begin
            alloc_type (element_type);  (* get size of base type *)
            if flexible (* or generic *)        (* get number of elements *)
              then nelems := efw + 1    (* at least more than fits in one ef *)
              else with index_type^ do
                nelems := maxval - minval + 1;
            if packable
              then map_array (element_type, minimal_packing, nelems, elem_size, twidth, talign)
              else map_array (element_type, no_packing, nelems, elem_size, twidth, talign);
            element_size := elem_size;          (* record width of elements of array *)
            if flexible (* or generic *) then begin (* don't count the hidden size word *)
              size := 0;
              alignment := flex_arr_alignment;
            end
            else begin
              size := twidth;                   (* copy results of mapping *)
              alignment := lcm (array_alignment, talign);       (* insure standard alignment *)
            end;
          end;
        end;

      records:
        begin
          if packable
            then map_record (type_desc, minimal_packing, 0, record_alignment, toff)
            else map_record (type_desc, no_packing, 0, record_alignment, toff);
        end

    end (* case kind *) ;
  end
 end;
$PAGE alc_subr
(* ALC SUBR is equivalent to ALLOC TYPE, except that it is always called
   with a subroutine type and the corresponding parameter list, and will
   not only compute the total parameter list size, but will also allocate
   the individual parameters within the list. *)

public procedure alc_subr (type_desc: typ; parm_list: sym);
 begin
  with type_desc^ do begin
    if size <> 0 then return;           (* already allocated *)
    map_parm_list (type_desc, parm_list);
    size := subr_packing [no_packing].width;
    alignment := subr_packing [no_packing].alignment;
  end;
 end.
  _ \’