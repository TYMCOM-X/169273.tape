$TITLE PASTSM - Pascal Type Semantics

module pastsm;
$PAGE includes and declarations

$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM paserr.inc
$SYSTEM pasutl.inc
$SYSTEM pasval.inc
$SYSTEM pascmp.inc
$SYSTEM pa1xrf.inc
$SYSTEM pasesm.inc
$SYSTEM pastal.inc



type tsm_flags = set of ( forward_ok, flexible_ok, flex_rec_ok, generic_ok );

public function type_semantics ( typetree: parse_node; flags: tsm_flags ): typ;
   forward;
$PAGE search_next
(* SEARCH NEXT scans a next chained list for a symbol with a specified name *)

function search_next ( nsym: sym; name: nam ): sym;
 begin
  search_next := nsym;
  while search_next <> nil do begin
    if search_next^.name = name                 (* desired symbol found *)
      then return;
    search_next := search_next^.next
  end
 end;
$PAGE do_packing
(* DO PACKING takes a type tree and an optional parse tree for a packing-width
   expression, and converts the type to a packed type. *)

procedure do_packing ( var t: typ; width: parse_node );

 var
   widthval: val;
   dummy, ntype: typ;

 begin
  t^.packable := true;

  (* Check for a fixed length string declared as an array *)

  if t^.kind = arrays then begin
    ntype := t;
    with ntype^ do begin
      if (element_type = type_char) andif (index_type <> nil)
        then with index_type^ do 
          if (kind = ints) andif (minval = 1) then begin
            t := new_type (strings);    (* have a string *)
            with t^ do begin
              str_kind := nonvarying;
              flexible := ntype^.flexible;              (* may have "packed array [1..*] of char" *)
              if flexible
                then str_length := maximum (char_range)
                else str_length := ntype^.index_type^.maxval;
            end;
            if ntype^.index_type^.type_id = nil then
              dispose (ntype^.index_type); (* discard unneeded index type *)
            dispose (ntype);    (* discard unused type node *)
          end
    end;
  end (* if array *) ;

  alloc_type (t);               (* computes minimum bit width *)

  (* apply bit packing if specified *)

  if width <> nil then begin            (* have preferred bit size *)
    if t^.kind in [scalars, ints, bools, chars, reals] then begin
      if not constant (width, type_int, widthval, dummy)
        then widthval.ival := t^.base_size;  (* to suppress further errors *)
      if widthval.ival < t^.base_size
        then err_node (err_packing_invalid, width)
        else t^.base_size := widthval.ival;
    end
    else err_node (err_psize_not_allowed, width);       (* not allowed with this type kind *)
  end;
 end;
$PAGE subr_formal_type
(* SUBR TYPE creates a procedure or function type node from the trees
   corresponding to the interesting information about the type.  The caller may
   specify whether or not names are required in the parameter list; but if they
   are not required, either all or none must be given.  Note also: if a return
   type is specified for a proc type, a func type is created with a warning. *)

public function subr_formal_type
         (   subr_sym: parse_node;      (* "procedure" or "function" *)
             subr_parm_list: parse_node;        (* <parameter list> *)
             func_type: parse_node;     (* tree describing return type *)
             parms_required: boolean;   (* are names required? *)
             var parmlist: sym_list     (* the parameter name list, if required *)
                                ): typ;

 var
   pkind: sym_kind;                     (* value or var parm *)
   fparm: sym;                          (* first parm of group with same type *)
   psym: sym;
   ptype: typ;                          (* type of parameter group *)
   pn_count: parm_range;                (* number of named parms in group *)
   named_parms: parm_range;             (* total named parms in list *)
   i: parm_range;                       (* total parms in list *)
   i1: parm_range;
   parm: parse_node;                    (* for stepping through the list *)
   pn: parse_node;                      (* for stepping through a group *)
   parm_mode: tsm_flags; (* flags for parameter type semantics *)

 begin
  i := 0; (* count the parameters *)
  named_parms := 0;
  if subr_parm_list <> nil then begin (* parameters are specified *)
    parm := subr_parm_list^.defn;
    while parm <> nil do begin
      pn_count := 0;
      pn := parm^.defn;
      while pn <> nil do begin
        pn_count := pn_count + 1;
        pn := pn^.next;
      end;
      named_parms := named_parms + pn_count - 1;
      i := i + max (pn_count - 1, 1);
      parm := parm^.next;
    end (* while parm <> nil *);
  end (* if subr_parm_list <> nil *);

  if (named_parms <> i) and (parms_required or (named_parms <> 0)) then
    err_node (err_no_names, subr_parm_list);

  if subr_sym^.sym = proceduresy
    then subr_formal_type := new_proc (procs, i)
    else subr_formal_type := new_proc (funcs, i);

  with subr_formal_type^ do begin
    fortran_call := false;

    (* Process the parameter list *)

    if parms_required then begin
      parm_mode := [flexible_ok, generic_ok];
      parmlist.first := nil;
      parmlist.last := nil;
    end
    else
      parm_mode := [flexible_ok, generic_ok, forward_ok];

    (* Loop over parameter groups: <var parm decl>s or <value parm decl>s. *)

    if subr_parm_list <> nil then begin (* parameters are specified *)
      i := 0;
      parm := subr_parm_list^.defn;
      while parm <> nil do begin
	if parm^.defn <> nil then begin

	  if parm^.sym = value_parm_decl  (* get kind of parm *)
	    then pkind := values
	    else pkind := vars;

	  pn_count := 0;
	  pn := parm^.defn;               (* -> [ id ... ] -> typeid *)
	  fparm := nil;                   (* remember first of (possibly) several symbols *)
	  while pn^.next <> nil do begin
	    pn_count := pn_count + 1;
	    if parms_required then begin
	      psym := new_sym (pkind);    (* generate one sym for each id *)
	      with psym^ do begin
		if search_next (parmlist.first, pn^.name) <> nil
		  then err_node (err_already_dcled, pn)   (* have two parms with same name *)
		  else name := pn^.name;
	      end;
	      xrf_use (psym, pn^.source, decl_xrf);
	      if fparm = nil then
		fparm := psym;
	      chain_sym (parmlist, psym);
	    end (* if parms_required *);
	    pn := pn^.next;                               (* advance to next parm *)
	  end (* while pn^.next <> nil *);
	  ptype := type_semantics (pn, parm_mode);

	  (* Set the types of the parm symbols in this list. *)

	  while fparm <> nil do begin
	    with fparm^ do begin
	      type_desc := ptype;
	      dcl_class := parameter_sc;
	      public_dcl := false;
	      fparm := next;
	    end;
	  end;

	  (* Set the parameter descriptions in the parameter type node. *)

	  i1 := i + max (pn_count, 1);
	  while i < i1 do begin
	    i := i + 1;
	    params[i].parm_kind := pkind;
	    params[i].parm_type := ptype;
	  end;
	end (* if parm^.defn <> nil *);

        parm := parm^.next;                             (* get next parameter group *)
      end (* while parm <> nil *);
    end (* if subr_parm_list <> nil *);

    (* Process the return type. *)

    if func_type = nil then begin               (* no return type *)
      if kind = funcs then
        err_node (err_no_return, subr_sym)
    end
    else begin                          (* have return type *)
      if kind = procs then begin                (* assume he meant function *)
        err_node (err_ret_for_proc, subr_sym);
        kind := funcs
      end;
      return_type := type_semantics (func_type, [forward_ok])
    end;

  end (* with subr_formal_type^ *) ;
 end;
$PAGE make_array_type
(* MAKE ARRAY TYPE processes the definition chain of an <array type> construct,
   which appears as: <index type>* -> <component type>.  If there is more than
   one index type, it recursively calls itself with the tail of the chain, to
   build a multi-dimensional array type. *)

function make_array_type ( arr_defn: parse_node; pack_it: boolean; flex_ok: boolean ): typ;
 var
  node: parse_node;

 begin
  make_array_type := new_type (arrays); (* create type node for this dimension *)
  with make_array_type^ do begin
    node := arr_defn;
    if flex_ok           (* process index type, guarenteed to be range, or type id *)
      then begin
        index_type := type_semantics (node, [flexible_ok, generic_ok]);
        if index_type <> nil then begin         (* if index type is flexible, mark array as such *)
          flexible := index_type^.flexible;
          generic := index_type^.generic;
        end;
      end
      else index_type := type_semantics (node, []);

    node := node^.next;                         (* advance to component type *)
    if node^.next = nil
      then element_type := type_semantics (node, [])    (* component type *)
      else element_type := make_array_type (node, pack_it, false);      (* more index types *)
    if (index_type = nil) or (element_type = nil) then begin (* bad array type *)
      dispose (make_array_type);
      make_array_type := nil;
    end;
  end (* with *) ;
  if make_array_type <> nil then begin
    if pack_it
      then do_packing (make_array_type, nil)
      else alloc_type (make_array_type);
  end;
 end;
$PAGE fieldlist
(* FIELDLIST processes fields and sub-variants in a record or individual variant
   case.  The global structure of the parse tree chain processes is as follows:
   [ <field id decl> ]* -> [ <variant part> ].  Substructure is described below. *)

procedure fieldlist
             (  rec: typ;               (* top-level record node *)
                parent: typ;            (* containing record or variant node *)
                fldlst: parse_node;             (* pointer to start of chain to process *)
                first_fld_no: fld_range;        (* logical offset of first field *)
                var last_field: sym  ); (* last field declared in entire record *)

var fl, idlist, id, typedecl: parse_node;
    tagfield, var_case, lab, range: parse_node;
    ftype: typ;
    first_var, cur_var: typ;
    fld_no: fld_range;                  (* logical field number to be assigned next field *)
$PAGE field - in fieldlist
(* FIELD generates a new field node, and chains it to the fixed part of the
   parent node.  Checks that the field is not previously declared. *)

procedure field ( id: parse_node; ftype: typ );

var fsym: sym;

begin
  with id^.name^ do begin     (* check for previously declared field *)
    if scopechain <> nil then with scopechain^ do
      if (kind = fields) andif (fld_record = rec) then begin
	err_node (err_already_dcled, id);
	return
      end
  end;
  fsym := new_sym (fields);
  with fsym^ do begin
    name := id^.name; (* set info *)
    block := cur_block;
    type_desc := ftype;
    with id^ do begin
      scopechain := name^.scopechain; (* add to scope for duplicate field test *)
      name^.scopechain := scopechain
    end;
    fld_record := rec;
    fld_variant := parent;
    fld_number := fld_no;
    fld_no := fld_no + 1;
  end;
  if parent^.field_list = nil         (* chain to record *)
    then parent^.field_list := fsym;
  if rec^.field_list = nil
    then rec^.field_list := fsym;
  if last_field <> nil
      then last_field^.next := fsym;
  last_field := fsym;
  xrf_use (fsym, id^.source, decl_xrf);
end;
$PAGE append_variant - in fieldlist
 (* APPEND VARIANT appends a variant to tag of the <variant case> being processed. *)

var tag: typ;                          (* tag node being constructed *)
    last_var: typ;                     (* last variant field appended *)

procedure append_variant ( avar: typ );

begin
  if tag^.first_variant = nil
    then tag^.first_variant := avar
    else last_var^.next_variant := avar;
  avar^.tag := tag;
  last_var := avar;
end;
$PAGE variant_label - in fieldlist
(* VARIANT LABEL semanticates a <range list>, and sets the label information
   in the specified variant.  Returns true if the labels are okay. *)

var tagtype: typ;                              (* gives the type of variant labels *)

function variant_label ( avar: typ; range: parse_node ): boolean;

var rngval: val; dt: typ;

begin
  variant_label := false;                      (* assume bad *)
  avar^.others_var := false;
  if range^.sym = elipsis then begin                   (* defn is lb -> ub *)
    if constant (range^.defn, tagtype, rngval, dt) then begin
      avar^.minlab := rngval.ival;
      if constant (range^.defn^.next, tagtype, rngval, dt) then begin
	avar^.maxlab := rngval.ival;
	if avar^.minlab > avar^.maxlab
	  then err_node (err_low_upb, range^.defn^.next)
	  else variant_label := true
      end
    end
  end
  else begin                                   (* range is single value *)
    if constant (range, tagtype, rngval, dt) then begin
      avar^.minlab := rngval.ival;
      avar^.maxlab := rngval.ival;
      variant_label := true
    end
  end
end;
$PAGE check_conflict - in fieldlist
(* CHECK CONFLICT checks for conflicts between different variant labels. *)

procedure check_conflict ( avar: typ; lab: parse_node );

label 100;    (* error exit *)

var cvar: typ;

  procedure report_error;      (* report conflict and exit *)
  begin
    err_node (err_case_conflict, lab);
    goto 100
  end;

begin
  cvar := tag^.first_variant;          (* scan all variants declared so far *)
  if avar^.others_var then begin       (* search for another "others" case *)
    while cvar <> nil do begin
      with cvar^ do begin
	if others_var then report_error;      (* match, too bad *)
	cvar := next_variant
      end
    end (* while *) ;
  end
  else begin                           (* have a label range *)
    while cvar <> nil do begin         (* search for a non-nil intersection *)
      with cvar^ do begin
	if avar^.minlab <= minlab
	  then begin
	    if minlab <= avar^.maxlab then report_error
	  end
	else if avar^.minlab <= maxlab
	  then report_error;
	cvar := next_variant;
      end
    end (* while *) ;
  end;
 100 (* error exit *) :
end;
$PAGE field_list - main routine
begin
  (* Process the <field id decl> defined as <identifier list> -> <type decl>.
     The identifier list contains a chained list of one or more <identifier>s,
     to be declared as fields in the record. *)

  fl := fldlst;
  fld_no := first_fld_no;
  while (fl <> nil) andif (fl^.sym = field_id_decl) do begin
    idlist := fl^.defn;                         (* get pieces of group *)
    typedecl := idlist^.next;
    fl := fl^.next;                     (* get next decl on list if any *)
    id := idlist^.defn;                 (* get id's to be declared; is nil if there
                                           is an error in the id list. *)

    (* Get the type for the fields; note that a flexible type is permitted for
       the very last field. Clearly, there can only be one field in the last
       declaration group if the type is to be flexible. *)

    if (fl = nil) and                           (* this is last field and there is no tag *)
       ((id = nil) orif (id^.next = nil))       (* there is (0 or) 1 field in this last group *)
      then begin
        ftype := type_semantics (typedecl, [flexible_ok]);
        if ftype <> nil then    (* rec is flexible, if any trailing components are *)
	  parent^.flexible := ftype^.flexible;
      end
      else ftype := type_semantics (typedecl, []);

    while id <> nil do begin    (* declare all field names in the group *)
      if search_next (rec^.field_list, id^.name) <> nil
        then err_node (err_already_dcled, id)   (* field already declared *)
        else field (id, ftype);                 (* new name, add to record *)
      id := id^.next;
    end (* while id *) ;
  end (* while fl *) ;  (* fl := fl^.next performed above *)

  (* Process a <variant part> if present.  The definition chain is arranged thus:
    <tag field> -> [ <variant case> ]*. *)

  if (fl <> nil) andif (fl^.defn <> nil) then begin                       (* must be <variant part>, if non-nil *)
    tag := new_type (tags);                     (* create a tag node *)
    parent^.variant_tag := tag;                 (* point parent at it *)
    tag^.tag_recvar := parent;                  (* and vice versa *)

    tagfield := fl^.defn;                       (* get parts of construct *)
    var_case := tagfield^.next;

    (* Process the <tag field> ::= [ <identifier> ] -> <type decl> *)

    id := tagfield^.defn;                       (* get tag id/type *)
    if id^.next = nil
      then begin                                (* just a <type decl> *)
        tagtype := type_semantics (id, []);
        fld_no := fld_no + 1;   (* alloc a slot for unnamed tag in rec const *)
      end
      else begin
        tagtype := type_semantics (id^.next, []);       (* <id> <type decl> *)
        field (id, tagtype);
        tag^.tag_field := last_field;           (* point tag node at field *)
      end;
    if (tagtype <> nil) andif
       (not (tagtype^.kind in [bools, ints, chars, scalars, unknown_type])) then begin
      err_node (err_scalar_case_required, id);
      tagtype := nil;                           (* suppress type checking *)
    end;
    tag^.tag_type := tagtype;                   (* set type in tag node *)

    (* iterate over the <variant case>s.  They are structured as follows:
       <range list> | "others" ->  <field list>. *)

    last_var := nil;                    (* have no variants yet *)
    while var_case <> nil do begin

      (* create a variant node for each label or label range applying to
         current <field list> *)

      lab := var_case^.defn;                    (* <range list> or "others" *)
      if lab^.sym = otherssy then begin         (* the others case *)
        first_var := new_type (variants);
        first_var^.others_varrue;
        check_conflict (first_var, lab);
        append_variant (first_var);
      end
      else begin                                (* process list of labels in <range list> *)
        range := lab^.defn;
        first_var := nil;                       (* must remember first of several *)
        while range <> nil do begin
          cur_var := new_type (variants);
          if variant_label (cur_var, range) then check_conflict (cur_var, range);
          append_variant (cur_var);
          if first_var = nil then first_var := cur_var;
          range := range^.next
        end (* while range *) ;
      end (* else <range list> *) ;

      (* process the field list *)

      fieldlist (rec, first_var, lab^.next, fld_no, last_field);

      rec^.flexible := first_var^.flexible;

      (* append field_list and variant_tag to all variants *)

      cur_var := first_var;
      while cur_var <> nil do begin
        with cur_var^ do begin
          field_list := first_var^.field_list;
          variant_tag := first_var^.variant_tag;
          cur_var := next_variant
        end;
      end;

      var_case := var_case^.next;               (* process next <variant case> *)
    end;

  end (* if <variant part> *) ;

  if rec = parent then pop_scope (rec^.field_list);     (* at top level, delete field names from scope *)
end;
$PAGE chk_class
(* CHK CLASS scans a global pseudo-symbol chain (the heap_chain or the file_chain
   for a class containing a given type.  A heap class represents all variables (on
   the heap) which may be accessed by pointers of a given type.  A file class
   represents the file status information for all the files of a given type. *)

function chk_class ( var chain: sym; class: storage_class; target_type: typ ): sym;

begin
  chk_class := chain;
  while (chk_class <> nil) andif not equivtypes (chk_class^.type_desc, target_type) do
    chk_class := chk_class^.next;
  if chk_class = nil then begin
    chk_class := new_sym (vars); (* create a new heap class for this type *)
    with chk_class^ do begin
      block := root_block^.children; (* all heap classes have global scope *)
      next := chain;
      type_desc := target_type; (* the class is identified by a representative type *)
      dcl_class := class; (* identifies a heap class *)
      public_dcl := true; (* all heap classes are globally accessible *)
      abnormal_use := false;
      allocated := false;
      init_value.kind := no_value;
    end;
    chain := chk_class;
  end;
end (* chk_class *);
$PAGE t_semant
(* T_SEMANT is a helping function for type_semantics.  It processes a parse
   tree describing an unpacked type.  Note that care is taken here never to
   return a indirect_type as type_semantics and its callers do not expect to
   have to deal with such a node. *)

function t_semant ( typetree: parse_node ): typ;

 var
   nsym: sym;
   ntype: typ;
   st: typ;
   elem, lbnode, ubnode, precnode: parse_node;
   lbval, ubval, precval: val;
   lbtype, ubtype: typ;
   len: char_range;
   is_flexible: boolean;
   pfsym, parmlst, rettype: parse_node;
   last_field: sym;
   no_parms: sym_list;

 begin
  t_semant := nil;                      (* left this way until valid type constructed *)
  case typetree^.sym of

    (* IDENTIFIER: There are two cases. (1) The identifier is previously declared,
       in which case it must be a type identifier.  (2) The identifier is not
       declared, in which case it is assumed to be a forward type declaration. A
       declaration for the identifier is entered in the current block, with an
       unknown type.  When the type is ultimately declared, the unknown_type node
       is transformed into an indirect type node, pointing to the true type. *)

    ident:
      begin
        nsym := typetree^.name^.scopechain;
        if nsym <> nil then begin                       (* ident previously declared *)
          if nsym^.kind <> types then begin             (* => not a type id *)
            err_node (err_not_type, typetree);
            return;                                     (* nil FV already set *)
          end;
          xrf_use (nsym, typetree^.source, ref_ctxt);
          ntype := nsym^.type_desc;             (* get true type if indirect *)
          while (ntype <> nil) andif (ntype^.kind = indirect_type)
            do ntype := ntype^.actual_type;
          if (ntype = type_ptr) andif not (sp_ptr_opt in cur_block^.semantic_options) then begin
            err_node (err_ptr_no_special, typetree);
            cur_block^.semantic_options := cur_block^.semantic_options + [sp_ptr_opt];
          end;
        if (ntype = type_fullword) and not (sp_wor_opt in cur_block^.semantic_options) then begin
          err_node (err_mword_no_special, typetree);
          cur_block^.semantic_options := cur_block^.semantic_options + [sp_wor_opt];
        end;
        end

        else begin                      (* not dcl'ed, have a forward type *)
          ntype := new_type (unknown_type);
          nsym := declare (typetree, types, ntype);             (* declare ident in current block *)
          ntype^.type_id := nsym;               (* thread together *)
          ntype^.declaration := typetree^.source; (* forward declaration location *)
        end;
        t_semant := ntype;              (* return type of (new) id *)
      end;

    (* PAREN EXPR is returned for a scalar type by the parse.  We must check 
       that all nodes of the parenthesized list are in fact identifiers; if
       not, discard the errorneous nodes. *)

    paren_expr:
      begin
        st := new_type (scalars);
        with st^ do begin               (* fill in info as we go *)
          base_type := st;              (* base type is itself *)
          minval := 0; maxval := -1;    (* increment maxval as we find names *)

          elem := typetree^.defn;       (* iterate over all nodes on list *)
          while elem <> nil do begin
            maxval := maxval + 1;
            if elem^.sym <> ident then          (* paren_expr not a kosher scalar type *)
              err_node (err_id_expected, elem)
            else begin                                  (* is really id *)
              nsym := declare (elem, consts, st);
              if nsym <> nil then begin                 (* nsym = nil => already declared *)
                with nsym^ do begin
                  dcl_class := constant_sc;
                  public_dcl := false;
                  init_value := cst_scalar (maxval)
                end;
                chain_sym (cst_list, nsym);
                xrf_use (nsym, elem^.source, decl_xrf);
              end;
            end;
            elem := elem^.next;                         (* iteration step *)
          end (* while *) ;

          type_id := cst_list.first;            (* this prevents cascading dumps of scalar type.
                                                   declare_types overrides this *)
        end (* with *) ;
        t_semant := st;
      end;



    (* ELIPSIS indicates a subrange type.  The definition chain of the node
       has in order the lower bound, the upper bound or "*", and an optional
       PREC clause.   First, the base type of the subrange is determined;
       then the type node of the appropriate class is created. *)

    elipsis:
      begin
        lbnode := typetree^.defn;                       (* get pointers to parts of defn chain *)
        ubnode := lbnode^.next;
        if ubnode^.next <> nil                          (* get precision, if any *)
          then precnode := ubnode^.next^.next           (* -> "prec" -> <expression> *)
          else precnode := nil;
        if not constant (lbnode, nil, lbval, lbtype) then return;
        if ubnode^.sym = starsy                 (* have "*" upper bound *)
          then begin
            (* A star bound is legitimate with scalar and integer ranges only.
               Here we set the ubtype to the lower bound type.  If it is valid
               the scalar and integer case is chosen, and the star bound is
               explicitly processed.  If the lbtype is not a scalar or integer
               or real, then we fall through and report a bad base type.  If
               lbtype is real or a precision is specified with an integer, then
               the upperbound is semanticated as a constant, and an error is
               reported. *)

            ubtype := lbtype;
            if precnode <> nil then begin       (* n..* prec m is invalid *)
              err_node (err_flex_invalid, typetree);
              return;
            end;
          end
        else if not constant (ubnode, nil, ubval, ubtype) then return;

        (* scalar or integer subrange *)

        if ([lbtype^.kind, ubtype^.kind] <= [scalars, ints, bools, chars]) and (precnode = nil) then begin
          if (lbtype = type_int) and (sp_wor_opt in cur_block^.semantic_options) then
            lbtype := type_fullword; (* let's not be picky *)
          if ubnode^.sym = starsy
            then begin
              ubtype := lbtype;                 (* use maximum value of range *)
              ubval.ival := lbtype^.maxval;
            end
          else if not constant (ubnode, lbtype, ubval, ubtype (* discarded *)) then return;
                                                (* check that ub is compatible with lb *)
          if lbval.ival > ubval.ival then begin                         (* not in ascending order *)
            if lbval.ival = ubval.ival + 1 then
              err_node (err_rng_empty, ubnode)
            else begin
              err_node (err_low_upb, ubnode);
              return;
            end;
          end;
          t_semant := dcl_subrange (lbtype^.base_type, lbval.ival, ubval.ival);
          t_semant^.flexible := (ubnode^.sym = starsy);
        end

        (* real subrange *)

        else if ([lbtype^.kind, ubtype^.kind] <= [ints, reals]) or (precnode <> nil) then begin
          if precnode <> nil then begin
            if not constant (precnode, type_int, precval, st (* dummy *)) then return;
            if not (precval.ival in [minimum (prec_type) .. maximum (prec_type)])
              then begin                                (* precision out of range *)
                err_node (err_bad_precision, precnode);
                return;
              end;
          end
          else precval.ival := type_real^.precision;    (* no precision, use default *)

          t_semant := new_type (reals);         (* generate type node of appropriate precision,
                                                   and use this to convert constants *)
          with t_semant^ do begin
            precision := precval.ival;
            rminval := type_real^.rminval;      (* use best available for reading constants *)
            rmaxval := type_real^.rmaxval;
          end;

          (* we resemanticize the limits to insure that integers are converted
             to reals, and reals are rounded properly. *)

          if not constant (lbnode, t_semant, lbval, st) then return;
          if not constant (ubnode, t_semant, ubval, st) then return;
          with t_semant^ do begin               (* set bounds just read *)
            if lbval.valp^.real_val > ubval.valp^.real_val
              then begin                        (* order bad *)
                err_node (err_low_upb, ubnode);
                dispose (t_semant);
                t_semant := nil;
                return;
              end;
            rminval := lbval.valp^.real_val;
            rmaxval := ubval.valp^.real_val;
          end;
        end

        else begin                              (* invalid types for limits *)
          if lbtype^.kind in [scalars, ints, bools, chars, reals]
            then err_node (err_not_scalar, ubnode)
            else err_node (err_not_scalar, lbnode);
          return;
        end;
      end;


    (* STAR, i.e. "*", indicates the index type of a generic array.  This is
       the only context in which the BNF allows it to appear.  A 'generic'
       integer subrange is created.  The limits are the minimum and maximum
       values so that any constant subscript may be used, without an error
       being reported. *)

    starsy:
      begin
        t_semant := dcl_subrange (type_int, minimum (int_type), maximum (int_type));
        with t_semant^ do begin                 (* flag type as generic *)
          generic := true;
          flexible := true;
        end;
      end;


    (* STRING TYPE introduces a string type.  The definition chain of the node
       gives the maximum length of the type. If missing, the default length is
       used; if "*", then the length is flexible. *)

    string_type:
      begin
        is_flexible := false;   (* until determined otherwise *)
        if typetree^.defn = nil (* default length to be used *)
          then len := 132

        else if typetree^.defn^.sym = starsy then begin (* flexible length *)
          len := maximum (char_range);  (* to avoid compile-time subscripting errors *)
          is_flexible := true;
        end

        else begin                              (* length specified *)
          if not constant (typetree^.defn, type_int, ubval, st (* dummy *)) then return;
          if ubval.ival < 0 then begin
            err_node (err_str_len, typetree^.defn);
            return
          end;
          len := ubval.ival;
        end;

        t_semant := new_type (strings); (* generate type node *)
        with t_semant^ do begin
          str_kind := varying;
          str_length := len;
          flexible := is_flexible;
        end
      end;

    (* POINTER TYPE: the definition chain gives the type referenced by the 
       pointer. *)

    pointer_type:
      begin
        st := type_semantics (typetree^.defn, [flexible_ok, flex_rec_ok, forward_ok]);
        if st = nil then return;
        t_semant := new_type (pointers);
        with t_semant^ do begin
          target_type := st;
          heap_class := chk_class (heap_chain, dynamic_sc, target_type);
        end;
      end;

    (* FILE TYPE: the definition chain contains the type definition of the file
       component. *)

    file_type:
      begin
        if typetree^.defn^.sym = starsy then
          t_semant := dcl_file (binaryfile, nil)
        else begin
          st := type_semantics (typetree^.defn, []);
          if st = nil then return;
          t_semant := dcl_file (typedfile, st);
        end;
        t_semant^.file_class := chk_class (file_chain, fileblk_sc, t_semant);
      end;

    (* SET TYPE: the definition chain contains one node, the set element type
      description.  This must yield a scalar type. *)

    set_type:
      begin
        st := type_semantics (typetree^.defn, []);
        if st = nil then return;
        if not (st^.kind in [ints, chars, bools, scalars, unknown_type])
          then begin                                    (* must have scalar elements *)
            err_node (err_bad_element_type, typetree^.defn);
            return
          end;
        if (st^.kind <> unknown_type) andif
	   ( (st^.minval < set_lwb_limit) or (st^.maxval > set_upb_limit) )
          then begin (* bad set range *)
            err_node (err_set_range, typetree^.defn);
            return;
          end;
        t_semant := new_type (sets);            (* create set type *)
        t_semant^.set_element_type := st
      end;

    (* SUBR TYPE yields a declaration for a formal procedure or function.  The
       definition chain appears as follows: [ procedure | function ] ->
       [ <parameter list> ] -> [ <return type> ]. *)

    subr_type:
      begin
        pfsym := typetree^.defn;                        (* points either "procedure" or "function" *)
        if pfsym^.next^.sym = parm_list then begin
          parmlst := pfsym^.next;
          rettype := parmlst^.next
        end
        else begin
          parmlst := nil;
          rettype := pfsym^.next
        end;
        t_semant := subr_formal_type (pfsym, parmlst, rettype, false, no_parms);
      end;                                      (* false => parms not required *)

    (* ARRAY TYPE yields a declaration for an array type.  The definition chain
       appears as follows: <index type>* -> <component type>. *)

    array_type:
      begin
        t_semant := make_array_type (typetree^.defn, false, true);
      end;

    pk_array_type:
      begin
        t_semant := make_array_type (typetree^.defn, true, true);
      end;

    (* RECORD TYPE processes a <record type> declaration. The definition chain
       is arranged thus: [ <field id decl> ]* -> [ <variant part> ]. Here we
       create the main record type, and call fieldlist to process the underlying
       structure. *)

    record_type:
      begin
        ntype := new_type (records);            (* create top-level record node *)
        elem := typetree^.defn;                 (* check for packed *)
        last_field := nil;                      (* for chaining fields *)
        fieldlist (ntype, ntype, elem, 1, last_field);  (* process fields, etc. *)
        t_semant := ntype;
      end;

    (* TYPE DECL is a place holding node in the parse tree.  It is introduced
       when there is a missing type declaration, or unrecoverable error in a
       type declaration.  As an error has been reported in the parse, just return
       nil to suppress further errors. *)

    type_decl:
      t_semant := nil;

    (* OTHERS indicates a weird looking node, most likely an <expression> 
       which should have been part of a range. Report an error, and return nil. *)

     others:
       begin
         err_node (err_type_dcl_invalid, typetree);
       end

  end (* case sym *) ;
 end;
$PAGE type_semantics
(* TYPE SEMANTICS semanticates a parse tree describing a type.  At this level we
   may seen either a <packed type> or a tree for an unpacked type.  If a <packed
   type> is present, the packing information is processed and appended to the
   node generated for the inferior unpacked type tree by t_semant.  This
   includes transforming a packed array of char into a string.  The format
   of a <packed type> ::= [ <constant width> ] -> <some unpacked type>. Also,
   we check that forward, flexible and generic types are used in permissable
   contexts. *)

public function type_semantics (* typetree: parse_node; flags: tsm_flags ): typ *) ;

 var
   width, typenode: parse_node;
   widthval: val;

 begin
  if typetree^.sym <> packed_type then begin    (* simple unpacked type *)
    type_semantics := t_semant (typetree);
    if type_semantics <> nil then
      alloc_type (type_semantics);
  end

  else begin                            (* packed type *)
    width := typetree^.defn;            (* get components of tree *)
    if width^.next <> nil
      then typenode := width^.next      (* defn chain: width -> unpacked type *)
      else begin
        typenode := width;              (* defn chain: unpacked type *)
        width := nil;
      end;
    type_semantics := t_semant (typenode);
    if (type_semantics <> nil) andif (typenode^.sym <> pk_array_type) then begin
      if (typenode^.sym = ident) or
         (type_semantics^.kind in [strings, pointers, procs, funcs]) then
        err_node (err_not_packable, typenode)
      else
        do_packing (type_semantics, width);
    end;
  end (* if <packed type> *);

  (* check for an over-sized set *)

  if (type_semantics <> nil) andif (type_semantics^.kind = sets) andif
     (type_semantics^.base_size > set_size_limit) then
    err_node (err_set_range, typetree^.defn);

  (* perform checks for flexible, generic, and forward types *)

  if type_semantics <> nil then begin
    with type_semantics^ do begin
      if flexible andif not (flexible_ok in flags)
        then err_node (err_bad_flexible, typetree)
      else if flexible andif (kind = records) andif not (flex_rec_ok in flags)
        then err_node (err_flex_rec_invalid, typetree)
      else if generic andif (not (generic_ok in flags))
        then err_node (err_bad_generic, typetree)
      else if (kind = unknown_type) andif (not (forward_ok in flags))
        then begin                      (* discard the node, others not prepared to deal with it *)
	  err_print (err_bad_forward, typetree^.source,
		     type_semantics^.type_id^.name^.text, typetree^.column);
          type_semantics := nil;
        end;
    end;
  end;
 end.
   <Î