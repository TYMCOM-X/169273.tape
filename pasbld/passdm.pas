module passdm;
$TITLE PASSDM -- pascal symbol table dump and xref facility
$LENGTH 44
(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P A S S D M                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  This is the symbol table dumping module.  It  performs
        the  functions  required  by  the  SYMBOLS,  XREF,  and CALLS
        options.
     
     USAGE:
     
           EXTERNAL PROCEDURE XR_SYM_CALL;
     
     INPUT:
     
        .XRF-file    PASSDM reopens the  XRF  file  if  the  XREF  or
                    SYMBOLS  option is specified.  The calls graph is
                    examined if the CALLS option  is  specified,  and
                    the scope chain of a given block is reattached to
                    the name tree if the SYMBOLS option is specified.
     
     OUTPUT:
     
        .LST-file    The output from PASSDM is appended to  the  list
                    file.  The  symbols  reattached to the name table
                    are removed.
     
     ---------------------------------------------------------------- *)
$INCLUDE pascal.inc
$INCLUDE ptmcon.inc
$INCLUDE pasist.inc
$INCLUDE passet.inc
$INCLUDE pasxrf.typ
$INCLUDE pasfil.inc
$INCLUDE pascv.inc
$INCLUDE paspt.typ
$INCLUDE versio.inc
$INCLUDE tmpnam.inc
$PAGE declarations
external var (*from paslst*)
    time_string: string [24];
    mod_ident: string [12];
const
  quan: fio_width := 3;         (* quantum of indentation *)
  ck_col: fio_width := 44;      (* column for class/kind *)
  decl_col: fio_width := 64;    (* column for 'declared at' *)
  def_col: fio_width := 79;     (* column for 'defined as' *)
  src_ind: fio_width := 14;     (* width of xref source id and use field *)
  nam_ind: fio_width := 20;     (* width of calls proc name field *)
  max_nam: fio_width := 18;     (* max length of proc name string *)

type

  xr_kind_of_ref = (            (* type of reference from xrf file *)
    val_xrk,                    (* ref to value *)
    mod_xrk,                    (* modification *)
    var_p_xrk,                  (* passed as var parameter *)
    called_xrk,                 (* called directly *)
    ref_xrk,                    (* "reference" -- to address only *)
    decl_xrk);                  (* for decl when XREF and not SYMS *)

  xr_ptr = ^xr_rec;             (* for building reference chains *)

  xr_rec = packed record        (* element on chain *)
    xsrc_id: source_id;         (* where reference was made *)
    xnext: xr_ptr;              (* next on chain *)
    xrkind: xr_kind_of_ref      (* what kind of reference *)
  end;

  xr_arr = packed array [1..*] of xr_ptr;
  xr_aptr = ^xr_arr;            (* pointer to array of pointers *)

  decl_arr = packed array [1..*] of source_id;
  decl_ptr = ^ decl_arr;

  blk_action = procedure (blk); (* type of action routines *)

  blk_array = packed array[1..*] of blk;
  blk_ptr = ^ blk_array;

  title_str = string[80];       (* to remember subtitles *)

var
  x_vl_chain,                   (* points to var and label reference chains *)
  x_nvl_chain: xr_aptr;         (* a slack-allocated array for each type
                                   one per sym record in symbol table *)

  x_nvl_enable,                 (* set vectors parallel to above *)
  x_vl_enable: svector;         (* true if xref desired for symbol referred *)

  calls_matrix: svector;        (* holds the relation "called from" *)

  calls_vector: blk_ptr;        (* translates block number to blk node *)

  out_init,                     (* true if output stream has been initialized *)
  any_calls,                    (* true if any block has the CALLS...*)
  any_syms,                     (* or the SYMBOLS... *)
  any_xref: boolean;            (* or the XREF options in effect *)

  vl_syms_declared,             (* array of declaration source id's *)
  nvl_syms_declared: decl_ptr;  (* just pointers *)

  subtitle: title_str;          (* remembers current title for page skips *)

  xrf_file: file of xrf_record; (* our xref file identifier *)
$PAGE pre_order
procedure pre_order (action: blk_action);

(* PRE_ORDER does the pre-order traversal of 'interesting' block nodes
   (ie-- everything but the root block node), calling the formal proc-
   edure ACTION for each one. *)

  var
    b: blk;

begin
  b := root_block^.children;    (* starting with prog/mod block *)
  loop
    loop
      if b^.kind in [program_blk,module_blk,data_blk,subr_blk] then
        action (b);             (* interesting block, give block ptr! *)

    exit if b^.children = nil;
      b := b^.children
    end (* loop *);

    while (b^.peer = nil) and (b^.parent <> nil) do
      b := b^.parent;

  exit if b^.peer = nil;
    b := b^.peer
  end (* loop *);
end (* procedure pre_order *);
$PAGE enable_scope
procedure enable_scope (b: blk);

(* ENABLE_SCOPE sets the enable arrays true for every symbol on the
   block b's scope chain. *)

  var
    s: sym;
    sl: sym_list;

  procedure walk_scope_chain;

  (* this helper does the actual walking and setting of the bits. *)

  begin
    while s <> nil do begin
      if s^.kind in [labels,vars] then
        add_elem (x_vl_enable, 0, s^.id_number)
      else add_elem (x_nvl_enable, 0, s^.id_number);
      s := s^.next
      end
  end (* procedure walk_scope_chain *);

begin (* procedure enable_scope *);
  s := b^.parm_list.first;
  walk_scope_chain;
  s := b^.label_list.first;
  walk_scope_chain;
  s := b^.type_list.first;
  walk_scope_chain;
  s := b^.id_list.first;
  walk_scope_chain;
  s := b^.return_sym;
  walk_scope_chain
end (* procedure walk_scope_chain *);
$PAGE exam_options
procedure exam_options (b: blk);

(* EXAM_OPTIONS determines the necessary actions for the rest of
   the SYM/XREF/CALLS option processing.  Called once for each
   block, it sets global indicators ANY_CALLS, etc., and creates
   the various arrays as needed.  Note -- it is assumed that global
   (ie command line) options can be obtained from the program or
   module block in its SEMANTIC_OPTIONS field. *)

  var
    i: integer;

begin

  with b^ do begin
    if not any_calls andif
       (calls_opt in semantic_options) then begin
      any_calls := true;        (* set flag, create calls matrix *)
      calls_matrix := new_svector (blk_number,blk_number);
      new (calls_vector, blk_number);
      for i := 1 to blk_number do begin
        clr_set (calls_matrix, i);
        calls_vector^ [i] := nil
        end
      end;

    if not any_syms andif
       (symbols_opt in semantic_options) then begin
      any_syms := true; (* set flag, create holder for declared *)
      new (vl_syms_declared, sym_vl_number);
      for i := 1 to sym_vl_number do
        vl_syms_declared^ [i] := null_source;
      new (nvl_syms_declared, sym_nvl_number);
      for i := 1 to sym_nvl_number do
        nvl_syms_declared^ [i] := null_source
      end;

    if xref_opt in semantic_options then
      begin                     (* create xref data structure, put current in *)
      if not any_xref then begin
        any_xref := true;
        x_vl_enable := new_svector (0, sym_vl_number);
        clr_set (x_vl_enable, 0);
        x_nvl_enable := new_svector (0, sym_nvl_number);
        clr_set (x_nvl_enable, 0);
        new (x_vl_chain, sym_vl_number);
        for i := 1 to sym_vl_number do
          x_vl_chain^ [i] := nil;
        new (x_nvl_chain, sym_nvl_number);
        for i := 1 to sym_nvl_number do
          x_nvl_chain^ [i] := nil;
        end;

      enable_scope(b)           (* set true every symbol on level's bit *)
      end
    end (* with *);
end (* procedure exam_options *);
$PAGE interesting
function interesting (
  ind: xparm_val;
  vl: boolean
  ): boolean;

(* INTERESTING returns true if the exam_option array x_vl_enable and
   x_nvl_enable are true, that is, the symbol identified by ind and vl
   is in a module for which XREF has been specified. *)

begin
  interesting :=
    (vl andif in_set (x_vl_enable, 0, ind) )  or
    ( (not vl) andif in_set (x_nvl_enable, 0, ind) )
end (* function interesting *);
$PAGE get_calls
procedure get_calls (b: blk);

(* GET_CALLS is an action routine for the pre-order walk routine to
   fill the calls matrix with the "called from" relation.  The
   block being inspected is the target of the relation, while the
   calls chains in the symbol table provides the domain elements. *)

  var 
    called: call_link;          (* to walk each block's call chain *)

begin
  with b^ do begin
    calls_vector^  [number] := b;  (* to translate block number to this block *)
    called := calls;
    while called <> nil do begin  (* for each routine called by b *)
      add_elem (calls_matrix, called^.called_subr^.number, number);

      (* the above stmt added the relation "called^ called from b" *)

      called := called^.rlink
      end
    end
end (* procedure get_calls *);
$PAGE chain_on
procedure chain_on (
  loc: source_id;               (* reference's source id *)
  kind: xr_kind_of_ref;         (* kind of reference done *)
  par: xparm_val;               (* symbol number *)
  var_lab: boolean              (* true if var/label numbering *)
  );

(* CHAIN_ON attaches a xref file record to the reference data structure.
   A new element is pushed onto the proper stack as determined by the
   symbol number, and the numbering identification. *)

  var
    t: xr_ptr;                  (* for the new entry *)

begin
  if interesting (par, var_lab) then begin
    new (t);
    with t^ do begin
      xsrc_id := loc;
      xrkind := kind;
      if var_lab then begin
        xnext := x_vl_chain^ [par];
        x_vl_chain^ [par] := t
        end
      else begin
        xnext := x_nvl_chain^ [par];
        x_nvl_chain^ [par] := t
        end
      end
    end
end (* procedure chain_on *);
$PAGE reverse
procedure reverse (var first: xr_ptr);

(* REVERSE reverses the links in a singly linked list of reference nodes.
   A good general-purpose algorithm.  Watch carefully now... *)

  var
    prev, next: xr_ptr;

begin
  if first <> nil then begin    (* nothing to reverse?? *)
    prev := nil;                (* the new pointer for current *)
      loop
        next := first^.xnext;   (* get one ahead of current *)
        first^.xnext := prev;   (* fix current to point to previous one *)
      exit if next = nil;               (* no more to do *)
        prev := first;          (* new pointer ... *)
        first := next           (*   to assign to new current *)
      end
    end
end (* procedure reverse *);
$PAGE discard
procedure discard (                     (* dispose array of chains *)
  var base: xr_aptr;                    (* which array of chains *)
  count: id_range );                    (* how many chains in array *)

  (* DISCARD gets a pointer to a slack-allocated array serving as
     the head pointers for reference chains for each symbol. To dispose,
     first dispose every element on each chain, and then dispose the
     head pointer array. *)

  var
    first, next: xr_ptr;                (* to walk chains *)
    i: id_range;                        (* to index array *)

begin
  for i := 1 to count do begin
    first := base^ [i];                 (* select the ith chain *)
    while first <> nil do begin         (* and chuck every element *)
      next := first^.xnext;
      dispose (first);
      first := next
      end
    end;
  dispose (base)                        (* now chuck the array itself *)
end (* procedure discard *);
$PAGE xrf_scan
procedure xrf_scan;

(* XRF_SCAN accumulates information from the xref file according to
   the options in effect as determined by exam_options. This should
   be called only if at least one of XREF or SYMB is in effect. *)

  var
    curr_loc: source_id;        (* we're the fukharwi *)

begin
  curr_loc := null_source;
  reset (xrf_file, tempname ('XRF'));
  if eof (xrf_file) then return; (* can't get it -- forget it *)

  repeat
    with xrf_file^ do
      case code of              (* what kind of record is it? *)

        file_xrf:               (* change of file number of curr_loc *)
          curr_loc.file_no := parameter;

        page_xrf:               (* change of page number *)
          curr_loc.page_no := parameter;

        line_xrf:               (* change of line number *)
          curr_loc.line_no := parameter;

        decl_xrf:               (* a declaration *)
          if any_syms then begin  (* if SYMB, put decl loc in array *)
            if var_lab_parm then
              vl_syms_declared^ [parameter] := curr_loc
            else nvl_syms_declared^ [parameter] := curr_loc
            end
          else if any_xref then   (* if XREF, chain as if any other ref *)
            chain_on (curr_loc, decl_xrk, parameter, var_lab_parm);

        value_ctxt:             (* now come the simple chain-on-ers *)
          if any_xref then
            chain_on (curr_loc, val_xrk, parameter, var_lab_parm);

        ref_ctxt:
          if any_xref then
            chain_on (curr_loc, ref_xrk, parameter, var_lab_parm);

        mod_ctxt:
          if any_xref then
            chain_on (curr_loc, mod_xrk, parameter, var_lab_parm);

        var_parm_ctxt:
          if any_xref then
            chain_on (curr_loc, var_p_xrk, parameter, var_lab_parm);

        others:
          (* not interested *)

      end (* case and with *);

    get (xrf_file)
  until eof (xrf_file);
  close (xrf_file)
end (* procedure xrf_scan *);
$PAGE output_util
procedure do_new_page (var fb: file_block);

(* DO_NEW_PAGE is called when FIO decides to start a new page. *)

begin
  fio_line (fb, 'Pascal, Version ' || version || ' Symbol Table Dump   ' ||
    time_string || ' Module: ' || mod_ident || ' Page ' ||
    cv_int (listfb.pageno) );
  if subtitle <> '' then 
    fio_line (fb, subtitle);
  fio_skip (fb);
  fio_skip (fb)
end (* procedure do_new_page *);


procedure output_init;

(* OUTPUT_INIT is called the first time from DO_OUTPUT (when OUT_INIT is
   false).  It sets up the list file and initializes the subtitle. *)

begin
  out_init := true;
  subtitle := '';
(*  fio_reopen (listfb);  *)
  listfb.page_header := do_new_page;
  fio_page (listfb)
end (* procedure output_init *);
$PAGE get_title

function get_title (b: blk): title_str;

(* GET_TITLE returns the name of the block b.  It either pulls the
   name out of the appropriate symbol table record, or returns an
   appropriate dummy ( '<block>' if no name ). *)

begin
  with b^ do
    case kind of

      program_blk, module_blk, data_blk:
        if id = nil then get_title := '<block>'
        else get_title := 
          uppercase (substr (id^.text, 1, min (length (id^.text), max_nam) ) );

      subr_blk:
        if (subr_sym = nil) orif
          not (subr_sym^.kind in [consts, vars, values]) orif
          (subr_sym^.name = nil) then
          get_title := '<block>'
          else get_title := 
            uppercase (substr (subr_sym^.name^.text, 1,
              min (length (subr_sym^.name^.text), max_nam) ) );

      others:
        get_title := '<block>'
    end (* case and with *);
end (* function get_title *);
$PAGE get_blk_type
function get_blk_type (b: blk): title_str;

(* GET_BLK_TYPE returns a string identifying the kind of block to which
   b points (i.e. 'public procedure', 'data module', etc.), or simply
   'block' if it can't figure it out. *)

begin
  with b^ do
    case kind of

      program_blk:
        get_blk_type := 'program';

      module_blk:
        get_blk_type := 'module';

      data_blk:
        get_blk_type := 'data module';

      subr_blk:
        if (subr_sym = nil) orif
          not (subr_sym^.kind in [consts, vars, values] ) then
          get_blk_type := 'block'
        else begin
          if subr_sym^.public_dcl then
            get_blk_type := 'public '
          else if subr_sym^.dcl_class = external_sc then
            get_blk_type := 'external '
          else get_blk_type := '';

          if (subr_sym^.type_desc = nil) orif
            not (subr_sym^.type_desc^.kind in [procs, funcs]) orif
            (subr_sym^.type_desc^.return_type = nil) then
            get_blk_type := get_blk_type || 'procedure'
          else get_blk_type := get_blk_type || 'function'
        end
    end
end;
$PAGE do_enclosing
procedure do_enclosing (var b: blk);

(* DO_ENCLOSING writes out the 'Enclosing blocks:' section of a symbol
   table dump when the SYMBOLS option is specified.  The block passed
   is the immediate lexical parent of the current block. *)

begin
  fio_skip (listfb);
  fio_tab (listfb, quan);
  fio_line (listfb, 'Enclosing blocks:');
  while (b <> nil) and (b <> root_block) do begin
    fio_tab (listfb, 2*quan);
    fio_write (listfb, get_title (b) );
    fio_tab (listfb, ck_col);
    fio_write (listfb, get_blk_type (b) );
    fio_tab (listfb, decl_col);
    if b^.kind = subr_blk then
      fio_write (listfb, cv_source_id (b^.declaration) );
    fio_tab (listfb, def_col);
    fio_line (listfb, 'Level=' || cv_int (b^.level) );
    b := b^.parent
  end
end (* procedure do_enclosing *);
$PAGE reattach and pop_scope_chain
procedure reattach (first: sym);

(* REATTACH places the sym nodes on the scopechain starting with first^
   back on the scopechains of their name table nodes.  The current value
   of the name table scopechain is preserved in sym^.scopechain, so
   that the chain may be removed with the POP_SCOPE routine in PASUTL. *)

  var
    s: sym;

begin
  s := first;
  while s <> nil do begin
    if s^.name <> nil then begin
     copechain := s^.name^.scopechain;  (* preserve current top *)
      s^.name^.scopechain := s          (* before attaching new *)
      end;
    s := s^.next                        (* next on chain to attach *)
    end
end (* procedure reattach *);


procedure pop_scope_chain (fsym: sym);

(* POP_SCOPE_CHAIN is copied from PASUTL to remove a scope chain. *)

  var
    s: sym;

begin
  s := fsym;
  while s <> nil do begin
    with s^ do begin
      if name <> nil then
        name^.scopechain := scopechain;
      s := s^.next
      end
    end
end (* procedure pop_scope_chain *);
$PAGE get_base_type
function get_base_type (t: typ): title_str;

(* GET_BASE_TYPE creates an appropriate string to identify the type node t^.
   This can be a component of a var/const descriptor, but will usually be
   called to identify a type name as a symbol by itself. *)

begin
  with t^ do
    case kind of
      bools: get_base_type := 'boolean';
      ints, scalars: get_base_type := 'subrange';
      chars: get_base_type := 'char';
      reals: get_base_type := 'real';
      sets: get_base_type := 'set';
      pointers: get_base_type := 'pointer';
      arrays:
        if packable then
          get_base_type := 'packed array'
        else get_base_type := 'array';
      records, variants:
        if packable then
          get_base_type := 'packed record'
        else get_base_type := 'record';
      files: get_base_type := 'file';
      strings:
        if str_kind = varying then
          get_base_type := 'varying string'
        else get_base_type := 'fixed string';
      procs: get_base_type := 'procedure';
      funcs: get_base_type := 'function';
      indirect_type:
        if actual_type = nil then
          get_base_type := 'unknown'
        else get_base_type := get_base_type (actual_type);
      others: get_base_type := 'unknown'
    end (* case and with *)
end (* function get_base_type *);
$PAGE do_vc_type
function do_vc_type (s: sym): title_str;

(* DO_VC_TYPE returns a string suitable for concatenating to a symbol name
   which is a var or const.  If the const is a procedure, the null string
   is returned, otherwise a suitable type decriptor is returned. *)

begin
  do_vc_type := '';
  with s^ do begin
    if type_desc = nil then
      do_vc_type := ': unknown'
    else with type_desc^ do begin
      if (type_id <> nil) andif (type_id^.kind = types) andif
        (type_id^.name <> nil) then
        do_vc_type := ': ' ||
          substr (type_id^.name^.text, 1,
            min (length (type_id^.name^.text), max_nam) )
      else begin
        if kind = procs then begin
          if s^.kind <> consts then
            do_vc_type := ': procedure';  (* unnamed procedure variable *)
          return
          end
        else if kind = funcs then begin
          if return_type = nil then
            do_vc_type := ': unknown'
          else begin
            if (return_type^.type_id = nil) orif
              (return_type^.type_id^.name = nil) then
              do_vc_type := ': ' ||     (* nameless fval, get generic *)
                get_base_type (return_type)
            else do_vc_type := ': ' || 
              substr (return_type^.type_id^.name^.text, 1,
                min (length (return_type^.type_id^.name^.text), max_nam) )
            end
          end
        else do_vc_type := ': ' || get_base_type (type_desc)
        end
      end
    end
end (* function do_vc_type *);
$PAGE get_kind
function get_kind (s: sym): title_str;

(* GET_KIND can be called to compose a string that will describe the
   kind of a symbol s^ is ('public var', 'pointer type' are a few
   possibilities). *)

begin
  with s^ do
    case kind of

      types:
        if type_desc = nil then
          get_kind := 'unknown type'
        else get_kind := get_base_type (type_desc) || ' type';
      labels: get_kind := 'label';
      consts, vars:
        begin
          if public_dcl then
            get_kind := 'public '
          else if dcl_class = external_sc then
            get_kind := 'external '
          else if dcl_class = static_sc then
            get_kind := 'static '
          else get_kind := '';
          if kind = vars then begin
            if dcl_class = parameter_sc then
              get_kind := 'var parameter'
            else get_kind := get_kind || 'var'
            end
          else begin
            if (type_desc <> nil) then begin
              if type_desc^.kind = procs then
                get_kind := get_kind || 'procedure'
              else if type_desc^.kind = funcs then
                get_kind := get_kind || 'function'
              else get_kind := get_kind || 'const'
              end
            else get_kind := get_kind || 'const'
            end
        end (* consts and vars *);

      values: get_kind := 'value parameter';

      others: get_kind := ''
    end (* case and with *)
end (* function get_kind *);
$PAGE do_symbol - in inorder
procedure inorder (b: blk);

(* INORDER calls DO_SYMBOL to print the formatted information about the
   symbol.  INORDER traverses the name tree in order, so the symbols in
   block b^ are listed in alphabetic order.  By enclosing the recursive
   routine within INORDER, a parameter load is saved (to reduce the stack
   depth required for the walk). *)

  procedure DO_SYMBOL (s: sym);

  (* DO_SYMBOL performs all the output for the symbol s in block b. *)

    var
      col, i: 0..10000;
      usage: xr_ptr;
      tempstr: title_str;
      temploc: source_id;

  begin
    fio_tab (listfb, 2*quan);   (* start up in 2 quanta *)
    fio_write (listfb,          (* write out symbol name *)
      substr (s^.name^.text, 1, 
        min (length (s^.name^.text), max_nam) ) );
    if symbols_opt in b^.semantic_options then begin
      if s^.kind in [vars, consts, values] then
        fio_write (listfb, do_vc_type (s) );
      fio_tab (listfb, ck_col); (* symbols option, print line of info *)
      if s = b^.return_sym then
        fio_write (listfb, 'function value')
      else fio_write (listfb, get_kind (s) );
      fio_tab (listfb, decl_col );
      if s^.kind in [vars, labels] then
        temploc := vl_syms_declared^ [s^.id_number]
      else temploc := nvl_syms_declared^ [s^.id_number];
      with temploc do
	if (file_no <> null_source.file_no) or (page_no <> null_source.page_no)
	     or (line_no <> null_source.line_no) then
	  fio_write (listfb, cv_source_id (temploc) );
      fio_tab (listfb, def_col);
      if s^.kind in [vars, values] then
        fio_write (listfb, '+' || cv_radix (s^.item_addr, adr_width) )
      else if s^.kind = labels then
        fio_write (listfb, cv_source_id (s^.lab_declaration) );
      col := 9999               (* to force new line *)
      end
    else col := (3 * quan) + (2 * src_ind);  (* else start at second place *)

    if xref_opt in b^.semantic_options then begin
      if s^.kind in [vars, labels] then
        usage := x_vl_chain^ [s^.id_number]
      else usage := x_nvl_chain^ [s^.id_number];
      reverse (usage);
      while usage <> nil do begin       (* print out each usage *)
        if col + src_ind > listfb.width then begin
          fio_skip (listfb);
          col := quan * 3
          end;
        fio_tab (listfb, col);
        tempstr := cv_source_id (usage^.xsrc_id);
        case usage^.xrkind of
          val_xrk: tempstr := tempstr || 'V';
          mod_xrk: tempstr := tempstr || 'M';
          var_p_xrk: tempstr := tempstr || 'P';
          called_xrk: tempstr := tempstr || 'C';
          ref_xrk: tempstr := tempstr || 'R';
          decl_xrk: tempstr := tempstr || 'D'
        end (* case *);

        fio_write (listfb, tempstr);
        col := col + src_ind;
        usage := usage^.xnext
        end (* while *);
      end (* if xref_opt *);

    fio_skip (listfb)
    end (* procedure do_symbol *);
$PAGE walk - in inorder - and inorder
  procedure walk (n: nam);

  (* WALK runs the show, finding sym nodes on the scopechain of the name
     tree whose block pointers point to b (the parameter to INORDER). 
     Those syms are the syms defined in b, so DO_SYMBOLS outputs info. *)

  begin
    if n <> nil then begin
      walk (n^.alink);
      if (n^.scopechain <> nil) andif
        (n^.scopechain^.block = b) then   (* b is parameter to inorder *)
        do_symbol (n^.scopechain);
      walk (n^.zlink)
      end
  end (* procedure walk *);


begin (* procedure inorder *)
  walk (root_name)
end (* procedure inorder *);
$PAGE do_names
procedure do_names (b: blk);

(* DO_NAMES controls the output generated from the SYMBOLS and XREF
   options within a single block.  It emits the header for the section
   if one is needed, then reattaches the block's symbols to the name
   table.  The name table is then walked in order, and the block's
   symbols are removed. *)

begin
  with b^ do
  if (parm_list.first <> nil) or        (* make sure there's something *)
    (label_list.first <> nil) or        (*   to do *)
    (type_list.first <> nil) or
    (id_list.first <> nil) or
    (return_sym <> nil) then begin

    fio_skip (listfb);          (* a blank line to mark section *)
    fio_tab (listfb, quan);     (* indent header a quantum *)
    if symbols_opt in semantic_options then begin
      fio_write (listfb, 'NAME [:TYPE]');
      fio_tab (listfb, ck_col);
      fio_write (listfb, 'CLASS/KIND');
      fio_tab (listfb, decl_col);
      fio_write (listfb, 'DECLARED');
      fio_tab (listfb, def_col);
      fio_write (listfb, 'DEFINED')
      end
    else fio_write (listfb, 'SYMBOLS:');
    fio_skip (listfb);

    reattach (parm_list.first); (* chain all symbols back to name table *)
    reattach (label_list.first);
    reattach (type_list.first);
    reattach (id_list.first);
    if return_sym <> nil then reattach (return_sym);

    inorder (b);                (* do the walk for b *)
    pop_scope_chain (parm_list.first);
    pop_scope_chain (label_list.first);
    pop_scope_chain (type_list.first);
    pop_scope_chain (id_list.first);
    if return_sym <> nil then pop_scope_chain (return_sym)
    end
end (* procedure do_names *);
$PAGE do_calls
procedure do_calls (b: blk);

(* DO_CALLS outputs the "called from" relation for block b as determined
   by GET_CALLS.  The name corresponding to every block calling b is
   printed out, using nam_ind as the tab quantum and max_nam as the
   max length to print.  Routines called from an unnamed mainline are
   called from '<block>', the string used for the unnamed program block. *)

  var
    col,                        (* for output column *)
    i: 1..10000;                (* for blocks *)

begin
  if not is_empty (calls_matrix, b^.number) then begin
    fio_skip (listfb);
    fio_tab (listfb, quan);
    fio_write (listfb, 'Called from:');
    col := 9999;                (* to force end of line on first name *)

    for i := 1 to blk_number do
      if in_set (calls_matrix, b^.number, i) and
        (calls_vector^ [i] <> nil) then begin
        if col + nam_ind > listfb.width then begin
          fio_skip (listfb);    (* would have overflowed -- new line *)
          col := quan * 2
          end;
        fio_tab (listfb, col);
        fio_write (listfb, get_title (calls_vector^ [i]) );
        col := col + nam_ind
        end;

    fio_skip (listfb)
    end
end (* procedure do_calls *);
$PAGE do_output
procedure do_output (b: blk);

(* DO_OUTPUT is the controller for the output of a single block.  It is
   in charge of the formatting, and controls the titles and page numbers. *)

  var
    tempstr: title_str;         (* temp for new title line *)
    bt: blk;                    (* temp for walking block chains *)

begin
  subtitle := '';               (* to inhibit new title line if at bottom *)
  tempstr := get_blk_type (b) || (* figure out the title line *)
    ' ' ||                      (* block type and block name *)
    get_title (b);
  tempstr [1: 1] := 
    uppercase (substr (tempstr, 1, 1) );  (* capitalize first letter of type *)
  if not out_init then
    output_init;
  fio_skip (listfb);            (* blank lines without subtitle line *)
  fio_skip (listfb);
  fio_line (listfb, tempstr); (* do it, may have forced a new page *)
  subtitle := tempstr || ' (continued)';
  bt := b^.parent;
  if (bt <> nil) and (bt <> root_block) then
    do_enclosing (bt);
  if calls_opt in b^.semantic_options then
    do_calls (b);
  if (symbols_opt in b^.semantic_options) or
     (xref_opt in b^.semantic_options) then
    do_names (b)
end (* procedure do_output *);
$PAGE xr_sym_call body
public procedure xr_sym_call;

(* XR_SYM_CALL is called from wherever to append the output generated
   by the XREF, SYMBOL, and CALLS options to the list file. *)

begin
  any_syms := false;            (* init work indicators *)
  any_xref := false;
  any_calls := false;
  out_init := false;
  pre_order (exam_options);     (* examine blocks pre-order for options *)

  if any_xref or any_syms then
    xrf_scan;                   (* collect data from .XRF file *)

  if any_calls then
    pre_order (get_calls);      (* compute calls matrix from pre-order walk *)

  if any_xref or any_calls or any_syms then  (* if anything to do *)
    pre_order (do_output);

  if any_xref then begin        (* dispose xref data structures *)
    del_svector (x_vl_enable);
    del_svector (x_nvl_enable);
    discard (x_vl_chain, sym_vl_number);
    discard (x_nvl_chain, sym_nvl_number)
    end;

  if any_syms then begin
    dispose (vl_syms_declared);
    dispose (nvl_syms_declared);
    end;

  if any_calls then begin
    del_svector (calls_matrix);
    dispose (calls_vector)
    end;
end (* procedure xr_sym_call and module passdm *).
    [ L«