$TITLE pasblk

module pasblk;

$PAGE includes
$include pascal.inc
$include pasist.inc
$include paspt.typ
$include pasif.typ
$include pasfil.inc
$include paserr.inc
$include passw.inc
$include pasanl.inc
$include pasutl.inc
$include pastsm.inc
$INCLUDE passet.typ
$include pa1dmp.inc
$INCLUDE pa1xrf.inc
$include pasesm.inc
$include pasbod.inc
$INCLUDE pasifu.inc
$INCLUDE passmo.inc
$INCLUDE pastal.inc
$INCLUDE TIMUTL.inc


public var
    env_compilation: boolean; (* TRUE if compilation is an envmodule *)
$PAGE open_block
(* OPEN BLOCK establishes a block as the current block. *)

public procedure open_block ( this_block: blk );
 begin
  cur_block := this_block;
 end;
$PAGE close_block
(* CLOSE BLOCK terminates the current block.  It performs all required bookkeeping
   such as popping local names off of the name "stack".  And, it establishes the
   parent of the current block as the current block. *)

public procedure close_block;

 var ssym, lab: sym;
     cblk, lblk, nblk: blk;             (* for reversing child chain *)
     have_public: boolean;

 begin
  all_opts := all_opts + cur_block^.semantic_options;

  (* Reverse the chain of child blocks so that they appear in their lexical
     order.  The reverse order is the result of the action of "new_blk".
     Note that forward declared subroutines appear in the chain at the point
     at which the forward declaration was made. Also, check for any block
     which is an unresolved forward reference, and check that a MODULE block
     contains at last one public subroutine. *)

  have_public := false;
  cblk := cur_block^.children;
  lblk := nil;
  while cblk <> nil do begin                    (* scan chain, remembering previous node *)
    with cblk^ do begin
      nblk := peer;                             (* remember next in reverse order *)
      peer := lblk;                             (* now put in lexical order *)
      if (kind = subr_blk) andif (subr_sym <> nil) andif (subr_sym^.public_dcl) then
        have_public := true; (* check for public subroutine *)
      if (kind = subr_blk) andif forward_dcl then begin (* have undeclared forward proc *)
        if (subr_sym <> nil) andif (subr_sym^.name <> nil)
          then err_print (err_no_forward_body, declaration, '', 0)
      end;
    end;
    lblk := cblk;
    cblk := nblk
  end;
  cur_block^.children := lblk;                  (* point block at normal order chain *)
  if (cur_block^.kind = module_blk) and (not have_public)
    then error (err_no_entry);                  (* must have one subroutine accessible to outside *)

  (* Terminate the current block (i.e. level of lexical scope).  Remove all
     symbols from their scopechains, and make this blocks parent the current
     block. *)

  with cur_block^ do begin
    if switch (cur_block^.dump_switches,'ST') then begin
      dmpblock (cur_block);
      dmpstable (cur_block)
    end;
    pop_scope (parm_list.first);                        (* remove symbols on all chains *)
    pop_scope (label_list.first);
    pop_scope (type_list.first);
    pop_scope (id_list.first);
    pop_scope (return_sym);                     (* degenerate list *)
    cur_block := parent;
  end;
 end;
$PAGE set_subr_options
(* SET SUBR OPTIONS processes a <subr options> list.  The definition is simply
   a list of identifiers denoting subroutine options.  This checks that the names
   are in fact valid options, and sets the appropriate flags in the block or
   type node for the subroutine or main program block.  The "inherited" parameter
   indicates whether only "*" options or only non-"*" options are to be processed.
   If the block node is nil, the subroutine is assumed to be external, affecting
   the options that may be set. *)

procedure set_subr_options ( optlist: parse_node;
                             inherited: boolean;
                             subr_type: typ;
                             subr_block: blk       );

 var wrd: parse_node;
     nxt: parse_node;

 begin
  if optlist <> nil then begin
    wrd := optlist^.defn;                       (* scan list of option names *)
    while wrd <> nil do begin
      nxt := wrd^.next;
      if wrd^.sym = starsy then
        if not inherited or (subr_block = nil)
          then wrd := wrd^.defn
          else wrd := nil
      else
        if not inherited
          then wrd := nil;
      if wrd <> nil then
        set_sem_options (wrd, subr_type, subr_block);
      wrd := nxt;
    end;
  end;
 end;
$PAGE block header

procedure block ( this_block: blk; opts: parse_node );

  const class_syms: symbol_set := [externalsy, publicsy, staticsy];
$PAGE declare_labels
(* DECLARE LABELS processes a single <label declaration> whose definition is thus:
   [ <class> ] -> [ <intconst> ]*. The <intconst>s are converted to identifiers
   and entered in the symbol table as symbols of type "labels". *)

procedure declare_labels;
 var
  lab: parse_node;
  labsym: sym;

 begin
  (* Label declarations may not appear in a data module. *)

  if cur_block^.kind = data_blk
    then err_node (err_data_dcl_bad, ptree);
  if cur_block^.kind = root_blk
    then err_node (err_env_dcl_bad, ptree);

  (* Any storage class given is erroneous. *)

  lab := ptree^.defn;
  if (lab <> nil) andif (lab^.sym in class_syms) then begin
    err_node (err_class_bad, lab);
    lab := lab^.next
  end;

  (* Process each label *)

  while lab <> nil do begin

    make_label (lab);                   (* transform <intconst> into identifier *)
    labsym := declare (lab, labels, nil);       (* declare it in the current block;
                                                   the lab_defined bit is set false. *)
    labsym^.lab_declaration := lab^.source;

    lab := lab^.next;
  end;

  get_ptree;
 end;
$PAGE declare_consts
(* DECLARE CONSTS processes a single <const declaration> construct, appearing thus:
   [ <class> ] -> [ <const id decl> ]*; where the const id decl groups have
   this definition: <identifier> -> [ ":" <type decl> ] -> <expression>. *)

procedure declare_consts;
 var
  class, group, id, typedecl, initval: parse_node;      (* hooks into tree *)
  public_flag: boolean;                 (* class information *)
  sclass: storage_class;
  cstsym: sym;
  csttype, given_type: typ;
  cstval: val;

 begin
  (* Get optional storage class information *)

  class := ptree^.defn;
  if class = nil then begin (* Nothing at all *)
    del_tuples;
    get_ptree;
    return;
  end;

  public_flag := false;         (* defaults *)
  sclass := constant_sc;
  group := class^.next;
  case class^.sym of            (* change defaults on basis of <class> *)
    publicsy:   public_flag := true;
    externalsy: sclass := external_sc;
    staticsy:   err_node (err_class_bad, class);
    others:     group := class                  (* no <class>, first node is first group *)
  end;
  if (cur_block^.kind = root_blk) and public_flag
    then err_node (err_env_dcl_bad, class);
  if (cur_block^.level > 1) andif (public_flag or (sclass = external_sc))
    then err_node (err_bad_subr_decl, class);   (* only allow public and external at level 1 *)

  (* Process each <const id decl> *)

  new_chain;
  while group <> nil do begin

    id := group^.defn;                  (* get pieces of decl *)
    if id^.next^.sym = colon            (* have ":" <type decl> *)
      then begin
        typedecl := id^.next^.next;
        initval := typedecl^.next
      end
      else begin                        (* no type, just initial value *)
        typedecl := nil;
        initval := id^.next;
        if public_flag or (sclass = external_sc)        (* must have type *)
          then err_node (err_cst_type, id)
      end;

    if typedecl = nil
      then given_type := nil            (* get required type for const *)
      else if sclass = external_sc
        then given_type := type_semantics (typedecl, [forward_ok])
        else given_type := type_semantics (typedecl, []);

    if initval <> nil then begin        (* determine initial value *)
      if constant (initval, given_type, cstval, csttype) then ;
                                                (* hereafter, csttype is the type *)
      if sclass = external_sc
        then err_node (err_ext_initial, initval);       (* take hard line *)
    end
    else begin                          (* no initial value *)
      csttype := given_type;
      cstval.kind := no_value;
      if sclass <> external_sc
        then err_node (err_no_initial, id)      (* must have value for all but external *)
    end;

    cstsym := declare (id, consts, csttype);    (* declare the name *)
    if cstsym <> nil then begin                 (* nil => previously declared *)
      with cstsym^ do begin
        public_dcl := public_flag;
        dcl_class := sclass;
        init_value := cstval
      end;
      xrf_use (cstsym, id^.source, decl_xrf);
    end;

    group := group^.next;
  end (* while *);
  del_tuples;

  get_ptree;
 end;
$PAGE declare_types
(* DECLARE TYPE processes a single <type declaration> construct, with definition:
   [ <class> ] -> [ <type id decl> ]*; the definition chain of each type decl group
   is: <identifier> -> <type decl>. *)

procedure declare_types;
 var
  group, id, typedesc: parse_node;              (* hooks into definition chains *)
  typeid: sym;
  typenode: typ;

 (* CHK FRWRD TYPE checks if the identifier defines an unknown_type. *)

 function chk_frwrd_type ( name: nam ): boolean;
  begin
   chk_frwrd_type := false;                (* assume not *)
   if (name^.scopechain <> nil) andif (name^.scopechain^.block = cur_block)
     then                               (* previous declared *)
       with name^.scopechain^ do
         if (kind = types) and (type_desc <> nil)       (* is type *)
           then chk_frwrd_type := (type_desc^.kind = unknown_type)
  end;

 (* SET ID sets the type identifier of a type node to the specified symbol
    if the type node does not already have a type name. *)

 procedure set_id (id: sym; tnode: typ);
  begin
   with tnode^ do begin
    if (type_id = nil)                          (* no previous name *)
      orif (type_id^.kind <> types) then begin  (* may happen for scalars, see type semantics *)
        type_id := id;
        if kind = scalars then set_id (id, base_type);
    end;
   end
  end;

 begin
  (* Any storage class information is erroreous.  *)

  group := ptree^.defn;
  if (group <> nil) andif (group^.sym in class_syms) then begin
    err_node (err_class_bad, group);
    group := group^.next
  end;

  (* Process each <type id decl> on the list *)

  new_chain;
  while group <> nil do begin

    id := group^.defn;          (* get nodes of <type id decl> *)
    typedesc := id^.next;

    typenode := type_semantics (typedesc, [flexible_ok, flex_rec_ok, generic_ok]);      (* semanticate the type *)

    (* If type identifier was forward referenced, then name has been entered into
       the symbol table, with a type node marked as "unknown".  Here, we change
       that node into an "indirect" node referencing the actual declared type.
       This is done because we cannot discard either the unknown type node or
       the true type node as both may have outstanding pointers to them. *)

    if chk_frwrd_type (id^.name) then begin
      typeid := id^.name^.scopechain;           (* get old symbol *)
      if typenode <> nil then begin             (* patch unknown type node *)
        set_id (typeid, typenode);              (* set id of new, old node's id already correct *)
        with typeid^.type_desc^ do begin        (* change the node *)
          kind := indirect_type;
          actual_type := typenode;
        end;
      end
    end

    else begin
      typeid := declare (id, types, typenode);          (* create new node *)
      if (typeid <> nil) and (typenode <> nil)          (* if no error *)
        then set_id (typeid, typenode)
    end;

    if typeid <> nil then
      xrf_use (typeid, id^.source, decl_xrf);

    group := group^.next;
  end (* while *) ;
  del_tuples;

  get_ptree;
 end (* declare types *) ;
$PAGE declare_vars
(* DECLARE VARS processes a single <var declaration> construct, arranged thus:
   [ <sclass> ] -> [ <var id decl> ]*;  the definition of <var id decl>, in turn 
   is:  <identifier list> -> <type decl> -> [ <initial value> ].  The definition
   of the id list is a chain of <identifier>s. *)

procedure declare_vars;
 var
   class, group, idlist, id, typedecl, initval: parse_node;
   vartype: typ;
   varval: val;
   public_flag: boolean;
   sclass: storage_class;
   nsym: sym;
   dt: typ;

 begin
  (* Get storage sclass information *)

  class := ptree^.defn;
  if class = nil then begin (* nothing at all *)
    del_tuples;
    get_ptree;
    return;
  end;
  public_flag := false;         (* defaults *)
  sclass := static_sc;
  group := class^.next;
  case class^.sym of
    publicsy:   public_flag := true;
    externalsy: sclass := external_sc;
    staticsy:   ;                               (* defaults are good *)
    others:     begin
                  if cur_block^.level = 1
                    then sclass := static_sc
                    else sclass := local_sc;
                  group := class
                end
  end (* case *) ;
  if (cur_block^.kind = root_blk) and (sclass <> external_sc)
    then err_node (err_env_dcl_bad, ptree);
  if (cur_block^.level > 1) andif (public_flag or (sclass = external_sc))
    then err_node (err_bad_subr_decl, ptree^.defn);     (* public and external allowed only at level 1 *)

  if (cur_block^.kind = data_blk) and (not public_flag)
    then err_node (err_data_dcl_bad, ptree);    (* only public vars in data modules *)


  (* Process each declaration group, i.e. <var id decl> *)

  new_chain;
  while group <> nil do begin

    idlist := group^.defn;                      (* get parts of decl *)
    typedecl := idlist^.next;
    initval := typedecl^.next;

    if sclass = external_sc             (* get type of vars to be declared *)
      then vartype := type_semantics (typedecl, [forward_ok])
      else vartype := type_semantics (typedecl, []);

    varval.kind := no_value;                            (* get initial value, if any *)
    if initval <> nil then begin
      if constant (initval, vartype, varval, dt) then ; (* semanticate initial value *)
      if sclass = external_sc then                      (* cannot have initial value for externals *)
        err_node (err_ext_initial, initval)
      else if sclass = local_sc then                    (* or for locals *)
        err_node (err_loc_initial, initval);
    end;

    id := idlist^.defn;
    while id <> nil do begin
      nsym := declare (id, vars, vartype);
      if nsym <> nil then begin                         (* nil => previously declared *)
        with nsym^ do begin
          public_dcl := public_flag;
          dcl_class := sclass;
          init_value := varval
        end;
        xrf_use (nsym, id^.source, decl_xrf);
      end;
      id := id^.next;                                   (* advance to next id in <id list> *)
    end (* while id *) ;

    group := group^.next;                               (* advance to next dcl group *)
  end (* while *) ;
  del_tuples;

  get_ptree;
 end (* declare_vars *);
$PAGE dcl_conditions
(* DECLARE CONDITIONS processes a single <condition declaration> construct:
   <condition declaration> ::= [ <sclass> ] -> [ <identifier> ]*  *)

procedure dcl_conditions;

 var
   classsy, id: parse_node;
   public_flag: boolean;
   class: storage_class;
   csym: sym;

 begin

  (* Get storage class information. *)

  classsy := ptree^.defn;
  class := static_sc; public_flag := false;   (* defaults *)
  id := classsy^.next;
  case classsy^.sym of
    publicsy:     public_flag := true;
    externalsy:   class := external_sc;
    staticsy:     err_node (err_class_bad, classsy);
    others:       id := classsy     (* there is no <sclass>, backup *)
  end;

  (* Declare each identifier. *)

  while id <> nil do begin
    csym := declare (id, conditions, nil);
    if csym <> nil then begin           (* declaration succeeds *)
      with csym^ do begin       (* fill in addressing info *)
	dcl_class := class;
	public_dcl := public_flag;
      end;
    end;
    id := id^.next;               (* get next identifier *)
  end;

  get_ptree;
 end;
$PAGE subroutine
(* SUBROUTINE processes a subroutine header whose definition chain appears thus:
   [ <class> ] -> "procedure" | "function" -> <identifier> -> [ <parameter list> ] ->
   [ <return type> ] -> [ <subr options> ] -> [ "forward" ].  If the declaration
   is either external or forward, only a definition for the defined symbol is
   created.  Otherwise, block is called to process the following structure. *)

procedure subroutine
  options special(coercions);

 var
   classsy, pfsy, idsym, parmlst, rettype,
     opts, forwardsym, node: parse_node;                (* hooks into definition chain *)
   ssym: sym;                                   (* symbol, type, and block nodes for subr *)
   stype: typ;
   sblock: blk;
   class: storage_class;
   public_flag: boolean;
   psym: sym;
   params: sym_list;


 (* CHK FORWARD checks if the subroutine name is the name of a forward declared
    subroutine in the current block. *)

 function chk_forward ( name: nam ): boolean;
  begin
   chk_forward := false;                        (* assume it is not *)
   if (name <> nil) then                                (* not a dummy id *)
     if (name^.scopechain <> nil) andif (name^.scopechain^.block = cur_block)
       then begin                                       (* same name declared in current block *)
         with name^.scopechain^ (* the symbol *) do begin
           if (kind = consts) andif (init_value.kind = subr_cst)        (* is name of actual subr *)
             then chk_forward := init_value.blorward_dcl
         end
       end
  end;


 begin
  (* Subroutines are only allowed in procedure and module blocks. *)

  if cur_block^.kind = data_blk
    then err_node (err_data_dcl_bad, ptree);

  (* Get storage class information *)

  classsy := ptree^.defn;
  class := constant_sc; public_flag := false;           (* defaults *)
  pfsy := classsy^.next;
  case classsy^.sym of
    publicsy:   public_flag := true;
    externalsy: class := external_sc;
    staticsy:   err_node (err_class_bad, classsy);
    others:     pfsy := classsy
  end;
  if (cur_block^.level > 1) andif (public_flag or (class = external_sc))
    then err_node (err_bad_subr_decl, classsy); (* not permitted except at top level *)
  if (cur_block^.kind = root_blk) and (class <> external_sc)
    then err_node (err_env_dcl_bad, ptree);


  (* Extract interesting portions of definition chain *)

  idsym := pfsy^.next;  node := idsym;
  parmlst := node^.next;
  if (parmlst <> nil) andif (parmlst^.sym = parm_list)  (* check for optional construct *)
    then node := parmlst
    else parmlst := nil;
  rettype := node^.next;
  if (rettype <> nil) andif ((rettype^.sym <> subr_options) and (rettype^.sym <> forwardsy))
    then node := rettype
    else rettype := nil;
  opts := node^.next;
  if (opts <> nil) andif (opts^.sym = subr_options)
    then node^.next := opts^.next (* save the options list *)
    else opts := nil;
  forwardsym := node^.next;


  (* If the subroutine identifier has a prior FORWARD declaration, then we ignore
     the current subroutine header, and take all relevant information from the
     existing symbol and block nodes. *)

  if chk_forward (idsym^.name) then begin
    ssym := idsym^.name^.scopechain;
    with ssym^ do begin
      stype := type_desc;
      sblock := init_value.blkp;
    end;
    sblock^.forward_dcl := false;               (* don't let this happen again *)
    del_ptree (opts); (* use the forward declared options *)
    opts := ptr (ord (sblock^.owner)); (* yes, that's where we hid them *)
  end

  (* The subroutine identifier is *not* a forward declared procedure.  However,
     there are two exceptional cases: (1) the <identifier> was omitted (<= the
     name ptr is nil), and (2) the name is previously declared in the current
     block as other than a forward subroutine.  In either case, we create a
     dummy symbol to which to attach the block and type information. *)

  else begin
    new_chain;
    stype := subr_formal_type (pfsy, parmlst, rettype, (class <> external_sc), params);
                                        (* create the subroutine type, processing the
                                           parameters. Parameter names are required
					   for all but external declarations. *)
    del_tuples;

    ssym := nil;                                (* create a symbol *)
    if idsym^.name <> nil then ssym := declare (idsym, consts, stype);
    if ssym = nil then begin                    (* nil name, or declare fails *)
      ssym := new_sym (consts);         (* create a dummy symbol *)
      ssym^.type_desc := stype;
    end;
    xrf_use (ssym, idsym^.source, decl_xrf);

    with ssym^ do begin                         (* fill in addressing info *)
      dcl_class := class;
      public_dcl := public_flag;
    end;

  if class = external_sc then begin     (* external has no body, hence no block *)
    alloc_type (stype);
    ssym^.init_value.kind := no_value;              (* no block attached *)
    set_subr_options (opts, true, stype, nil);              (* nil block => external *)
    get_ptree;
    return; (* <---- return with external declaration *)
  end;

    sblock := new_blk (subr_blk, cur_block);
    with sblock^ do begin (* Create the new subroutine block. *)
      forward_dcl := (forwardsym <> nil);
      subr_sym := ssym;
      with ssym^.init_value do begin            (* point sym at block, showing non-forward *)
        kind := subr_cst;
        blkp := sblock;
      end;
      declaration := idsym^.source;
      parm_list := params;
    end;
    set_subr_options (opts, true, stype, sblock);       (* non-nil block => actual subr *)
  end (* not forward case *) ;


  (* All done if this is a FORWARD declaration.  We will pick up the body of
     the block later. *)

  if sblock^.forward_dcl then begin
    sblock^.owner := ptr (ord (opts)); (* stow the options list here so we
                                          can retrieve it for the true declaration *)
    get_ptree;
    return;
  end;


  (* Process the body of the block.  Put the parameters in scope, and create
     a dummy symbol for the return value, if this is a function.  Allocate
     the parameter list and the return symbol. *)

  with sblock^ do begin
    psym := parm_list.first;
    while psym <> nil do begin
      with psym^ do begin
        block := sblock;
        if name <> nil then begin
          scopechain := name^.scopechain;
          name^.scopechain := psym
        end;
        psym := next;
      end;
    end;
    if stype^.kind = funcs then begin           (* make a dummy symbol for the return value *)
      dcl_rt_sym (sblock, idsym, stype^.return_type);
      if sblock^.return_sym <> nil then
        xrf_use (sblock^.return_sym, idsym^.source, decl_xrf);
    end;
    alc_subr (stype, parm_list.first, return_sym);
  end (* with sblock^ *) ;

  xrf_block (sblock, pfsy^.source);
  block (sblock, opts);                         (* process body *)
  del_ptree (opts);
  xrf_write (end_xrf);
 end;
$PAGE block - main routine
(* BLOCK processes the body of the main program and subroutines.  It "reads"
   parse trees representing declaration statements and the block's body, and
   dispatches them to various routines for processing. *)

(* procedure block ( this_block: blk; opts: parse_node ); *)

 begin
  (* Establish this block as the current block by setting global pointers. *)

  open_block (this_block);


  (* Process declarations *)

  get_ptree;
  while (ptree <> nil) andif
        ( (ptree^.sym <> beginsy) and (ptree^.sym <> null_stmt) ) do begin
    case ptree^.sym of

      label_declaration:  declare_labels;

      const_declaration:  declare_consts;

      type_declaration:   declare_types;

      var_declaration:    declare_vars;

      cond_declaration:   dcl_conditions;

      subr_decl:          subroutine;

      declaration:        ;      (* returned as placed holder in case of error *)

      others:             get_ptree     (* whatever it is, we don't want it *)

    end;
  end (* while declarations *) ;


  (* Process body of block *)

  case cur_block^.kind of

    subr_blk:
      begin
        set_subr_options (opts, false, cur_block^.subr_sym^.type_desc, cur_block);
        body;                   (* ptree will always be non-nil for subroutines *)
      end;

    module_blk, data_blk:
      if ptree <> nil then begin                        (* body for external procedure compilation ??? *)
        err_node (err_module_has_body, ptree);
        cur_block^.kind := program_blk;         (* assume he means it *)
        set_subr_options (opts, false, nil, cur_block);
        body;
      end;

    program_blk:
      if ptree = nil then begin                 (* no body for main program ??? *)
        error (err_no_prgm_body);
        cur_block^.kind := module_blk
      end
      else begin
        set_subr_options (opts, false, nil, cur_block);
        body;
      end;

    root_blk:
      if ptree <> nil then (* body for ENVIRONMENT ??? *)
        err_node (err_env_has_body, ptree)

   end;

  (* Close the block's scope *)

  if cur_block^.kind <> root_blk then (* NEVER close the root block. *)
    close_block;
 end;
$PAGE semantics
(* SEMANTICS is the master semantication routine.  It creates the top-level
   program block, processes the ptree (if any), and calls "block" to read
   the declarations and (optional) body of the main block. *)

public procedure semantics;

 var
   main: blk;
   id, opts: parse_node;
   mod_ident: string[6];
   module_name: nam;
   module_kind: block_kind;

 begin

  (* Process the compilation ptree.  May either see a <program id>, a <module id>,
     or a <data module id>.  The BNF specifies that a <module id> node will be
     returned if no ptree is actually present.  (There is no module identifier.)
     This conforms to the archaic syntax. *)

  get_ptree;
  opts := nil;
  module_name := nil;
  if (ptree^.sym = program_id) or (ptree^.sym = module_id) or
     (ptree^.sym = datamod_id) or (ptree^.sym = envmod_id) then begin
    case ptree^.sym of
      program_id:       module_kind := program_blk;
      module_id:        module_kind := module_blk;
      datamod_id:       module_kind := data_blk;
      envmod_id:        module_kind := root_blk
    end;
    id := ptree^.defn;                          (* get info out of defn chain *)
    if id <> nil then begin
      if id^.sym = ident then begin             (* have name for compilation *)
        module_name := id^.name;
        opts := id^.next;
        id^.next := nil; (* save the options list *)
      end
      else begin
        opts := id;
        ptree^.defn := nil; (* save the options list *)
      end;
    end;
  end
  else module_kind := module_blk;               (* compilation type not specified; make
                                                   assumption following old convention. *)
  if module_name = nil then begin               (* have no name derive one *)
    mod_ident := substr (file_list^.file_name, index (file_list^.file_name, ':') + 1);
    mod_ident := substr (mod_ident, 1, search (mod_ident, ['.','['], length (mod_ident) + 1) - 1);
    module_name := entername (mod_ident);
  end;

  if module_kind <> root_blk then begin
    env_compilation := false;
    main := new_blk (module_kind, root_block); (* create the module block *)
    main^.comp_dtime := daytime;
    main^.id := module_name;
    ext_block := new_blk (extern_blk, main); (* define the external block *)
  end
  else begin
    env_compilation := true;
    main := root_block;
    env_dtime := daytime;
    env_name := module_name;
  end;

  (* Processing the declarations and body. *)

  if opts <> nil then begin             (* have <subr options> *)
    set_subr_options (opts, true, nil, main);
    prog_options.semantic_options := main^.semantic_options;
    prog_options.dump_switches := main^.dump_switches;
  end;
  xrf_block (main, ptree^.source);
  block (main, opts);
  del_ptree (opts);
  xrf_write (end_xrf);

  while ptree <> nil do
    get_ptree;
 end.
    
è