$TITLE pasutl -- symbol table utility routines
$LENGTH 42

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P A S U T L                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:   This  module  contains  the  symbols  table   utility
        routines.  These  routines  are  used  by  nearly every other
        module  in  the  compiler.  The  symbol  table  structure  is
        specified  in  PASIST.INC,  and  particularly  in PASIST.TYP,
        which it includes.
     
     ENTRY POINTS:
     
        entername   is called with a string containing  a  name,  and
                    returns  a  pointer  to  the  name tree node (NAM
                    node) for that symbol.  There  is  only  one  NAM
                    node for each symbol name, regardless of how many
                    distinct symbols have that name.
     
        cst_int     takes a string containing an unsigned decimal  or
                    octal  integer  number,  and returns a value node
                    (VAL  node)  containing  the   integer   constant
                    represented by the string.
     
        cst_real    is  called  with  a string containing an unsigned
                    real constant and with a is called with a  string
                    containing   an   unsigned   real  number  and  a
                    precision specification, and returns a value node
                    (VAL node) containing the real number represented
                    by  the  string,  as  a  real  constant  of   the
                    specified precision.
     
        cst_string  takes  a  string,  and  returns a value node (VAL
                    node)  containing  that  string   as   a   string
                    constant.
     
        cst_nil     returns  a value node (VAL node) of pointer type,
                    which  by  definition  represents   the   pointer
                    constant NIL.
     
        new_blk     returns  a  new  scope  block  (BLK  node)  of  a
                    specified  kind  (program,  module,   datamodule,
                    subroutine,  or  handler)  and  with  a specified
                    parent.
     
        new_sym     returns  a  new  symbol  node  (SYM  node)  of  a
                    specified  kind.  The node is not linked into any
                    of the symbol table data structures yet.
     
        new_type    returns a new type node (TYP node) of a specified
                    kind.  The  node  is  not  linked into any of the
                    symbol table data structures yet.
     
        dcl_subrange
                    takes a scalar type node and a pair of  integers,
                    and  returns  a  new  type node of subrange kind,
                    with the specified  base  type  and  minimum  and
                    maximum values.
     
        dcl_int     returns  a new type node, representing a subrange
                    of  type  INTEGER  with  specified  minimum   and
                    maximum values.
     
        dcl_char    returns  a new type node, representing a subrange
                    of type CHAR with specified minimum  and  maximum
                    values.
     
        dcl_real    returns  a  new  type  node  representing  a real
                    subrange with specified  minimum  value,  maximum
                    value, and precision.
     
        dcl_set     returns  a new type node, representing a set type
                    with a specified element type.
     
        dcl_pointer returns a new type node, representing  a  pointer
                    type to a specified target type.
     
        dcl_string  returns  a  new  type node, representing a string
                    type with a specified length and kind (varying or
                    nonvarying).
     
        dcl_file    returns a new type node, representing a file type
                    with a specified binary attribute  and  component
                    type.
     
        dcl_aggregate
     
        dcl_array   returns  a  new  type node, representing an array
                    type which is the same  as  some  flexible  array
                    type, but with a specified upper bound.
     
        chain_sym   adds  a  symbol  to  a  symbol chain, such as the
                    chains which are hung off the scope block.
     
        pop_scope   removes a symbol  and  all  the  symbols  on  its
                    sym^.next   chain  from  the  current  scope,  by
                    splicing them off of the scope chains from  their
                    name nodes.  Pop_scope is used at block exit, and
                    at the conclusion of "with" statements.
     
        declare     causes a symbol to be  declared  in  the  current
                    block,  with  a  specified  symbol  kind and type
                    node.  Declare also checks for  duplicate  symbol
                    declarations in a block.
     
        dcl_rt_sym  creates  the  dummy  symbol node which represents
                    the return  value  temporary  in  a  function  or
                    condition   handler  definition.  The  symbol  is
                    built and added to the symbol table as though  it
                    had been declared.
     
        make_label  takes  a parse node for a label identifier (which
                    is of course  an  integer).  It  creates  a  name
                    table   node  for  the  label  (by  entering  the
                    appropriate digit string in the name  table)  and
                    returns a label symbol with that name.
     
     CHANGES:
	                added   makearray  and  makerecord  routines;
                        modified all value node creation routines  to
                        add  value  nodes  to  value  chain; modified
                        new_block  and  new_sym  to  initialize   new
                        fields (see CIN-#2).
                        The    makearray,   makerecord,   makestring,
                        makeset, and cst_scalar routines  were  split
                        out  of  PASUTL  into  the new module PASVAL,
                        since these routines are used in  the  second
                        pass  (by  the  constant  expression  folding
                        code), while the rest of PASUTL is only  used
                        in the first pass.
     
     ---------------------------------------------------------------- *)

$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasval.inc
$INCLUDE paserr.inc
$PAGE entername
(* ENTERNAME searches the name table for a node whose text field matches
   the name parameter.  If one is found, a pointer to it is returned.
   Otherwise, a new name table node is created and inserted at the appropriate
   place in the tree, and a pointer to it is returned.  *)

public function entername ( name: line_string ): nam;

  function add_name: nam;                       (* this function returns a new name table *)
  begin (* node, containing name. *)
    new (add_name, length(name));                        (* create the node *)
    with add_name^ do begin                     (* initialize its fields *)
      alink := nil;
      zlink := nil;
      scopechain := nil;
      visited := false;
      file_loc := 0;
      text := name;
    end;
  end (* add_name *);

begin
  if root_name = nil then begin
    root_name := add_name;
    entername := root_name;
    return;
  end;

  entername := root_name;                       (* start at the top of the tree *)
  while name <> entername^.text do begin
    with entername^ do begin
      if name < text then begin		(* follow "<" link *)
	if alink = nil then begin
	  alink := add_name;
	  entername := alink;
	  return;
	end;
	entername := alink;
      end
      else begin                                (* follow ">" link *)
	if zlink = nil then begin
	  zlink := add_name;
	  entername := zlink;
	  return;
	end;
	entername := zlink;
      end;
    end;
  end (* while name <> entername^.text *);
end (* entername *);
$PAGE cst_int
(* CST_INT converts a decimal or octal integer from source string form to
   an int_type value, and then calls CST_SCALAR to add it to the scalar
   constant table.  *)

public function cst_int ( int_str: line_string; radix: int_type; lindex: line_index ): val;

var
    int_lim, int_rem: machine_word;
    int_val: machine_word;                          (* computed integer value *)
    digit: 0 .. 22;                              (* next digit from string *)
    dig_ind: line_index;                        (* to step through the digits *)

label 100;

begin
  int_lim := maximum (machine_word) div radix;
  int_rem := maximum (machine_word) mod radix;

  int_val := 0;
  for dig_ind := 1 to length (int_str) do begin
    digit := ord(int_str[dig_ind]) - ord('0');  (* get the next digit *)
    if digit > 9 then (* 'A'..'F' --radix down *)
      digit := digit - 7; (* ord('A') - ord('9') + 1 = 7 *)

    if (int_val > int_lim) or ( (int_val = int_lim) and (digit > int_rem) ) then begin
      err_text (err_cst_too_large, int_str);
      goto 100; (* <---- constant too large, use default *)
    end;

    if digit >= radix then begin
      err_print (err_digit_bad, cur_source, '', lindex + dig_ind - 1);
      goto 100; (* <---- invalid octal/decimal digit, use default value *)
    end;

    int_val := (int_val * radix) + digit;
  end;

  if ( (int_val > type_int^.maxval) and
       not (sp_wor_opt in cur_block^.semantic_options) ) or
     (int_val > type_fullword^.maxval) then begin
    err_text (err_cst_special, int_str);
    goto 100; (* <---- constant > 32 bits, use default *)
  end;

  cst_int := cst_scalar (int_val);
  return; (* <---- return with normal constant *)

100: (* <---- error, use default value *)

  cst_int := cst_scalar (type_int^.maxval);
end (* cst_int *);
$PAGE cst_real
(*  CST_REAL returns a real constant table node containing the
    specified real value.  *)


public function cst_real ( rtext: line_string; rprec: prec_type ): val;

var r: real_type;

begin
  getstring (rtext, r);
  if iostatus <> io_ok then
    err_text (err_bad_rl_const, rtext);
  cst_real := mkreal (r, rprec)
end (* cst_real *);
$PAGE cst_string & cst_nil
(*  CST_STRING returns a string constant table node which contains the
    specified text string.  *)

public function cst_string ( str: line_string ): val;

begin
  cst_string := makestring(length(str));
  cst_string.valp^.str_val := str;
end (* cst_string *);



(*  CST_NIL returns a pointer constant table node for the constant NIL.  *)

public function cst_nil: val;

begin
  cst_nil.kind := ptr_cst;
end (* cst_nil *);
$PAGE new_blk
(*  NEW_BLK returns a pointer to a new block node of the specified kind and
    with the specified parent.  All other pointers in the block are initialized
    to nil, except for the level number, which is computed from the level number
    of the parent block, and the block number.  *)

public function new_blk ( kind: block_kind; parent_ptr: blk ): blk;

begin
  case kind of

    root_blk:
        new (new_blk,root_blk);

    program_blk:
      begin
        new (new_blk,program_blk);
        new_blk^.id := nil;
      end;

    module_blk:
      begin
        new (new_blk,module_blk);
        new_blk^.id := nil;
      end;

    data_blk:
      begin
        new (new_blk, data_blk);
        new_blk^.id := nil;
      end;

    subr_blk:
      begin
        new (new_blk, subr_blk);
        new_blk^.subr_sym := nil;
      end;

    class_blk:
        new (new_blk, class_blk);

    extern_blk:
        new (new_blk, extern_blk)

  end (* case kind *);

  with new_blk^ do begin
    parent := parent_ptr;
    peer := nil;
    children := nil;
    visited := false;
    file_loc := 0;
    if parent = nil then
      begin
        level := 0;
        semantic_options := [];
        dump_switches := nil;
        if kind <> root_blk then begin (* Add a class block at level 0. *)
          peer := root_block^.peer;
          root_block^.peer := new_blk;
        end
        else			(* This is THE root block. *)
	  max_level := 0
      end
    else
      begin
        level := parent^.level + 1;
        semantic_options := parent^.semantic_options;
        dump_switches := parent^.dump_switches;
        peer := parent^.children; (* The chain is built in reverse order here. *)
        parent^.children := new_blk; (* It is inverted by CloseBlock in PASBOD. *)
      end;
    if level > max_level then
      max_level := level;
    apparent_level := level;
    max_call_level := 0;
    if kind = root_blk
      then blk_number := 0
      else blk_number := blk_number + 1;
    number := blk_number;
    recursive := false;
    class_bound := false;
    return_sym := nil;
    parm_list.first  := nil; parm_list.last  := nil;
    label_list.first := nil; label_list.last := nil;
    type_list.first  := nil; type_list.last  := nil;
    id_list.first    := nil; id_list.last    := nil;
    owner := nil;
    calls := nil;
    downward_call_thread := nil;
    upward_call_thread := nil;
    lex_thread := nil;
    pos_local_size := 0;
    neg_local_size := 0;
    pos_stk_begin := 0;
    neg_stk_begin := 0;
    pos_stk_end := 0;
    neg_stk_end := 0;
  end;
end (* new_blk *);
$PAGE new_sym
(* NEW_SYM returns a symbol node of the specified type, which is not
   linked into any of the symbol table data structures yet.  *)

public function new_sym ( kind: sym_kind ): sym;

var vl_temp: vl_link;

begin
  case kind of

    labels:
      begin
        new (new_sym,labels);
        with new_sym^ do begin
          lab_defined := false;
          lab_nonlocal_use := false;
        end;
      end;

    types :
      new (new_sym,types);

    fields:
      begin
        new (new_sym,fields);
        with new_sym^ do begin
          fld_record := nil;
          fld_variant := nil;
          fld_offset := 0;
          fld_width := 0;
        end;
      end;

    consts:
      new (new_sym,consts);

    values:
      new (new_sym,values);

    vars  :
      new (new_sym,vars);

    for_inds:
      new (new_sym, for_inds);

    conditions:
      new (new_sym, conditions);

    std_procs:
        new (new_sym,std_procs);

    std_funcs:
        new (new_sym,std_funcs)

  end (* case kind *);

  with new_sym^ do begin
    name := nil;
    block := nil;
    next := nil;
    scopechain := nil;
    type_desc := nil;
    visited := false;
    file_loc := 0;

    if kind in [vars, labels] then begin
      sym_vl_number := sym_vl_number + 1;
      id_number := sym_vl_number;
      new (vl_temp);
      with vl_temp^ do begin
        last := vl_list;
        symbol := new_sym;
      end;
      vl_list := vl_temp;
    end
    else begin
      sym_nvl_number := sym_nvl_number + 1;
      id_number := sym_nvl_number;
    end;

    if kind in [consts, values, vars, for_inds, conditions] then begin
      abnormal_use := false;
      non_local_use := false;
      parm_use := false;
      allocated := false;
      standard := false;
      maskable := false;
      item_addr := 0;
      init_value.kind := no_value;
    end;
  end;
end (* new_sym *);
$PAGE new_type
(* NEW_TYPE returns a type node of the specified kind, which is not
   linked into any of the symbol table data structures yet.  *)

public function new_type ( kind: type_kind ): typ;

begin
  case kind of
    bools:      new (new_type,bools);
    ints:       new (new_type,ints);
    reals:      new (new_type,reals);
    chars:      new (new_type,chars);
    scalars:    new (new_type,scalars);
    sets:       new (new_type,sets);
    pointers:   new (new_type,pointers);
    arrays:     new (new_type,arrays);
    files:      new (new_type,files);
    strings:    new (new_type,strings);
    records:    new (new_type,records,records);
    variants:   new (new_type,variants,variants);
    tags:       new (new_type,tags);
    unknown_type:  new (new_type, unknown_type);
    indirect_type: new (new_type, indirect_type)
  end (* case kind *);

  with new_type^ do begin
    base_size := 0;
    type_id := nil;
    visited := false;
    file_loc := 0;
    packable := false;
    flexible := false;
    generic := false;
    case kind of

      ints,
      chars:
          base_type := nil;

      bools,
      scalars:
        begin
          base_type := nil;
          cst_list.first := nil;
          cst_list.last := nil;
        end;

      sets:
          set_element_type := nil;

      pointers:
          begin
            target_type := nil;
            heap_class := nil;
          end;

      arrays:
        begin
          element_size := 0;
          element_type := nil;
          index_type := nil;
        end;

      files:
          component_type := nil;

      records:
        begin
          field_list := nil;
          variant_tag := nil;
        end;

      variants:
        begin
          field_list := nil;
          variant_tag := nil;
          tag := nil;
          next_variant := nil;
        end;

      tags:
        begin
          tag_field := nil;
          tag_type := nil;
          tag_recvar := nil;
          first_variant := nil;
        end;

      unknown_type, indirect_type:
        actual_type := nil

    end (* case kind *);
  end (* with new_type *);
end (* new_type *);
$PAGE new_proc
(*  NEW PROC returns a procedure or function type node, with a specified
    number of parameter typts.  *)

public function new_proc ( kind: type_kind; parm_count: parm_range ): typ;

var i: parm_range;

begin
  case kind of
    procs:  new (new_proc, procs, parm_count);
    funcs:  new (new_proc, funcs, parm_count)
  end;

  with new_proc^ do begin
    type_id := nil;
    base_size := 0;
    packable := false;
    flexible := false;
    generic := false;
    parmlist_size := 0;
    return_type := nil;
    class_block := nil;
    for i := 1 to parm_count do
      params[i].parm_type := nil;
  end;
end (* new_proc *);
$PAGE dcl_subrange, dcl_int, & dcl_char
(*  DCL_SUBRANGE returns a pointer to a type node for a subrange of the
    specified element type, between the specified minimum and maximum
    values.  *)

public function dcl_subrange ( base: typ; min,max: int_type ): typ;

begin
  dcl_subrange := new_type(base^.kind);
  with dcl_subrange^ do begin
    base_type := base;                          (* base type of scalars (in the general sense) is
                                                   always given as the maximal subrange.  assumes
                                                   that the maximal range is passed to us as "base" *)
    minval := min;
    maxval := max;
  end;
end (* dcl_subrange *);



(*  DCL_INT returns a pointer to an integer subrange type node with
    the specified minimum and maximum elements.  *)

public function dcl_int ( min,max: int_type ): typ;

begin
   dcl_int := dcl_subrange(type_int,min,max);
end (* dcl_int *);



(*  DCL_CHAR returns a pointer to a type node for a character subrange
   with the specified minimum and maximum values.  *)

public function dcl_char ( min,max: int_type ): typ;

begin
  dcl_char := dcl_subrange(type_char,min,max);
end (* dcl_char *);
$PAGE dcl_real
(* DCL REAL creates a real subrange type. *)

public function dcl_real ( min, max: real_type; precis: prec_type ): typ;
begin
  dcl_real := new_type (reals);
  with dcl_real^ do begin
    rminval := min;
    rmaxval := max;
    precision := precis;
  end;
end;
$PAGE dcl_set, dcl_pointer, & dcl_string
(*  DCL_SET returns a pointer to a type node for a set of the specified
    element type.  *)

public function dcl_set ( base: typ ): typ;

begin
  dcl_set := new_type(sets);
  dcl_set^.set_element_type := base;
end (* dcl_set *);



(*  DCL_POINTER returns a pointer to a pointer type node with the
    specified target type.  *)

public function dcl_pointer ( target: typ ): typ;

begin
  dcl_pointer := new_type(pointers);
  dcl_pointer^.target_type := target;
end (* dcl_pointer *);



(*  DCL_STRING returns a pointer to a type node for a string of the
    specified length and kind.  *)

public function dcl_string ( length: char_range; skind: string_kind; flex: boolean ): typ;

begin
  dcl_string := new_type(strings);
  with dcl_string^ do begin
    str_kind := skind;
    str_length := length;
    flexible := flex;
  end;
end (* dcl_string *);
$PAGE dcl_file
(*  DCL_FILE returns a pointer to a file type node with the specified component
    type and file kind.  *)

public function dcl_file ( kind: file_modes; component: typ ): typ;

begin
  dcl_file := new_type (files);
  dcl_file^.file_kind := kind;
  dcl_file^.component_type := component;
end (* dcl_file *);
$PAGE dcl_array
(*  DCL_ARRAY takes a flexible or generic array type and an integer array
    size, and returns an array type node for an array with the specified
    size which is compatible with the specified type.  *)

public function dcl_array ( flex_array: typ; array_size: int_type ): typ;

begin
  dcl_array := new_type (arrays);
  with dcl_array^ do begin
    type_id := flex_array^.type_id;
    packable := flex_array^.packable;
    flexible := false;
    generic := false;
    element_size := flex_array^.element_size;
    element_type := flex_array^.element_type;
    if flex_array^.generic then
      index_type := dcl_subrange (flex_array^.index_type^.base_type,
                                  1,
                                  array_size)
    else
      index_type := dcl_subrange (flex_array^.index_type^.base_type,
                                  flex_array^.index_type^.minval,
                                  array_size + flex_array^.index_type^.minval - 1);
  end;
end (* dcl_array *);
$PAGE chain_sym, pop_scope
(* CHAIN_SYM adds a symbol to a symbol chain. *)

public procedure chain_sym ( var list: sym_list; nsym: sym );
 begin
  if list.last = nil
    then list.first := nsym
    else list.last^.next := nsym;
  list.last := nsym;
 end;


(* POP_SCOPE removes all symbols on a sym^.next chained list from scope,
   by splicing them off of their name's scopechain. *)

public procedure pop_scope ( fsym: sym );
 var s: sym;
 begin
  s := fsym;
  while s <> nil do begin
    with s^ do begin
      if name <> nil
        then name^.scopechain := scopechain;
      s := next;
    end
  end
 end;
$PAGE declare
(* declare enters a symbol declaration for an identifier into the current block.
   the identifier is described by an ident parse node, and the desired kind of
   symbol. this checks if the symbol is previously declared in the block, and if
   so errors and returns nil.  otherwise, it returns a pointer to a completed
   symbol node. *)

public function declare ( id: parse_node; skind: sym_kind; stype: typ ): sym;
 var ns: sym;
 begin
  with id^ do begin
    if (name^.scopechain <> nil) andif (name^.scopechain^.block = cur_block)
      then begin                                (* is dcl'ed, and in current block *)
        err_node (err_already_dcled, id);
        declare := nil;
        return
      end;
  end;

  ns := new_sym (skind);                        (* create the symbol *)
  with ns^ do begin                             (* fill in the symbol *)
    name := id^.name;
    block := cur_block;
    type_desc := stype;
    with id^ do begin                           (* thread onto name chain *)
      scopechain := name^.scopechain;
      name^.scopechain := ns
    end;
  end;
  case skind of
    types: chain_sym (cur_block^.type_list, ns);    (* add to block's list *)
    labels: chain_sym (cur_block^.label_list, ns);
    others: chain_sym (cur_block^.id_list, ns)
  end;
  declare := ns;
 end;
$PAGE dcl_rt_sym
(* DCL RT SYM is used to declare the dummy symbol which represents the return
   value temporary in a function or condition handler (with a return value). This
   creates the symbol, and chains it in appropriately.  It is assumed that the
   parameters to the block (if any) have been put in scope. *)

public procedure dcl_rt_sym
          ( rsblock: blk; rsid: parse_node; rstype: typ );
 begin
   with rsblock^ do begin
    if rsid^.name <> nil then begin
      with rsid^.name^ do begin                 (* check for a name conflict with parm *)
        if (scopechain <> nil) andif (scopechain^.block = rsblock)
          then err_node (err_func_parm_conflict, rsid)
          else begin                            (* okay to generate symbol *)
            return_sym := new_sym (vars);
            with return_sym^ do begin
              name := rsid^.name;
              scopechain := name^.scopechain;   (* put name in scope *)
              name^.scopechain := return_sym;
              block := rsblock;
              dcl_class := parameter_sc;
              public_dcl := false;
              type_desc := rstype;
              init_value.kind := no_value
            end
          end
      end
    end
   end (* with rsblock^ *) ;
 end;
$PAGE int_string
(* int string converts a positive integer value into a string.  it is used
   by make_label to make a name for a <intconst> label. *)

function int_string (int: int_type): line_string;
 var
   i: int_type;
   buffer: packed array[1..line_length] of char;
   bidx: 0..line_length;

 begin
  i := int;
  bidx := line_length;                          (* number of characters in buffer *)
  repeat
    buffer [bidx] := char (ord ('0') + i mod 10);
    i := i div 10;
    bidx := bidx - 1;
  until i = 0;
  int_string := substr (buffer, bidx+1);        (* return converted number *)
 end;
$PAGE make_label
(* MAKE LABEL enters a label identifier, an <intconst>, into the name
   table and transforms the constant token into an identifier token *)

public procedure make_label ( lab: parse_node );
 var labname: nam;
 begin
  if lab^.sym = intconst then begin             (* just in case we ever allow id's as labels, too *)
    with lab^ do begin
      labname := entername (int_string (value.ival));   (* create a name for the label *)
      sym := ident;
      name := labname
    end
  end
 end.
  W g\