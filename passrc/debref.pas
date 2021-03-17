$TITLE DEBREF - Pascal Debugger reference routines
  
$HEADER debref

module debre$  options nocheck, special (coercions, word, ptr);
  
$INCLUDE debug.typ
  
$INCLUDE debug.inc
$INCLUDE debsym.inc
$INCLUDE debscp.inc
$INCLUDE debasm.inc
$INCLUDE deblib.inc
$INCLUDE deblex.inc
$INCLUDE debio.inc

const
  max_parm_reg_size := 6; (* max parameter list size which will
						   be passed in registers *)
type
  indirect_word_ptr = ^packed record
    filler: half_word;
    address: half_word
  end;
$PAGE ext$scalar
(* EXT$SCALAR extracts an integer from a descriptor for some scalar type.  *)

public function ext$scalar (    desc: descriptor;
			    var status: status_code): machine_word;

  begin
    with desc do begin 
      if not addr.is_packed then
	ext$scalar := int_type_ptr (addr.wordoffset)^
      else
	ld$byte (addr.wordoffset, addr.bitoffset, dtype.size, ext$scalar);

      (* The compiler may store packed negative integers in halfwords.
	 Check for this and if negative halfword found then sign extend
	 value extracted. *)

      if (dkind = int_dt) andif			(* integer value *)
	 (dtype.size = (bits_per_addressible_unit div 2)) andif	(* stored in 18 bits *)
	 (dtype.minval < 0) andif		(* lower subrange limit is negative *)
	 (ext$scalar >= 400000b) then		(* value is negative *)
	ext$scalar := ext$scalar - 1000000b;
      if (user_const and (dkind = int_dt)) andif (status_in_given_radix <> success) then
	status := status_in_given_radix
    end
  end (* ext$scalar *);
$PAGE sto$scalar
(* STO$SCALAR stores an integer into an object described via a
   descriptor.  If the object is less than a full word in size then
   the value is assumed to be right justified within parameter VALUE.  *)

public procedure sto$scalar (    desc:	descriptor;
				 value:	machine_word);

  begin
    with desc.addr do
      if not is_packed then
	int_type_ptr (wordoffset)^ := value
      else
	st$byte (wordoffset, bitoffset, desc.dtype.size, value)
  end;
$PAGE ext$address
(* EXT$ADDRESS extracts an integer (intended to be used as an address) from
   a descriptor for an integer or pointer.  If it was a user-entered
   constant without an explicit radix, the value as interpreted in the
   machine's address radix is used. *)

public function ext$address (    desc: descriptor;
			     var status: status_code): unit_range;

  begin
    a$$ert (desc.dkind in [pointer_dt, file_dt, subr_dt, int_dt (* constant *)]);
    if not desc.user_const then
      ext$address := ext$scalar (desc, status)
    else begin
      a$$ert (desc.dkind = int_dt);
      ext$address := desc.address_value;
      if desc.status_in_addr_radix <> success then
	status := desc.status_in_addr_radix
    end
  end (* ext$address *);
$PAGE ext$setelem
(* EXT$SETELEM extracts an element from a descriptor for a set. *)
 
public function ext$setelem (    desc: descriptor;
				 elem: set_range): boolean;

  type
    set_overlay_ptr = ^packed array [0..32767] of boolean;

  var
    offset: int_type;

  begin
    with desc do
      if (elem >= set_min) and (elem <= set_max) then begin
	offset := elem - set_min;
	if addr.is_packed then
	  offset := offset + addr.bitoffset;
	ext$setelem := set_overlay_ptr (addr.wordoffset)^ [offset]
      end
      else
	ext$setelem := false
  end (* ext$setelem *);
$PAGE sto$setelem
(* STO$SETELEM stores an element into a set described by a descriptor. *)
 
public procedure sto$setelem (    desc: descriptor;
				  elem: set_range;
				  elem_val: boolean);

  type
    set_overlay_ptr = ^packed array [0..32767] of boolean;

  var
    offset: int_type;

  begin
    with desc do
      if (elem >= set_min) and (elem <= set_max) then begin
	offset := elem - set_min;
	if addr.is_packed then
	  offset := offset + addr.bitoffset;
	set_overlay_ptr (addr.wordoffset)^ [offset] := elem_val
      end
  end (* sto$setelem *);
$PAGE ext$real
(* EXT$REAL extracts a real number from a descriptor record for a real.  *)

public function ext$real (    desc:	descriptor;
			  var status:	status_code): real_type;

  type
    real_ptr =	   ^real_type;
    single_real_ptr = ^minimum(real_type)..maximum(real_type) prec single_real_prec;

  begin
    if desc.dkind = real_dt then
      if desc.dtype.precision > single_real_prec then
	ext$real := real_ptr (desc.addr.wordoffset)^
      else
	ext$real := single_real_ptr (desc.addr.wordoffset)^ (* implicitly convert to double precision *)
    else	(* integer -> real conversion required *)
      ext$real := ext$scalar (desc, status)
  end (* ext$real *);
$PAGE ext$lenstr, sto$lenstr
(* EXT$LENSTR extracts the character length of a string given a descriptor. *)

public function ext$lenstr (    desc: descriptor): char_range;

  begin
    if desc.dkind = char_dt then
      ext$lenstr := 1
    else if desc.dkind = string_dt then begin
      if desc.dtype.str_kind = varying then
	ext$lenstr := int_type_ptr (desc.addr.wordoffset)^
      else
	ext$lenstr := desc.dtype.str_length
    end
    else if desc.dkind = substr_dt then
      ext$lenstr := desc.substr_len
    else
      a$$ert (false)
  end (* ext$lenstr *);



(* STO$LENSTR stores the length word of a varying string given a descriptor for
   the string.  *)

public procedure sto$lenstr (    desc: descriptor;
				 len: char_range);

  begin
    a$$ert (desc.dkind = string_dt);
    a$$ert (desc.dtype.str_kind = varying);
    sto$scalar (desc, len)
  end (* sto$lenstr *);
$PAGE a$$backwards
(* A$$BACKWARDS decides if a string assignment should be carried out backwards to
   avoid incorrect results (as can happen when there is overlap, so that changing
   a left hand side character alters a character still to be extracted from the
   right hand side).  *)

public function a$$backwards (    lhs_desc, rhs_desc: descriptor): boolean;

  function bit_address (desc: descriptor): bit_range;
    var
      i, remaining: char_range;
    begin
      bit_address := desc.addr.wordoffset * bits_per_addressible_unit;
      if desc.addr.is_packed then
	bit_address := bit_address + desc.addr.bitoffset;
      if desc.dtype.str_kind = varying then
	bit_address := bit_address + str_lword_size;
      if desc.dkind = substr_dt then
	for i := 1 to desc.substr_start - 1 do begin
	  bit_address := bit_address + char_size;
	  remaining := bits_per_addressible_unit - bit_address mod bits_per_addressible_unit;
	  if remaining < char_size then
	    bit_address := bit_address + remaining
	end;
    end;

  begin
    a$$ert (lhs_desc.dkind in [string_dt, substr_dt]);
    a$$ert (rhs_desc.dkind in [string_dt, substr_dt, char_dt]);
    if rhs_desc.dkind = char_dt then
      a$$backwards := false
    else
      a$$backwards := bit_address (rhs_desc) < bit_address (lhs_desc)
  end;
$PAGE ext$strchar
(* EXT$STRCHAR extracts a character from a string given a descriptor for
   the (sub)string and the index of the desired character relative to the
   beginning of the (sub)string.  *)

public function ext$strchar (    desc: descriptor;
				 relative_index: char_range): char;

  type
    string_overlay_ptr = ^packed array [1..maximum (char_range)] of char;

  var
    status: status_code;
    str_ptr: string_overlay_ptr;
    offset: char_range;

  begin
    a$$ert (desc.dkind in [char_dt, string_dt, substr_dt]);
    if desc.dkind = char_dt then begin
      status := success;
      ext$strchar := chr (ext$scalar (desc, status));
      a$$ert (status = success)
    end
    else with desc do begin
      if dtype.str_kind = nonvarying then begin
	str_ptr := string_overlay_ptr (addr.wordoffset);
	if addr.is_packed then
	  offset := (addr.bitoffset div char_size) + 1
	else
	  offset := 1
      end
      else begin
	str_ptr := string_overlay_ptr (addr.wordoffset
					      + str_lword_size div bits_per_addressible_unit);
	offset := 1
      end;
      if dkind = substr_dt then
	offset := offset + substr_start - 1;
      ext$strchar := str_ptr^[offset + relative_index - 1]
    end
  end (* ext$strchar *);
$PAGE sto$strchar
(* STO$STRCHAR stores a character into a string given a descriptor for the string. *)

public procedure sto$strchar (    desc: descriptor;
				  relative_index: char_range;
				  ch: char);

  type
    string_overlay_ptr = ^packed array [1..maximum (char_range)] of char;

  var
    str_ptr: string_overlay_ptr;
    offset: char_range;

  begin
    with desc do begin
      a$$ert (dkind in [string_dt, substr_dt]);
      if dtype.str_kind = nonvarying then begin
	str_ptr := string_overlay_ptr (addr.wordoffset);
	if addr.is_packed then
	  offset := (addr.bitoffset div char_size) + 1
	else
	  offset := 1
      end
      else begin
	str_ptr := string_overlay_ptr (addr.wordoffset
					      + str_lword_size div bits_per_addressible_unit);
	offset := 1
      end;
      if dkind = substr_dt then
	offset := offset + substr_start - 1;
      str_ptr^ [offset + relative_index - 1] := ch
    end
  end (* sto$strchar *);
$PAGE init$desc
(* INIT_DESC initializes a descriptor record.  *)

public procedure init$desc (var desc: descriptor);

  begin
    with desc do begin 
      assignable := true;
      user_const := false;
      type_ptr := ord (nil);
      addr := (ord (nil), false, 0);
      dtype.kind := unknown_type;
      value.kind := no_value;
      dkind := unknown_dt
    end
  end (* init$desc *);
$PAGE sym_list_lookup
(* SYM_LIST_LOOKUP searches a linked list of symbol nodes for a 
   specific identifier.  LIST_HEAD is the symbol node heading the
   list.  ID_TEXT is the text of the identifier.  If found the symbol
   table offset of the symbol node corresponding to the ID is returned.
   Otherwise, ord (nil)  is returned.  The list is assumed linked via
   the NEXT field of the symbol nodes; the list must be terminated by
   a nil NEXT field.  *)

function sym_list_lookup (list_head: intsym;
			  id_text:   id_string): intsym;

  var
    found:	boolean;
    cur_sym:	sym;
    next_intsym:	intsym;
    cur_text:	id_string;

  begin
    sym_list_lookup := list_head;
    found := false;

    while (not found) and (sym_list_lookup <> ord (nil)) do begin
      cur_sym := deref$ptr (sym_list_lookup);
      next_intsym := cur_sym^.next; (* save next field before DEREF *)
      with nam (deref$ptr (cur_sym^.name))^ do
	cur_text := uppercase (substr (text, 1, len));
      found := cur_text = id_text; (* match? *)
      if not found then
	sym_list_lookup := next_intsym
    end
  end (* sym_list_lookup *);
$PAGE with_lookup
(* WITH_LOOKUP looks up an identifier in the WITH table.  If the ID
   is found, parameter ID_INTSYM is set to the symbol table offset of 
   the field represented by ID_TEXT and parameter ADDR is set to the
   address of the WITHed record.  If the ID is not found, then ID_INTSYM
   will be set to ord (nil).   *)

procedure with_lookup (    id_text:	id_string;
		       var id_intsym:	intsym;
		       var addr:	addr_record);

  var
    level: with_level;

  begin
    id_intsym := ord (nil); (* in case there are no active withs! *)
    with db$base^.with_table do
      for level := active_withs downto 1 do begin
	id_intsym := sym_list_lookup (withs [level].first_field, id_text);
      exit if id_intsym <> ord (nil) do addr := withs [level].rec_base;
      end
  end (* with_lookup *);
$PAGE id_lookup
(* ID_LOOKUP looks up an identifier in the symbol table based on the
   scope of parameter SCOPE_STACK.  If the ID is found, parameter
   ID_INTSYM is set to the offset of its symbol table node; otherwise
   ID_INTSYM is set to ord (nil).  Parameter DISPLAY_REC is set to the
   display record for the scope level at which the ID was found.  If
   found in the root block node, then the display for level 1 is
   returned.  *)

procedure id_lookup (    id_text:	id_string;
			 scope_stack:	scope_type;
		     var id_intsym:	intsym;
		     var display_rec:	display);

  var
    level:	display_lvl_range;
    cur_intblk:	intblk;
    cur_blk:	blk;
    first_param,
    return_val:	intsym;

  begin
    with scope_stack do begin
      level := display_levels; (* start at top - search down to level 0 *)
      id_intsym := ord (nil); (* to indicate not yet found *)

      (* Check one scope level per iteration, starting with the topmost *)

      loop
	if level > 0 then
	  cur_intblk := displays [level].blk_node	(* get block node from scope stack *)
	else
	  cur_intblk := level0blk; (* use root block node, always at offset 0 *)
	cur_blk := deref$ptr (cur_intblk);
	first_param := cur_blk^.parm_list.first; (* Save some fields we may need later (after
						     other DEREFs) *)
	return_val := cur_blk^.return_sym;

	id_intsym := sym_list_lookup (cur_blk^.id_list.first, id_text); (* try local symbols *)
	if id_intsym = ord (nil) then begin	(* if not found, check parameter list *)
	  id_intsym := sym_list_lookup (first_param, id_text);

	  if id_intsym = ord (nil) then	(* still not found, check function return
						     value symbol *)
	    id_intsym := sym_list_lookup (return_val, id_text)
	end;

      exit if (level = 0) or (id_intsym <> ord (nil));
	level := level - 1 (* not found - try next lower level *)
      end;

      if level = 0 then
	level := 1; (* found in root block, or not at all *)
      display_rec := displays [level]
    end (* with *);
  end (* id_lookup *);
$PAGE set$field$addr
(* SET$FIELD$ADDR is given the symbol table offset of a symbol node
   for a field and an address record with the base address of the record
   containing the field.  It returns an address record containing the
   address of the field. *)

public function set$field$addr (    field_intsym: intsym;
				    addr:         addr_record): addr_record;

  begin
    set$field$addr := addr;
    with sym (deref$ptr (field_intsym))^, set$field$addr do begin
      wordoffset := wordoffset + fld_offset div bits_per_addressible_unit;
      if is_packed then
	bitoffset := bitoffset + fld_offset mod bits_per_addressible_unit
      else
	bitoffset := fld_offset mod bits_per_addressible_unit;
      is_packed := (bitoffset <> 0) or (fld_width < bits_per_addressible_unit)
    end
  end (* set$field$addr *);
$PAGE set$kind
 (* SET$KIND is given a (non-dereferenced) pointer to a type node and
    then sets type related information in a descriptor record.  *)

public procedure set$kind (var desc:        descriptor;
			       desc_inttyp: inttyp);

  const
    desc_kind: array [type_kind] of desc_kinds := (
	scalar_dt, scalar_dt, char_dt, int_dt,
	real_dt, set_dt, pointer_dt, file_dt, string_dt,
	array_dt, record_dt, unknown_dt, unknown_dt, subr_dt,
	subr_dt, unknown_dt, unknown_dt);

  var
    desc_typ,
    elem_typ: typ;

  begin
    with desc do begin
      desc_typ := deref$ptr (desc_inttyp);
      dtype := desc_typ^; (* copy the record itself *)
      type_ptr := desc_inttyp; (* copy pointer also *)
      dkind := desc_kind [desc_typ^.kind];

      (* if the type is sets then we save the bounds on the set element
	 type.  This is for compatability with user entered set constants,
	 where the element type is merely the base type of the 'apparent'
	 type of the elements, e.g., INTEGER.  *)

      if dtype.kind = sets then
	if desc_typ^.set_element_type <> ord (nil) then begin
	  elem_typ := deref$ptr (desc_typ^.set_element_type);
	  set_min := elem_typ^.minval;
	  set_max := elem_typ^.maxval
	end
	else begin			(* empty set - no type info *)
	  set_min := 1;
	  set_max := 0
	end;
      a$$ert (dkind <> unknown_dt)
    end
  end (* set$kind *);
$PAGE mak$scalar$desc
(* MAK$SCALAR$DESC creates a descriptor for a given type of scalar
   constant object. *)

public procedure mak$scalar$desc (var desc: descriptor;
				      desc_inttyp: inttyp;
				      scal_val:    machine_word);

  begin
    init$desc (desc); (* fill in with default values *)
    with desc do begin
      assignable := false;
      user_const := true;
      set$kind (desc, desc_inttyp);
      if desc.dkind = int_dt then begin
	desc.address_value := scal_val;
	desc.status_in_given_radix := success;
	desc.status_in_addr_radix := success
      end;
      value.scalar_val [1] := scal_val;
      addr := (ord (address (value.scalar_val [1])), false, 0)
    end
  end (* mak$scalar$desc *);
$PAGE comp$types
(* COMP$TYPES checks to see if two descriptors are value compatible, i.e. the types of the
   objects indicate that the RIGHT object could be assigned to the LEFT object.  This
   does not guarantee such an assignment might not provoke a range check error,
   depending on the value of the RIGHT object at a particular time.  *)

public function comp$types (left:  descriptor;
			    right: descriptor): boolean;


  (* Function COMP permits recursive calls to comp$types by converting
     symbol table offsets to descriptors. *)

  function comp (left_inttyp:  inttyp;
		 right_inttyp: inttyp): boolean;

    var
      left_desc,
      right_desc: descriptor;

    begin
      set$kind (left_desc, left_inttyp);
      set$kind (right_desc, right_inttyp);
      comp := comp$types (left_desc, right_desc)
    end;


  begin
    comp$types := true;
    if left.type_ptr = right.type_ptr then return; (* if same node, return immediately *)

    if left.dkind in [char_dt, string_dt, substr_dt] then begin	(* strings and chars are *)
      comp$types := right.dkind in [char_dt, string_dt, substr_dt]; (* always compatible *)
      return (* <-- return *)
    end;

    (* not identical and not strings or chars *)

    comp$types := (left.dkind = right.dkind) orif
		  ((left.dkind = real_dt) and (right.dkind = int_dt));
    if not comp$types then return;

    case left.dkind of 

      int_dt,				(* Here we only demand that the descriptor *)
      real_dt: ; (* kinds be the same *)

      scalar_dt:
	comp$types := left.dtype.base_type = right.dtype.base_type;

      set_dt:
	comp$types := (right.dtype.set_element_type = ord (nil)) orif	(* empty set on rhs ? *)
	  (left.dtype.set_element_type  (nil)) orif	(* empty set? *)
	  comp (left.dtype.set_element_type,
		 right.dtype.set_element_type);

      pointer_dt:
	with left.dtype do begin
	  if (target_type = ord (nil)) orif	(* NIL or type PTR *)
	     (right.dtype.target_type = ord (nil)) then
	    comp$types := true
	  else begin
	    if typ (deref$ptr (target_type))^.kind = unknown_type then	(* target type may legally be undefined *)
	      comp$types := true
	    else begin
	      if typ (deref$ptr (right.dtype.target_type))^.kind = unknown_type then
		comp$types := true
	      else
		comp$types := comp (target_type, right.dtype.target_type);
	    end
	  end
	end;

      array_dt,				(* we require type nodes to be the same, *)
      record_dt:				(* really irrelevant since record and array *)
	comp$types := false; (* assignments are not permitted *)

      file_dt:
	if left.dtype.component_type = ord (nil)  (* binary files compatible *)
	  then comp$types := right.dtype.component_type = ord (nil)
	else if right.dtype.component_type = ord (nil)  (* only with each other *)
	  then comp$types := left.dtype.component_type = ord (nil)
	else comp$types := comp (left.dtype.component_type,
				  right.dtype.component_type);

      others:				(* includes SUBR_DT and SLICE_DT, for which *)
	comp$types := false			(* assignment is not permitted *)

    end (* case *);
  end (* comp$types *);
$PAGE check$type
(* CHECK$TYPE checks to see if a descriptor is (value) compatible with
   a specified type.  Scalar and real values are subrange checked.  *)

public procedure check$type (    desc:		  descriptor;
				 required_inttyp: inttyp;
			     var status:	  status_code);

  var
    scalar_value: int_type;
    real_value:   real_type;
    test_desc:    descriptor;   

  begin
    with desc do begin

      (* Check if types are compatible *)

      set$kind (test_desc, required_inttyp);
      if not comp$types (test_desc, desc) then
	status := type_incompatability
      else

      (* Range check scalar and real types. *)

	with typ (deref$ptr (required_inttyp))^ do
	  if (kind in [scalars, ints, chars]) and	(* scalars *)
	     not (dkind in [string_dt, substr_dt]) then begin
	    scalar_value := ext$scalar (desc, status);
	    if scalar_value < minval then
	      status := below_range
	    else if scalar_value > maxval then
	      status := above_range;
	  end
	  else if kind = reals then begin			(* reals *)
	    real_value := ext$real (desc, status);
	    if real_value < rminval then
	      status := below_range
	    else if real_value > rmaxval then
	      status := above_range
	  end
    end
  end (* check$type *);
$PAGE index_check
(* INDEX_CHECK checks to see if the type of an array or string
   index is correct and then bounds checks the array or string
   index.  DESC is the array or string descriptor; INDEX_DESC is a
   descriptor for the index.  *)

procedure index_check (    desc:	descriptor;
			   index_desc:	descriptor;
		       var status:	status_code);

  var
    index,
    upper_bound: int_type;

  begin
    with desc do begin
      if not (index_desc.dkind in [int_dt, scalar_dt, char_dt]) then begin
	status := ill_idx_type;
	return (* <-- return *)
      end;
      index := ext$scalar (index_desc, status);

      if dtype.kind = strings then		(* string *)
	if index_desc.dkind <> int_dt then
	  status := ill_idx_type
	else begin
	  if index < 1 then
	    status := below_range
	  else begin				(* index of varying length string is
						     checked against string length word *)
	    if dtype.str_kind = varying then
	      upper_bound := int_type_ptr (addr.wordoffset)^
	    else		(* non-varying - get upperbound from type node *)
	      upper_bound := dtype.str_length;
	    if index > upper_bound then
	      status := above_range
	  end
	end

      else begin				(* array *)
	if not dtype.generic then		(* for generic, just being scalar type is enough *)
	  check$type (index_desc, dtype.index_type, status);
	if not (status in severe) then(* bounds check *)
	  if index < lower_bnd then		(* CHECK$TYPE's bounds check is not sufficient *)
	    status := below_range	(* array is flexible or generic *)
	  else if index > upper_bnd then
	    status := above_range;

      end
    end
  end (* index_check *);
$PAGE get$element$addr
(* GET$ELEMENT$ADDR calculates the address of an array or string element.
   DESC is a descriptor for the array or string.  
   INDEX is the array or string index.  The function return value
   will be set to the address of the specified element.  *)

public function get$element$addr (    desc:  descriptor;
				      index: int_type): addr_record;

  var
    lower_bound:	  int_type;
    elem_size,
    elem_offset:	  bit_range;
    elems_per_word,
    words_per_elem: unit_range;

  begin
    (* Get size of element and lower bound of array. *)

    with desc do
      if dtype.kind = strings then begin		(* strings *)
	lower_bound := 1;
	elem_size := char_size
      end
      else begin					(* array *)
	lower_bound := lower_bnd;
	elem_size := dtype.element_size
      end;

    (* Calculate element address. *)

    elem_offset := index - lower_bound;

      (* Unfortunately we cannot assume that the array address is word
	 aligned if the element size is less than a word.  Non-varying,
	 flexible strings passed as parameters are passed by byte pointer,
	 which shows up here as a packed array address.  *)

    with desc, get$element$addr do
      if elem_size < bits_per_addressible_unit then begin	(* array elements are < 1 word in size *)
	elems_per_word := bits_per_addressible_unit div elem_size;
	wordoffset := addr.wordoffset + (elem_offset div elems_per_word);
	if (dtype.kind = strings) andif (dtype.str_kind = varying) then
	  wordoffset := wordoffset + 1; (* skip string length word *)
	bitoffset := (elem_offset mod elems_per_word) * elem_size;
	if addr.is_packed then
	  bitoffset := bitoffset + addr.bitoffset;
	if bitoffset > bits_per_addressible_unit - elem_size then begin
	  wordoffset := wordoffset + 1;
	  bitoffset := (bitoffset - bits_per_addressible_unit) + (bits_per_addressible_unit mod elem_size)
	end;
	is_packed := true
      end
      else begin				(* array elements >= 1 word in size *)
	words_per_elem := ((elem_size - 1) div bits_per_addressible_unit) + 1;
	wordoffset := addr.wordoffset + elem_offset * words_per_elem;
	is_packed := false
      end

  end (* get$element$addr *);
$PAGE func_sym
(* FUNC_SYM returns true if the symbol node passed in corresponds to
   a function.  Functions have two symbol nodes - one corresponding to
   the function itself and one to the function return value.  This
   routine MUST be passed the symbol node corresponding to the return
   value to work properly.  *)

function func_sym (    id_intsym: intsym): boolean;

  var
    sym_blk: blk;

  begin
    sym_blk := deref$ptr (sym (deref$ptr (id_intsym))^.block); (* if return value symbol node, then this *)
						  (* is functions block node *)
    func_sym := sym_blk^.return_sym = id_intsym; (* if ID_INTSYM is functions RETURN_SYM, then *)
						  (* return true *)
  end;
$PAGE passed_by_addr
(* PASSED_BY_ADDR returns true if the symbol represented by ID_INTSYM
   would be passed by address if used as a value parameter.  ID_INTSYM 
   should be a symbol node for either a value parameter or the 
   'return value' symbol node for a function.  *)

function passed_by_addr (    id_intsym: intsym): boolean;

  var
    id_typ: typ;

  begin
    id_typ := deref$ptr (sym (deref$ptr (id_intsym))^.type_desc);

    if id_typ^.kind in [arrays, strings, records] then	(* if array, string or record, *)
      passed_by_addr := true			(* then always passed by address *)
    else
      passed_by_addr := id_typ^.size > 2 * bits_per_addressible_unit; (* else passed by addr if larger
						     than 2 words *)
  end (* passed_by_addr *);
$PAGE argument_address
(* ARGUMENT_ADDRESS calculates the address of a parameter's argument
   word (i.e., for value parameters, the address of the value; for
   parameters passed by address, the address of the pointer to the
   actual value).  Parameter ID_INTSYM is the symbol node pointer for
   the parameter.  Parameter STACKBASE is the stackframe pointer for
   the stack frame corresponding to the procedure.  The address of the 
   argument word is the function return value.  *)

function argument_address (    id_intsym: intsym;
			       stackbase: ^stack_frame): unit_range;

  var
    id_sym:	   sym;
    id_offset:	   unit_range;
    id_blk:	   blk;
    parm_ptr_offset: unit_range;
    subr_typ:	   typ;

  begin
    id_sym := deref$ptr (id_intsym);
    a$$ert (id_sym^.dcl_class = parameter_sc);
    id_offset := id_sym^.item_addr; (* save offset w/in argument block *)
    id_blk := deref$ptr (id_sym^.block); (* subr's block node *)
    parm_ptr_offset := id_blk^.parm_list_base; (* offset of arg block pointer if *)
						  (* arg block size > 6 words *)
    subr_typ := deref$ptr (sym (deref$ptr (id_blk^.subr_sym))^.type_desc); (* subr's type node from its symbol node *)

    (* If there are more than 6 argument words, then the local frame
       contains a pointer to the argument block; otherwise the argument
       block is in the local frame.  *)

    if subr_typ^.parmlist_size > max_parm_reg_size then begin
      argument_address := ord (stackbase) + parm_ptr_offset;
      argument_address := indirect_word_ptr (argument_address)^.address + id_offset;
    end
    else
      argument_address := ord (stackbase) + id_offset

  end (* argument_address *);
$PAGE get_address
(* GET_ADDRESS is passed a symbol table offset for a symbol of type
   VARS, VALUES or CONSTS and a display record corresponding to the
   lexic level of the symbol.  The address of the symbol is calculated
   and returned in parameter ADDR.  If the symbol node is that of a
   function, it must be the symbol node corresponding to the function
   return value or else STATUS will be set to FUNC_INACTIVE.  Note
   that symbols of KIND FIELDS are not a legal input to this routine.  *)

procedure get_address (    id_intsym:	intsym;
			   display_rec:	display;
		       var addr:	addr_record;
		       var status:	status_code);

  type
    bpointer = packed record
      pbits, sbits: elem_sizes;
      dummybit, ibit: 0..1;
      ireg: 0..17b;
      reladdr: half_word
    end;

  var
    id_sym:	sym;
    id_kind:	sym_kind;
    id_offset:	unit_range;
    id_class:	storage_class;
    id_public:	boolean;
    id_name:	intnam;
    id_inttyp:	inttyp;
    id_typ:	typ;
    id_intblk:	intblk;
    bp_ptr:	^bpointer;
    bptr:	bpointer;
    parm_ptr_offset:	unit_range;
    mod_name:	id_string;
    name_text:	id_string;
    link_entry:	^linkentry;

  begin
    id_sym := deref$ptr (id_intsym); (* get symbol table entry *)

    (* Check that kind of symbol is legitimate.  Note that fields are NOT. *)

    if not (id_sym^.kind in [consts, vars, values]) then begin
      status := ill_sym_type;
      return					(* <-- error return *)
    end;

    with id_sym^ do begin
      id_kind := kind; (* save some info from deref *)
      id_inttyp := type_desc;
      id_offset := item_addr;
      id_class := dcl_class;
      id_public := public_dcl;
      id_name := name;
      id_intblk := block
    end;

    if id_kind = consts then begin		(* procedure symbols are illegal *)
      id_typ := deref$ptr (id_sym^.type_desc);
      if id_typ^.kind = procs then begin
	status := ill_sym_type;
	return					(* <-- error return *)
      end
      else if id_typ^.kind = funcs then begin	(* if function were active, ID_INTSYM would *)
	status := func_inactive; (* be 'return value' symbol node of KIND VARS *)
	return  (* <-- error return *)
      end;
    end;

    (* calc address of symbol based on storage class *)

    with addr, display_rec do begin

      if func_sym (id_intsym) then begin

	(* special case handling of function return values *)

	wordoffset := ord (stackbase) + id_offset;
	if passed_by_addr (id_intsym) then
	  wordoffset := indirect_word_ptr (wordoffset)^.address;
	return (* <-- return *)
      end;

      case id_class of

	local_sc:					(* local variable *)
	  if stackbase = nil then
	    status := routine_inactive
	  else
	    wordoffset := ord (stackbase) + id_offset;

	parameter_sc:
	  begin
	    if stackbase = nil then
	      status := routine_inactive
	    else begin
	      wordoffset := argument_address (id_intsym, stackbase);
						(* get address of arg word *)

	      a$$ert (id_kind in [vars, values]);
	      if (id_kind = vars) orif		(* must indirect, if var param or *)
		 passed_by_addr (id_intsym) then begin	(* value param passed by address *)

		(* The following hack handles non_varying, flexible, string
		   value parameters.  For such creatures, a byte pointer is 
		   passed rather than an address.  Here we convert the byte
		   pointer to a packed address.  Note that the byte pointer
		   points to the byte before the first character of the string. *)

		id_typ := deref$ptr (id_inttyp);
		if (id_kind = values) andif		(* value parameter *)
		   (id_typ^.kind = strings) andif	(* of type string *)
		   (id_typ^.str_kind = nonvarying) andif	(* nonvarying, and *)
		   (id_typ^.flexible)	then begin	(* flexible ! *)
		  bp_ptr := ptr (wordoffset);
		  bptr := bp_ptr^; (* get the byte pointer *)
		  with bptr do begin
		    if pbits < char_size then begin	(* increment it one char *)
		      reladdr := reladdr + 1;
		      pbits := bits_per_addressible_unit - char_size;
		    end
		    else
		      pbits := pbits - char_size;

		    wordoffset := reladdr; (* fill in address record *)
		    is_packed := true;
		    bitoffset := bits_per_addressible_unit - (sbits + pbits);
		  end (* with *);
		end (* if *) 

		else			(* parameter passed by address *)
		  wordoffset := indirect_word_ptr (wordoffset)^.address;
	      end (* if *);
	    end (* else *);
	  end (* parameter_sc case *);

	static_sc:				(* publics and statics *)
	   wordoffset := staticbase + id_offset;

	external_sc:				(* externals *)
	  begin
	    mod_name := '';
	    with nam (deref$ptr (id_name))^ do
	      name_text := substr (text, 1, len); (* get name of symbol *)
	    link_entry := sym$lookup (name_text, mod_name); (* lookup in Link symbol table *)
	    if link_entry = nil then
	      status := undef_external
	    else
	      wordoffset := link_entry^.symaddr;
	  end;

	constant_sc:				(* constants *)
	  if id_offset >= 400000b then		(* constants may be allocated in low seg, *)
	    wordoffset := prog_blk^.hiseg_base + id_offset - 400000b	(* if overlay compilation *)
	  else
	    wordoffset := prog_blk^.lowseg_base + id_offset;

	others:
	  a$$ert (false)		(* should not happen !!! *)
      end (* case *);

    end (*  with *);
  end (* get_address *);
$PAGE set$bounds
(* SET$BOUNDS sets the bounds fields in a descriptor for an array or
   string.  For an array, fields LOWER_BND and UPPER_BND are set in the
   descriptor.  For flexible strings, field STR_LENGTH is set in the
   descriptor's type node.  For flexible arrays or strings which are
   preceded by the actual upperbound, the ADDR record in the descriptor
   will be updated to address the first element of the array rather
   than the bound word.  Parameters ID_INTSYM and STACKBASE are required
   only if the array or string descriptor corresponds to a formal
   parameter identifier.  In that case the bounds are in the parameter
   list rather than before the array.  *)

public procedure set$bounds (var desc:		descriptor;
				 id_intsym:	intsym;
				 stackbase:	^stack_frame);

  var
    idx_typ:	typ;
    lower_bound:	int_type;
    id_sym:	sym;
    param_addr:	unit_range;

  begin
    with desc, dtype do begin
      if (kind = arrays) andif			(* if array, get lower bound field *)
	 (index_type <> ord (nil)) then begin	(* we're almost certain to need it later *)
	idx_typ := deref$ptr (index_type);	
	lower_bound := idx_typ^.minval
      end;

      (* if array is not flexible then simply get bounds from array index
	 type node.  Nothing need be done for a non-flexible string.  *)

      if not flexible then begin
	if dkind = array_dt then begin
	  lower_bnd := lower_bound;
	  upper_bnd := idx_typ^.maxval
	end
      end

      (* O.K. - now we handle the flex arrays/strings.  We first check
	 for the fun case - a flexible (and possibly generic) array
	 or string formal parameter.  In that case ID_INTSYM must be 
	 non-nil and be a symbol node for a formal parameter.  If so
	 the upper (and lower if generic) bound precedes the arrays
	 *address* (not the array itself) in the parameter list.  *)

      else begin
	if id_intsym <> ord (nil) then		(* before we can check for the fun case, *)
	  id_sym := deref$ptr (id_intsym); (* we need the symbol node *)
	if (id_intsym <> ord (nil)) andif
	   (id_sym^.kind in [vars, values]) andif	(* must be true if we have a parameter *)
	   (id_sym^.dcl_class = parameter_sc) then begin
	  param_addr := argument_address (id_intsym, stackbase); (* address of array/string pointer *)
	  if dkind = string_dt then
	    str_length := int_type_ptr (param_addr - 1)^		(* string formal parameter *)
	  else
	    upper_bnd := int_type_ptr (param_addr - 1)^; (* array formal parameter *)
	  if generic then begin			(* generic params preceded by both *)
	    lower_bnd := int_type_ptr (param_addr -2)^;		
	  end
	  else if dkind = array_dt then
	    lower_bnd := lower_bound
	end (* if *)

	(* Flexible array but not formal parameter - bound(s) should
	   precede array.  Note address is updated to point to first 
	   element of array.  *)

	else begin
	  if dkind = string_dt then		(* string *)
	    str_length := int_type_ptr (addr.wordoffset)^
	  else begin				(* array *)
	    lower_bnd := lower_bound; (* if not formal param, can't be generic *)
	    upper_bnd := int_type_ptr (addr.wordoffset)^
	  end;
	  addr.wordoffset := addr.wordoffset + 1
	end (* else *);

      end 
    end (* with *);
  end (* set$bounds *);
$PAGE set$size
(* SET$SIZE replaces the contents of the SIZE field of the type node
   within a descriptor by the allocated size of a symbol.  *)

public procedure set$size (    id_intsym:	intsym;
			   var desc:		descriptor);

  begin
    with sym (deref$ptr (id_intsym))^ do
      if kind = fields then
	desc.dtype.size := fld_width
  end;
$PAGE set_id_desc
(* SET_ID_DESC takes an identifier which is the first ID in a
   reference.  It looks the ID up and then fills in as much 
   information as possible in a descriptor record for the
   reference.  In particular, the address of the symbol is 
   established, type information is filled in and the
   ASSIGNABLE field is set.  *)

procedure set_id_desc (    id_text:	id_string;
			   scope_stack:	scope_type;
		       var desc:	descriptor;
		       var status:	status_code);

  var
    id_intsym:	intsym;
    display_rec:	display;
    id_sym:	sym;

  begin
    with desc do begin
      with_lookup (id_text, id_intsym, addr); (* field of a WITHed record? *)
      if id_intsym <> ord (nil) then
	addr := set$field$addr (id_intsym, addr)	(* get address of field *)
      else begin					(* not a field, lookup in symbol table *)
	id_lookup (id_text, scope_stack, id_intsym, display_rec);
	if id_intsym = ord (nil) then begin
	  status := id_undefined;
	  return					(* <-- error return *)
	end;
	get_address (id_intsym, display_rec, addr, status); (* get address of symbol *)
	if status in severe then return; (* <-- error return *)
      end;

      id_sym := deref$ptr (id_intsym);
      assignable := (id_sym^.kind <> values) and	(* not a value parameter *)
		    (id_sym^.kind <> consts); (* not a constant *)
      set$kind (desc, id_sym^.type_desc); (* set type related info *)
      if dkind in [string_dt, array_dt] then
	set$bounds (desc, id_intsym, display_rec.stackbase); (* set array bounds *)

      set$size (id_intsym, desc)		(* replace type width by symbol's allocated width *)
    end
  end (* set_id_desc *);
$PAGE get$descriptor
(* GET$DESCRIPTOR parses either a constant or a reference and returns
   a descriptor for the object.  SCOPE_STACK describes the scope within
   which a reference is evaluated.   DESC is the descriptor returned
   and STATUS is the status code returned.  *)

public procedure get$descriptor (var lex_scan_rec: lex_scan_rec_type;
				     scope_stack:  scope_type;
				 var desc:	   descriptor;
				 var status:	   status_code);

const
  first_cons: token_set := [intconst, realconst, stringconst, lbrack];

var
  minus_flag,
  expr_flag:	boolean;
$PAGE field_ref - in get$descriptor
(* FIELD_REF is passed a descriptor for a record.  It scans the
   next token looking for a field identifier.  The field 
   identifier's symbol node is looked up.  Then the address
   field in the descriptor is updated to the address of
   the field, and the type information of the descriptor is updated
   to reflect the type of the field.  *)

procedure field_ref (var desc:   descriptor;
		     var status: status_code);

  var
    field_intsym: intsym;


  begin
    with desc do begin
      if dkind <> record_dt then begin	(* Make sure we have a record *)
	status := not_record;
	return (* <-- return *)
      end;
      lex$scan (lex_scan_rec, status); (* scan the field identifier *)
      if status in severe then return; (* <-- error return *)
      if lex_scan_rec.tkind <> ident then begin
	status := field_id_expected;
	return (* <-- return *)
      end;

      (* lookup the field id in the field symbol list hung off the records type node *)
      field_intsym := sym_list_lookup (dtype.field_list, lex_scan_rec.next_token.id_text);
      if field_intsym = ord (nil) then begin
	status := no_such_field;
	return (* <-- return *)
      end;

      addr := set$field$addr (field_intsym, addr); (* calc the address of the field *)
      set$kind (desc, sym (deref$ptr (field_intsym))^.type_desc);
						 (* reset desc type info to reflect field's type *)
      set$size (field_intsym, desc); (* set size field in DTYPE to *allocated* size *)
      if dkind in [string_dt, array_dt] then
	set$bounds (desc, ord (nil), nil) (* set array bounds in descriptor *)
    end
  end (* field_ref *);
$PAGE ptr_ref - in get$descriptor
(* PTR_REF is passed a descriptor for a pointer or file.  It updates
   the descriptor to address the pointer target or file component.
   The descriptor's type information is also updated appropriately. *)

procedure ptr_ref (var desc:   descriptor;
		   var status: status_code);

  begin
    with desc do begin
      if not (dkind in [pointer_dt, file_dt]) then begin	(* make sure we have a ptr or file *)
	status := not_file_or_ptr;
	return  (* <-- error return *)
      end;

      assignable := true; (* In case was value param *)
      addr.wordoffset := ext$address (desc, status); (* set addr of dereferenced object *)
      if status in severe then return; (* <-- error return *)

      if addr.wordoffset = ord (nil) then
	status := nil_pointer
      else if addr.wordoffset = 0 then
	status := ptr_uninitialized
      else
	addr.is_packed := false;		
      if status in severe then return; (* <-- error return *)

      if dkind = pointer_dt then begin		(* pointer deref *)
	if dtype.target_type = ord(nil) then	(* will if ptr type is PTR *)
	  status := target_unknown
	else				(* target type may legally be unknown *)
	  if typ (deref$ptr (dtype.target_type))^.kind = unknown_type then
	    status := target_unknown;
	if status in severe then return; (* <-- error return *)
	set$kind (desc, dtype.target_type) (* set type to that of target *)
      end
      else begin				(* file dereference *)
	if dtype.component_type = ord (nil) then begin  (* ck for binary file *)
	  status := target_unknown; (* forbid dereference *)
	  return (* <-- error return *)
	end;
	addr.wordoffset := indirect_word_ptr (addr.wordoffset)^.address; (* now it is buffer address *)
	set$kind (desc, dtype.component_type) (* set type of component *)
      end;
      if dkind in [array_dt, string_dt] then
	set$bounds (desc, ord (nil), nil) (* set array bounds in descriptor *)

    end
  end (* ptr_ref *);
$PAGE index_ref - in get$descriptor
(* INDEX_REF parses the indices of an indexed reference.  The
   index expression may take one of three forms: ( <c or r>
   indicates a constant or reference, parenthesis and '*' are
   metacharacters indicating optional and repeated constructs)
      [<c or r> (, <c or r>)*]			(* element reference *)
      [<c or r> : <c or r>]			(* substring reference *)
       [(<c or r> (,<c or r>)*,) <c or r>..<c or r>]	(* slice reference *)
   Non-local CURSOR is assumed to point just past the left bracket
   on entry.  It will point just past the right bracket on a
   successful return.  The descriptor, which presumably describes
   an array or string on entry, will be updated on exit to describe the
   referenced element, substring or slice.  *)

procedure index_ref (var desc:   descriptor;
		     var status: status_code);

  var
    idx_desc:	    descriptor;
    index,
    index2:	    int_type;
    delimiter:	    token_kind;
    element_inttyp: inttyp;
    upper_bound:    int_type;
    array_inttyp:   inttyp;
    start_temp,
    len_temp:	    char_range;
    elem_size:	    bit_range;


  begin
    with desc do begin
      if not (dkind in [array_dt, string_dt]) then begin
	status := not_array_string;
	return (* <-- return *)
      end;

      get$descriptor (lex_scan_rec, scope_stack, idx_desc, status); (* descr. for 1st index *)
      lex$scan (lex_scan_rec, status); (* get first delimiter *)
      if status in severe then return; (* <-- error return *)

      (* Substring reference *)

      if lex_scan_rec.tkind = colon then begin
	if not (dkind = string_dt) then begin		(* DESC must be for string *)
	  status := not_string;
	  return (* <-- return *)
	end;
	index_check (desc, idx_desc, status); (* type and bounds check string index *)
	if status in severe then return; (* <-- error return *)
	start_temp := ext$scalar (idx_desc, status); (* save substring start index *)
	get$descriptor (lex_scan_rec, scope_stack, idx_desc, status);
						  (* get descriptor for substring length *)
	if status in severe then return; (* <-- error return *)
	len_temp := ext$scalar (idx_desc, status); (* idx_desc is for length, convert to *)
	if len_temp < 0 then begin
	  status := illegal_value;
	  return (* <-- return *)
	end;
	idx_desc.value.scalar_val [1] := start_temp + len_temp - 1; (* descriptor for last char of *)
	with idx_desc.addr do begin		(* substring for index check *)
	  wordoffset := ord (address (idx_desc.value.scalar_val [1]));
	  is_packed := false
	end;
	if len_temp > 0 then
	  index_check (desc, idx_desc, status); (* type and bounds check last char index *)
	if status in severe then return; (* <-- error return *)

	dkind := substr_dt;
	substr_start := start_temp;
	substr_len := len_temp;
	lex$scan (lex_scan_rec, status);
	if status in severe then return (* <-- error return *)
      end (* substring code *)

      (* Array indexing or array slice *)

      else begin
	repeat					(* One index processed per iteration *)
	  array_inttyp := type_ptr; (* save - we'll need it later *)
	  index_check (desc, idx_desc, status); (* type and bounds check index *)
	  if status in severe then return; (* <-- error return *)
	  if dkind = string_dt then
	    element_inttyp := scope_stack.displays[1].prog_blk^.charpoint
	  else
	    element_inttyp := dtype.element_type;

	  delimiter := lex_scan_rec.tkind;
	  index := ext$scalar (idx_desc, status);
	  if status in severe then return; (* <-- error return *)
	  if delimiter <> elipsis then begin	(* if not slice, change desc type to
						     element type *)
	    if dtype.kind = strings then
	      elem_size := char_size
	    else
	      elem_size := dtype.element_size;
	    desc.addr := get$element$addr (desc, index); (* calc element address *)
	    set$kind (desc, element_inttyp); (* set desc type to the element type *)
	    dtype.size := elem_size;
	    if dkind in [array_dt, string_dt] then
	      set$bounds (desc, ord (nil), nil) (* set bounds field in descriptor *)
	  end;

	  (* if more indicies remain, then get descriptor for next index. *)

	  if lex_scan_rec.tkind = comma then begin
	    if not (dkind in [array_dt, string_dt]) then begin
	      status := too_many_subscripts;
	      return (* <-- return *)
	    end;
	    get$descriptor (lex_scan_rec, scope_stack, idx_desc, status);
	    if status in severe then return; (* <-- error return *)
	    lex$scan (lex_scan_rec, status);
	    if status in severe then return (* <-- error return *)
	  end;

	until delimiter <> comma;

	(*  array slice *)

	if delimiter = elipsis then begin
	  get$descriptor (lex_scan_rec, scope_stack, idx_desc, status); (* get descriptor
						  for 2nd element of slice range *)
	  if status in severe then return; (* <-- error return *)
	  index_check (desc, idx_desc, status);
	  if status in severe then return; (* <-- error return *)
	  index2 := ext$scalar (idx_desc, status);
	  if status in severe then return; (* <-- error return *)
	  if index2 < index then begin
	    status := illegal_range;
	    return (* <-- return *)
	  end;
	  lex$scan (lex_scan_rec, status);
	  if status in severe then return; (* <-- error return *)

	  (* Slices of strings are converted to substrings. *)

	  if dkind = string_dt then begin
	    dkind := substr_dt;
	    substr_start := index;
	    substr_len := index2 - index + 1
	  end

	  (* Slices of arrays are of kind SLICE_DT.  The descriptor is simply the
	     normal array descriptor with the ADDR field set to the first 
	     element of the slice and LOWER_BND and UPPER_BND set to the slice
	     bounds.  *)

	  else begin
	    desc.addr := get$element$addr (desc, index);
	    dkind := slice_dt;
	    lower_bnd := index;
	    upper_bnd := index2
	  end
	end

      end (* indexing or slice reference *);

      if lex_scan_rec.tkind <> rbrack then
	status := rbrack_expected
    end (* with *);
  end (* index_ref *);
$PAGE ref - in get$descriptor
(* REF parses a reference and returns a descriptor record for the
   object.  Parameter TOKEN contains the first token of the 
   reference on entry.  On a successful return, non-local variable
   CURSOR will point just past the last token of the reference.  *)

procedure ref (var desc:	descriptor;
	       var status:	status_code);

  var
    prev_cursor: cursor_range;

  begin
    if lex_scan_rec.tkind <> ident then begin
      status := ident_expected;
      return  (* <-- error return *)
    end;

    (* set address, type, etc. information about the initial identifier. *)

    set_id_desc (lex_scan_rec.next_token.id_text, scope_stack, desc, status);
    if status in severe then return; (* <-- error return *)
    prev_cursor := lex_scan_rec.cursor; (* don't want to scan PAST the reference *)
    lex$scan (lex_scan_rec, status);
    if status in severe then return; (* <-- error return *)

    (* Process one field, pointer or index reference per iteration.  *)

    while (lex_scan_rec.tkind in [period, lbrack, arrow]) do begin
      case lex_scan_rec.tkind of
	period:  field_ref (desc, status);
	arrow:   ptr_ref (desc, status);
	lbrack:  index_ref (desc, status)
      end;

      if status in severe then return; (* <-- error return *)
      prev_cursor := lex_scan_rec.cursor;
      lex$scan (lex_scan_rec, status);
      if status in severe then return (* <-- error return *)
    end;

    lex_scan_rec.cursor := prev_cursor (* CURSOR should point to 1st char beyond reference *)
  end (* ref *);
$PAGE set_range - in get$descriptor
(* SET_RANGE is a helper routine for parsing set constants.  It
   parses a single (possibly empty) element or element range.
   The range of elements parsed is then unioned with the previous
   contents of parameter VALUE.  If parameter ELEM_INTTYP is
   initially NIL, then it is set to the type node offset of the
   element type.  If it was not initially NIL, then it is simply
   checked for compatability with the type of the range parsed.  *)

procedure set_range (var value:		value_node;
		     var elem_inttyp:	inttyp;
		     var status:	status_code);

  var
    prev_cursor:	cursor_range;
    elem_desc:	descriptor;
    range1,
    range2,
    new_origin,
    new_end:	int_type;
    new_len:	bit_range;
    i:		0..max_set_size;

  begin
    prev_cursor := lex_scan_rec.cursor; (* look ahead 1 token to check for empty set *)
    lex$scan (lex_scan_rec, status);
    if status in severe then return; (* <-- error return *)
    lex_scan_rec.cursor := prev_cursor;
    if lex_scan_rec.tkind <> rbrack then		(* watch out for empty set *)
      with value do begin

	(* Parse first element of range *)

	get$descriptor (lex_scan_rec, scope_stack, elem_desc, status);
	if status in severe then return; (* <-- error return *)
	if not (elem_desc.dkind in [scalar_dt, int_dt, char_dt]) then begin
	  status := ill_set_elem_type;
	  return (* <-- return *)
	end;

	range1 := ext$scalar (elem_desc, status);
	if status in severe then return; (* <-- error return *)
	range2 := range1; (* will be reset later if 2nd element of range given *)
	if elem_inttyp = ord (nil) then		(* if element type not yet set, *)
	  elem_inttyp := elem_desc.type_ptr	(* then set it *)
	else
	  check$type (elem_desc, elem_inttyp, status); (* make sure this one matches previous *)
	if status in severe then return; (* <-- error return *)

	prev_cursor := lex_scan_rec.cursor; (* lookahead a token to check for elipsis *)
	lex$scan (lex_scan_rec, status);
	if status in severe then return; (* <-- error return *)

	(* Parse second element of range, if any *)

	if lex_scan_rec.tkind = elipsis then begin	(* have a range of elements *)
	  get$descriptor (lex_scan_rec, scope_stack, elem_desc, status);
	  if status in severe then return; (* <-- error return *)
	  if not (elem_desc.dkind in [scalar_dt, int_dt, char_dt]) then begin
	    status := ill_set_elem_type;
	    return (* <-- return *)
	  end;
	  check$type (elem_desc, elem_inttyp, status); (* type must match 1st element of range *)
	  if status in severe then return; (* <-- error return *)
	  range2 := ext$scalar (elem_desc, status);
	  if status in severe then return (* <-- error return *)
	end
	else				(* just parse element, not delimiter *)
	  lex_scan_rec.cursor := prev_cursor;

	(* error check ranges, establish new origin, end and length of set *)

	if (range2 < range1) orif (range1 < 0) then begin
	  status := illegal_range;
	  return (* <-- return *)
	end;
	if set_len = 0 then begin			(* VALUE empty initially *)
	  new_origin := range1;
	  new_end := range2
	end
	else begin				(* VALUE initially not empty *)
	  new_origin := min (set_origin, range1);
	  new_end := max (set_origin + set_len - 1, range2)
	end;
	new_len := new_end - new_origin + 1;
	if new_len > max_set_size then begin
	  status := set_too_big;
	  return (* <-- return *)
	end;

	(* union old set and range just parsed.  Start at end so we can
	   do union in place without over writing what's already there.  *)

	for i := new_end downto new_origin do begin
	  if (i >= set_origin) andif		(* if value within old set, *)
	     (i <= set_origin + set_len - 1) then 
	    set_val [i - new_origin] := set_val [i - set_origin]	(* just shift old set value *)
	  else
	    set_val [i - new_origin] := false; (* else set false *)
	  if (i >= range1) andif (i <= range2) then	(* if within new range, *)
	    set_val [i - new_origin] := true	(* set to true *)
	end;

	(* set new origin and length for set *)

	set_origin := new_origin;
	set_len := new_len

      end (* with *);
  end (* set_range *);
$PAGE cons - in get$descriptor
(* CONS parses and returns a descriptor for either an integer, real, string
   or set constant.  Parameter TOKEN should contain the first token of
   the constant on entry.  Non-local variable CURSOR will point just
   past the last token of the constant on a successful return.  *)

procedure cons (var desc:	descriptor;
		var status:	status_code);

var
  elem_inttyp: inttyp;

begin
  with desc do begin
    assignable := false;
    user_const := true;
    value := lex_scan_rec.next_token.cons_node; (* token holds real, int or string consts *);
    
    case lex_scan_rec.tkind of

      intconst:					(* integer constant *)
        begin
	  addr.wordoffset := ord (address (value.scalar_val [1]));
          address_value := lex_scan_rec.next_token.address_value;
	  status_in_given_radix := lex_scan_rec.next_token.status_in_given_radix;
	  status_in_addr_radix := lex_scan_rec.next_token.status_in_addr_radix;
          set$kind (desc, scope_stack.displays [1].prog_blk^.intpoint)
        end;

      realconst:				(* real constant *)
        begin
          addr.wordoffset := ord (address (value.real_val [1]));
          set$kind (desc, scope_stack.displays [1].prog_blk^.realpoint);
          dtype.precision := value.real_prec
        end;

      stringconst:				(* string or char constant *)
        begin
          if value.str_len = 1 then begin	(* char, not string *)
            value.kind := scalar_cst;
            value.scalar_val [1] := ord (value.str_val [1]);
	    addr.wordoffset := ord (address (value.scalar_val [1]));
            set$kind (desc, scope_stack.displays [1].prog_blk^.charpoint)
          end
          else begin				(* really is a string *)
            addr.wordoffset := ord (address (value.str_val));
            dkind := string_dt; (* can't call SET$KIND *)
            with dtype do begin			(* partially fill in type node *)
              kind := strings;
              str_kind := nonvarying;
              str_length := value.str_len;
              flexible := false
            end
          end
        end;

      lbrack:					(* set constant *)
        begin
          dkind := set_dt;
          dtype.kind := sets; (* set kind field of descriptor's type node *)
          dtype.set_element_type := ord (nil);
          addr.wordoffset := ord (address (value.set_val));
          value.kind := set_cst;
          value.set_origin := 1; (* SET_ORIGIN is initialized in case empty
						   set is entered, then SET_MIN will be set to
						   1 and SET_MAX to 0 in the descriptor *)
          value.set_len := 0; (* VALUE initially contains empty set *)
          elem_inttyp := ord (nil);
      
          repeat				(* process 1 element or element range per iteration *)
            set_range (value, elem_inttyp, status); (* parse next range, union with set
						   accumulated so far *)
            if status in severe then return; (* <-- error return *)
            if dtype.set_element_type = ord (nil) then
              dtype.set_element_type := elem_inttyp;
            lex$scan (lex_scan_rec, status);
	    if status in severe then return (* <-- error return *)
          until lex_scan_rec.tkind <> comma;

          if lex_scan_rec.tkind <> rbrack then		(* should be at closing bracket now *)
            status := rbrack_expected;

          set_min := value.set_origin;
          set_max := value.set_origin + value.set_len - 1
        end;

      others:
	a$$ert (false)
    end
  end
end (* cons *);
$PAGE get$descriptor - body
begin
  init$desc (desc); (* initialize the descriptor *)
  lex$scan (lex_scan_rec, status);
  if status in severe then return; (* <-- error return *)
  minus_flag := false;
  expr_flag := false;
  if lex_scan_rec.tkind in [plus, minus] then begin	(* If signed then, *)
    minus_flag := lex_scan_rec.tkind = minus; (* set sign flag and *)
    expr_flag := true; (* set expression flag for use later *)
    lex$scan (lex_scan_rec, status);
    if status in severe then return (* <-- error return *)
  end;

  if lex_scan_rec.tkind = ident then			(* reference *)
    ref (desc, status)
  else if lex_scan_rec.tkind in first_cons then		(* constant *)
    cons (desc, status)
  else
    status := cons_ref_expected; (* else error *)

  with desc do begin
    if expr_flag then
      assignable := false; (* signed refs not assignable *)
    if minus_flag and not (status in severe) then begin	(* negate value if minus sign seen *)
      addr.is_packed := false; (* value will be moved to value node within descriptor *)
      if dkind = int_dt then begin
        value.scalar_val [1] := - ext$scalar (desc, status);
        addr.wordoffset := ord (address (value.scalar_val [1]))
      end
      else if dkind = real_dt then begin
        value.real_val [1] := - ext$real (desc, status);
        addr.wordoffset := ord (address (value.real_val [1]))
      end
      else
	status := type_incompatability
    end
  end
end (* get$descriptor *).
 $ ¾