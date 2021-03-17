$TITLE vaxcsp - constant pooling
$LENGTH 42
(*   +--------------------------------------------------------------+
     |                                                              |
     |                         V A X C S P                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  VAXCSP contains all routines  necessary  for  constant
        pooling.
     
     ENTRY POINTS:
     
        pool_constants
	     pools the constants in the constant area, and rearranges
	     and pads the constant area to establish required  align-
	     ments.
     
     ALGORITHM:   The  constant  area  is first partitioned into five
        sub-areas:  single longword constants, fixed length  strings,
        variable   length   strings,  sets  and  everything  else  or
        'others'.  The  partitioning  is  done  as  follows.   Single
        longwords  are  entered  into  a  hash  table,  the 'longword
        table'.  Duplicates are pooled  immediately.  Partitions  for
        fixed  length  strings,  variable length strings and sets are
        realized  by  creating  three  structures  called  'partition
        lists'.  Partition lists are constructed of two record types,
        SIZE records and LINK records.  The SIZE records  are  linked
        together  into  an  ordered  list  (1  for  each of the three
        categories).  Each SIZE record in turn  serves  as  the  list
        head  for  a  list  of  LINK  records.  The LINK records each
        contain a pointer to the  DEFMARK  code  record  beginning  a
        constant.  All  the constants on a given LINK list are of the
        same length.  Finally the 'others' partition is realized  via
        a list of LINK records.
     
        The actual pooling is done in six separate steps:
          1. Single  longword,  realword,   wordlen   and
             bytelen  constants are  pooled as the  longword table is
	     created.
          2. Variable length strings are pooled  with  themselves  by
             testing  strings  of  the  same  length   for  equality.
          3. Fixed  length  strings  are  pooled with variable length
             strings by testing strings of the same length for equality.
          4. Fixed length  strings  are  pooled  with  themselves  by
             testing  strings  of  the  same  length  for   equality.
          5. Sets are pooled with themselves by testing sets  of  the
             same length (in bytes) for equality.
	  6. The constant  area is  rearranged by  alignment classes,
	     and  padding  bytes  inserted as  required to  establish
	     alignments.
          7. Single  longwords, ..., bytelens  are  pooled  with  the
             others  partition by  looking up each  properly  aligned
	     longword, ..., bytelen  others  constant in the longword
	     table.  Any  entries found are  deleted and  pooled with
             the others constant.
     
        The pooling of a given or 'old' constant with all or part  of
        an  arbitrary  or  'new' constant is done as follows.  At the
        time of the pooling the length of the old constant is  known.
        The  old constant, beginning with the initial defmark record,
        is traversed.  For each  defmark  record  encountered  a  new
        defmark  record  is  created  in  the  corresponding position
        within the new constant.  The new defmark record  is  set  to
        point  to  the  same  DEFINITION node pointed at from the old
        defmark record.  Defmark records may be embedded  within  the
        old  constant  if  the  old  constant  previously  had  other
        constants pooled with it.
     
        The fact that only  single  longwords  are  pooled  with  the
        others partition has been exploited.  The length of constants
        in the others partition is not available in the others  list.
        If  the  others  partition  was  to  be pooled with itself or
        another partition then the partiioning pass would have to  be
        modified  to  store  the  length  of  others constants in the
        others list.
     
     ---------------------------------------------------------------- *)

module vaxcsp;
$PAGE includes
$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE vaxcg.typ
$INCLUDE vaxcgu.inc
$PAGE declarations
  
(* Longword hash table declarations.  The table is represented by an
   array of pointers to entry records.  The entry records are created
   dynamically as necessary.  Collisions are resolved via chaining. *)

const
  lwd_tab_max := 228;				(* table size (a prime) minus one *)
  area_description: string := 'Constant Area:';

type
  lwd_table_index = 0..lwd_tab_max;
  entry_ptr = ^longwd_entry;
  longwd_entry = packed record			(* table entries *)
    overflow: entry_ptr;			(* collision list *)
    def_mark: code;				(* defmark code record for constant *)
    deleted: boolean				(* entry deleted flag *)
  end;

var
  longwd_table: packed array [lwd_table_index] of entry_ptr;


(* Partition lists declarations.  SIZE and LINK records are used to
   partition the constant area by linking together all fixed length 
   strings, or all variable length strings or all sets.  SIZE records
   are linked together into an ordered list.  Three such lists exist -
   for fixed and variable strings and for sets.  Each size record serves
   as list head for a list of LINK records.  The LINK records point to
   the DEFMARK code record which begins a constant of the relevant type.
   The constant's length in bytes is that given in the size record
   heading the list. *)

type
  size = ^size_rec;
  link = ^link_rec;
  size_rec = packed record
    num_bytes: unit_range;			(* length in bytes of constants, excludes defmark *)
    first_link: link;				(* pointer to first link record *)
    next_size: size				(* pointer to next size record *)
  end;
  link_rec = packed record
    next_link: link;				(* pointer to next link record *)
    first_code: code				(* defmark code record for constant corresponding
						   to this record *)
  end;


var
  variable_head: size;				(* variable length strings size list head *)
  fixed_head: size;				(* fixed length strings size list head *)
  set_head: size;				(* sets size list head *)

  others_head: link; 				(* head of unordered list of link records which
						   link those constants which fall into the
						   'others' partition. *)
  
  s_m_longwords: code;	(* list of single and multiple longword constants *)
  lw_aligned: code; 	(* list of arbitrary-length longword aligned constants *)
  word_aligned: code;	(* list of word aligned constants *)
  unaligned: code;	(* list of unaligned constants *)
  
  last_s_m_longwords,
  last_lw_aligned,
  last_word_aligned,
  last_unaligned: code;	(* tails of above lists *)
$PAGE int_equivalent
(* INT EQUIVALENT pulls the value from a number of different code record variants
   that can all be regarded as containing single integral values of some sort. *)
  
function int_equivalent (cur_code: code): int_type;
  
begin
  with cur_code^ do
    case kind of
      bytelen: int_equivalent := byte_value;
      wordlen: int_equivalent := word_value;
      fullword,
      realword,
      doubleword: int_equivalent := fwd;  (* note "equivalencing" here! *)
      others: 
	assert (false)
    end
end;
$PAGE lookup
(* LOOKUP looks up longword (or equivalent) KEY in the longword table.  A pointer
   to the table entry is returned if the key is found; otherwise NIL 
   is returned.  *)

function lookup ( key: longword; key_kind: code_types ): entry_ptr;

var
  index: lwd_table_index;
  found: boolean;
  cur_code: code;

begin
  index := abs ( key mod (lwd_tab_max + 1) );
  lookup := longwd_table[index];
  found := false;
  while (lookup <> nil) andif not found do
    with lookup^ do begin
      if not deleted then begin
        cur_code := def_mark^.next;

        (* may be multiple DEFMARK code records due to previous pooling *)

        while cur_code^.kind = defmark do cur_code := cur_code^.next;
        found := (int_equivalent (cur_code) = key) and (cur_code^.kind = key_kind)
      end;
      if not found then lookup := overflow
    end (* with *);
end;
$PAGE byte_length
(* BYTE LENGTH returns the length in bytes of the constant in the given code record. *)
  
function byte_length (const_rec: code): unit_range;
  
begin
  case const_rec^.kind of
    bytelen,
    setbyte:     byte_length := 1;
    wordlen:     byte_length := 2;
    fullword,
    realword,
    indirect_word:    byte_length := 4;
    quadword,
    doubleword:  byte_length := 8;
    stringword:  byte_length := 2 + length (const_rec^.strvalue);
    pstringword: byte_length := length (const_rec^.strvalue);
    others:      assert (false)
  end
end;
$PAGE pool_cons
(* POOL CONS pools a given constant with a portion of another.
   The given or 'old' constant is specified by OLD_DEFMARK and
   OLD_LENGTH.  OLD_DEFMARK must point to the first defmark code
   record preceding the old constant.  OLD_LENGTH is the length of 
   the old constant in bytes;  defmark records are not included in
   the length.  PREV_CODE designates the constant to be pooled with,
   the 'new' constant.  PREV_CODE must point to the code record previous 
   to the code record actually containing the constant.  This is
   necesary because a new defmark code record is created and inserted
   after PREV_CODE.  This is set to point to the definition node of the
   old constant.  New defmark records are also created for any defmark
   records embedded in the old constant.  This can occur if the old
   constant previously had other constants pooled with it. *)

procedure pool_cons (old_defmark: code;
		     old_length: unit_range;
		     prev_code: code);

var
  cur_code_old: code;				(* current code record of old constant *)
  cur_code_new: code;				(* current code record of new constant *)
  length_count: unit_range;			(* counts non-defmarks (in bytes) in old constant *)
  new_defmark: code;				(* pointer to new defmarks created *)

begin
  cur_code_old := old_defmark;
  cur_code_new := prev_code;
  length_count := 0;

  (* Traverse old constant.  Set KIND of each code record to NULLCODE. *)

  while length_count < old_length do begin
    
    (* if old code record is a defmark, then create a new one and insert
       in corresponding location in 'new' constant *)

    if cur_code_old^.kind = defmark then begin
      new ( new_defmark, defmark );
      with new_defmark^ do begin
        next := cur_code_new^.next;
        cur_code_new^.next := new_defmark;
        defname := cur_code_old^.defname
      end
    end

    (* if not a defmark, then increment count and advance 'new' constant
       pointer one non-defmark code record. *)

    else begin
      length_count := length_count + byte_length (cur_code_old);
      repeat
        cur_code_new := cur_code_new^.next
      until cur_code_new^.kind <> defmark
    end;
    
    cur_code_old^.kind := nullcode; (* mark record to be ignored *)
    cur_code_old := cur_code_old^.next
  end
end;
$PAGE partition
(* PARTITION is used by POOL_CONSTANTS to partition the constant area
   into five areas: single longwords, fixed length strings, variable
   length strings, sets and anything else ('others').  Single LONGWORDs
   are pooled as they are entered into a table.  Fixed length strings,
   variable length strings and sets each have a structure, called a 
   partition list, created which links constants of the same size into
   sublists.  Constants in the 'others' category are linked into a 
   single linked list.    *)

  
procedure partition;

var
  def_mark: code;				(* pointer to DEFMARK code record which
						   begins a constant *)
  cur_code: code;				(* curent code record being examined within
						   current constant *)
  num_cbytes: unit_range;			(* number of code bytes for current constant *)
  end_of_const: code;				(* last code record of current constant, for use
						   when chaining constants by alignment class *)
$PAGE put_in_lwd_table - in partition 
(* LONGWD_TAB enters a single LONGWORD constant (or something equivalent) into the longword table.
   Duplicate constants are pooled with the constant previously entered.  DEF_MARK is the DEFMARK code
   record for the constant.  This routine assumes that the code record following DEF_CODE, via the NEXT
   field, contains the longword (or equivalent) value. *)

procedure put_in_lwd_table;

var
  entry: entry_ptr;				(* duplicate entry, if any *)
  lwd_code: code;				(* pointer to the LONGWORD code record *)
  index: lwd_table_index;

begin
  lwd_code := def_mark^.next;
  while lwd_code^.kind = defmark do
    lwd_code := lwd_code^.next;
  entry := lookup (int_equivalent (lwd_code), lwd_code^.kind);
  if entry <> nil then 				(* if record was already in table *)
    pool_cons (def_mark, byte_length (lwd_code), entry^.def_mark) (* pool with table entry *)
  else begin
    index := abs ( int_equivalent (lwd_code) mod (lwd_tab_max + 1) );
    new ( entry );
    entry^.overflow := longwd_table[index];
    entry^.def_mark := def_mark;
    entry^.deleted := false;
    longwd_table[index] := entry
  end
end;
$PAGE build_par - in partition 
(* BUILD PAR adds a constant to the partition list headed by PAR_LIST.
   The constant is added to the size list corresponding to constants
   of length NUM_CBYTES.  The DEFMARK code record for the constant is
   pointed to by DEF_MARK. *)

procedure build_par (var par_list: size);

var
  size_rec: size;				(* pointer to size record *)
  cur_size: size;				(* size record whose size we are examining *)
  last_size: size;				(* size record previous to CUR_SIZE 
						   - for list insertion *)
  new_link: link;				(* pointer to new link record created *)

begin
  
  (* Find SIZE record of correct size.  If none exists create one and
     link into the SIZE list. *)

  if (par_list = nil) orif (num_cbytes < par_list^.num_bytes) then begin
    new ( size_rec );
    size_rec^.num_bytes := num_cbytes;
    size_rec^.first_link := nil;
    size_rec^.next_size := par_list;
    par_list := size_rec
  end
  else begin					(* search ordered SIZE list *)
    cur_size := par_list^.next_size;
    last_size := par_list;
    while (cur_size <> nil) andif (cur_size^.num_bytes <= num_cbytes) do begin
      last_size := cur_size;
      cur_size := cur_size^.next_size
    end;
    if last_size^.num_bytes <> num_cbytes then begin
      new ( size_rec );				(* no match - create new size *)
      size_rec^.num_bytes := num_cbytes;
      size_rec^.first_link := nil;
      size_rec^.next_size := cur_size;		(* record and insert in middle or *) 
      last_size^.next_size := size_rec		(* at end of SIZE list *)
    end
    else					(* size record of right size already exists *)
      size_rec := last_size
  end;

  (* Create a link record for the new constant and insert it at the
     beginning of the list headed by SIZE_REC. *)

  new ( new_link );
  new_link^.next_link := size_rec^.first_link;
  size_rec^.first_link := new_link;
  new_link^.first_code := def_mark

end (* build_par *);
$PAGE put_on_chain - in partition
(* PUT ON CHAIN puts a constant onto a chain with the other constants of its
   alignment class.  Later, the constants area will be rearranged with padding bytes
   between the alignment classes, and between individual constants, as required
   to guaranty the expected alignment of the constants in each class.  *)
  
procedure put_on_chain (var list_head, list_tail: code);
  
begin
  if list_head = nil then  (* list currently empty? *)
    list_head := def_mark
  else
    list_tail^.next := def_mark;
  list_tail := end_of_const;
  list_tail^.next := nil
end;
$PAGE others_par - in partition 
(* OTHERS PAR creates a new link on the others partition list for a constant in the 'others'
   category.  DEF_MARK points to the defmark code record for the constant.  CUR_CODE initially
   points to some part of the constant beyond the initial defmark.  It is set to point to the
   defmark record following the constant.  END_OF_CONST is advanced to point at the last code
   record of the current constant.  *)

procedure others_par;

var
  new_link: link;

begin
  new (new_link);
  new_link^.next_link := others_head;
  new_link^.first_code := def_mark;
  others_head := new_link;
  while (cur_code <> nil) andif (cur_code^.kind <> defmark) do begin
    end_of_const := cur_code;
    cur_code := cur_code^.next
  end;
  put_on_chain (lw_aligned, last_lw_aligned)
end;
$PAGE partition - main routine
(* Begin body of partition *)

begin
  
  cur_code := cst_area.first;
  if cur_code^.kind = origin then cur_code := cur_code^.next;

  (* Process one constant per iteration of while loop. *)

  while cur_code <> nil do begin		
    def_mark := cur_code;
    While (cur_code <> nil) andif (CUR_CODE^.KIND = DEFMARK) Do	(* Ignore 1st defmark, rest null recs *)
      CUR_CODE := CUR_CODE^.NEXT;
    if cur_code = nil then begin (* had trailing labels (to null objects) *)
      gen_longword (cst_area,0); (* fake a target for the labels *)
      cur_code := cst_area.last
    end;
    end_of_const := cur_code;
    case cur_code^.kind of

      fullword,
      realword:
        begin
          cur_code := cur_code^.next;
          if (cur_code = nil) orif (cur_code^.kind = defmark) then begin
	    put_in_lwd_table;
	    put_on_chain (s_m_longwords, last_s_m_longwords)
	  end
	  else
	    others_par 
        end;

      doubleword:
	begin
	  cur_code := cur_code^.next;
	  others_par;
	end;

      stringword:
	begin
	  num_cbytes := length (cur_code^.strvalue);
	  cur_code := cur_code^.next;
	  if (cur_code = nil) orif (cur_code^.kind = defmark) thenn
	    build_par (variable_head);
	    put_on_chain (word_aligned, last_word_aligned)
	  end
	  else
	    others_par
	end;

      pstringword:
	begin
	  num_cbytes := length (cur_code^.strvalue);
	  cur_code := cur_code^.next;
	  if (cur_code = nil) orif (cur_code^.kind = defmark) then begin
	    build_par (fixed_head);
	    put_on_chain (unaligned, last_unaligned)
	  end
	  else
	    others_par
	end;

      setbyte:
	begin
          num_cbytes := 0;
	  while (cur_code <> nil) andif (cur_code^.kind = setbyte) do begin
	    end_of_const := cur_code;
	    cur_code := cur_code^.next;
	    num_cbytes := num_cbytes + 1
	  end;
	  if (cur_code = nil) orif (cur_code^.kind = defmark) then begin
	    build_par (set_head);
	    put_on_chain (unaligned, last_unaligned);
	  end
	  else
	    others_par
	end;

      bytelen,
      wordlen:
        begin
	  cur_code := cur_code^.next; 
	  if (cur_code = nil) orif (cur_code^.kind = defmark) then begin
	    if end_of_const^.kind = bytelen then
	      put_on_chain (unaligned, last_unaligned)
	    else
	      put_on_chain (word_aligned, last_word_aligned);
	    put_in_lwd_table
	  end
	  else
	    others_par
        end;

      others: assert (false)				(* can't happen !?! *)

    end (* case *);
  end (* while *);
  
end (* partition *);
$PAGE fixed_var_pool
(* FIXED VAR POOL pools the fixed length string partition with
   the variable length string partition.  The pooling is done by 
   testing strings of the same length for equality. *)

var 
  cur_fix_size: size;				(* current fixed string SIZE record *)
  cur_var_size: size;				(* current variable string SIZE record *)
  cur_var_link: link;				(* link record for current variable string
						   which we are trying to pool with *)
  len_word: code;				(* length word of variable length string *)
  fixed_link: link;				(* for link records in SIZE list headed
						   by CUR_FIX_SIZE *)
  cur_fixed_code: code;				(* for following code records from fixed_link *)
  temp_code: code;				(* for inserting new code records *)
  cur_length: unit_range;			(* size in bytes of current string, excludes
						   length word *)

procedure fixed_var_pool;

begin
  
  cur_fix_size := fixed_head;
  cur_var_size := variable_head;
  
  while (cur_fix_size <> nil) and (cur_var_size <> nil) do begin
    cur_length := cur_fix_size^.num_bytes;

    (* find variable length string size record for strings with the 
       same number of bytes, if one exists *)

    while (cur_var_size <> nil) andif (cur_var_size^.num_bytes < cur_length) do
      cur_var_size := cur_var_size^.next_size;
    if (cur_var_size <> nil) andif (cur_length = cur_var_size^.num_bytes) then begin
      cur_var_link := cur_var_size^.first_link;	(* found matching size record *)
  
      while cur_var_link <> nil do begin
        if cur_var_link^.first_code^.kind <> nullcode then begin
  
	  (* try all fixed strings of current size against the current variable string *)
  
	  fixed_link := cur_fix_size^.first_link;
          len_word := cur_var_link^.first_code;
	  while fixed_link <> nil do begin
	    if fixed_link^.first_code^.kind <> nullcode then begin
	      cur_fixed_code := fixed_link^.first_code;
	      repeat
		cur_fixed_code := cur_fixed_code^.next
	      until cur_fixed_code^.kind <> defmark;
	      while (len_word^.next^.kind <> stringword) and
		     (len_word^.next^.kind <> pstringword) do 
		len_word := len_word^.next;
	      if cur_fixed_code^.strvalue = len_word^.next^.strvalue then begin
		if len_word^.next^.kind = stringword then begin 
		  new (temp_code, wordlen);  (* create seperate code record for lengthword *)
		  temp_code^.next := len_word^.next;
		  temp_code^.word_value := length (len_word^.next^.strvalue);
		  len_word^.next := temp_code;
		  len_word := temp_code; (* advance pointer to the new length word record *)
		  assert (len_word^.next^.kind = stringword);
		  len_word^.next^.kind := pstringword  (* change stringword into a pstringword *)
		end;
		pool_cons (fixed_link^.first_code, cur_length, len_word)
	      end
	    end;
	    fixed_link := fixed_link^.next_link
	  end;
  
        end;
        cur_var_link := cur_var_link^.next_link  (* go on to next var. string of current size *)
      end
  
    end;
    cur_fix_size := cur_fix_size^.next_size  (* advance to next size *)
  end
  
end (* fixed_var_pool *);
$PAGE pool_link_list
(* POOL LINK LIST attempts to pool the constants pointed at from a
   LINK list with all or part of an arbitrary constant.  FIRST_LINK
   is the first link record in the LINK list.  PREV_CODE is the code 
   record previous to the code record beginning the arbitrary constant.
   The constants pointed at from the LINK list are assumed to be of 
   size CONST_SIZE (the size is the length in bytes of the constant value).  *)

procedure pool_link_list (prev_code: code; first_link: link;
                          const_size: unit_range);

var
  cur_link: link;
  cur_link_word: code;
  code_record: code;
  length_count: unit_range;
  equal: boolean;
  set_bit: 0..7;

begin
  
  cur_link := first_link;
  while cur_link <> nil do begin		(* process 1 link record per iteration *)
    if cur_link^.first_code^.kind <> nullcode then begin
      code_record := prev_code;
      cur_link_word := cur_link^.first_code;
      length_count := 0;
      equal := true;
  
      while (length_count < const_size) and equal do begin	(* compare constants *)
        repeat					(* advance a code record, ignoring defmarks *)
          cur_link_word := cur_link_word^.next
        until cur_link_word^.kind <> defmark;
        repeat 
          code_record := code_record^.next
        until code_record^.kind <> defmark;
	if cur_link_word^.kind <> code_record^.kind then
	  equal := false
	else
	  case code_record^.kind of
	  
	    bytelen,
	    wordlen,
	    fullword,
	    realword:
	      equal := int_equivalent (cur_link_word) = int_equivalent (code_record);
	    doubleword:
	      equal := cur_link_word^.dvalue = code_record^.dvalue;
	    setbyte: begin
	      equal := true;
	      for set_bit := minimum (set_bit) to maximum (set_bit) do
		exit if cur_link_word^.setval [set_bit] <> code_record^.setval [set_bit] do
		  equal := false
	    end;
  
	    pstringword,
	    stringword:
	      equal := cur_link_word^.strvalue = code_record^.strvalue;
  
	    others: 
	      assert (false)
	  end;
        length_count := length_count + byte_length (code_record)
      end;
  
      if equal then pool_cons (cur_link^.first_code, const_size, prev_code)
    end;
    cur_link := cur_link^.next_link
  end
  
end;
$PAGE pool_self
(* POOL SELF pools a partition list, specified by LIST_HEAD, with itself.  The
   pooling is done by testing constants of the same length for equality.  *)

procedure pool_self ( list_head: size );

var
  cur_size: size;
  cur_link: link;
  code_rec: code;
  cur_length: unit_range;

begin
  
  cur_size := list_head;
  while cur_size <> nil do begin
    cur_length := cur_size^.num_bytes;
    cur_link := cur_size^.first_link;
  
    while cur_link <> nil do begin
      if cur_link^.first_code^.kind <> nullcode then begin
        code_rec := cur_link^.first_code;
        while code_rec^.next^.kind = defmark do code_rec := code_rec^.next;
        pool_link_list (code_rec, cur_link^.next_link, cur_length)
      end;
      cur_link := cur_link^.next_link
    end;
  
    cur_size := cur_size^.next_size;
  end;
  
end;
$PAGE align
(* ALIGN rearranges the constant area so that the constants are grouped by their
   alignment classes.  Padding bytes are inserted to align the start of each class
   appropriately, and to align individual constants within their classes as necessary.  *)
  
procedure align (initial_ic: unit_range);
  
  var
    cur_code: code;
    lookahead: code;
    padding_bytes: code;
    ic: unit_range;
  cons_comment: code;
  
  procedure pad_bytes (var list_head: code; alignment: unit_range);
  
    var temp_code: code;
 
    begin
      while (ic mod alignment) <> 0 do begin
	new (temp_code, bytelen);
	temp_code^.byte_value := 0;
	temp_code^.next := list_head;
	list_head := temp_code;
	ic := ic + 1
      end
    end;
  
  procedure chain_into_cst_area (var list_head, list_tail: code; comment_str: string);
  
    begin
      new (cst_area.last^.next, comment, length (comment_str));
      with cst_area.last^.next^ do begin
	ctext := comment_str;
	next := list_head
      end;
      cst_area.last := list_tail
    end;
  
  begin
  
    ic := initial_ic;
    new (cons_comment, comment, length (area_description));
    if cst_area.first^.kind = origin
      then cst_area.first^.next := cons_comment
      else cst_area.first := cons_comment;
    cons_comment^.next := nil;
    cons_comment^.ctext := area_description;
    cst_area.last := cons_comment;
  
  
    (* Alignment class 1: single and multiple longword constants.  Beginning of class
       must be longword aligned.  No padding required within class.  *)
  
    if s_m_longwords <> nil then begin  (* class nonempty? *)
      pad_bytes (s_m_longwords, 4);
      chain_into_cst_area (s_m_longwords, last_s_m_longwords, 'Single and multiple longwords:')
    end;
  
  
    (* Alignment class 2: arbitrary-length, longword aligned constants.  Beginning of
       class must be longword aligned, and longword alignment must be reestablished
       between constants.  Note that this class corresponds to the partition "others",
       which has not had anything pooled with it at this point.  Therefore defmark
       code records occur only at the start of each constant.   This class contains unpacked
       arrays and records, and (since they can't be told from the former) packed arrays
       and records. *)
  
    if lw_aligned <> nil then begin
      cur_code := lw_aligned;
      pad_bytes (lw_aligned, 4);
  
      repeat
	while cur_code^.kind = defmark do
	  cur_code := cur_code^.next;	(* locate next non-defmark record *)
	ic := ic + byte_length (cur_code);
	while (cur_code^.next <> nil) andif (cur_code^.next^.kind <> defmark) do begin
	  cur_code := cur_code^.next;	(* locate last record of constant *)
	  ic := ic + byte_length (cur_code)
	end;
	if cur_code^.next <> nil then begin	(* if another constant follows ... *)
	  lookahead := cur_code^.next;
	  padding_bytes := lookahead;
	  pad_bytes (padding_bytes, 4);	(* ... assure its alignment        *)
	  cur_code^.next := padding_bytes;
	  cur_code := lookahead		(* advance to defmark of the next constant *)
	end
      until cur_code^.next = nil;
  
      chain_into_cst_area (lw_aligned, last_lw_aligned, 'Longword aligned:')
    end;
  
  
    (* Alignment class 3: arbitrary length, word aligned constants.  Beginning of class
       must be word aligned, and word alignment must be reestablished between constants. 
       This class contains varying strings and single wordlen constants.  *)
  
    if word_aligned <> nil then begin
      cur_code := word_aligned;
      pad_bytes (word_aligned, 2);
  
      loop
	repeat
	  cur_code := cur_code^.next	(* find next (p)string record - only they can
					   disturb the word alignment  *)
	until (cur_code = nil) orif (cur_code^.kind in [stringword, pstringword]);
      exit if cur_code = nil;
	lookahead := cur_code^.next;
	while (lookahead <> nil) andif
	      (lookahead^.kind = nullcode) do	(* see if another constant follows ...   *)
	  lookahead := lookahead^.next;
      exit if lookahead = nil;
	ic := ic + length (cur_code^.strvalue);	(* ... and if so, assure its alignment   *)
	padding_bytes := cur_code^.next;
	pad_bytes (padding_bytes, 2);
	cur_code^.next := padding_bytes;
	cur_code := lookahead
      end;
  
      chain_into_cst_area (word_aligned, last_word_aligned, 'Word aligned:')
    end;
  
  
    (* Alignment class 4: unaligned.  No alignment required before or within class. 
       This class contains fixed strings, single bytelen constants, and constant sets.  *)

    if unaligned <> nil then begin
      chain_into_cst_area (unaligned, last_unaligned, 'Unaligned:')
    end
  
  end (* align *);
$PAGE long_others_pool
(* LONG OTHERS POOL attempts to pool longword table entries with 
   each properly aligned code record of each constant in the 'others' partition.  It is 
   important to note that this routine assumes the 'others' partition
   has not had anything pooled with it previously.  This assumption
   is made because the size of a constant in the others partition is
   not known.  The end of a constant is assumed to be marked by the
   first defmark encountered after the initial one. *)

procedure long_others_pool;

var
  cur_link: link;				(* LINK record pointing to current 'others' constant *)
  cur_code, temp_code: code;		(* code record within current 'others' constant *)
  ic: unit_range;
  entry: entry_ptr;

begin
  cur_link := others_head;
  while cur_link <> nil do begin		(* process 1 'others' constant per iteration *)
    cur_code := cur_link^.first_code;
    if cur_code^.kind <> nullcode then begin
      ic := 0;
      while (cur_code^.next <> nil) andif
	    not (cur_code^.next^.kind in [defmark, nullcode, comment]) do begin
	temp_code := cur_code^.next;
	if ((temp_code^.kind in [fullword, realword, doubleword]) and ((ic mod 4) = 0))
	    or ((temp_code^.kind = wordlen) and not odd(ic))
	    or (temp_code^.kind = bytelen)  then begin
	  entry := lookup (int_equivalent (temp_code), temp_code^.kind);
	  if entry <> nil then begin
	    pool_cons (entry^.def_mark, byte_length (temp_code), cur_code);
	    entry^.deleted := true
	  end
	end;
	cur_code := temp_code;
	ic := ic + byte_length (temp_code)
      end
    end;
    cur_link := cur_link^.next_link
  end
end;
$PAGE pool_constants
(* POOL CONSTANTS is the main routine for constant pooling. *)

public procedure pool_constants (initial_ic: unit_range);

var
  i: lwd_table_index;

begin
  if cst_area.first <> nil then begin
    variable_head := nil;
    fixed_head := nil;
    set_head := nil;
    others_head := nil;

    for i := lowerbound (longwd_table) to upperbound (longwd_table) do
      longwd_table[i] := nil;

    (* initialize chains for rethreading constant area by alignment class *)

    s_m_longwords := nil;
    lw_aligned := nil;
    word_aligned := nil;
    unaligned := nil;

    partition;				(* partition constant area *)

    pool_self ( variable_head );			(* pool variable length strings with self *)
    fixed_var_pool;	(* pool fixed strings with variable strings *)
    pool_self ( fixed_head );			(* pool fixed length strings with self *)
    pool_self ( set_head );			(* pool sets with self *)

    align (initial_ic);		(* rearrange cst_area and introduce byte padding
				     to guaranty alignments *)

    long_others_pool				(* pool single longwords (and equivalents)
						   with others partition *)

  end
end.
 . H.