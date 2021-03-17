$TITLE p10csp - constant pooling
$LENGTH 42
(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P 1 0 C S P                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  P10CSP contains all routines  necessary  for  constant
        pooling.
     
     ENTRY POINTS:
     
        pool_constants
                    pools the constants in the constant area.
     
     ALGORITHM:   The  constant  area  is first partitioned into five
        sub-areas:  single fullword constants, fixed length  strings,
        variable   length   strings,  sets  and  everything  else  or
        'others'.  The  partitioning  is  done  as  follows.   Single
        fullwords  are  entered  into  a  hash  table,  the 'fullword
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
     
        The actual pooling is done in seven separate steps:
          1. Single  fullword  constants  are  pooled as the fullword
             table is created.
          2. Variable string  length  words  are  looked  up  in  the
             fullword table; any entries found are deleted and pooled
             with the string length words.
          3. Fixed length  strings  are  pooled  with  themselves  by
             testing  strings  of  the  same  length  (in  words) for
             equality.
          4. Variable length strings are pooled  with  themselves  by
             testing  strings  of  the  same  length  (in  words) for
             equality.
          5. Sets are pooled with themselves by testing sets  of  the
             same length (in words) for equality.
          6. Fixed  length  strings  are  pooled with variable length
             strings by testing strings of the same length (in words)
             for equality.
          7. Single fullwords are pooled with the others partition by
             looking up each word of  each  others  constant  in  the
             fullword  table.  Any  entries  found  are  deleted  and
             pooled with the others constant.
     
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
     
        The fact that only  single  fullwords  are  pooled  with  the
        others partition has been exploited.  The length of constants
        in the others partition is not available in the others  list.
        If  the  others  partition  was  to  be pooled with itself or
        another partition then the partiioning pass would have to  be
        modified  to  store  the  length  of  others constants in the
        others list.
     
     ---------------------------------------------------------------- *)

module p10csp;
$PAGE includes, forward declarations
$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE p10cg.typ
$INCLUDE p10cgu.inc
$PAGE declarations
(* Fullword hash table declarations.  The table is represented by an
   array of pointers to entry records.  The entry records are created
   dynamically as necessary.  Collisions are resolved via chaining. *)

const
  fwd_tab_max := 228;				(* table size (a prime) minus one *)

type
  fwd_table_index = 0..fwd_tab_max;
  entry_ptr = ^fullwd_entry;
  fullwd_entry = packed record			(* table entries *)
    overflow: entry_ptr;			(* collision list *)
    def_mark: code;				(* defmark code record for constant *)
    deleted: boolean				(* entry deleted flag *)
  end;

var
  fullwd_table: packed array [fwd_table_index] of entry_ptr;


(* Partition lists declarations.  SIZE and LINK records are used to
   partition the constant area by linking together all fixed length 
   strings, or all variable length strings or all sets.  SIZE records
   are linked together into an ordered list.  Three such lists exist -
   for fixed and variable strings and for sets.  Each size record serves
   as list head for a list of LINK records.  The LINK records point to
   the DEFMARK code record which begins a constant of the relevant type.
   The constant's length in words is that given in the size record
   heading the list. *)

type
  size = ^size_rec;
  link = ^link_rec;
  size_rec = packed record
    num_recs: unit_range;			(* length in words of constants, excludes defmark *)
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
  dreal_head: size;				(* double real list head *)


(* OTHERS_HEAD is the list head for an unordered list of link records
   which link those constants which fall into the 'others' partition. *)

var
  others_head: link;
$PAGE init_pool
(* INIT POOL performs all initialization necessary for constant pooling. *)

procedure init_pool;

var
  i: fwd_table_index;

begin
  variable_head := nil;
  fixed_head := nil;
  set_head := nil;
  dreal_head := nil;
  others_head := nil;
  
  for i := lowerbound (fullwd_table) to upperbound (fullwd_table) do
    fullwd_table[i] := nil
end;
$PAGE lookup_in_fwd_tab
(* LOOKUP looks up PDP10WORD KEY in the fullword table.  A pointer
   to the table entry is returned if the word is found otherwise NIL 
   is returned.  *)

function lookup_in_fwd_tab ( key: pdp10word ): entry_ptr;

var
  index: fwd_table_index;
  found: boolean;
  cur_code: code;

begin
  index := abs ( key.value mod (fwd_tab_max + 1) );
  lookup_in_fwd_tab := fullwd_table[index];
  found := false;
  while (lookup_in_fwd_tab <> nil) andif not found do begin
    with lookup_in_fwd_tab^ do begin
      if not deleted then begin
        cur_code := def_mark^.next;

        (* may be multiple DEFMARK code records due to previous pooling *)

        while cur_code^.kind = defmark do cur_code := cur_code^.next;
        found := cur_code^.fwd.value = key.value
      end;
      if not found then lookup_in_fwd_tab := overflow
    end (* with *);
  end (* while *);
end;
$PAGE enter
(* ENTER creates a new entry in the fullwood table.  An entry is
   created for the constant beginning with the DEFMARK code record
   pointed at by DEF_CODE. This routine assumes that the code record
   following DEF_CODE (via the NEXT field) is the code record containing
   the fullword value. *)

procedure enter ( def_code: code );

var
  index: fwd_table_index;
  new_entry: entry_ptr;
  key: pdp10word;

begin
  key := def_code^.next^.fwd;
  index := abs ( key.value mod (fwd_tab_max + 1) );
  new ( new_entry );
  with new_entry^ do begin
    def_mark := def_code;
    deleted := false;
    overflow := fullwd_table[index];
    fullwd_table[index] := new_entry
  end
end;
$PAGE pool_cons
(* POOL CONS pools a given constant with a portion of another.
   The given or 'old' constant is specified by OLD_DEFMARK and
   OLD_LENGTH.  OLD_DEFMARK must point to the first defmark code
   record preceding the old constant.  OLD_LENGTH is the length of 
   the old constant in words;  defmark records are not included in
   the length.  PREV_CODE designates the constant to be pooled with,
   the 'new' constant.  PREV_CODE must point to the code record previous 
   to the code record actually containing the constant.  This is
   necesary because a new defmark code record is created and inserted
   after PREV_CODE.  This is set to point to the definition node of the
   old constant.  New defmark records are also created for any defmark
   records embedded in the old constant.  This can occur if the old
   constant previously had other constants pooled with it. *)

procedure pool_cons (old_defmark: code; old_length: unit_range;
                      prev_code: code);

var
  cur_code_old: code;				(* current code record of old constant *)
  cur_code_new: code;				(* current code record of new constant *)
  length_count: unit_range;			(* counts non-defmarks in old constant *)
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
        kind := defmark;
        defname := cur_code_old^.defname
      end
    end

    (* if not a defmark, then increment count and advance 'new' constant
       pointer one non-defmark code record. *)

    else begin
      length_count := length_count + 1;
      repeat
        cur_code_new := cur_code_new^.next
      until cur_code_new^.kind <> defmark
    end;
    
    cur_code_old^.kind := nullcode;
    cur_code_old := cur_code_old^.next
  end
end;
$PAGE partition
(* PARTITION is used by POOL_CONSTANTS to partition the constant area
   into five areas: single fullwords, fixed length strings, variable
   length strings, sets and anything else ('others').  Single FULLWORDs
   are pooled as they are entered into a table.  Fixed length strings,
   variable length strings and sets each have a structure, called a 
   partition list, created which links constants of the same size into
   sublists.  Constants in the 'others' category are linked into a 
   single linked list.  The area is passed in as a parameter but is
   assumed to contain only FULLWORDs, STRINGWORDs, SETWORDs and
   DEFMARKs.  The area list is traversed once; each constant 
   encountered is linked to the appropriate partition. *)

procedure partition (var area_list: code_list);

var
  def_mark: code;				(* pointer to DEFMARK code record which
						   begins a constant *)
  cur_code: code;				(* curent code record being examined within
						   current constant *)
  num_cwords: unit_range;			(* number of code records for current constant,
						   excluding defmark *)
$PAGE fullwd_tab - in partition 
(* FULLWD_TAB enters a single FULLWORD constant into the fullword table.
   Duplicate constants are pooled with the constant previously entered.
   DEF_CODE is the DEFMARK code record for the constant.  This routine
   assumes that the code record following DEF_CODE, via the NEXT field,
   contains the fullword value. *)

procedure fullwd_tab (def_code: code);

var
  entry: entry_ptr;				(* duplicate entry, if any *)
  fwd_code: code;				(* pointer to the FULLWORD code record *)

begin
  fwd_code := def_code^.next;
  while fwd_code^.kind = defmark do
    fwd_code := fwd_code^.next;
  entry := lookup_in_fwd_tab (fwd_code^.fwd);
  if entry = nil 				(* if record wasn't already in table *)
    then enter ( def_code )			(* then just enter it *)
    else pool_cons (def_code, 1, entry^.def_mark)	(* else pool with table entry *)
end;
$PAGE build_par - in partition 
(* BUILD PAR adds a constant to the partition list headed by PAR_LIST.
   The constant is added to the size list corresponding to constants
   of length NUM_CWORDS.  The DEFMARK code record for the constant is
   pointed to by DEF_MARK. *)

procedure build_par (var par_list:size; def_mark: code; num_cwords: unit_range);

var
  size_rec: size;				(* pointer to size record *)
  cur_size: size;				(* size record whose size we are examining *)
  last_size: size;				(* size record previous to CUR_SIZE *)
						(* - for list insertion *)
  new_link: link;				(* pointer to new link record created *)

begin
  
  (* Find SIZE record of correct size.  If none exists create one and
     link into the SIZE list. *)

  if (par_list = nil) orif (num_cwords < par_list^.num_recs) then begin
    new ( size_rec );
    size_rec^.first_link := nil;
    size_rec^.next_size := par_list;
    par_list := size_rec
  end
  else begin					(* search ordered SIZE list *)
    cur_size := par_list^.next_size;
    last_size := par_list;
    while (cur_size <> nil) andif (cur_size^.num_recs <= num_cwords) do begin
      last_size := cur_size;
      cur_size := cur_size^.next_size
    end;
    if last_size^.num_recs <> num_cwords then begin
      new ( size_rec );				(* no match - create new size *)
      size_rec^.next_size := cur_size;		(* record and insert in middle or *) 
      last_size^.next_size := size_rec;		(* at end of SIZE list *)
      size_rec^.first_link := nil
    end
    else begin					(* size record of right size already exists *)
      size_rec := last_size
    end
  end;
    
  size_rec^.num_recs := num_cwords;

  (* Create a link record for the new constant and insert it at the
     beginning of the list headed by SIZE_REC. *)

  new ( new_link );
  new_link^.next_link := size_rec^.first_link;
  size_rec^.first_link := new_link;
  new_link^.first_code := def_mark

end (* procedure build_par *);
$PAGE others_par - in partition 
(* OTHERS PAR creates a new link on the others partition list for
   a constant in the 'others' category.  DEF_MARK points to the defmark
   code record for the constant.  CUR_CODE is a VAR parameter which
   initially points to some part of the constant beyond the initial
   defmark.  It is set to point to the defmark record following the
   constant.  *)

procedure others_par (def_mark: code; var cur_code: code);

var
  new_link: link;

begin
  new (new_link);
  with new_link^ do begin
    next_link := others_head;
    first_code := def_mark
  end;
  others_head := new_link;
  while (cur_code <> nil) andif (cur_code^.kind <> defmark) do
    cur_code := cur_code^.next
end;
$PAGE partition - main routine
(* Begin body of partition *)

begin
  cur_code := area_list.first;

  (* Process one constant per iteration of while loop. *)

  while cur_code <> nil do begin		
    def_mark := cur_code;
    while (cur_code <> nil) andif (cur_code^.kind = defmark) do
      cur_code := cur_code^.next;
    if cur_code = nil then begin (* had trailing labels (to null objects) *)
      gen_word (area_list, ('F', 0), fullword); (* fake a target for the labels *)
      cur_code := area_list.last
    end;
    num_cwords := 0;
    case cur_code^.kind of

      fullword, realword:
        begin
          cur_code := cur_code^.next;
          if cur_code = nil then begin
            fullwd_tab ( def_mark )
          end
          else begin
	    num_cwords := 1;
	    case cur_code^.kind of

	      defmark:  fullwd_tab ( def_mark );	(* single fullword *)

	      stringword:				(* variable string or others *)
		begin
		  while (cur_code <> nil) andif (cur_code^.kind = stringword) do begin
		    cur_code := cur_code^.next;
		    num_cwords := num_cwords + 1
		  end;
		  if (cur_code = nil) orif (cur_code^.kind = defmark)
		    then build_par (variable_head, def_mark, num_cwords)
		    else others_par (def_mark, cur_code)
		end;

	      others:  others_par (def_mark, cur_code)  (* others *)

	    end (* inner case *);
          end (* if *);
        end (* fullword case of outer case *);

	drealword:
	  begin
	    cur_code := cur_code^.next;
	    assert (cur_code <> nil);
	    assert (cur_code^.kind = drealword2);
	    cur_code := cur_code^.next;
	    if (cur_code = nil) orif (cur_code^.kind = defmark)
	      then build_par (dreal_head, def_mark, 2)
	      else others_par (def_mark, cur_code);
	  end;

        stringword:
          begin
            while (cur_code <> nil) andif (cur_code^.kind = stringword) do begin
              cur_code := cur_code^.next;
              num_cwords := num_cwords + 1
            end;
            if (cur_code = nil) orif (cur_code^.kind = defmark)
              then build_par (fixed_head, def_mark, num_cwords)
              else others_par (def_mark, cur_code)
          end;

        setword:
          begin
            while (cur_code <> nil) andif (cur_code^.kind = setword) do begin
              cur_code := cur_code^.next;
              num_cwords := num_cwords + 1
            end;
            if (cur_code = nil) orif (cur_code^.kind = defmark)
         then build_par (set_head, def_mark, num_cwords)
              else others_par (def_mark, cur_code)
          end;

        others: assert (false)				(* can't happen !?! *)

      end (* outer case *);
    end (* while *);
  end (* procedure partition *);
$PAGE pool_fullword
(* POOL FULLWORD attempts to pool a single fullword constant with
   an arbitrary fullword code record.  The parameter PREV_CODE
   is the code record previous to (via the NEXT field) the arbitrary
   fullword code record (defmark code records may be present between
   PREV_CODE and the fullword).  This is necessary because if pooling does
   take place a defmark code record must be inserted before the
   arbitrary fullword code record.  The arbitrary fullword is looked
   up in the fullword table.  If an entry is found then the single 
   fullword is pooled with the arbitrary fullword and the fullword
   table entry is marked as deleted. *)

procedure pool_fullword (prev_code: code);

var
  entry: entry_ptr;
  cur_code: code;

begin
  cur_code := prev_code;
  while cur_code^.next^.kind = defmark do cur_code := cur_code^.next;
  entry := lookup_in_fwd_tab (cur_code^.next^.fwd);
  if entry <> nil then begin
    pool_cons (entry^.def_mark, 1, cur_code);
    entry^.deleted := true
  end
end;
$PAGE var_len_pool
(* VAR LEN POOL pools single fullword constants with the length field
   of variable length strings.  The variable length string partition list 
   is traversed.  For each length word, POOL_FULLWORD is called to
   attempt to pool a single fullword constant with the length word.
   VARIABLE_HEAD is the list head for the variable length string partition
   list.  *)

procedure var_len_pool ( variable_head: size );

var
  cur_size: size;
  cur_link: link;

begin
  cur_size := variable_head;
  while cur_size <> nil do begin		(* traverse SIZE list *)
    cur_link := cur_size^.first_link;
    while cur_link <> nil do begin		(* traverse LINK sublist *)
      if cur_link^.first_code^.kind <> nullcode
        then pool_fullword ( cur_link^.first_code );
      cur_link := cur_link^.next_link
    end;
    cur_size := cur_size^.next_size
  end
end;
$PAGE pool_link_list
(* POOL LINK LIST attempts to pool the constants pointed at from a
   LINK list with all or part of an arbitrary constant.  FIRST_LINK
   is the first link record in the LINK list.  PREV_CODE is the code 
   record previous to the code record beginning the arbitrary constant.
   The constants pointed at from the LINK list are assumed to be of 
   size CONST_SIZE (the size is the length in words of the constant
   value - defmark records are excluded).  *)

procedure pool_link_list (prev_code: code; first_link: link;
                          const_size: unit_range);

var
  cur_link: link;
  cur_link_word: code;
  code_rec_word: code;
  length_count: unit_range;
  equal: boolean;

begin
  cur_link := first_link;
  while cur_link <> nil do begin		(* process 1 link record per iteration *)
    if cur_link^.first_code^.kind <> nullcode then begin
      code_rec_word := prev_code;
      cur_link_word := cur_link^.first_code;
      length_count := 0;
      equal := true;
      while (length_count < const_size) andif equal do begin	(* compare constants *)
        repeat					(* advance a code record, ignoring defmarks *)
          cur_link_word := cur_link_word^.next
        until cur_link_word^.kind <> defmark;
        repeat 
          code_rec_word := code_rec_word^.next
        until code_rec_word^.kind <> defmark;
        equal := cur_link_word^.fwd.value = code_rec_word^.fwd.value;
        length_count := length_count + 1
      end;
      if equal then pool_cons (cur_link^.first_code, const_size, prev_code);
    end;
    cur_link := cur_link^.next_link
  end
end;
$PAGE pool_self
(* POOL SELF pools a partition list, specified by LIST_HEAD, with
   itself.  The pooling is done by testing constants of the same
   length for equality. *)

procedure pool_self ( list_head: size );

var
  cur_size: size;
  cur_link: link;
  code_rec: code;
  cur_length: unit_range;

begin
  cur_size := list_head;
  while cur_size <> nil do begin
    cur_length := cur_size^.num_recs;
    cur_link := cur_size^.first_link;
    while cur_link <> nil do begin
      if cur_link^.first_code^.kind <> nullcode then begin
        code_rec := cur_link^.first_code;
        while code_rec^.next^.kind = defmark do code_rec := code_rec^.next;
        pool_link_list (code_rec, cur_link^.next_link, cur_length)
      end (* if *);
      cur_link := cur_link^.next_link
    end (* inner while *);
    cur_size := cur_size^.next_size;
  end (* outer while *);
end;
$PAGE fixed_var_pool
(* FIXED VAR POOL pools the fixed length string partition with
   the variable length string partition.  The pooling is done by 
   testing strings of the same length for equality.  FIXED_HEAD and
   VARIABLE_HEAD are the list heads for the fixed and variable length
   string partitions respectively. *)

var 
  cur_fsize: size;				(* current fixed string SIZE record *)
  cur_vsize: size;				(* current variable string SIZE record *)
  cur_link: link;				(* link record for current variable string *)
						(* which we are trying to pool with *)
  len_word: code;				(* length word of variable length string *)
  fixed_link: link;				(* first link record for SIZE list *)
						(* headed by CUR_FSIZE *)
  cur_length: unit_range;			(* size in words of current string, *)
						(* excludes length word and defmarks *)

procedure fixed_var_pool (fixed_head: size; variable_head: size);

begin
  cur_fsize := fixed_head;
  cur_vsize := variable_head;
  while (cur_fsize <> nil) andif (cur_vsize <> nil) do begin
    cur_length := cur_fsize^.num_recs;

    (* find variable length string size record for strings with the 
       same number of words, if one exists *)

    while (cur_vsize <> nil) andif ( (cur_vsize^.num_recs - 1) < cur_length) do
      cur_vsize := cur_vsize^.next_size;
    if (cur_vsize <> nil) andif (cur_length = (cur_vsize^.num_recs - 1) ) then begin
      cur_link := cur_vsize^.first_link;	(* found matching size record *)
      fixed_link := cur_fsize^.first_link;
      while cur_link <> nil do begin
        if cur_link^.first_code^.kind <> nullcode then begin
          len_word := cur_link^.first_code;
          while len_word^.kind = defmark do len_word := len_word^.next;
          pool_link_list (len_word, fixed_link, cur_length)
        end;
        cur_link := cur_link^.next_link
      end
    end;
    cur_fsize := cur_fsize^.next_size
  end
end;
$PAGE full_others_pool
(* FULL OTHERS POOL attempts to pool fullword table entries with 
   each word of each constant in the 'others' partition.  It is 
   important to note that this routine assumes the 'others' partition
   has not had anything pooled with it previously.  This assumption
   is made because the size of a constant in the others partition is
   not known.  The end of a constant is assumed to be marked by the
   first defmark encountered after the initial one. *)

procedure full_others_pool ( others_head: link );

var
  cur_link: link;				(* LINK record pointing to current 'others' constant *)
  cur_code: code;				(* current code record within current 'others' constant *)

begin
  cur_link := others_head;
  while cur_link <> nil do begin		(* process 1 'others' constant per iteration *)
    cur_code := cur_link^.first_code;
    if cur_code^.kind <> nullcode then begin
      while (cur_code^.next <> nil) andif
	    (cur_code^.next^.kind <> defmark) andif
	    (cur_code^.next^.kind <> nullcode) do begin
	pool_fullword ( cur_code );
	cur_code := cur_code^.next
      end;
    end;
    cur_link := cur_link^.next_link
  end
end;
$PAGE pool_constants
(* POOL CONSTANTS is the main routine for constant pooling. *)

public procedure pool_constants;

begin
  init_pool;					(* initialize *)
  partition (cst_area);				(* partition constant area *)
  var_len_pool (variable_head);			(* pool fullwords with string length words *)
  fixed_var_pool ( fixed_head, variable_head );	(* pool fixed strings with variable strings *)
  pool_self ( fixed_head );			(* pool fixed length strings with self *)
  pool_self ( variable_head );			(* pool variable length strings with self *)
  pool_self ( set_head );			(* pool sets with self *)
  pool_self ( dreal_head );			(* pool double reals with self *)
  full_others_pool ( others_head )		(* pool single fullwords with others partition *)
end.
  3M'Ü