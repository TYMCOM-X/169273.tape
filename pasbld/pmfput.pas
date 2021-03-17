$TITLE pmfput -- routines for call stack manipulation and string output
$LENGTH 43

module output_module;

$INCLUDE pmf.typ
$PAGE pmfstk.typ file
$INCLUDE pmfstk.typ
$PAGE pmfinp.inc file
$INCLUDE pmfinp.inc
$PAGE pmferr.inc file
$INCLUDE pmferr.inc
$PAGE module description
(*  Whenever a macro call is found, procedure 'new_call' will create a new
    call stack entry on the 'collecting' chain.  This entry contains the
    definition of the macro and TDPmber of arguments collected for it
    so far.  When all the arguments for a macro call have been collected,
    procedure 'exp_call' moves its call stack entry from the 'collecting'
    chain to the 'expanding' chain.  When the last of the expansion text
    from a macro call has been read, procedure 'end_call' will delete the
    entry from the call stack.  *)

(*  The call stack is maintained as a doubly-linked list of blocks, each
    representing a single stack entry.  This allows the dynamic expansion
    of the stack and the deletion of arbitrary blocks.  There is also a
    chain pointer in each block, which is used to maintain the 'collecting'
    and 'expanding' chains within the call stack.  *)

(*  The argument stack contains an entry for each argument of each macro
    which is currently collecting arguments.  These are stored as a back-
    linked chain of argument blocks.  When 'exp_call' is called, it will
    copy all the argument entries for the currently expanding macro into
    the 'arg_list' array, and delete them from the linked list.  *)

(*  The evaluation stack is stored as a doubly-linked chain of blocks, each
    containing 'eval_blk_size' characters.  An eval cursor consists of a
    pointer to an eval stack block and an index of a character within that
    block.  An argument stack entry then consists of an eval cursor and the
    length of the argument text.  Eval stack blocks are allocated as needed.
    'Eval_ptr' is a cursor which always points to the next free eval stack
    location.  *)
$PAGE call stack data
var
  eval_ptr: eval_cursor := (nil,0);             (* Top of the eval stack. *)

  arg_ptr: arg_pointer := nil;                (* Top of the argument stack. *)

public var
  call_ptr: call_pointer := nil;              (* Top of the call stack. *)
  chn_collect: call_pointer;                    (* Top-most call now collecting arguments. *)
  chn_expand: call_pointer;                    (* Top-most call now being expanded. *)


(*  When we begin expanding a macro, ', 'exp_call' will store the number of
    arguments which have been collected for it in 'num_args'.  *)

public var
  num_args: arg_index;
$PAGE argument list data
(*  Characters are stored into the evaluation stack by the 'put_string'
    routine when the argument-collection flag is true.  The contents of
    arguments in the eval stack are accessed by a set of procedures and
    functions.  'Arg_length' returns the length of an argument.  'Arg_char'
    returns an indexed character from an argument.  'Arg_store' stores a
    character at an indexed location in an argument.  'Arg_copy' will
    copy a substring of an argument into a substring of a string block,
    and 'arg_back' will copy a substring of an argument back onto the input
    stack.  *)

(*  The argument list array contains argument accessing data for the
    macro call which is currently being expanded.  For each argument,
    'arg_length' is its length.  'Cur_block' is a pointer to some eval
    block containing text for the argument, and 'cur_index' is an index,
    relative to the first character of the argument, of the start of
    'cur_block'.  The location of a given character relative to the
    start of the current block can thus be obtained by subtracting
    'cur_index' from the index of the character in the argument.  The
    resulting index will not necessarily refer to a character in the
    current block of the argument.  *)

var
  arg_list: array [1..max_args] of
    record
      arg_length: string_index;
      cur_block: eval_pointer;
      cur_index: index_range
    end;
$PAGE putinit
public procedure putinit;

  var
    pa: arg_pointer;
    pc: call_pointer;
    pe: eval_pointer;

begin

  (*  Evaluation stack.  *)

  with eval_ptr do begin
    if block = nil then
      begin
	new (block);
	block^.last := nil;
      end
    else
      while block^.last <> nil do begin
	pe := block;
	block := block^.last;
	dispose (pe);
      end;
    block^.next := nil;
    index := 1;
  end;

  (*  Argument stack.  *)

  while arg_ptr <> nil do begin
    pa := arg_ptr;
    arg_ptr := arg_ptr^.link;
    dispose (pa);
  end;

  (*  Call stack.  *)

  while call_ptr <> nil do begin
    pc := call_ptr;
    call_ptr := call_ptr^.down;
    dispose (pc);
  end;
  chn_collect := nil;
  chn_expand := nil;

end (* putinit *);
$PAGE new_call
(*  NEW_CALL sets up a call stack entry on the 'collecting' chain,
    so that we can start collecting arguments for it.  *)

public procedure new_call ( defn: definition );

  var
    call: call_pointer;

begin
  new (call);
  with call^ do begin
    def := defn;
    up := nil;
    down := call_ptr;
    chain := chn_collect;
    n_args := 0;
  end;
  if call_ptr <> nil then
    call_ptr^.up := call;
  call_ptr := call;
  chn_collect := call;

end (* new_call *);
$PAGE new_arg
(*  NEW_ARG is called to make a new entry on the argument stack.  Sub-
    sequent calls to PUT_STRING will add text to this argument.  *)

public procedure new_arg;

  var
    arg: arg_pointer;

begin
  with chn_collect^ do begin
    if n_args = max_args then
      error (er_arg_num);                     (* Too many arguments. *)
    n_args := n_args + 1;
  end;
  new (arg);
  with arg^ do begin
    base := eval_ptr;
    length := 0;
    link := arg_ptr;
  end;
  arg_ptr := arg;
end (* new_arg *);
$PAGE arg_rescan
(*  ARG_RESCAN will copy the text of the most recently scanned
    argument back into the input stack, and reset the argument stack
    entry to a length of zero.  Subsequent macro processing will copy
    the expansion of this argument as the new value of this argument.  *)

public procedure arg_rescan;

  var
    len: string_index;

begin
  with eval_ptr, arg_ptr^ do begin
    while (*eval_ptr.*)block <> base.block do
      with (*eval_ptr.*)block^ do begin
	if (*eval_ptr.*)index > 1 then
	  blk_back ((*eval_ptr.block^.*)text,1,(*eval_ptr.*)index-1);
	  (*eval_ptr.*)block := (*eval_ptr.block^.*)last;
	(*eval_ptr.*)index := eval_blk_lim;
      end;
    len := (*eval_ptr.*)index - (*arg_ptr^.*)base.index;
    (*eval_ptr.*)index := (*arg_ptr^.*)base.index;
    if len <> 0 then
      blk_back ((*eval_ptr.*)block^.text,(*eval_ptr.*)index,len);
      (*arg_ptr^.*)length := 0;
  end;
end (* arg_rescan *);
$PAGE exp_call
(*  EXP_CALL is called when all the arguments for the currently collecting
    macro call have been collected.  The call stack entry for the macro
    will be converted to an 'expanding' entry and placed at the top of the
    expanding chain, and the top-most suspended call (if any) will become
    the currently collecting macro call.  *)

public procedure exp_call;

  var
    pa: arg_pointer;
    pc: call_pointer;
    i: arg_index;

begin

  (*  Save the argument count.  *)

  num_args := chn_collect^.n_args;

  (*  Save the arguments in the static argument list.  *)

  for i := num_args downto 1 do begin
    with arg_list [i], arg_ptr^, base do begin
      arg_length := length;
      cur_block := block;
      cur_index := 1 - index;
    end;
    pa := arg_ptr;
    arg_ptr := arg_ptr^.link;
    dispose (pa);
  end;
  for i := num_args+1 to max_args do
    arg_list[i].arg_length := 0;

  (*  It is safe to reset the eval stack pointer, even though we haven't yet
      used the arguments, because no calls to 'put_string' can be made before
      we have finished with the arguments.  *)

  if num_args <> 0 then
    with eval_ptr, arg_list [1] do begin
      block := cur_block;
      index := 1 - cur_index;
    end;

  (*  Move the call stack entry to the expanding chain.  *)

  pc := chn_collect^.chain;
  chn_collect^.chain := chn_expand;
  chn_expand := chn_collect;
  chn_collect := pc;

end (* exp_call *);
$PAGE end_call
(*  END_CALL is called when the current macro definition has been
    processed, and its call stack entry may be discarded.  *)

public procedure end_call;

  var
    p: call_pointer;

begin
  p := chn_expand;
  with chn_expand^ do begin
    if up <> nil
      then up^.down := down
      else call_ptr := down;
    if down <> nil
      then down^.up := up;
    chn_expand := chain;
  end;
  dispose (p);
end (* end_call *);
$PAGE put_string
(*  PUT_STRING is used to put a string to the current destination.
    This may be either the output file (if arg_switch is false) or
    the top of the eval stack (if arg_switch is true).  Spaces sent
    to the output file are replaced by tabs where appropriate, and
    are deleted at the ends of lines.  Note that the processing
    when arg_switch is true depends on the fact that the longest
    possible parameter string to PUT_STRING is no longer than the
    size of an eval block, so that a string will never have to be
    split across more than two eval blocks.  This is true because
    the strings being passed are tokens, and token_size (see pmfscn)
    is less than eval_blk_size.  *)

public procedure put_string ( s: string_parm );

  var
    len: string_index;                              (* len = length(s) *)
    i:   string_index;                           (* i indexes through the string. *)
    ch:  char;                                     (* ch = s[i] *)
    rem: 0 .. 7;                               (* rem is the distance from the last tab stop. *)
    ctr: string_index;
    part: eval_index;

begin
  if chn_collect <> nil then                 (* Put the string on the eval stack. *)
    begin
      len := length(s);
      with arg_ptr^ do
	length := length + len;                 (* Update argument length. *)
      with eval_ptr do
	if index + len > eval_blk_size then
	  begin                                 (* We will need another block. *)
	    part := eval_blk_lim - index;
	    with block^ do begin
	      text [index:part] := s;           (* Assign the first part. *)
	      if next <> nil then
		block := next                   (* Next block has already been allocated. *)
	      else begin
		new (next);                     (* Allocate another block. *)
		with next^ do begin
		  next := nil;
		  last := block;
		end;
		block := next;
	      end;
	    end;
	    len := len - part;                  (* Assign the remainder. *)
	    if len <> 0 then
	      block^.text [1:len] := substr(s,part+1);
	    index := len + 1;
	  end
	else
	  begin                                 (* The string won't fill the current block. *)
	    block^.text [index:len] := s;
	    index := index + len;
	  end;
    end
  else                                         (* Write the string to the output file. *)
    for i := 1 to length(s) do
      begin
	ch := s[i];
	if ch = eol then 
	  writeln (output)
	else if ch <> eof_ch then 
	  write (output,ch)
      end;
end (* put_string *);
$PAGE arg_locate & arg_length
(*  ARG_LOCATE is called with an index which is known to be within the
    bounds of the specified argument.  It will find the block containing
    the specified character, and will return the index within that block
    of the indexed character.  *)

function arg_locate ( arg: arg_index; i: string_index ): eval_index;

  var ind: index_range;

begin
  with arg_list [arg] do begin
    ind := i - cur_index;
    while ind < 1 do begin
      cur_block := cur_block^.last;
      cur_index := cur_index - eval_blk_size;
      ind := ind + eval_blk_size;
    end;
    while ind > eval_blk_size do begin
      cur_block := cur_block^.next;
      cur_index := cur_index + eval_blk_size;
      ind := ind - eval_blk_size;
    end;
    arg_locate := ind;
  end;
end (* arg_index *);



(*  ARG_LENGTH will return the length of an argument.  *)

public function arg_length ( arg: arg_index ): string_index;

begin
  arg_length := arg_list[arg].arg_length;
end (* arg_length *);
$PAGE arg_char & arg_store
(*  ARG_CHAR will return the indexed character of the specified argument, or
    will return 'etx' if the index is outside the bounds of the arhument.  *)

public function arg_char ( arg: arg_index; i: string_index ): char;

  var ind: eval_index;

begin
  with arg_list [arg] do
    if (i = 0) or (i > arg_length) then
      arg_char := etx
    else begin
      ind := arg_locate(arg,i);
      arg_char := cur_block^.text [ind];
    end;
end (* arg_char *);



(*  ARG_STORE will store the specified character at the indexed
    location in the specified argument.  *)

public procedure arg_store ( arg: arg_index; i: string_index; c: char );

  var ind: eval_index;

begin
  with arg_list [arg] do begin
    if (i = 0) or (i > arg_length) then
      error (er_arg_ind);                       (* Program error--bad argument index. *)
    ind := arg_locate(arg,i);
    cur_block^.text [ind] := c;
  end;
end (* arg_store *);
$PAGE arg_copy
(*  ARG_COPY will copy a subrange of an argument into a subrange of a
    string block.  *)

public procedure arg_copy ( arg: arg_index;
			    i, len: string_index;
			    dest: str_pointer;
			    d_i: string_index );

  var
    ind: eval_index;                           (* Argument eval block index. *)
    d_ind: string_index;                      (* Destination string index. *)
    r_len: string_index;                       (* Length remaining to be copied. *)
    mv_len: eval_index;                           (* Length to be copied in this step. *)

begin
  if len = 0 then return;
  with arg_list [arg] do begin
    if (i = 0) or (i+len > arg_length + 1) then
      error (er_arg_ind);                   (* Program error--bad argument index. *)
    ind := arg_locate(arg,i);
    d_ind := d_i;
    r_len := len;
    with dest^ do begin
      while ind+r_len > eval_blk_lim do begin
	mv_len := eval_blk_lim - ind;
	str_text [d_ind:mv_len] := substr(cur_block^.text,ind);
	cur_block := cur_block^.next;
	cur_index := cur_index + eval_blk_size;
	ind := 1;
	d_ind := d_ind + mv_len;
	r_len := r_len - mv_len;
      end;
      str_text [d_ind:r_len] := substr(cur_block^.text,ind);
    end;
  end;
end (* arg_copy *);
$PAGE arg_back
(*  ARG_BACK will copy the specified subrange of an argument back onto the
    input stack, making calls to BLK_BACK to copy the text of the indivi-
    dual blocks containing the argument.  The blocks must be copied from
    last to first, due to the way strings are put back onto the input
    stack.  *)

public procedure arg_back ( arg: arg_index; i, len: integer );

  var
    r_len: integer;                         (* Length remaining to be copied. *)
    ind: integer;                           (* Argument eval block index. *)

begin
  with arg_list [arg] do begin
    if i >= 1
      then ind := i - 1
      else ind := 0;
    if ind + len <= arg_length
      then r_len := len
      else r_len := arg_length - ind;
    if r_len <= 0 then return;
    ind := arg_locate(arg,ind+r_len);
    while r_len > ind do begin
      blk_back (cur_block^.text,1,ind);
      cur_block := cur_block^.last;
      cur_index := cur_index - eval_blk_size;
      r_len := r_len - ind;
      ind := eval_blk_size;
    end;
    if r_len <> 0 then
      blk_back (cur_block^.text,ind-r_len+1,r_len);
  end;
end (* arg_back *).
  