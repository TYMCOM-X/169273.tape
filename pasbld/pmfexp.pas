$TITLE pmfexp -- macro expansion routines
$LENGTH 43

module expand_module;

$INCLUDE pmf.typ
$PAGE pmfcmd.inc file
$INCLUDE pmfcmd.inc
$PAGE pmfinp.inc file
$INCLUDE pmfinp.inc
$PAGE pmfput.inc file
$INCLUDE pmfput.inc
$PAGE pmfdef.inc file
$INCLUDE pmfdef.inc
$PAGE pmferr.inc file
$INCLUDE pmferr.inc


(* Local Type *)

type
  kind_set = set of macro_kinds;
$PAGE built-in macro definitions
public procedure expinit;

begin
  define ('#DEFINE',def_mac,nil);
  define ('#LITERAL',lit_mac,nil);
  define ('#ASSIGN',asgn_mac,nil);
  define ('#UNDEF',undef_mac,nil);
  define ('#OPSYN',opsyn_mac,nil);
  define ('#NUM',num_mac,nil);
  define ('#IF',if_mac,nil);
  define ('#EQN',eqn_mac,nil);
  define ('#NEN',nen_mac,nil);
  define ('#LTN',ltn_mac,nil);
  define ('#LEN',len_mac,nil);
  define ('#GTN',gtn_mac,nil);
  define ('#GEN',gen_mac,nil);
  define ('#EQC',eqc_mac,nil);
  define ('#NEC',nec_mac,nil);
  define ('#LTC',ltc_mac,nil);
  define ('#LEC',lec_mac,nil);
  define ('#GTC',gtc_mac,nil);
  define ('#GEC',gec_mac,nil);
  define ('#SUBSTR',substr_mac,nil);
  define ('#INDEX',index_mac,nil);
  define ('#SEARCH',search_mac,nil);
  define ('#VERIFY',verify_mac,nil);
  define ('#LENGTH',length_mac,nil);
  define ('#UPC',upc_mac,nil);
  define ('#LWC',lwc_mac,nil);
  define ('#AND',and_mac,nil);
  define ('#OR',or_mac,nil);
  define ('#NOT',not_mac,nil);
  define ('#SAVE',save_mac,nil);
  define ('#LIB',lib_mac,nil);
  define ('#STR',str_mac,nil);
  define ('#EVAL',eval_mac,nil);
  define ('#ERROR',error_mac,nil);
  define ('#MACRO',macro_mac,nil);
  define ('#PASCAL',pascal_mac,nil);
  define ('#NOPASCAL',nopascal_mac,nil);
end;
$PAGE get_name
(*  GET_NAME returns a selected parameter from the argument list.  It
    is converted to upper case and checked to make sure that it is a
    legal symbol.  *)

function get_name ( n: arg_index ): sym_string;

  var
    ind1: string_index;
    ind2: string_index;
    i: string_index;
    len: string_index;
    buf: packed array [1..sym_size] of char;
    temp_char: char;

begin
  if arg_length(n) = 0 then
    error (er_bad_sym);                            (* Bad symbolic parameter. *)
  ind1 := 1;
  temp_char := arg_char(n,ind1);
  while (temp_char = space) or (temp_char = eol) or (temp_char = tab) do begin
    ind1 := ind1 + 1;
    temp_char := arg_char(n,ind1);
  end;
  ind1 := ind1 - 1;
  ind2 := arg_length(n);
  temp_char := arg_char(n,ind2);
  while (temp_char = space) or (temp_char = eol) or (temp_char = tab) do begin
    ind2 := ind2 - 1;
    temp_char := arg_char(n,ind2);
  end;
  len := ind2 - ind1;
  if (len = 0) or (len > sym_size) then
    error (er_bad_sym);                           (* Bad symbolic parameter. *)
  for i := 1 to len do
    buf [i] := uppercase(arg_char(n,ind1+i));
  get_name := substr(buf,1,len);
  if (not (get_name[1] in alphas)) or
    (verify(get_name,alphanumerics) <> 0) then
    error (er_bad_sym);                        (* Bad symbolic parameter. *)
end (* get_name *);
$PAGE def_str
(*  DEF_STR is called when the arguments for a system '#DEFINE' or
    '#ASSIGN' macro call have been stored on the stack, and the
    definition text for the definition node is to be generated.
    The first argument is the macro name, the last is the macro
    definition text, and any intermediate arguments are parameter
    names.  *)

function def_str: str_pointer;

  var
    def_length: string_index;
$PAGE def_str: mark_args
  (*  MARK_ARGS will scan through the macro definition text.  All
      occurrences of parameter names will be replaced by an argument
      marker and zero or more nulls, and the parameter number will be
      pushed onto the input stack.  Parameter delimiters which are
      adjacent to parameter names will also be converted to nulls.
      The resulting definition text length, counting two for each
      argument marker and nothing for nulls, will be saved.  *)

  procedure mark_args;

    var
      i,
      name_loc: string_index;
      c: char;
      arg: arg_index;
      parm_names: array [arg_index] of sym_string;
      buf: packed array [1..sym_size] of char;
      count: 1 .. sym_size;

  begin
    for arg := 2 to num_args - 1 do
      parm_names [arg-1] := get_name (arg);
    def_length := arg_length(num_args);
    i := 1;
    c := uppercase(arg_char(num_args,1));
    while c <> etx do
      if c in alphas then
	begin

	  (*  Pick up an alphanumeric symbol.  *)

	  count := 1;
	  buf [1] := c;
	  name_loc := i;
	  i := i + 1;
	  c := uppercase(arg_char(num_args,i));
	  while c in alphanumerics do begin
	    if count <> sym_size then begin
	      count := count + 1;
	      buf [count] := c;
	    end;
	    i := i + 1;
	    c := uppercase(arg_char(num_args,i));
	  end;

	  (*  Look up the symbol in the parameter name list.  *)

	  parm_names [0] := substr(buf,1,count);
	  arg := num_args - 2;
	  while parm_names [arg] <> parm_names [0] do
	    arg := arg - 1;
	  if arg <> 0 then
	    begin

	      (*  This is a parameter name, so mark the reference.  *)

		 (*  Null out any adjacent parameter delimiters.  *)

	      if (name_loc <> 1) andif
	      (arg_char (num_args,name_loc-1) = arg_delimiter) then begin
		arg_store (num_args,name_loc-1,nul);
		def_length := def_length - 1;
	      end;
	      if c = arg_delimiter then begin
		arg_store (num_args,i,nul);
		def_length := def_length - 1;
	      end;

	      (*  Mark the parameter reference, and null out the rest
	       of the name.  *)

	      arg_store (num_args,name_loc,arg_marker);
	      def_length := def_length - (i - name_loc) + 2;
	      for name_loc := name_loc + 1 to i - 1 do
		arg_store (num_args,name_loc,nul);

	      (*  Save the parameter number in the input stack.  *)

	      put_back (chr(arg+ord(first_arg)));
	    end;
	end
      else
	begin                                        (* Step over a non-alphabetic character. *)
	  i := i + 1;
	  c := uppercase(arg_char(num_args,i));
	end;
  end (* mark_args *);
$PAGE def_str: make_definition
  (*  MAKE_DEFINITION will return a pointer to a string block containing
      the completed definition text for the macro.  This is obtained by
      scanning the definition text backwards, saving blocks of text and
      argument references.  Examination of the 'user_macro' code in EXPAND
      should clarify the utility of this format in macro expansion.  *)

  function make_definition: str_pointer;

    var
      def_blk: str_pointer;
      i : string_index;
      blk_len,
      str_ind: eval_index;
      c: char;

  begin
    new (def_blk,def_length);
    make_definition := def_blk;
    with make_definition^ do begin
      if length(str_text) = 0  then return;
      blk_len := 0;
      str_ind := 1;
      for i := arg_length(num_args) downto 1 do begin
	c := arg_char (num_args,i);
	if (c = arg_marker) orif (c = nul) then
	  begin                                   (* End a block and insert a marker. *)
	    if blk_len <> 0 then begin            (* Copy a block. *)
	      arg_copy (num_args,i+1,blk_len,def_blk,str_ind);
	      str_ind := str_ind + blk_len;
	      blk_len := 0;
	    end;
	    if c = arg_marker then begin          (* Insert an argument marker. *)
	      str_text [str_ind] := arg_marker;
	      str_text [str_ind + 1] := get_char(c);
	      str_ind := str_ind + 2;
	    end;
	  end
	else
	  blk_len := blk_len + 1;                 (* Extend the block. *)
      end;
      if blk_len <> 0 then                     (* Copy the last block. *)
	arg_copy (num_args,1,blk_len,def_blk,str_ind);
    end;
  end (* make_definition *);
$PAGE def_str: main function body

  var
    def: str_pointer;
    len: string_index;

begin
  if num_args = 1 then                          (* Definition is empty. *)
    begin
      new (def,0);
      def^.str_text := '';
      def_str := def;
    end
  else if num_args = 2 then                      (* Definition is a literal string. *)
    begin
      len := arg_length(2);
      new (def,len);
      arg_copy (2,1,len,def,1);
      def_str := def;
    end
  else                                            (* Definition is parameterized. *)
    begin
      mark_args;
      def_str := make_definition();
    end;
end (* def_str *);
$PAGE eval_num
(*  EVAL_NUM will evaluate the specified argument as a constant numeric
    expression, and will return its value.  *)

function eval_num ( arg: arg_index ): integer;

  var
    i: string_index;
    c: char;

  procedure nextc;                          (* Gets the next non-spacing character. *)

  begin
    repeat
      i := i + 1;
      c := arg_char(arg,i);
    until (c <> space) and (c <> eol) and (c <> tab);
  end (* nextc *);

  function expr: integer;

    function term: integer;

      var fac1: integer;

      function factor: integer;
      begin
	if c = '(' then begin
	  nextc;
	  factor := expr();
	  if c <> ')' then
	    error (er_bad_num);
	  nextc;
	end
	else begin
	  factor := 0;
	  while c in ['0'..'9'] do begin
	    factor := factor*10 + ord(c)-ord('0');
	    i := i + 1;
	    c := arg_char(arg,i);
	  end;
	  if (c = space) or (c = eol) or (c = tab) then
	    nextc;
	end;
      end (* factor *);

    begin (* term *);
      term := factor();
      while c in ['*','/'] do
	if c = '*' then
	  begin
	    nextc;
	    term := term * factor();
	  end
	else
	  begin
	    nextc;
	    fac1 := factor();
	    if fac1 = 0
	      then term := 0
	      else term := term div fac1;
	  end;
    end (* term *);

  begin (* expr *);
    expr := term();
    while c in ['+','-'] do
      if c = '+' then
	begin
	  nextc;
	  expr := expr + term();
	end
      else
	begin
	  nextc;
	  expr := expr - term();
	end;
  end (* expr *);

$PAGE eval_num:  main fxn body
begin (* eval_num *);
  i := 0;
  nextc;
  eval_num := expr();
  if c <> etx then
    error (er_bad_num);
end (* eval_num *);
$PAGE num_back
(*  NUM_BACK will convert an integer to a character string and write
    it back onto the input stack.  *)

procedure num_back ( n: integer );

  var
    num: integer;
    neg: boolean;

begin
  neg := (n < 0);
  num := abs(n);
  if num = 0 then
    put_back ('0')
  else
    while num <> 0 do begin
      put_back (chr((num mod 10)+ord('0')));
      num := num div 10;
    end;
  if neg then
    put_back ('-');
end (* num_back *);
$PAGE rel_c_kinds
(*  REL_C_KINDS will return a set of the character relational macro
    call kinds which are satisfied by the relation between the first
    and second arguments of the current macro call.  Note that ETX is
    less than any character which can appear in an argument, so that
    a shorter argument will always be less than a longer argument
    which begins with the same characters.  *)

function rel_c_kinds: kind_set;

  var
    ind: string_index;
    c1: char;
    c2: char;

begin
  ind := 1;
  loop
    c1 := arg_char(1,ind);
    c2 := arg_char(2,ind);
  exit if c1 < c2   do rel_c_kinds := [ltc_mac,lec_mac,nec_mac];
  exit if c1 > c2   do rel_c_kinds := [gtc_mac,gec_mac,nec_mac];
  exit if c1 = etx  do rel_c_kinds := [eqc_mac,lec_mac,gec_mac];
    ind := ind + 1;
  end;
end (* rel_c_kinds *);
$PAGE search_verify
(*  SEARCH_VERIFY performs the argument processing for the search and
    verify macro calls.  It builds a table in which the only marked
    characters are those which are/are not in the second argument,
    depending on the 'in_table' parameter.  Then it scans the first
    argument until it finds a marked character.  If 'in_table' is
    true, it will be looking for a character which is not in the
    second argument.  *)

procedure search_verify ( in_table: boolean );

  var
    c: char;
    i: string_index;
    ctable: packed array [char] of boolean;

begin
  for c := minimum(char) to maximum(char) do
    ctable [c] := not in_table;
  for i := 1 to arg_length(2) do
    ctable [arg_char(2,i)] := in_table;
  ctable [etx] := false;                      (* Always stop at end of argument. *)
  i := 1;
  while ctable[arg_char(1,i)] do
    i := i + 1;
  if arg_char(1,i) <> etx then                     (* Scan succeeded. *)
    num_back (i)
  else if num_args >= 3 then                   (* Scan failed--return failure argument. *)
    arg_back (3,1,arg_length(3));
end (* search_verify *);
$PAGE get_file_id
(*  GET_FILE_ID will load the external variable 'lib_file_id' from
    the first argument.  *)

procedure get_file_id;

  var
    ind: string_index;
    c: char;

begin
  lib_file_id := '';
  ind := 1;
  loop
    c := arg_char(1,ind);
  exit if c = etx;
    lib_file_id := lib_file_id || c;
    ind := ind + 1;
  end;
end (* get_file_id *);
$PAGE expand
(*  EXPAND is called when all the arguments for a macro call have
    been stored on the stack.  It determines what kind of macro is
    being expanded.  If it is a user macro, then the macro definition
    text is simply pushed back onto the input stack, with all argument
    references substituted.  If it is a system macro, then the appro-
    priate system macro processing is performed.  *)

public procedure expand ( defn: definition );

  var
    def: definition;
    i,
    low,
    size: string_index;
    arg: arg_index;
    text: str_pointer;
    mac_true: boolean;

begin
  exp_call;                                  (* Convert to an expanding call. *)
  put_back (end_exp);                              (* Flag end of macro expansion text. *)
  with defn^ do begin
    case kind of

      user_macro,                           (* User defined macro call. *)
      user_literal:                             (* User defined literal expansion. *)
	with def_text^ do begin                 (* Copy the definition to the input stack. *)
	  low := 1;
	  while low <= length(str_text) do begin
	    size := length(str_text) - low + 1;
	    i := index(substr(str_text,low,size),arg_marker,size+1);
	    str_back (def_text,low,i-1);
	    low := low + i + 1;
	    if i <= size then begin
	      arg := ord(str_text[low-1]) - ord(first_arg);
	      arg_back (arg,1,arg_length(arg));
	    end;
	  end;
	end;

      def_mac:                                  (* #DEFINE macro call. *)
	define (get_name(1),user_macro,def_str());

      lit_mac:                                    (* #LITERAL macro call. *)
	define (get_name(1),user_literal,def_str());

      asgn_mac:                                 (* #ASSIGN macro call. *)
	begin
	  def := lookup (get_name(1));
	  if def = nil then
	    error (er_und_mac);                 (* Undefined macro. *)
	  with def^ do begin
	    if def_text <> nil then
	      dispose (def_text);
	    kind := user_macro;
	    def_text := def_str();
	  end;
	end;

      undef_mac:                                (* #UNDEF macro call. *)
	undef (get_name (1));

      opsyn_mac:                               (* #OPSYN macro call. *)
	begin
	  if num_args < 2 then
	    error (er_few_arg);                 (* Too few arguments. *)
	  def := lookup(get_name(2));
	  if def = nil then
	    error (er_und_mac);                 (* Undefined macro. *)
	  with def^ do begin
	    if def_text <> nil then
	      with def_text^ do begin
		new (text,length(str_text));
		text^.str_text [1:length(str_text)] := substr(str_text,1,length(str_text));
	      end
	    else
	      text := nil;
	    define (get_name(1),kind,text);
	  end;
	end;

      num_mac:                                  (* #NUM macro call. *)
	num_back (eval_num(1));

      if_mac:                                        (* #IF macro call. *)
	begin
	  if arg_length(1) <> 0
	    then arg_back (2,1,arg_length(2))
	    else arg_back (3,1,arg_length(3));
	end;

      eqn_mac:                                  (* #EQN macro call. *)
	if eval_num(1) =  eval_num(2) then put_back ('1');

      nen_mac:                                    (* #NEN macro call. *)
	if eval_num(1) <> eval_num(2) then put_back ('1');

      ltn_mac:                                    (* #LTN macro call. *)
	if eval_num(1) <  eval_num(2) then put_back ('1');

      len_mac:                                    (* #LEN macro call. *)
	if eval_num(1) <= eval_num(2) then put_back ('1');


      gtn_mac:                                  (* #GTN macro call. *)
	if eval_num(1) >  eval_num(2) then put_back ('1');

      gen_mac:                                    (* #GEN macro call. *)
	if eval_num(1) >= eval_num(2) then put_back ('1');

      eqc_mac,                                    (* #EQC macro call. *)
      nec_mac,                                  (* #NEC macro call. *)
      ltc_mac,                                  (* #LTC macro call. *)
      lec_mac,                                  (* #LEC macro call. *)
      gtc_mac,                                  (* #GTC macro call. *)
      gec_mac:                                  (* #GEC macro call. *)
	if kind in rel_c_kinds() then put_back ('1');

      substr_mac:                              (* #SUBSTR macro call. *)
	if num_args >= 3
	  then arg_back (1,eval_num(2),eval_num(3))
	  else arg_back (1,eval_num(2),max_string_length);

      index_mac:                            (* #INDEX macro call. *)
	begin
	  for low := 1 to arg_length(1) - arg_length(2) + 1 do begin
	    mac_true := true;
	    for i := 1 to arg_length(2) do
	    exit if arg_char(1,low+i-1) <> arg_char(2,i)
	      do mac_true := false;
	  exit if mac_true;
	  end;
	  if mac_true then
	    num_back (low)
	  else if num_args >= 3 then
	    arg_back (3,1,arg_length(3));
	end;

      verify_mac:                               (* #VERIFY macro call. *)
	search_verify (true);

      search_mac:                              (* #SEARCH macro call. *)
	search_verify (false);

      length_mac:                             (* #LENGTH macro call. *)
	num_back (arg_length(1));

      upc_mac:                                     (* #UPC macro call. *)
	for i := arg_length(wnto 1 do
	  put_back (uppercase(arg_char(1,i)));

      lwc_mac:                                        (* #LWC macro call. *)
	for i := arg_length(1) downto 1 do
	  put_back (lowercase(arg_char(1,i)));

      and_mac:                                        (* #AND macro call. *)
	begin
	  mac_true := true;
	  for arg := 1 to num_args do
	  exit if arg_length(arg) = 0
		  do mac_true := false;
	  if mac_true then
	    put_back ('1');
	end;

      or_mac:                                   (* #OR macro call. *)
	for arg := 1 to num_args do
	exit if arg_length(arg) <> 0
	       do put_back ('1');

      not_mac:                                        (* #NOT macro call. *)
	if arg_length(1) = 0 then
	  put_back ('1');

      save_mac:                                    (* #SAVE macro call. *)
	begin
	  get_file_id;
	  if not lib_save() then
	    error (er_lib_fil);                 (* Bad library file. *)
	end;

      lib_mac:                                  (* #LIB macro call. *)
	begin
	  get_file_id;
	  if not lib_load() then
	    error (er_lib_fil);                 (* Bad library file. *)
	end;

      str_mac:                                  (* STR macro call. *)
	begin
	  put_back ('''');
	  arg_back (1,1,arg_length(1));
	  put_back ('''');
	end;

      eval_mac:                                 (* EVAL macro call. *)
	arg_back (1,1,arg_length(1));

      error_mac:                               (* ERROR macro call. *)
	error (er_err_mac);

      macro_mac:                            (* MACRO macro call. *)
	if lookup(get_name(1)) <> nil
	  then put_back ('1');

      pascal_mac:                           (* PASCAL macro call *)
        with cmd_options do
	  option_list := option_list + [pascal_switch];

      nopascal_mac:                         (* NOPASCAL macro call *)
        with cmd_options do
	  option_list := option_list - [pascal_switch];

    end (* case kind *);
  end (* with exp_def^ *);
end (* expand *).
 z {