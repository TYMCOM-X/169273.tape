$TITLE DEBUG$ - the PASCAL Debugger

$HEADER debug

module debug$  options nocheck, special (coercions, word, ptr);
  
$INCLUDE debug.typ
  
$INCLUDE debio.inc
$INCLUDE debbrk.inc
$INCLUDE debdmp.inc	
$INCLUDE debref.inc	
$INCLUDE debasm.inc
$INCLUDE debscp.inc
$INCLUDE debsym.inc
$INCLUDE debprt.inc	
$INCLUDE deblib.inc
$INCLUDE deblex.inc
$INCLUDE debbol.inc
  
const
  version_string: string := 'Version 0B.1';
  initial_follow_set: token_set = [semicolon, eofsy]; (* initial set of legal cmd terminators *)
  prompt: string := '>>';
  
  init_source_id: augmntd_source_id := (* used to initialize augmented source ids *)
    (nil    (* program_block *),
	('' (* module_name   *),
	 '' (* file_name     *),
	 -1 (* file_num      *),
	 '' (* page_name     *),
	 -1 (* page_num      *),
	 0  (* line_num      *) ));
$PAGE word_list
type
  word_list =	(* includes both commands and display options *)
   ( open_wrd,		clear_wrd,	display_wrd,	step_wrd,	sstep_wrd,
     stop_wrd,		abort_wrd,	proceed_wrd,	with_wrd,	where_wrd,	com_wrd,
     help_wrd,		version_wrd,	if_wrd,		kind_wrd,	call_wrd,       remark_wrd,

     break_wrd (* both a command and a display option *),

     iostatus_wrd,	extstatus_wrd,	stack_wrd,	loc_wrd,	scope_wrd,
     files_wrd,		pages_wrd,	modules_wrd );

  command_wrds = open_wrd..break_wrd;
  display_opts = break_wrd..modules_wrd;
  name_string = packed array [1..10] of char;
  
const
  words: array [word_list] of
    record
      name: name_string;
      abbrev: 1..dimension (name_string)
    end :=
      (	('OPEN      ', 1),
	('CLEAR     ', 1),
	('DISPLAY   ', 1),
	('STEP      ', 1),
	('SSTEP     ', 2),
	('STOP      ', 4),
	('ABORT     ', 5),
	('PROCEED   ', 1),
	('WITH      ', 1),
	('WHERE     ', 2),
	('COMMANDS  ', 3),
	('HELP      ', 1),
	('VERSION   ', 3),
        ('IF        ', 2),
        ('KIND      ', 1),
	('CALL      ', 3),
	('REMARK    ', 3),
	('BREAKPOINT', 1),
	('IOSTATUS  ', 2),
	('EXTSTATUS ', 3),
	('STACK     ', 3),
	('LOCATION  ', 3),
	('SCOPE     ', 2),
	('FILES     ', 1),
	('PAGES     ', 1),
	('MODULES   ', 1));
$PAGE a$$ert
(* A$$ERT is used to check that critical assumptions which
   the debugger makes do in fact hold.  If the parameter BOOL_EXPR
   is false then a message is printed and then STOP is called.  *)

public procedure a$$ert (bool_expr: boolean);

begin
  if not bool_expr then begin
    writ$str ('Internal debugger error - please report.');
    stop					(* Dead !!! *)
  end
end  (* a$$ert *);
$PAGE query$
(* QUERY$ asks a question and waits for a yes (<cr>) or no reply.  *)

public function query$ (question: packed array [1..*] of char): boolean;

var
  response: (good, bad);
  answer:   string [4];
  too_long: boolean;

begin
  writ$str (question);
  writ$str ('?  ');
  
  repeat
    read$line (answer, too_long);
    answer := uppercase (answer);

    response := good;
    if (answer = '') or (answer = 'Y') or (answer = 'YES') then
      query$ := true
    else if (answer = 'N') or (answer = 'NO') then
      query$ := false
    else begin
      writ$str ('YES/NO?  ');
      response := bad
    end
  until response = good
end;
$PAGE debug$
(* DEBUG$ is the entry point into the debugger.  It is called 
   directly from the runtime.  There are no parameters; all 
   information has been placed in the runtime static record. *)

public procedure debug$;			

procedure do_cmd (follow_stmt:	token_set); forward;

label
  100;

var
  lex_scan_rec:		lex_scan_rec_type;  (* this record constitutes "static" storage
					       for the lexical scanner module DEBLEX *)
  db_static:		^db_static_record;	(* assigned value of DB$BASE whenever debugger
						   entered *)
  rt_static:		^rt_static_record;	(* assigned value of RT$STATIC whenever debugger
						   entered *)
  status:		status_code;		(* error status variable for DEBUG$ and children *)
  scope_stack:		scope_type;	(* describes current scope *)
  brkpt_string:		cmd_str;	(* if entered due to brkpt, temp for brkpt
					   string to be executed *)
  cur_stmt_debug:	boolean;	(* true => module containing current statement
					   is in DEBUG mode *)
  def_mod_debug:	boolean;	(* true => default module is in DEBUG mode *)
  init_high_seg_num:	ovl_mod_no;(* overlay mod num of high seg resident when debugger
				      was entered; that high seg restored before returning *)
  ovl_modno_ptr:	^ovl_mod_no;	(* used to access high segs' ovl mod no at
					   location 400010b *)
  saved_iostatus,	(* for holding value of iostatus upon entry *)
  saved_extstatus:	int_type;  (* for holding value of extstatus upon entry *)
  too_long:		boolean;  (* used when reading command lines from terminal *)
$PAGE stmt_kind table
(* Table of kinds of Pascal source statements for use by KIND command *)

const
  stmt_kind_table: array [stmt_kinds] of packed array [1..21] of char :=
  (  'Assignment statement.',
     'IF statement.        ',
     'FOR statement.       ',
     'LOOP statement.      ',
     'WHILE statement.     ',
     'CASE statement.      ',
     'WITH statement.      ',
     'GOTO statement.      ',
     'I/O statement.       ',
     'RETURN statement.    ',
     'STOP statement.      ',
     'EXIT statement.      ',
     'UNTIL clause.        ',
     'REPEAT statement.    ',
     'END keyword.         ',
     'Procedure call.      ');
$PAGE lookup_word_list - in debug$
(* LOOKUP_WORD_LIST searches for a keyword in the WORDS table. *)

function lookup_word_list (    minscalar,
			       maxscalar: word_list;
			   var nameidx:	  word_list): boolean;

var
  name:        id_string;
  i:           word_list;
  prev_cursor: cursor_range;

begin
  prev_cursor := lex_scan_rec.cursor; (* remember where token began *)

  lookup_word_list := false;
  lex$keyword (lex_scan_rec, name);
  for i := minscalar to maxscalar do
    if (words [i].abbrev <= length (name)) andif
        (substr (words [i].name, 1, length (name)) = name) then begin
      nameidx := i;
      lookup_word_list := true;
      return
    end;

  lex_scan_rec.cursor := prev_cursor		(* leave pointing to start of token  *)
end;
$PAGE print_words - in debug$
(* PRINT WORDS outputs a list of keywords.  As many as possible are placed on a
   single line, with a width of 71 (a good quess for most terminals). *)

procedure print_words (s:      packed array [1..*] of char;
		       minwrd: word_list;
		       maxwrd: word_list);

 var
   cnt:  0..71;
   len:  0..dimension (name_string);
   word: word_list;

 begin
  writ$nl (s);

  cnt := maximum (cnt);			(* cause a new line for first word *)
  for word := minwrd to maxwrd do begin
    len := index (words[word].name, ' ', dimension (name_string) + 1) - 1;
						 (* get length of non-blank chars *)
    if (cnt + len + 1) > 71 then begin		(* move to new line *)
      writ$eol;
      writ$str ('  ');				(* indent two spaces *)
      cnt := 2
    end;
    writ$str (substr (words[word].name, 1, len));
    writ$str (' ');				(* follow word with blank *)
    cnt := cnt + len + 1
  end;

  writ$eol;
  writ$eol
 end;
$PAGE check_status - in debug$
(* CHECK_STATUS examines a status code and takes one of four actions:
  	1. it simply returns,
  	2. it calls ERROR with an appropriate error message,  
        3. it prints an error message and stops DEAD.  *)

procedure check_status (status: status_code);
  
  (* ERROR prints an error message and aborts the rest of the
     command line.  If the error message is not null then
     current command line is printed with an arrow indicating the
     current cursor position.
     A non-local goto to the body of DEBUG$ is then taken. *)

  procedure error (s: packed array [1..*] of char);

  var
    i: cursor_range;

  begin
    if s <> '' then begin
      writ$nl (lex_scan_rec.cmd_line);
      for i := 1 to lex_scan_rec.cursor - 1 do
	writ$str (' ');
      writ$nl ('^')
    end;
    writ$nl (s);
    goto 100
  end;
$PAGE check_status - body
begin
  case status of
    success,
    execute:		return;

    cmd_too_long:	error ('Command line too long.');
    no_str_delim:	error ('Closing string delimiter missing.');
    bad_radix:		error ('Not a proper radix code.');
    bad_digits:		error ('No digits, or digits improper for radix.');
    too_many_bits:	error ('Positive value too large for machine.');
    not_terminated:	error ('Command not properly terminated.');
    not_assignable:	error ('Lhs object may not be assigned to.');
    ill_command:	error ('Illegal command.');
    lineno_missing:	error ('Line number required.');
    illegal_brkpt_num:	error ('Illegal breakpoint number.');
    no_such_brkpt:	error ('No such breakpoint.');
    too_many_brkpts:	error ('Breakpoint not set: too many.');
    display_keyword_expected:	error ('Display option expected.');
    then_expected:	error ('''THEN'' expected.');
    end_expected:	error ('''END'' expected.');
    stmt_expected:	error ('Command expected.');
    rparen_expected:	error ('Right parenthesis expected.');
    int_cons_expected:	error ('Integer constant expected.');
    not_addressible:	error ('Not addressible.');
    illegal_cmd:	error ('Illegal command.');
    exec_must:		error ('Execution command must be final command of line.');
    scope_ref_expected:	error ('Syntax error in scope reference.');
    ill_frame_no:	error ('Illegal stack frame number.');
    mod_not_found:	error ('Module not found.');
    mod_not_debug:	error ('Module not compiled in DEBUG mode.');
    file_not_found:	error ('File not found.');
    dtime_mismatch,		(* file name and msg printed in OPEN$DEB$FILE *)
    no_instead:		error ('');
    page_not_found:	error ('Page not found.');
    notdefined,		(* scope errors - messages and additional *)
    notpascal,			(* info is printed in DO_OPEN_CMD *)
    badnest,
    noscope,
    notdebug:		error ('');
    format_code_expected:	error ('Format code expected.');
    not_record:		error ('Record type expected.');
    too_many_withs:	error ('Too many WITHs.');
    ill_sym_type:	error ('Symbol is of illegal type.');
    routine_inactive:	error ('Routine containing variable is not active.');
    undef_external:	error ('External symbol undefined.');
    field_id_expected,
    ident_expected:	error ('Identifier expected.');
    ill_set_elem_type:	error ('Illegal set element type.');
    illegal_range:	error ('Illegal value or range.');
    set_too_big:	error ('Set too large.');
    rbrack_expected:	error ('Right bracked expected.');
    cons_ref_expected:	error ('Constant or reference expected.');
    type_incompatability:	error ('Type incompatability.');
    id_undefined:	error ('Identifier not declared or not in scope.');
    func_inactive:	error ('Function not active or not in scope.');
    illegal_value,
    below_range,
    above_range:	error ('Scalar or real out of range.');
    bad_real:		error ('Real number has illegal format.');
    no_such_field:	error ('No such field.');
    not_file_or_ptr:	error ('Non-pointer/file dereferenced.');
    nil_pointer:	error ('Pointer value is NIL.');
    ptr_uninitialized:	error ('Pointer value is not initialized.');
    not_array_string:	error ('Subscript follows non-array/string.');
    not_string:		error ('Substring reference follows non-string.');
    ill_idx_type:	error ('Index type incorrect.');
    too_many_subscripts:	error ('Too many subscripts for array.');
    undiscriminated_union:	error ('Record contains undiscriminated union.');
    oct_hex_invalid:	error ('Octal or Hex format valid for integer values only.');
    cannot_print:	error ('Cannot display value.');
    addr_radix_expected:	error ('Value in machine''s address radix expected.');
    not_boolean:	error ('Boolean value required.');
    relop_invalid:	error ('Operator invalid for type of operands.');
    no_file_block:	error ('Module contains no executable statements.');
    target_unknown:	error ('Pointer or file target type unknown.');
    bad_length_word:	error (''); (* message and value printed in PRINT_STRING *)
    cur_mod_not_debug:	error ('Currently open module not compiled with DEBUG option.');
    bad_iostatus:	error ('The value of IOSTATUS isn''t in range of predefined type IO_STATUS.');

    bad_reason: begin
      writ$nl ('Fatal error -');
      writ$nl ('  Debugger entered abnormally -- not properly initialized.');
      stop		(* <-- DEAD !!! *)
    end;

    others:		error ('Unknown error code, please report.')
  end
end  (* check_status *);
$PAGE check_debug - in debug$
(* CHECK_DEBUG should be called before all operations which require
   the default module to have been compiled in DEBUG mode. *)

procedure check_debug;

begin
  if not def_mod_debug then
    check_status (cur_mod_not_debug) (* triggers error, will not return *)
end;
$PAGE get_token, scanned_token, scanned_integer, scanned_id - in debug$
(* These routines are conveniences for lexical scanning, and referencing the
   results of scanning.  They also serve to encapsulate knowledge about how to
   get those results out the lexical scanners "static" state record, and to ensure
   that error handling is consistent.  *)

(* GET_TOKEN causes scanning of the next token of the current command line.  *)

procedure get_token;
  begin
    lex$scan (lex_scan_rec, status);
    check_status (status)
  end;


(* SCANNED_TOKEN returns the kind of the last token scanned.  *)

function scanned_token: token_kind;
  begin
    scanned_token := lex_scan_rec.tkind
  end;


(* SCANNED_INTEGER returns a scanned integer constant, and ensures that any deferred
   error detected in the scanner is fed to CHECK STATUS now.  (See DEBLEX notes
   on handling of radix problems.)  *)

function scanned_integer: machine_word;
  begin
    a$$ert (lex_scan_rec.next_token.tkind = intconst); (* be sure of whats there *)
    check_status (lex_scan_rec.next_token.status_in_given_radix);
    scanned_integer := lex_scan_rec.next_token.cons_node.scalar_val [1]
  end;


(* SCANNED_ID returns the text of a scanned identifier.  *)
  
function scanned_id: id_string;
  begin
    a$$ert (lex_scan_rec.next_token.tkind = ident); (* be sure of whats there *)
    scanned_id := lex_scan_rec.next_token.id_text
  end;
$PAGE conditional_scan - in debug$
(* CONDITIONAL_SCAN conditionally scans the next token in the command
   line.  The token is scanned and returned in parameter TOKEN if and
   only if the token is of a kind contained in parameter TEST_SET.
   Parameter FOUND is set to true if the next token is in TEST_SET
   and to false otherwise.    *)

procedure conditional_scan (    test_set:	token_set;
			    var found:		boolean);

var
  prev_cursor:	cursor_range;

begin
  prev_cursor := lex_scan_rec.cursor;
  get_token;
  found := scanned_token in test_set;
  if not found then
    lex_scan_rec.cursor := prev_cursor
end;
$PAGE check_end - in debug$
(* CHECK_END checks for proper termination of a command.  The cursor
   is assumed to point to the beginning of the terminator token.
   If that token is not in FOLLOW_STMT then an error is signalled (and
   thus check_status will not return).  Note that the terminator is merely
   checked for; the cursor is not moved past it.  *)

procedure check_end (follow_stmt: token_set);

var
  prev_cursor:  cursor_range;

begin
  prev_cursor := lex_scan_rec.cursor;
  get_token;
  if not (scanned_token in follow_stmt) then
    check_status (not_terminated) (* triggers error, will not return *)
  else
    lex_scan_rec.cursor := prev_cursor
end (* check_end *);
$PAGE set_with - in debug$
procedure set_with (var new_withs:	with_tab;
			desc:		descriptor);

begin
  with new_withs do
    if desc.dkind <> record_dt then
      status := not_record
    else if active_withs = max_with then
      status := too_many_withs
    else begin
      active_withs := active_withs + 1;
      with withs [active_withs] do begin
	rec_inttyp := desc.type_ptr;
	rec_base := desc.addr;
	first_field := desc.dtype.field_list
      end
    end
end (* set_with *);
$PAGE cstmt_info - in debug$
(* CSTMT_INFO returns a source_id_record describing the location
   of the current statement.  If necessary the overlay module containing
   the current statement is loaded into core.  Before returning the
   overlay module containing the currently open scope is restored
   if necessary.  Must not be called when reason = init since
   rt_static^.cstmt = 0.  *)

procedure cstmt_info (var cstmt_id:  source_id_record);
 
begin
  a$$ert (cur_stmt_debug);
  a$$ert (ord (rt_static^.cstmt) <> 0);
  with db_static^ do begin
    if not cur_stmt_in then begin
      a$$ert (cur_ovl_mod <> nil_ovl_modno);
      ld$ovl (cur_ovl_mod)
    end;
    info$stmt (rt_static^.cstmt, cstmt_id, status);
    if not def_mod_in then begin
      a$$ert (def_ovl_mod <> nil_ovl_modno);
      ld$ovl (def_ovl_mod)
    end
  end
end (* cstmt_info *);
$PAGE scope_addr - in debug$
(* SCOPE_ADDR is passed a scope stack and returns an address which is
   within the module constituting the scope.  The address is used to
   keep track of whether overlay modules containing the current 
   statement and the currently open scope are in core.  
   The address used is either the program block pointer or the
   link word in the stack frame.  The stack frame may or may not
   be available if an inactive routine was opened.  However that
   can only occur if the routine opened was in DEBUG mode, in
   which case the program block pointer is available.
   A somewhat subtle trick is used to avoid accessing the scope
   stack if the currently open module is not in DEBUG mode.
   That can only happen if the open occurred when the debugger
   was initially entered.  In that case the topmost stack frame
   is in scope and its stack frame pointer is available from the
   run time monitor's static storage record.  Note that this 
   means DEF_MOD_DEBUG must be updated before SCOPE_ADDR is called.  *)

function scope_addr (scope_stack: scope_type): ptr;

var
  stkframe: ^stack_frame;
in
  with scope_stack do begin
    if def_mod_debug then
      stkframe := displays[display_levels].stackbase
    else
      stkframe := rt_static^.basis_rec.basis;
    if stkframe = nil then begin		(* Module not active, use prog blk addr *)
      a$$ert (displays[display_levels].prog_blk <> nil);
      scope_addr := displays[display_levels].prog_blk
    end
    else
      scope_addr := stkframe^.link_addr
  end
end (* scope_addr *);
$PAGE get_module - in debug$
(* GET_MODULE parses a module reference with the syntax: [ <module id>@ ].
   If an identifier followed by an '@' is not found then the cursor is
   not advanced and parameter MOD_NAME is not modified.  If found, then
   MOD_NAME is set to the text of the module identifier.  *)

procedure get_module (var mod_name: mod_string);

var
  prev_cursor:	cursor_range;
  found:	boolean;

begin
  prev_cursor := lex_scan_rec.cursor;
  conditional_scan ([ident], found);
  if found then begin
    conditional_scan ([atsign], found);
    if found then
      mod_name := scanned_id
    else
      lex_scan_rec.cursor := prev_cursor
  end
end (* get_module *);
$PAGE parse_source_ref - in debug$
(* PARSE_SOURCE_REF parses a source reference with the syntax:
  
     [ <module>@ ] [ <file>- ] [ <page>/ ] <line>
  
   The fields of the source id record are modified only if the
   corresponding construct is found. *)

procedure parse_source_ref (var source_id:  source_id_record);

  var
    prev_cursor: cursor_range;
    found:	  boolean;
    tok_type:	  token_kind;
  
  
  (* GET_FILE parses a file reference with the syntax:

       [ <file id>- | <file number>- ]

     If a file identifier is found the SOURCE_ID's FILE_NAME is set to 
     the text of the ID; if an integer constant is found then FILE_NUM is
     set to the number found.  Neither is modified, nor is the cursor
     changed if a syntactically correct file reference is not found.  *)

  procedure get_file;

  begin
    prev_cursor := lex_scan_rec.cursor;
    conditional_scan ([ident, intconst], found);
    if found then begin				(* so far so good *)
      tok_type := scanned_token;		(* remember if id or int *)
      conditional_scan ([minus], found);
      if not found then	
	lex_scan_rec.cursor := prev_cursor			(* false alarm, restore initial cursor *)
      else if tok_type = ident then			(* found one ! *)
	source_id.file_name := scanned_id
      else
	source_id.file_num := scanned_integer
    end
  end (* get_file *);
  
  
  (* GET_PAGE parses a page reference with the syntax:

       [ <page id>/ | <page number>/ ]

     If a page identifier is found the SOURCE_ID'S PAGE_NAME is set to it;
     if a page number is found the PAGE_NUM is set to it. Neither is modified,
     nor is the cursor changed if a syntactically correct page reference is
     not found.  *)

  procedure get_page;
  
  begin
    prev_cursor := lex_scan_rec.cursor;
    conditional_scan ([ident, intconst], found);
    if found then begin
      tok_type := scanned_token;
      conditional_scan ([slash], found);
      if found then begin
	if tok_type = ident then
	  source_id.page_name := scanned_id	(* preserved if slash scanned *)
	else
	  source_id.page_num := scanned_integer
      end
      else
	lex_scan_rec.cursor := prev_cursor
    end
  end (* get_page *);

begin
    get_module (source_id.module_name);
    get_file;
    get_page;
    get_token;
    if scanned_token = intconst then
      source_id.line_num := scanned_integer
    else
      status := lineno_missing
end (* parse_source_ref *);
$PAGE get_source_id - in debug$
(* GET_SOURCE_ID parses a <source reference>.  Default values for any
   components not explicitly given are supplied according to the rules
   for the BREAKPOINT and KIND commands.  The parsed source_id is
   returned in parameter AUG_SOURCE_ID.  *)

procedure get_source_id (var aug_source_id:  augmntd_source_id);

var
  default_module:	id_string;
  cstmt_id:		source_id_record;
  def_prog_blk:		^prog_block;

begin
  aug_source_id := init_source_id;
  parse_source_ref (aug_source_id.source_id);	(* this does parsing, rest of code sets defaults *)
  check_status (status);
  with aug_source_id, source_id do begin
    def_prog_blk := scope_stack.displays[1].prog_blk;	(* get prog blk for default module *)
  
  
    if (module_name = '') andif		(* check for line number only *)
       (file_name = '') andif (file_num = -1) andif
       (page_name = '') andif (page_num = -1) then begin
      if (rt_static^.reason <> init) andif cur_stmt_debug then begin
        cstmt_info (cstmt_id);		(* get source id for current stmt *)
        check_status (status);
      end;
      if def_mod_debug then begin
	get$mod$name (def_prog_blk, default_module, status);
	check_status (status)
      end;
      default_module := substr (default_module, 1, min (length (default_module), upperbound (cstmt_id.module_name)));
      if (rt_static^.reason = init) orif	(* have not begun execution *)
         (not cur_stmt_debug) orif		(* or current stmt not in debug *)
        (uppercase (cstmt_id.module_name) <> uppercase (default_module)) then begin
						(* or current stmt not in default module *)
        check_debug;
        program_block := def_prog_blk;		(* use currently open module *)
        file_num := 0;				(* and file and page 0 *)
        page_num := 0
      end
      else begin				(* have started execution and current statement
						   is in dubug and is in default module *)
        program_block := def_prog_blk;		(* use default module *)
        cstmt_id.line_num := line_num;			(* use file and page of current stmt *)
        source_id := cstmt_id;
      end
    end
  
    else begin					(* more than just the lineno was supplied *)
      if module_name = '' then begin		(* use default module *)
        check_debug;
        program_block := def_prog_blk;
      end;
      if (file_name = '') andif (file_num = -1) then  (* use file 0 *)
        file_num := 0;
      if (page_name = '') andif (page_num = -1) then  (* use page 0 *)
        page_num := 0
    end
  end
end  (* get_source_id *);
$PAGE parse_scope_ref - in debug$
(* PARSE_SCOPE_REF translates a scope reference to a scope_id_record.
   A scope reference has the syntax:
  
     <module_id>@  |
     [ <module_id>@ ] <routine_id>: [ <routine_id>: ]* [ (<invocation>) ]  *)

procedure parse_scope_ref (var scope_id:  scope_id_record);

  var
    module_id:  mod_string;
    routine_id: id_string;
    found:      boolean; (* for conditional scanning in local procs. *)
  
  
  (* GET_FRAME parses a stack frame instantiation number with the syntax:
     [ (<invocation>) ].  If the initial '(' is found, then the entire
     construct must be present.   *)

  procedure get_frame;

  begin
    conditional_scan ([lparent], found);
    if found then begin
      get_token;
      if scanned_token = intconst then begin
	scope_id.stack_frm_no := scanned_integer;
	get_token;
	if scanned_token <> rparent then
	  status := rparen_expected
      end
      else
	status := int_cons_expected
    end
  end (* get_frame *);


  (* GET_ROUTINE parses a routine reference with the syntax: [ <routine id>: ] .  *)

  procedure get_routine;

  var
    prev_cursor:	cursor_range;

  begin
    routine_id := ''; (* init regardless of what we find while scanning *)
    prev_cursor := lex_scan_rec.cursor;
    conditional_scan ([ident], found);
    if found then begin
      conditional_scan ([colon], found);
      if found then
	routine_id := scanned_id
      else
	lex_scan_rec.cursor := prev_cursor
    end
  end (* get_routine *);

  
begin
  with scope_id do begin
    name_count := 0;
    module_id := '';		(* initialize, as get_module won't *)
    get_module (module_id);			(* parse module, if any *)
    if module_id <> '' then begin		(* module present *)
      names[1] := module_id;
      delim := '@';
      name_count := name_count + 1
    end
    else					(* no module, assume routine first *)
      delim := ':';
    get_routine;
    while (routine_id <> '') and
          (name_count < maximum (lexic_lvl_range)) do begin (* parse routine names *)
      name_count := name_count + 1;
      names[name_count] := routine_id;
      get_routine
    end;
    if name_count = 0 then		(* error if neither module or routine found *)
      status := scope_ref_expected
    else begin					(* parse stack frame instantiation *)
      stack_frm_no := 1;
      get_frame
    end
  end
end (* parse_scope_ref *);
$PAGE assign_or_print - in debug$
(* ASSIGN_OR_PRINT processes a single assignment or print command. FOLLOW_STMT
   is the set of tokens which may legally terminate the command.  *)

procedure assign_or_print (follow_stmt:	token_set);

var
  desc1, desc2: descriptor;			(* descriptor for LHS and RHS object *)
  found:	boolean;
  format_code:	radix_type;

begin
  check_debug;
  get$descriptor (lex_scan_rec, scope_stack, desc1, status);  (* get descriptor for lhs ref. *)
  check_status (status);
  conditional_scan ([becomes], found);
  if found then				(* assignment statement *)
    if desc1.assignable then begin
      get$descriptor (lex_scan_rec, scope_stack, desc2, status);  (* get descriptor for rhs ref. *)
      check_status (status);
      check_end (follow_stmt);
      a$$ign (desc1, desc2, status)
    end
    else
      status := not_assignable
  else begin					(* print command *)
    conditional_scan ([colon], found);	(* check for format code *)
    format_code := decimal_radix;  (* default for integers *)
    if found then begin
      conditional_scan ([ident], found);
      if not found orif (length (scanned_id) <> 1) then
        check_status (format_code_expected)
      else
	case scanned_id [1] of
	  'O': format_code := octal_radix;
	  'D': format_code := decimal_radix;
	  'H': format_code := hex_radix;
	  others: check_status (format_code_expected)
	end
    end;
    check_end (follow_stmt);
    print$ (desc1, format_code, status)
  end
end (* assign_or_print *);
$PAGE do_if_cmd - in debug$
(* DO_IF_CMD executes an IF command.  FOLLOW_STMT is the set
   of tokens which may legally follow the command. *)

procedure do_if_cmd (follow_stmt:  token_set);

var
  result,				(* result of evaluation of <bool expr> *)
  found:	boolean;				(* for conditional scans *)
  begin_subcmd:	cursor_range;			(* index of beginning of then or else subcmd *)
  new_follow:	token_set;			(* legal terminators for subcmd *)
  final_cursor:	cursor_range;			(* index of end of entire IF cmd *)


  (* SCAN STMT advances the cursor past a single, possibly compound, statement (i.e. command).  *)

  procedure scan_stmt;

  begin
    get_token;
    if scanned_token = period then
      get_token;

    case scanned_token of
      beginsy:		(* compound stmt *)
	begin
	  repeat
	    scan_stmt;
	    check_status (status);
	    get_token				(* scan terminator *)
	  until (scanned_token = endsy) or (scanned_token = eofsy);
	  if scanned_token = eofsy then
	    status := end_expected
	end;

      ifsy:			(* IF command *)
	begin
	  repeat
	    get_token
	  until (scanned_token = thensy) or (scanned_token = eofsy);
	  if scanned_token = eofsy then
	    status := then_expected
	  else begin
	    scan_stmt;
	    check_status (status);
	    conditional_scan ([elsesy], found);
	    if found then
	      scan_stmt
	  end
	end;

      semicolon,					(* error check *)
      eofsy,
      endsy,
      elsesy:  status := stmt_expected;

      others:	(* simple stmt *)
	begin
	  repeat
	    final_cursor := lex_scan_rec.cursor;
	    get_token
	  until scanned_token in [eofsy, semicolon, endsy, elsesy];
	  lex_scan_rec.cursor := final_cursor				(* don't advance past terminator *)
	end
    end
  end (* scan_stmt *);
$PAGE do_if_cmd - body
begin
  eval$bool$expr (lex_scan_rec, scope_stack, result, status); (* evaluate the boolean expr *)
  check_status (status);
  conditional_scan ([thensy], found);	(* scan "THEN" *)
  if not found then begin				(* error ! *)
    status := then_expected;
    return
  end;
  
  begin_subcmd := lex_scan_rec.cursor;			(* beginning of THEN clause *)
  scan_stmt;			(* scan then clause *)
  check_status (status);
  conditional_scan ([elsesy], found);	(* "ELSE" part ?? *)

  if result and found then			(* true boolean and an ELSE clause present *)
    new_follow := [elsesy]		(* only a elsesy can follow THEN clause *)
  else
    new_follow := follow_stmt;
  if not result then
    begin_subcmd := lex_scan_rec.cursor;	(* false boolean => reset beg. of subcmd cursor *)
  if found then begin				(* ELSE clause present *)
    scan_stmt;			(* scan it *)
    check_status (status)
  end;

  (* At this point CURSOR should point past the end of 
     the entire IF command.  BEG_SUBCMD should point just past the
     THEN or ELSE keyword, depending on which clause is to
     be executed.  If they are equal, then the boolean evaluated
     to false and there was no ELSE clause.  *)

  check_end (follow_stmt);
  if begin_subcmd <> lex_scan_rec.cursor then begin
    final_cursor := lex_scan_rec.cursor;			(* save value cursor must have on exit *)
    lex_scan_rec.cursor := begin_subcmd;			(* backup *)
    do_cmd (new_follow); (* execute subcommand *)
    check_status (status); (* emit any error message while cursor is
			      still within subcommand *)
    lex_scan_rec.cursor := final_cursor (* restore cursor to point after entire ".IF" *)
  end
end (* do_if_cmd *);
$PAGE do_open_cmd - in debug$
(* DO_OPEN_CMD processes an OPEN command. *)

procedure do_open_cmd (follow_stmt:  token_set);

var
  prev_cursor:		cursor_range;
  new_scope:		scope_type;
  level:		stack_level;
  scope_id:		scope_id_record;
  initial_invoc:	stack_level;
  initial_count:	display_lvl_range;

begin
  prev_cursor := lex_scan_rec.cursor;
  get_token;

  (* No parameter - restore scope of current statement.  *)

  if scanned_token in follow_stmt then begin	(* no parameter - reset orginal scope *)
    lex_scan_rec.cursor := prev_cursor;		(* don't move cursor past terminator *)
    level := maximum (stack_level);		(* if level > any actual frame, open$frame *)
    open$frame (level, new_scope, status)	(*   opens the topmost frame *)
  end

  (* Integer given - open stack frame with specified number.  *)

  else if scanned_token = intconst then begin	(* integer - open stack frame with
					  	specified number *)
    check_end (follow_stmt);
    level := scanned_integer;
    open$frame (level, new_scope, status);
    if (status = success) andif (level <> scanned_integer) then begin
      writ$str ('Level not found, level ');
      writ$int (level, decimal_radix);
      writ$nl (' used instead.')
    end;
  end

  (* Scope reference given - open referenced routine (which may or
     may not be active). *)

  else begin				(* <scope ref> - open referenced frame *)
    lex_scan_rec.cursor := prev_cursor;		(* reset cursor to start of reference *)
    parse_scope_ref (scope_id);
    check_end (follow_stmt);
    initial_invoc := scope_id.stack_frm_no;
    if scope_id.delim = '@' then
      initial_count := scope_id.name_count
    else
      initial_count := scope_id.name_count + 1;
    ld$ovl (0);  (* ensure that main's high seg is resident so that 
                     link sym tab entries are valid *)
    open$routine (scope_id, new_scope,  status);
    if status = wronginvocation then begin	(* warning error only *)
      if (scope_id.stack_frm_no = 0) andif (initial_count > 1) then
        writ$nl ('(None of the named routines are active.)')
      else if scope_id.stack_frm_no <> 0 then
        with scope_id do begin
          if initial_count <> name_count then
            writ$nl ('(Routine ' || names[name_count] || ' is first active routine.)');
          if initial_invoc <> stack_frm_no then begin
            writ$str('(Invocation ');
            writ$int (stack_frm_no, decimal_radix);
            writ$nl (' used instead.)')
          end;
        end;
      status := success;
    end
  end;

  (* Check status code returned.  Errors are handled here so that
     additional information may be printed.  *)

  case status of
  
    notdefined:
      writ$str ('External name not defined: ' || scope_id.names[1]);
    notpascal:
      writ$nl ('Not a PASCAL module: ' || scope_id.names[1]);
    badnest:
      writ$nl ('Invalid subroutine: ' || scope_id.names[scope_id.name_count]);
    noscope,
    notdebug:
      writ$nl ('Module not compiled in debug mode.')

  end  (* case *);

  with db_static^ do
    if status in severe then begin		(* open failed - *)
      if not def_mod_in then				(* if default module no longer in core *)
        ld$ovl (def_ovl_mod);		(*   then load it *)
      if def_mod_debug then				(* reopen default module's .DEB file *)
	open$deb$file (scope_stack.displays[1].prog_blk, status)
    end;
  check_status (status);

  (* Successful open if we reach here. *)

  scope_stack := new_scope;
  db_static^.with_table.active_withs := 0;	(* kill any withs in effect *)
  def_mod_debug := true;			(* open cmd fails if not DEBUG *)
  with db_static^ do begin
    def_mod_in := true;				(* default module loaded by open if necessary *)
    def_ovl_mod := nil_ovl_modno;		(* but its module number is unknown *)
    def_mod_addr := scope_addr (scope_stack)	(* get address in module for overlay tracking *)
  end

end (* do_open_cmd *);
$PAGE process_debugger_cmd - in debug$
(* PROCESS_DEBUGGER_CMD processes a debugger command, i.e., a command
   beginning with a period.  FOLLOW_STMT is the set of tokens which
   may legally follow the command.  *)

procedure process_debugger_cmd (follow_stmt:  token_set);

const
  no_minus: boolean := false;		(* display pages - no '-' after file name/num *)

var
  keyword:		word_list;		(* cmd name or .DISPLAY keyword *)
  aug_source_id:	augmntd_source_id;(* for BREAK or KIND command *)
  brkpt_string:		cmd_str;	(* BREAK cmd string *)
  brkpt_no:		brkpt_range;	(* number of brkpt set by BREAKPOINT cmd *)
  found:		boolean;		(* used in the many calls to conditional_scan *)
  last_cursor:		cursor_range;	(* for resetting cursor in HELP, COMMANDS cmds *)
  num_of_frames:	stack_level;	(* number of stack frames to dump *)
  stmt_blk:		stmt_block_ptr;	(* stmt block ptr for KIND cmd *)
  scope_id:		scope_id_record;	(* parsed scope reference of OPEN cmd *)
  new_withs:		with_tab;	(* for setting withs in WITH cmd *)
  desc:			descriptor;		(* for describing references of WITH, WHERE cmds *)
  mod_name:		id_string;		(* for link symbol table lookup of CALL cmd *)
  link_entry:		^linkentry;	(* link symbol table ptr for CALL cmd *)
  octal_string:		id_string;	(* for translating extstatus *)
  symbol:		sym;	(* for searching for IO_STATUS *)
  target_value,
  cur_value:		pos_int; (* "     "      "     "        *)
$PAGE check_for_end - in process_debugger_cmd in debug$
(* CHECK_FOR_END checks for proper termination of a command.  It is
   functionally identical to CHECK_END, but, because it is declared
   wtihin PROCESS_DEBUGGER_CMD, it does not require a set of legal
   terminating tokens as a parameter.  This is an optimization done
   because of the many calls to check_for_end within PROCESS_DEBUGGER_CMD. *)

procedure check_for_end;

var
  prev_cursor:	cursor_range;

begin
  prev_cursor := lex_scan_rec.cursor;
  get_token;
  if not (scanned_token in follow_stmt) then
    check_status (not_terminated)			(* triggers error, will not return *)
  else
    lex_scan_rec.cursor := prev_cursor				(* don't really eat the terminator token *)
end (* check_for_end *);
$PAGE process_debugger_cmd - body
begin
  if not lookup_word_list (minimum (command_wrds), maximum (command_wrds), keyword) then begin
    status := ill_command;
    return (* <-- return *)
  end;
 
  case keyword of

    break_wrd:				(* .BREAKPOINT <source ref> [ <string> ]  *)
      begin
	get_source_id (aug_source_id);	(* parse <source ref> *)
	check_status (status);
	conditional_scan ([stringconst], found);
	if found then
	  with lex_scan_rec.next_token.cons_node do
	    brkpt_string := substr (str_val, 1, str_len)
	else
	  brkpt_string := '';
	check_for_end;
	if (aug_source_id.program_block = nil) andif
	   (aug_source_id.source_id.module_name <> '') then
	  ld$ovl (0);  (* ensure that main's high seg is resident so that
			  link sym tab entries are valid *)
	set$brkpt (aug_source_id, brkpt_string, brkpt_no, status);
	if not db_static^.def_mod_in then
	  ld$ovl (db_static^.def_ovl_mod);  (* in case BREAK displaced default module's high seg *)
	check_status (status);
	writ$str ('Break #');
	writ$int (brkpt_no, decimal_radix);
	writ$nl (' set.')
      end;

    call_wrd:					(* .CALL <identifier> *)
      begin
	ld$ovl (0);				(* called proc must be in MAIN overlay since found
						   via LINK symbol table, so force MAIN into core *)
	get_token;			(* get proc name *)
	if scanned_token <> ident then
	  status := ident_expected
	else begin
	  check_for_end;
	  mod_name := '';
	  link_entry := sym$lookup (scanned_id, mod_name);
						 (* lookup proc in link symbol table *)
	  if link_entry = nil then
	    status := undef_external
	  else
	    deb$call (link_entry^.symaddr);
	end;
	if not db_static^.def_mod_in then
	  ld$ovl (db_static^.def_ovl_mod);	(* in case CALL displaced default
						   module's overlay segment *)
      end;

    clear_wrd:				(* .clear [ <brkpt number> ] *)
      begin
	conditional_scan ([intconst], found);
	check_for_end;
	if found then
	  clr$brkpt (scanned_integer, status)
	else if query$ ('Clear all') then
	  clr$all$brkpts
      end;

    com_wrd,					(* .COMMANDS *)
    help_wrd:					(* .HELP *)
      begin
	repeat				(* less picky on termination with these commands *)
	  last_cursor := lex_scan_rec.cursor;
	  get_token
	until scanned_token in (follow_stmt + [eofsy]);
	lex_scan_rec.cursor := last_cursor;		(* cursor should point to start of terminator *)
	writ$eol;
	print_words ('Available commands: ', minimum (command_wrds), maximum (command_wrds));
	print_words ('Display options: ', minimum (display_opts), maximum (display_opts))
      end;

    display_wrd:  (* .DISPLAY <display option> *)
      begin
	if not lookup_word_list (minimum (display_opts), maximum (display_opts), keyword) then begin
	  status := display_keyword_expected;
	  return (* <-- return *)
	end;

	case keyword of

	  break_wrd:			(* .display breakpoints *)
	    begin
	      check_for_end;
	      dmp$breaks (status)
	    end;

	  files_wrd,			(* .display files [ <module>@ ]  *)
	  pages_wrd:			(* .display pages [ <module>@ ] [<file>] *)
	    begin
	      aug_source_id := init_source_id;
	      with aug_source_id, source_id do begin
		get_module (module_name);
		check_for_end;
		if module_name = ''  then begin
		  check_debug;
		  program_block := scope_stack.displays[1].prog_blk
		end
		else
		  ld$ovl (0);  (* ensure that main's high seg is resident so that
				  link sym tab entries are valid *)
		if keyword = files_wrd then
		  dmp$files (aug_source_id, status)
		else (* pages_wrd *) begin
		  if (file_name = '') andif (file_num = -1) then
		    file_num := 0;
		  dmp$pages (aug_source_id, status)
		end
	      end;
	      if not db_static^.def_mod_in then
		ld$ovl (db_static^.def_ovl_mod)  (* in case default module's high seg was displaced *)
	    end;

	  loc_wrd:
	    begin				(* .display location *)
	      check_for_end;
	      dmp$location
	    end;

	  modules_wrd:			(* .display modules *)
	    begin
	      check_for_end;
	      dmp$modules
	    end;

	  scope_wrd:			(* .display scope *)
	    begin
	      check_for_end;
	      check_debug;			(* require current module to be in DEBUG mode *)
	      dmp$scope (scope_stack, status)
	    end;

	  stack_wrd:			(* .display stack [ <num of frames> ]  *)
	    begin
	      conditional_scan ([intconst], found);
	      check_for_end;
	      if found then
		num_of_frames := scanned_integer
	      else
		num_of_frames := maximum (stack_level);
	      dmp$stack (num_of_frames)
	    end;

	  iostatus_wrd:
	    begin
	      symbol := deref$ptr (blk (deref$ptr (level0blk))^.id_list.first);
	      (* find first element of the predefined type IO_STATUS *)
	      while symbol <> nil do begin
		with nam (deref$ptr (symbol^.name))^ do
		  exit if substr (text, 1, len) = 'IO_OK';
		symbol := deref$ptr (symbol^.next)
	      end;
	      a$$ert (symbol <> nil);  (* every module's root block should have it *)

	      cur_value := 0;
	      res$io (saved_iostatus, saved_extstatus);
	      target_value := ord (iostatus);
	      while (cur_value < target_value) and (symbol <> nil) do begin
		cur_value := cur_value + 1; (* count up to ord (iostatus) *)
		symbol := deref$ptr (symbol^.next)
	      end;
	      if symbol = nil then (* counted ourselves clean off end of list *)
		status := bad_iostatus
	      else
		with nam (deref$ptr (symbol^.name))^ do
		  if (len >= 3) andif (text [1:3] = 'IO_') then
		    writ$nl (substr (text, 1, len))
		  else (* value too big, but didn't trip nil check *)
		    status := bad_iostatus
	    end;

	  extstatus_wrd:
	    begin
	      putstring (octal_string, saved_extstatus:12:o);
	      writ$str (octal_string [1:6]); writ$str (',,'); writ$nl (octal_string [7:6])
	    end;
	end  (* case *);
      end (* display command *);

    abort_wrd,				(* .ABORT *)
    stop_wrd:					(* .STOP *)
      begin
	check_for_end;
	if keyword = stop_wrd then
	  stop
	else
	  abort$
      end;

    if_wrd:					(* .IF <bool expr> then <cmd> [ else <cmd> ]  *)
      begin
	check_debug;
	do_if_cmd (follow_stmt)
      end;

    kind_wrd:					(* .KIND <source ref>  *)
      begin
	get_source_id (aug_source_id);
	check_status (status);
	check_for_end;
	if (aug_source_id.program_block = nil) andif
	   (aug_source_id.source_id.module_name <> '') then
	  ld$ovl (0);  (* ensure that main's high seg is resident so that
			  link sym tab entries are valid *)
	find$stmt$blk (aug_source_id, stmt_blk, status);
	if not db_static^.def_mod_in then
	  ld$ovl (db_static^.def_ovl_mod); (* in case KIND displaced default
					      module's high seg *)
	check_status (status);
	if stmt_blk <> nil then writ$nl (stmt_kind_table[stmt_blk^.stmt_kind])
      end  (* kind command *);

    open_wrd:					(* .OPEN [ <source ref> ]  *)
      do_open_cmd (follow_stmt);

    proceed_wrd:				(* .PROCEED [ <breakpoint count> ]  *)
      begin
	with rt_static^ do begin
	  conditional_scan ([intconst], found);
	  check_for_end;
	  if found then
	    brk_skip := scanned_integer
	  else
	    brk_skip := 1;
	  if brk_skip <= 0 then
	    status := illegal_range
	  else
	    status := execute
	end
      end  (* proceed command *);

    remark_wrd:	(* .REMARK <string> *)
      begin
	conditional_scan ([stringconst], found);
        repeat	(* be forgiving about termination *)
	  last_cursor := lex_scan_rec.cursor;
	  get_token
	until scanned_token in (follow_stmt + [eofsy]);
	lex_scan_rec.cursor := last_cursor
      end (* remark command *);
  
    sstep_wrd,				(* .SSTEP [ <stmt count> ]  *)
    step_wrd:					(* .STEP  [ <stmt count> ]  *)
      begin
	with rt_static^ do begin
	  conditional_scan ([intconst], found);
	  check_for_end;
	  if found then
	    step_count := scanned_integer
	  else
	    step_count := 1;
	  proc_skip := (keyword = step_wrd);
	  if step_count <= 0 then
	    status := illegal_range
	  else
	    status := execute;
	end
      end  (* step and sstep commands *);

    version_wrd:				(* .VERSION *)
      begin
	check_for_end;
	writ$str (version_string); writ$str (' of '); writ$nl (compdate)
      end;

    with_wrd:					(* .WITH [ <record ref> [, <record ref> ]* ]   *)
      begin
	check_debug;
	last_cursor := lex_scan_rec.cursor;
	conditional_scan (follow_stmt, found);
	if found then begin			(* clear withs *)
	  lex_scan_rec.cursor := last_cursor;
	  db_static^.with_table.active_withs := 0;
	end
	else begin				(* parameters given - set specified withs *)
	  new_withs.active_withs := 0;
	  repeat
	    get$descriptor (lex_scan_rec, scope_stack, desc, status);
	    check_status (status);
	    set_with (new_withs, desc);
	    check_status (status);
	    conditional_scan ([comma], found)
	  until not found;
	  check_for_end;
	  db_static^.with_table := new_withs
	end
      end;

    where_wrd:				(* .WHERE <reference>  *)
      begin
	check_debug;
	get$descriptor (lex_scan_rec, scope_stack, desc, status);
	check_status (status);
	check_for_end;
	if not desc.user_const then begin
	  with desc.addr do begin
	    writ$int (wordoffset, octal_radix);
	    if is_packed then begin
	      writ$str (' bit ');
	      writ$int (bitoffset, decimal_radix)
	    end;
	    writ$eol;
	  end
	end
	else
	  status := not_addressible
      end;

    others:
      a$$ert (false)  (* should be case for every command word! *)

  end (* case *);
end (* process_debugger_commands *);
$PAGE do_cmd - in debug$
(* DO_CMD processes a single (possibly compound) command;  If the next
   token is BEGIN, DO_CMD calls itself recursively until the matching
   END is scanned.  Otherwise, it checks to see if the command begins
   with a period and then calls either PROCESS_DEBUGGER_CMD or
   ASSIGN_OR_PRINT.  Parameter FOLLOW_STMT is the set of tokens which
   may legally follow the command.  STATUS is the status code to be
   returned.  *)

procedure do_cmd (*    follow_stmt: token_set  *);
			(* forward declared, since called from do_if_cmd *)

var
  found: boolean;

begin
  conditional_scan ([beginsy], found);
  if found then begin				(* compound statement *)
    repeat					(* process one sub_cmd per iteration *)
      do_cmd ([semicolon,endsy]);
      check_status (status);
      get_token			(* scan terminator, already checked for legality *)
    until (scanned_token = endsy) or (status = execute);
    if (status = execute) and (scanned_token <> endsy) then
      status := exec_must
    else
      check_end (follow_stmt)
  end
  else begin					(* simple statement *)
    conditional_scan ([period], found);
    if found then
      process_debugger_cmd (follow_stmt)
    else
      assign_or_print (follow_stmt)
  end
end  (* do_cmd *);
$PAGE process_cmd_line - in debug$
(* PROCESS_CMD_LINE executes all the commands contained in a single
   command line (which may have been just entered at the terminal or
   may be a breakpoint string). *)

procedure process_cmd_line;

begin
  if length (lex_scan_rec.cmd_line)  <>  0  then begin	(* nop if cmd line is null *)
    repeat					(* process 1 cmd per iteration *)
      do_cmd (initial_follow_set);
      check_status (status);
      get_token
    until (scanned_token = eofsy) or (status = execute);
    if (status = execute) and (scanned_token <> eofsy) then
      status := exec_must
  end
end (* process_cmd_line *);
$PAGE prepare_to_return_to_user - in debug$
(* PREPARE_TO_RETURN_TO_USER does all necessary clean up
   before returning to the user program. *)

procedure prepare_to_return_to_user;

begin
  rt_static^.reason := badreason;		(* invalidate reason code *)

  with db_static^ do begin
    if not cur_stmt_in then				(* reload overlay containing current *)
      ld$ovl (cur_ovl_mod);		(* statement if necessary *)

    ovl_modno_ptr := ptr (400010b);		(* restore original high seg *)
    if ovl_modno_ptr^ <> init_high_seg_num then	(* if current high seg different *)
      ld$ovl (init_high_seg_num);

    if cur_stmt_debug then		(* save info needed to determine if WITHs *)
      with scope_stack do begin			(* can be preserved on next entry *)
        prev_display_levels := display_levels;
	prev_top_display := displays[display_levels];
      end;
    
    debugger_active := false;
  end;
  res$io (saved_iostatus, saved_extstatus)  (* restore iostatus *)
end (* prepare_to_return_to_user *);
$PAGE debug$ - body
begin 
  status := success;				(* optimist *)
  sav$io (saved_iostatus, saved_extstatus);  (* save iostatus for restoration before return to user *)
  rt_static := rt$base; (* fetch pointer to static area shared with runtime *)
  db_static := db$base; (*   "      "    "    "      "  private to Debugger *)
				(* Note - overlay manager dabbles with both those areas *)

  with rt_static^ do begin

    (* Check if the Debugger is being entered recursively (probably due
       to a .CALL command).  If so, it's a fatal error. *)

    if (reason in [step, brkpt]) and db_static^.debugger_active then
      check_status (bad_reason)
    else
      db_static^.debugger_active := true;

    (* initialize debsym package if first entry to the debugger *)

    if reason = init then
      st$init;

    (* Establish scope stack.  Note that error return codes are essentially
       ignored.  If the status returned is not 'success' then the module
       is presumed to be not in debug mode.  *)

    with db_static^ do begin
      cur_stmt_in := true;		(* module containing current stmt must
					   be in core on entry *)
      cur_ovl_mod := nil_ovl_modno;	(* overlay module number unknown *)
      def_mod_in := true;			(* default module is same as current stmt mod now *)
      def_ovl_mod := nil_ovl_modno		(* module number also unknown *)
    end;
    open$stack (basis_rec.basis, scope_stack, status);
    cur_stmt_debug := status = success;			(* is the module containing the current
							   statement in DEBUG mode ? *)
    def_mod_debug := cur_stmt_debug;		(* default module must contain current stmt now *)
    status := success;			(* prohibit erroring out at this point *)

    (* Set info for keeping track of whether the module containing the
       current statement and the module containing the current scope
       are in core.  Also set info for restoring the high seg resident
       when the Debugger was entered (restored before returning to user program).  *)

    with db_static^ do begin
      cur_stmt_addr := scope_addr (scope_stack); (* address which lies within  the 
						    in-scope module - for tracking overlays *)
      def_mod_addr := cur_stmt_addr		(* default module is same as current stmt module now *)
    end;
    ovl_modno_ptr := ptr (400010b);		(* set overlay mod num of *)
    init_high_seg_num := ovl_modno_ptr^;		(* current high seg *)

    (* Reset proceed and step counts, and preserve WITHs if possible. *)

    brk_skip := -1;
    step_count := -1;

    with db_static^, with_table do
      if (active_withs > 0) and cur_stmt_debug then
	with scope_stack, displays[display_levels] do
	  if (reason = init) orif
	     (prev_display_levels <> display_levels) orif
	     (prog_blk <> prev_top_display.prog_blk) orif
	     (blk_node <> prev_top_display.blk_node) orif
	     (stackbase <> prev_top_display.stackbase) orif
	     (staticbase <> prev_top_display.staticbase) then
	    active_withs := 0
      else
	active_withs := 0;

    (* case statement on reason for entering the debugger *)

    case reason of

      badreason:					(* Should not happen *)
	check_status (bad_reason); (* FATAL error - will not return, will stop DEAD *)

      init:					(* first time debugger entered *)
	begin
	  clr$all$brkpts;  (* initialize breakpoint table *)
	  writ$str ('Pascal debugger, ');	(* write version id *)
	  writ$nl (version_string);
	  writ$eol
	end;

      brkpt:					(* reached a breakpoint *)
	begin
	  brkpt_string := db_static^.brk_strings[brkpt_num];
	  if brkpt_string = '' then
	    dmp$location
	  else begin (* process breakpoint command string *)
	    lex_scan_rec.cursor := 1;
            lex_scan_rec.cmd_line := brkpt_string;
	    lex_scan_rec.cursor_at_next_token := 0;	(* .next_token is undefined at this point *)
	    process_cmd_line;
	    if status <> execute then begin
	      dmp$location;
	      check_status (status)
	    end
	    else begin
              prepare_to_return_to_user;
	      return		(* <-- return to user program if brkpt string ended
				   in an execution cmd *)
	    end	
	  end
	end;

      others:					(* all others (trap, rterr) just dump location *)
	dmp$location;

    end  (* case *);

    (* Read command lines from the terminal and process them until
       an execution command is given.  *)

  100:
    status := success;
    repeat						(* process one line of terminal input per iteration *)
      writ$str (prompt);			(* write prompt to terminal *)
      read$line (lex_scan_rec.cmd_line, too_long); (* read response *)
      if too_long then
	check_status (cmd_too_long); (* triggers error - won't return *)
      lex_scan_rec.cursor := 1;				(* reset global command line cursor *)
      lex_scan_rec.cursor_at_next_token := 0;	(* .next_token is undefined at this point *)
      process_cmd_line;  (* process the command line *)
      check_status (status)
    until status = execute;

    if reason in [trap, rterr] then begin
      writ$nl ('Can''t continue after trap or run-time error.');
      goto 100
    end;

    prepare_to_return_to_user;

  end  (* with *);
end.
   T xj