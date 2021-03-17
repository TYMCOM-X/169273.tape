program xref;

$SYSTEM cmdutl
$SYSTEM fio


var title: string [65];
$PAGE symbol table declarations
type
    subr_name = ^ subr_name_node;
    module_name = ^ module_name_node;
    subr = ^ subr_node;
    calls_list = ^ calls_node;
    called_by_list = ^ called_by_node;

    subr_name_string = packed array [1..10] of char;
    module_name_string = packed array [1..6] of char;

    mod_number = 0 .. 255;
    proc_count = 0 .. 32767;

    subr_name_node = packed record
	proc_name: subr_name_string;
	llink, rlink: subr_name;
	r_terminal: boolean;
	sys_proc: boolean;
	unique: boolean;
	modules: subr;
    end;

    module_name_node = packed record
	mod_name: module_name_string;
	proc_list: subr;
	llink, rlink: module_name;
	r_terminal: boolean;
	calling_tag: boolean;
	called_tag: boolean;
	suppress: boolean;
	mod_num: mod_number;
    end;

    subr_node = packed record
	lib_subr: boolean;
	mod_ref: module_name;
	owner: subr_name;
	next: subr;
	next_proc: subr;
	calls: calls_list;
	called_by: called_by_list;
    end;
$PAGE
    calls_node = packed record
	next: calls_list;
	case boolean of
	    true: ( called_proc: subr_name );
	    false: ( called_mod: subr );
    end;

    called_by_node = packed record
	next: called_by_list;
	calling_mod: subr;
    end;

    use_record = packed record
	calling_procs: proc_count;
	called_procs: proc_count;
    end;

    mod_array = ^ array [1..*] of
		    ^ array [1..*] of
			use_record;

var
    n_mods: mod_number;
    module_name_table: module_name;
    subr_name_table: subr_name;
    use_table: mod_array;
$PAGE input file list declarations
type
    input_list = ^ input_node;

    input_node = record
      name: file_name;
      lib_file: boolean;
      next: input_list;
    end;

var
    infiles: input_list;
$PAGE read_file_list
procedure read_file_list ( var outfile: text; var in_list: input_list );

type
    cmdstring = string [256];

var equal_ok: boolean;
    list_end: input_list;

exception
    error;
$PAGE add_input_name
procedure add_input_name ( name: file_name; lib: boolean );

begin
  if in_list = nil then begin
    new (in_list);
    list_end := in_list;
  end
  else begin
    new (list_end^.next);
    list_end := list_end^.next;
  end;
  list_end^ := ( name, lib, nil );
end (* add_input_name *);
$PAGE process_line
procedure process_line ( line: cmdstring; var done: boolean );

var idx: integer;
    name: file_name;
    cmdfile: text;
    cline: cmdstring;
    cdone: boolean;
    i: integer;

begin
  idx := 1;
  done := false;
  while not cmd_eol (line, idx) do begin
    if cmd_check_punct (line, idx, '@') then begin
      if not cmd_file_name (line, idx, true, name) then begin
	writeln (tty, '?Bad command file name');
	signal (error);
      end;
      reset (cmdfile, '.CMD ' || name);
      if iostatus <> io_ok then begin
	reset (cmdfile, '.CCL ' || name);
	if iostatus <> io_ok then begin
	  writeln (tty, '?Cannot open command file ', name);
	  signal (error);
	end;
      end;
      begin
	while not eof (cmdfile) do begin
	  readln (cmdfile, cline);
	  process_line (uppercase (cline), cdone);
	end;
      exception
	error:
	  begin
	    writeln (tty, ' - in command file ', filename (cmdfile), ': ', cline);
	    close (cmdfile);
	    signal;
	  end;
      end;
      close (cmdfile);
      if  cmd_eol (line, idx) then
	done := true
      else if not cmd_check_punct (line, idx, ',') then begin
	writeln (tty, '?Bad character ''', line [idx], ''' after' , name);
	signal (error);
      end;
    end

    else (* not '@' *) begin
      if not cmd_file_name (line, idx, true, name) then begin
	writeln (tty, '?Bad file name');
	signal (error);
      end;
      if cmd_check_punct (line, idx, '=') then begin
	if equal_ok then begin
	  rewrite (outfile, '.XRF ' || name);
	  if iostatus <> io_ok then begin
	    writeln (tty, '?Cannot open output file ', name);
	    signal (error);
	  end;
	end
	else begin
	  writeln (tty, '?Illegal ''='' after ', name);
	  signal (error);
	end;
      end
      else if cmd_check_punct (line, idx, ',') then
	add_input_name (name, false)
      else if cmd_check_punct (line, idx, '/') then begin
	if cmd_lookup (line, idx, ['A'..'Z'], (('LIBRARY', 3, 0)), i) then begin
	  add_input_name (name, true);
	  done := cmd_eol (line, idx);
	end
	else begin
	  writeln (tty, '?Bad switch after ', name);
	  signal (error);
	end;
      end
      else if cmd_eol (line, idx) then begin
	add_input_name (name, false);
	done := true;
      end
      else begin
	writeln (tty, '?Bad character ''', line [idx], ''' after ', name);
	signal (error);
      end;
      equal_ok := false;
    end;
  end;
end (* process_line *);
$PAGE read_file_list - main routine
var line: cmdstring;
    idx: integer;
    fname: file_name;
    done: boolean;

begin
  repeat
    begin
      equal_ok := true;
      outfile := nilf;
      in_list := nil;
      repeat
	cmd_getline ('*', line, idx);
	process_line (uppercase (line), done);
      until done;
      if in_list = nil then
	stop;
      if outfile = nilf then begin
	with in_list^ do
	  fname := substr (name, 1, search (name, ['.', '['], length (name) + 1) - 1);
	rewrite (outfile, fname || '.XRF[,]');
	if iostatus <> io_ok then begin
	  writeln (tty, '?Unable to open output file ', fname);
	  signal (error);
	end;
      end;
    exception
      error:
	begin
	  if outfile <> nilf then
	    close (outfile);
	  while in_list <> nil do begin
	    list_end := in_list^.next;
	    dispose (in_list);
	    in_list := list_end;
	  end;
	  done := false;
	end;
    end;
  until done;
end (* read_file_list *);
$PAGE add_subr_name
(*  ADD SUBR NAME creates a new subr_name tree node with a specified right thread.  *)

function add_subr_name ( name: subr_name_string; sys: boolean; thread: subr_name ): subr_name;
begin
  new (add_subr_name);
  with add_subr_name^ do begin
    proc_name := name;
    llink := nil;
    rlink := thread;
    r_terminal := true;
    sys_proc := sys;
    modules := nil;
  end;
end (* add_subr_name *);
$PAGE enter_subr_name
(*  ENTER SUBR NAME returns a pointer to a subr_name tree node with a specified name.  *)

function enter_subr_name ( name: subr_name_string ): subr_name;

begin
  if subr_name_table = nil then begin
    subr_name_table := add_subr_name (name, false, nil);
    enter_subr_name := subr_name_table;
    return; (* <---- return with newly created subr_name node *)
  end;

  enter_subr_name := subr_name_table;
  loop
    with enter_subr_name^ do begin
      if (proc_name = name) and not sys_proc then
	return; (* <---- return with node found in tree *)

      if (proc_name > name) then begin
	if llink = nil then begin
	  llink := add_subr_name (name, false, enter_subr_name);
	  enter_subr_name := llink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_subr_name := llink
      end

      else (* proc_name <= name *) begin
	if r_terminal then begin
	  rlink := add_subr_name (name, false, rlink);
	  r_terminal := false;
	  enter_subr_name := rlink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_subr_name := rlink;
      end;
    end (* with enter_subr_name^ *);
  end (* loop *);
end (* enter_subr_name *);
$PAGE enter_sys_subr_name
(*  ENTER SYS SUBR NAME is the same as EnterSubrName, except that it enters
    a SysProc in the name tree.  *)

function enter_sys_subr_name ( name: subr_name_string ): subr_name;

begin
  if subr_name_table = nil then begin
    subr_name_table := add_subr_name (name, true, nil);
    enter_sys_subr_name := subr_name_table;
    return; (* <---- node added as root of tree - return *)
  end;

  enter_sys_subr_name := subr_name_table;
  loop
    with enter_sys_subr_name^ do begin
      if (proc_name = name) and sys_proc then
	return; (* <---- return with node found in tree *)

      if (proc_name >= name) then begin
	if llink = nil then begin
	  llink := add_subr_name (name, true, enter_sys_subr_name);
	  enter_sys_subr_name := llink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_sys_subr_name := llink
      end

      else (* proc_name < name *) begin
	if r_terminal then begin
	  rlink := add_subr_name (name, true, rlink);
	  r_terminal := false;
	  enter_sys_subr_name := rlink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_sys_subr_name := rlink;
      end;
    end (* with enter_sys_subr_name^ *);
  end (* loop *);
end (* enter_sys_subr_name *);
$PAGE add_module_name
(*  ADD MODULE NAME creates a new module table node with a specified right thread.  *)

function add_module_name ( name: module_name_string; thread: module_name ): module_name;

begin
  new (add_module_name);
  with add_module_name^ do begin
    mod_name := name;
    proc_list := nil;
    llink := nil;
    rlink := thread;
    r_terminal := true;
    called_tag := false;
    calling_tag := false;
  end;
end (* add_module_name *);
$PAGE enter_module_name
(*  ENTER MODULE NAME returns a pointer to a module table node with a specified name. *)

function enter_module_name ( name: module_name_string ): module_name;

begin
  if module_name_table = nil then begin
    module_name_table := add_module_name (name, nil);
    enter_module_name := module_name_table;
    return; (* <---- return with newly created module node *)
  end;

  enter_module_name := module_name_table;
  loop
    with enter_module_name^ do begin
      if mod_name = name then
	return; (* <---- return with node found in tree *)

      if mod_name > name then begin
	if llink = nil then begin
	  llink := add_module_name (name, enter_module_name);
	  enter_module_name := llink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_module_name := llink;
      end

      else (* mod_name < name *) begin
	if r_terminal then begin
	  rlink := add_module_name (name, rlink);
	  r_terminal := false;
	  enter_module_name := rlink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_module_name := rlink;
      end;
    end (* with enter_module_name^ *);
  end (* loop *);
end (* enter_module_name *);
$PAGE add_subr
(*  ADD SUBR returns a pointer to an entry with a given name on the module
    list for a specified subr_name node.  *)

function add_subr ( m: module_name; pr: subr_name; lib: boolean ): subr;

var last: subr;
    name: module_name_string;

begin
  name := m^.mod_name;
  add_subr := pr^.modules;
  last := nil;
  while (add_subr <> nil) andif (add_subr^.mod_ref^.mod_name <= name) do begin
    last := add_subr;
    add_subr := add_subr^.next;
  end;
  if (add_subr = nil) orif (add_subr^.mod_ref^.mod_name <> name) then begin
    if last = nil then begin
      new (pr^.modules);
      last := pr^.modules;
    end
    else begin
      new (last^.next);
      last := last^.next;
    end;
    with last^ do begin
      lib_subr := lib;
      mod_ref := m;
      next_proc := m^.proc_list;
      owner := pr;
      next := add_subr;
      calls := nil;
      called_by := nil;
    end;
    m^.proc_list := last;
    add_subr := last;
  end;
end (* add_subr *);
$PAGE add_calls
(*  ADD CALLS adds an entry for a specified subr_name to the call list of a given
    subr node.  *)

procedure add_calls ( m: subr; pr: subr_name );

var call, last: calls_list;

begin
  call := m^.calls;
  last := nil;
  while (call <> nil) andif
	( (call^.called_proc^.proc_name < pr^.proc_name) or
	  ( (call^.called_proc^.proc_name = pr^.proc_name) and
	    (call^.called_proc^.sys_proc < pr^.sys_proc) ) ) do begin
    last := call;
    call := call^.next;
  end;
  if (call = nil) orif (call^.called_proc <> pr) then begin
    if last = nil then begin
      new (m^.calls);
      last := m^.calls;
    end
    else begin
      new (last^.next);
      last := last^.next;
    end;
    with last^ do begin
      called_proc := pr;
      next := call;
    end;
  end;
end (* add_calls *);
$PAGE add_called_by
(*  ADD CALLED BY adds an entry for a specified subr node to the called_by list
    of a specified subr node.  *)

procedure add_called_by ( called, caller: subr );

var call, last: called_by_list;

begin
  call := called^.called_by;
  last := nil;
  while (call <> nil) andif (call^.calling_mod <> caller) do begin
    last := call;
    call := call^.next;
  end;
  if call = nil then begin
    if last = nil then begin
      new (called^.called_by);
      last := called^.called_by;
    end
    else begin
      new (last^.next);
      last := last^.next;
    end;
    with last^ do begin
      calling_mod := caller;
      next := call;
    end;
  end;
end (* add_calls *);
$PAGE get_sym_line
(*  GetSymLine reads a cross-reference file line.  If the line has the form
      *PROCEDURE  MODULE
	it sets Kind to DeclLine, ProcId to 'procedure', and ModId to 'module'.
      #PROCEDURE  MODULE
	it sets Kind to LibLine, ProcId to 'procedure', and ModId to 'module'.
      +PROCEDURE
	it sets Kind to SysLine, ProcId to 'procedure', and ModId to blanks.
      bPROCEDURE
	it sets Kind to CallLine, ProcId to 'procedure', and ModId to blanks.  *)

type sym_line_kind = ( decl_line, lib_line, sys_line, call_line );

procedure get_sym_line ( var kind: sym_line_kind;
			 var proc_id: subr_name_string;
			 var mod_id: module_name_string );

var ch: char;
    i: integer;
    temp_proc_id, temp_mod_id: string [256];
    sym_line: string [513];

begin
  case input^ of
    '*': kind := decl_line;
    '#': kind := lib_line;
    '+': kind := sys_line;
    ' ': kind := call_line;
    others: begin
      writeln (tty, '? Bad line in SYM file:');
      while not eoln (input) do begin
	read (input, ch);
	write (tty, ch);
      end;
      writeln (tty);
      stop;
    end;
  end;

  get (input);
  read (input, sym_line);
  readln (input);
  i := search (sym_line, [' '], length (sym_line) + 1);
  temp_proc_id := substr (sym_line, 1, i-1);
  if i < length (sym_line) then
    temp_mod_id := substr (sym_line, i+1)
  else
    temp_mod_id := '';
  if length (temp_mod_id) > 0 then begin
    i := verify (temp_mod_id, [' ']);
    if i > 1 then
      temp_mod_id := substr (temp_mod_id, i)
  end;
  proc_id := temp_proc_id;
  mod_id := temp_mod_id;
end (* get_sym_line *);
$PAGE build_symbol_table
(*  BUILD SYMBOL TABLE will read the symbol file and create the symbol table with
    the complete calls relation.  *)

procedure build_symbol_table;

var
    cur_subr: subr;
    kind: sym_line_kind;
    subr_id: subr_name_string;
    module_id: module_name_string;
    tinf: input_list;

begin
  subr_name_table := nil;
  module_name_table := nil;
  while infiles <> nil do begin
    reset (input, '.SYM ' || infiles^.name);
    if iostatus = io_ok then begin
      while not eof (input) do begin
	get_sym_line (kind, subr_id, module_id);
	case kind of
	  decl_line, lib_line:
	    cur_subr := add_subr (enter_module_name (module_id),
				  enter_subr_name (subr_id),
				  (kind = lib_line) or infiles^.lib_file);
	  call_line:
	    if not cur_subr^.lib_subr then
	      add_calls (cur_subr, enter_subr_name (subr_id));
	  sys_line:
	    if not cur_subr^.lib_subr then
	      add_calls (cur_subr, enter_sys_subr_name (subr_id))
	end (* case *);
      end;
      close (input);
    end
    else
      writeln (tty, '%Cannot open sym file ', infiles^.name);
    tinf := infiles^.next;
    dispose (infiles);
    infiles := tinf;
  end;
end (* build_symbol_table *);
$PAGE linearize_trees
(*  LINEARIZE TREES turns the subr_name tree and module table from trees into simple
    linked lists, chained on the rlink fields of their nodes.  *)

procedure linearize_trees;

var s: subr_name;
    m: module_name;

begin
  if subr_name_table = nil then return;
  while subr_name_table^.llink <> nil do
    subr_name_table := subr_name_table^.llink;
  s := subr_name_table;
  while s^.rlink <> nil do begin
    with s^ do begin
      if not r_terminal then
	while rlink^.llink <> nil do
	  rlink := rlink^.llink;
      s := rlink;
    end (* with s^ *);
  end (* while s^.rlink <> nil *);

  n_mods := 0;
  if module_name_table = nil then return;
  while module_name_table^.llink <> nil do
    module_name_table := module_name_table^.llink;
  m := module_name_table;
  loop
    with m^ do begin
      n_mods := n_mods + 1;
      mod_num := n_mods;
  exit if rlink = nil;
      if not r_terminal then
	while rlink^.llink <> nil do
	  rlink := rlink^.llink;
      m := rlink;
    end (* with m^ *);
  end (* while m^.rlink <> nil *);
end (* linearize_trees *);
$PAGE resolve_modules
(*  RESOLVE MODULES ensures that every subr_name node has at least one module node.
    If it doesn't, then it gets a module with a blank name.  In addition, the
    unique field of the subr_name node is set if the subr_name has exactly one module
    node.  *)

procedure resolve_modules;

var
    s: subr_name;
    m_sys, m_undef: module_name;
    stemp: subr;

begin
  m_sys := add_module_name ('*SYS*  ', nil);
  m_undef := add_module_name ('*UNDEF', nil);
  s := subr_name_table;
  while s <> nil do begin
    if s^.modules = nil then
      if s^.sys_proc
	then stemp := add_subr (m_sys, s, false)
	else stemp := add_subr (m_undef, s, false);
    s^.unique := (s^.modules^.next = nil);
    s := s^.rlink;
  end;
end (* resolve_modules *);
$PAGE construct_called_by_relation
(*  CONSTRUCT CALLED BY RELATION adds called_by lists to the symbol table to
    complete the called_by relation.  *)

procedure construct_called_by_relation;

var
    n: subr_name;
    s: subr;
    c: calls_list;
    cs: subr;

begin
  n := subr_name_table;
  while n <> nil do begin
    s := n^.modules;
    while s <> nil do begin
      c := s^.calls;
      while c <> nil do begin
	cs := c^.called_proc^.modules;
	  while (cs^.mod_ref <> s^.mod_ref) and
		(cs^.nextil) do
	  cs := cs^.next;
	if cs^.mod_ref <> s^.mod_ref then
	  cs := c^.called_proc^.modules;
	c^.called_mod := cs;
	add_called_by (cs, s);
	c := c^.next;
      end;
      s := s^.next;
    end;
    n := n^.rlink;
  end;
end (* construct_called_by_relation *);
$PAGE suppress_library_subrs
(*  SUPPRESS LIBRARY SUBRS sets the Suppress of any module name nodes with
    no called or non-library subr nodes.  *)

procedure suppress_library_subrs;

var
    m: module_name;
    s: subr;

begin
  m := module_name_table;
  while m <> nil do begin
    m^.suppress := true;
    s := m^.proc_list;
    while s <> nil do begin
      exit if (s^.called_by <> nil) or (not s^.lib_subr) do
	m^.suppress := false;
      s := s^.next;
    end (* while s <> nil *);
    m := m^.rlink;
  end (* while m <> nil *);
end (* suppress_library_subrs *);
$PAGE construct_use_table
(*  CONSTRUCT USE TABLE creates the vectors containing use_records for each
    calling module/called module pair.  *)

procedure construct_use_table;

var m1, m2: mod_number;
    m, mm: module_name;
    pr: subr;
    pr_calls: calls_list;
    pr_called: called_by_list;

begin
  new (use_table, n_mods);
  for m1 := 1 to n_mods do begin
    new (use_table^[m1], n_mods);
    for m2 := 1 to n_mods do begin
      with use_table^[m1]^[m2] do begin
	called_procs := 0;
	calling_procs := 0;
      end;
    end;
  end;

  m := module_name_table;
  while m <> nil do begin
    pr := m^.proc_list;
    while pr <> nil do begin
      pr_calls := pr^.calls;
      while pr_calls <> nil do begin
	pr_calls^.called_mod^.mod_ref^.called_tag := true;
	pr_calls := pr_calls^.next;
      end;
      pr_called := pr^.called_by;
      while pr_called <> nil do begin
	pr_called^.calling_mod^.mod_ref^.calling_tag := true;
	pr_called := pr_called^.next;
      end;
      mm := module_name_table;
      while mm <> nil do begin
	with use_table^[m^.mod_num]^[mm^.mod_num] do begin
	  if mm^.called_tag then
	    calling_procs := calling_procs + 1;
	  if mm^.calling_tag then
	    called_procs := called_procs + 1;
	end;
	mm^.calling_tag := false;
	mm^.called_tag := false;
	mm := mm^.rlink;
      end;
      pr := pr^.next_proc;
    end;
    m := m^.rlink;
  end;

end (* construct_use_table *);
$PAGE width
(* WIDTH returns the minimum number of character positions required to
   print its integer parameter *)

function width (val: integer): integer;

var
    absval: integer;

begin
  width := ord (val < 0);
  absval := abs (val);
  repeat
    width := width + 1;
    absval := absval div 10
  until absval = 0;
end;
$PAGE cv_int
(* CV_INT converts an integer to a minimal length string *)

function cv_int (val: integer): string;

begin
  putstring (cv_int, val: 0);
end;
$PAGE cvf_int
(* CVF_INT converts an integer to a fixed-length string *)

function cvf_int (val, columns: integer): string;

begin
  putstring (cvf_int, val: columns);
  if length (cvf_int) > columns then
    cvf_int := substr ('********************', 1, columns);
end;
$PAGE dump_relations
(*  DumpRelations will create the output cross-reference listing from the
    calls and called_by lists in the symbol table.  *)

procedure dump_relations;

var
    fname: string [40];
    fb: file_block;
    dumping: boolean;
    pr: subr_name;
    m: subr;
    calls: calls_list;
    called: called_by_list;
    none: subr;
    mm, mm1, mm2: module_name;
$PAGE print_proc_title - in dump_relations
(*  PrintProcTitle prints the page title information for each page of the cross
    reference listing.  *)

procedure print_proc_title ( var fb: file_block );

begin
  fio_write (fb, 'Cross Reference Listing for ' || title);
  fio_tab (fb, 100 - width (fb.pageno));
  fio_line (fb, 'Page ' || cv_int (fb.pageno));
  fio_tab (fb, 96);
  fio_line (fb, date);
  fio_line (fb, '     Procedure   in             Called By   in       ' ||
		'Calls       in');
  fio_line (fb, '     ----------  ------         ----------  ------   ' ||
		'----------  ------');
  fio_skip (fb);
  if dumping then
    fio_write (fb, '     ' || pr^.proc_name || '  ' || m^.mod_ref^.mod_name ||
	       ' (cont.)');
end (* print_proc_title *);
$PAGE dump_proc_relations
(*  DumpProcRelations dumps the procedural cross reference listing.  *)

procedure dump_proc_relations;

begin
  fb.page_header := print_proc_title;
  dumping := false;
  print_proc_title (fb);
  pr := subr_name_table;
  while pr <> nil do begin
    m := pr^.modules;
    while m <> nil do begin
      if (m^.called_by <> nil) or (not m^.lib_subr) then begin
	if pr^.unique
	  then fio_write (fb, '     ')
	  else fio_write (fb, '  *  ');
	fio_write (fb, pr^.proc_name || '  ' || m^.mod_ref^.mod_name);
	if m^.called_by = nil then
	  add_called_by (m, none);
	calls := m^.calls;
	called := m^.called_by;
	dumping := true;
	while (calls <> nil) or (called <> nil) do begin
	  if called <> nil then begin
	    fio_tab (fb, 33);
	    fio_write (fb, called^.calling_mod^.owner^.proc_name || '  ' ||
			   called^.calling_mod^.mod_ref^.mod_name);
	    called := called^.next;
	  end;
	  if calls <> nil then begin
	    fio_tab (fb, 54);
	    fio_write (fb, calls^.called_mod^.owner^.proc_name || '  ' ||
			   calls^.called_mod^.mod_ref^.mod_name);
	    if not calls^.called_mod^.owner^.unique then
	      if calls^.called_mod^.mod_ref^.mod_name = m^.mod_ref^.mod_name
		then fio_write (fb, '#')
		else fio_write (fb, '##');
	    calls := calls^.next;
	  end;
	  fio_skip (fb);
	end (* while (calls <> nil) or (called <> nil) *);
	dumping := false;
	fio_skip (fb);
      end;
      m := m^.next;
    end;
    pr := pr^.rlink;
  end;
end (* dump_proc_relations *);
$PAGE print_mod_title - in dump_relations
(*  PrintModTitle prints the title for the module cross-reference listing.  *)

procedure print_mod_title ( var fb: file_block );

begin
  fio_write (fb, 'Module Cross Reference Listing for ' || title);
  fio_tab (fb, 100 - width (fb.pageno));
  fio_line (fb, 'Page ' || cv_int (fb.pageno));
  fio_tab (fb, 96);
  fio_line (fb, date);
  fio_line (fb, '     Module         #Procs  Called  #Procs ' ||
		'     #Procs  Call    #Procs');
  fio_line (fb, '     ------         ------  ------  ------ ' ||
		'     ------  ------  ------');
  fio_skip (fb);
  if dumping then
    fio_write (fb, '     ' || mm^.mod_name || ' (cont.)');
end (* print_mod_title *);
$PAGE dump_mod_relations - in dump_relations
(*  DumpModRelations prints the module cross reference listing.  *)

procedure dump_mod_relations;

begin
  fb.pageno := 0;
  fb.page_header := print_mod_title;
  dumping := false;
  fio_page (fb);

  mm := module_name_table;
  while mm <> nil do begin
    if not mm^.suppress then begin
      fio_write (fb, '     ' || mm^.mod_name);
      mm1 := module_name_table;
      mm2 := module_name_table;
      dumping := true;
      loop
	while (mm1 <> nil) andif (use_table^[mm^.mod_num]^[mm1^.mod_num].called_procs = 0) do
	  mm1 := mm1^.rlink;
	while (mm2 <> nil) andif (use_table^[mm^.mod_num]^[mm2^.mod_num].calling_procs = 0) do
	  mm2 := mm2^.rlink;
      exit if (mm1 = nil) andif (mm2 = nil);
	if mm1 <> nil then begin
	  fio_tab (fb, 21);
	  fio_write (fb, cvf_int (use_table^[mm^.mod_num]^[mm1^.mod_num].called_procs, 6) ||
			 '  ' || mm1^.mod_name || '  ' ||
			 cv_int (use_table^[mm1^.mod_num]^[mm^.mod_num].calling_procs));
	  mm1 := mm1^.rlink;
	end;
	if mm2 <> nil then begin
	  fio_tab (fb, 49);
	  fio_write (fb, cvf_int (use_table^[mm^.mod_num]^[mm2^.mod_num].calling_procs, 6) ||
			 '  ' || mm2^.mod_name || '  ' ||
			 cv_int (use_table^[mm2^.mod_num]^[mm^.mod_num].called_procs));
	  mm2 := mm2^.rlink;
	end;
	fio_skip (fb);
      end;
      dumping := false;
      fio_skip (fb);
    end;
    mm := mm^.rlink;
  end;
end (* dump_mod_relations *);
$PAGE dump_relations - main routine
begin
  fio_attach (fb, output);
  fb.plength := 45;
  none := add_subr (add_module_name ('', nil), enter_subr_name ('<NONE>'), false);

  dump_proc_relations;

  dump_mod_relations;

  fio_close (fb);
end (* dump_relations *);
$PAGE xref - main program
begin
  open (tty);
  rewrite (tty);
  writeln (tty, 'XREF Version 3.0');
  writeln (tty);
  read_file_list (output, infiles);
  write (tty, 'Title?  ');
  break;
  readln (tty);
  read (tty, title);
  build_symbol_table;
  linearize_trees;
  resolve_modules;
  construct_called_by_relation;
  suppress_library_subrs;
  construct_use_table;
  dump_relations;
end.
  B@l“