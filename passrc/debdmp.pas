$TITLE DEBDMP - Pascal Debugger display command routines
  
$HEADER debdmp

module debdm$  options nocheck, special (coercions, word, ptr);

$INCLUDE debug.typ
  
$INCLUDE debio.inc
$INCLUDE debbrk.inc
$INCLUDE debsym.inc
$INCLUDE debug.inc
$INCLUDE debscp.inc
$INCLUDE deblib.inc
$INCLUDE debasm.inc
$PAGE dmp$modules
(* DMP$MODULES prints the names of the Pascal modules in the user
   program.  However since the module names are obtained from the
   LINK symbol table, modules in overlay segments are not included.  *)

public procedure dmp$modules;

var
  link_ptr:    ^linkentry;
  module_name: id_string;

begin
  ld$ovl (0);				     (* load the high seg of main routine, since we 
						access the code area via the FIRSTWORD pointer
						in the link symbol table *)
  link_ptr := first$module;

  while link_ptr <> nil do begin		(* process one module name per iteration *)
    if link_ptr^.modname = link_ptr^.firstword^.r50_name then begin
      (* only show Pascal modules *)
      module_name := r50$asc (link_ptr^.modname);
      if index (module_name, '$') = 0 then begin	(* Ignore Debugger's modules *)
	writ$str (module_name);
	if ord (link_ptr^.firstword^.program_block.last_file) = 0 then
	  writ$str ('(NOT DEBUG)');
	writ$eol
      end
    end;
    link_ptr := next$module (link_ptr)
  end;

  with db$base^ do			(* restore overlay module containing *)
    if not def_mod_in then begin		(* currently open scope, if necessary *)
      a$$ert (def_ovl_mod <> nil_ovl_modno);
      ld$ovl (def_ovl_mod);
    end
end (* dmp$modules *);
$PAGE dmp$files
(* DMP$FILES prints the names and numbers of all the files in 
   a given module.  The program block pointer for the module is
   taken from the PROGRAM_BLOCK field of parameter AUG_SOURCE_ID,
   if that field is not NIL.  If PROGRAM_BLOCK is NIL then the 
   module name in the source id record of AUG_SOURCE_ID is
   looked up in the link symbol table to obtain the address of
   the program block.
  
   Note that:
     1. The module containing the file is assumed to be resident in
	core when this routine is called,
     2. If a module name is supplied, the module will be found only
	if it is not in an overlay segment.			   *)

public procedure dmp$files (    aug_source_id:	augmntd_source_id;
			    var status:		status_code);

var
  prog_blk:   ^prog_block;
  file_blk: file_block_ptr;

begin
  with aug_source_id do			(* get a ptr to the prog blk of the module
						   containing the files *)
    if program_block = nil 
      then get$prog$ptr (source_id.module_name, prog_blk, status)
    else
      prog_blk := program_block;
  if status in severe then return;

  file_blk := prog_blk^.last_file;		(* Initialize file block pointer *)
  if file_blk = nil then
    status := no_file_block  (* module has no executable statements *)
  else

    (* Print one file name/number per iteration of while loop.
       Files are printed in the order they are linked, i.e., reverse
       order by number.  *)

    while file_blk <> nil do begin
      with file_blk^ do begin
	writ$int (file_number, decimal_radix);
	if file_name <> '' then
	  writ$str ('  ' || file_name);
	writ$eol
      end;
      file_blk := file_blk^.previous_file
    end
end (* dmp$files *);
$PAGE dmp$pages
(* DMP$PAGES prints the numbers and subtitles of all the pages within a
   given module and file.  The program block pointer for the module is
   taken from the PROGRAM_BLOCK field of parameter AUG_SOURCE_ID, if 
   that field is not NIL.  If PROGRAM_BLOCK is NIL, then the module name
   in the source id record of AUG_SOURCE_ID is looked up in the LINK
   symbol table to obtain the address of the program block.
  
   Note that:
     1. the module is assumed to be resident in core when this routine
	is called,
     2. if a module name is specified, the module will be found only if
	it is not in an overlay segment.			   *)

public procedure dmp$pages (    aug_source_id:	augmntd_source_id;
			    var status:		status_code);

var
  prog_blk:   ^prog_block;
  file_blk: file_block_ptr;
  page_blk: page_block_ptr;

begin
  (* Get pointers to the program block of the module containing the file
     and to the file block of the file containing the pages.  *)

  with aug_source_id do begin
    if program_block = nil then			(* get program block ptr *)
      get$prog$ptr (source_id.module_name, prog_blk, status)
    else
      prog_blk := program_block;
    if status in severe then return;		(* <-- error return *)

    with source_id do				(* get file block pointer *)
      get$file$ptr (prog_blk, file_name, file_num, file_blk, status);
    if status in severe then return;		(* <-- error return *)
  end;

  page_blk := file_blk^.last_page;		(* initialize page block ptr *)

  (* Print one page number and subtitle per iteration of while loop.
     Pages are printed in the order linked, i.e., reverse numeric order.  *)

  while ord (page_blk) <> ord (file_blk) do begin
    with page_blk^ do begin
      writ$int (page_number, decimal_radix);
      if subtitle <> '' then
	writ$str ('  ' || subtitle);
      writ$eol
    end;
    page_blk := page_blk^.previous_page
  end
end (* dmp$pages *);
$PAGE dmp_source
(* DMP_SOURCE prints out a source id.  File and page numbers (rather than
   names) are printed.  File and page numbers are omitted if they are zero.
  
   The format is <mod-name>@ [ <file-num>- ] [ <page-num>/ ] <line-num> *)


procedure dmp_source (source_id: source_id_record);
  
begin
  
  with source_id do begin
    writ$str (module_name);      (* write module name *)
    writ$str ('@');
    if file_num <> 0 then begin   (* write the file number *)
      writ$int (file_num, decimal_radix);
      writ$str ('-')
    end;
    if page_num <> 0 then begin   (* write the page number *)
      writ$int (page_num, decimal_radix);
      writ$str ('/')
    end;
    writ$int(line_num,decimal_radix)   (* and finally, the line number *)
  end
 
end (* dmp_source *);
$PAGE dmp$breaks
(* DMP$BREAKS prints the locations of all breakpoints which are
   currently set.  If the program is overlaid, this routine will
   load any non-resident modules containing breakpoints.  The
   module containing the current scope is brought back into
   core, if necessary, before returning.  *)

public procedure dmp$breaks (var status: status_code);

label  100;

var
  no_breaks: boolean;
  i:         brkpt_range;
  source_id: source_id_record;

begin
  with db$base^, rt$base^ do begin
    no_breaks := true;

    for i := minimum (brkpt_range) to maximum (brkpt_range) do
      if brk_table[ i ].stmt_blk_addr <> nil then	(* i.e. if brkpt is set *)
	with brk_table[ i ] do begin
	  no_breaks := false;
	  if not in_core			(* load module containing brkpt *)
	    then ld$ovl (mod_no);
	  writ$int (i, decimal_radix);		(* write brkpt number *)
	  writ$str  (' ');
	  info$stmt (stmt_blk_addr, source_id, status);	(* get source id for stmt at
						   which brkpt is set *)
	  if status in severe then goto 100;	(* ALWAYS restore in-scope module *)
	  dmp_source (source_id);		(* print source ref *)
	  writ$nl ('  ' || brk_strings[ i ])	(* print corresponding brkpt string *)
	end;

    if no_breaks then
      writ$nl ('No breakpoints set.');

100:
    if not def_mod_in then				(* restore overlay containing currently *)
      ld$ovl (def_ovl_mod);		(* open module if necessary *)
  end
end (* dmp$breaks *);
$PAGE dmp$location
(* DMP_LOCATION prints the reason that the Debugger was entered (this time
   in), and the location at which it was entered.  If the module corresponding
   to the topmost stack frame is in DEBUG mode, the source_id (cstmt) is
   printed.  Otherwise, the routine name is printed (if in TRACE mode). *)


public procedure dmp$location;

var
  rt_static: ^rt_static_record;
  status:    status_code;
  source_id: source_id_record;
  proc_name: id_string;
  proc_blk:  ^procedure_block;
  stkframe:  ^stack_frame;

begin
  status := success;	(* initialize our private status code *)
  rt_static := rt$base; (* get the base of area shared with runtime *)
  
  case rt_static^.reason of (* print why we're here *)
    init:
      writ$nl ('First statement of main program');
    badreason:
      writ$nl ('Erroneous entry to Debugger -- Please report');
    trap:
      writ$str ('Debugger entered after trap');
    rterr:
      writ$str ('Debugger entered after run-time error');
    step: ;  (* just tell him where we are *)
    brkpt: begin
      writ$str ('Breakpoint #');
      writ$int (rt_static^.brkpt_num, decimal_radix)
    end;
    intrp:
      writ$str ('Debugger entered after break hit')
  end;
  
  if not (rt_static^.reason in [trap, rterr, step, brkpt, intrp]) then return;
  stkframe := rt_static^.basis_rec.basis;
  if over$laid then
    load$overlay (stkframe);
  proc_name := proc$name (stkframe);
  if proc_name = '' then begin  (* module not in DEBUG or TRACE mode *)
    if main$frame (stkframe) then
      writ$str (' in Main Program')
    else begin
      writ$str (' in Unknown routine beginning at ');
      writ$int (ord (stkframe^.link_addr)-1, octal_radix)   (* addr(jsp 1,pntry.) at beginning *)
    end
  end
  else begin  (* print routine name, and statement if in DEBUG *)
    if main$frame (stkframe) then
      writ$str (' in Main Program')
    else begin
      writ$str (' in routine ');
      writ$str (proc_name)
    end;
    proc_blk := proc$blk (stkframe);
    a$$ert (proc_blk <> nil);  (* Since module in DEBUG or TRACE *)
    if (proc_blk^.prog_blk <> nil) andif (* in DEBUG, print stmt addr *)
       (ord (proc_blk^.prog_blk^.last_file) <> 0) then begin
      info$stmt (rt_static^.cstmt, source_id, status);
      if not (status in severe) then begin
	writ$str (' at ');
	dmp_source (source_id)
      end
    end
  end;
  writ$eol;   (* finish output line *)

  (* restore default overlay if necessary *)

  with db$base^ do
    if not def_mod_in then begin
      a$$ert (def_ovl_mod <> nil_ovl_modno);
      ld$ovl (def_ovl_mod)
    end
end (* dmp$location *);
$PAGE dmp$scope
(* DMP$SCOPE prints out the current scope stack.  Note that the module in scope
   must be in core, and it must be in DEBUG mode.  *)


public procedure dmp$scope (    scope_stack: scope_type;
			    var status:      status_code);

var
  proc_blk:	blk;
  modname:	id_string;
  level:	display_lvl_range;

begin
  (* For module or program, dump name and high and low segment bases *)

  with scope_stack.displays[1] do begin
    get$mod$name (prog_blk, modname, status);
    if status in severe then return;
    writ$str (modname);
    writ$str (' (HISEG=');
    writ$int (prog_blk^.hiseg_base, octal_radix);
    writ$str (', LOWSEG=');
    writ$int (prog_blk^.lowseg_base, octal_radix);
    writ$nl (')')
  end;

  (* For routines dump name and stack frame base.  Note - an open routine need not be active.  *)

  for level := 2 to scope_stack.display_levels do
    with scope_stack.displays[level] do begin
      proc_blk := deref$ptr (blk_node);
      a$$ert (proc_blk^.kind = subr_blk);
      with nam (deref$ptr (sym (deref$ptr (proc_blk^.subr_sym))^.name))^ do
	writ$str (substr (text, 1, len));
      writ$str (' (BASIS=');
      if stackbase = nil then
	writ$str ('UNDEFINED')
      else
	writ$int (ord (stackbase), octal_radix);
      writ$nl (')')
    end
end (* dmp$scope *);
$PAGE dmp$stack
(* DMP$STACK dumps a specified number of frames starting with the topmost.
   If zero is passed, all frames are dumped.  Information dumped includes:
 
      - Frame number (main = 1),
      - Routine name ('Unknown' or 'Mainline' if not in DEBUG or TRACE modes),
      - BASIS, which is the octal address of the frame itself on the stack,
      - LVL, which is the lexical level of the routine (main = 0, etc.),
      - LOC, which is either the octal or symbolic address of the routine, and
      - CALL, which is the octal or symbolic address of the point of call. *)

public procedure dmp$stack (frames_to_dump: stack_level)  options dump(dags);

type
  inst_word = packed record   (* used to fetch address field from instructions *)
    ops_and_acs: half_word;   (* currently unused *)
    inst_addr: half_word   (* the field of interest *)
  end;

var
  curframe,   (* count of frames dumped thus far in the main loop *)
  numframes:	 stack_level;   (* total number of frames on stack *)
  rt_static:	 ^rt_static_record;   (* pointers to our global "static" storage *)
  db_static:	 ^db_static_record;
  stkframe:	 ^stack_frame;   (* current stack frame in loop *)
  status:	 status_code;   (* for holding error code from info$stmt *)
  proc_name:	 id_string;   (* name of routine associated with current frame *)
  proc_blk:	 ^procedure_block;   (* pointer to procedure block of current frame *)
  source_id:	 source_id_record;
  module_number:	 ovl_mod_no;
  ret_addr:	 unit_range;
  stmt_rtn_addr: unit_range;   (* for holding address of STMT. routine *)
  inst_ptr:	 ^inst_word;   (* for examining code words *)

begin

  rt_static := rt$base;   (* get pointers for addressing permanent storage *)
  db_static := db$base;

  (* Determine the total number of stack frames by marching up stack once. *)

  numframes := 0;
  stkframe := rt_static^.basis_rec.basis;
  repeat
    stkframe := call$basis (stkframe);
    numframes := numframes + 1
  until stkframe = nil;

  (* Insure module corresponding to topmost frame in if program overlaid *)

  with db_static^ do
    if not cur_stmt_in then begin
      a$$ert (cur_ovl_mod <> nil_ovl_modno);
      ld$ovl (cur_ovl_mod)
    end;

  (* March up the stack again, dumping the goodies for each frame *)

  curframe := 1;
  stkframe := rt_static^.basis_rec.basis;
  proc_blk := proc$blk (stkframe);
  loop

    (* Print out frame number *)

    writ$int (numframes, decimal_radix);
    writ$str (' ');

    (* Print out routine name *)

    proc_name := proc$name (stkframe);
    if proc_name = '' then   (* not in DEBUG or TRACE mode *)
      if main$frame (stkframe) then
	writ$str ('Mainline')
      else
	writ$str ('Unknown')
    else writ$str (proc_name);

    (* Print out BASIS value, i.e., octal dump of stkframe pointer itself *)

    writ$str (' BASIS=');
    writ$int (ord (stkframe), octal_radix);

    (* Print out lexical level, which we get from procedure block, if present *)

    if proc_blk <> nil then begin
      writ$str (' LVL=');
      writ$int(proc_blk^.pflev-1, decimal_radix)   (* level is stored as +1 *)
    end;

    (* Print location of beginning of routine, as best as we can determine it *)

    writ$str (' LOC=');
    if (proc_blk=nil) orif (proc_blk^.prog_blk=nil) orif (* not in DEBUG *)
       (ord (proc_blk^.prog_blk^.last_file) = 0) then
      writ$int(ord (stkframe^.link_addr)-1, octal_radix)  (* addr(PNTRY. call) *)
    else begin   (* print out the source id of the call *)
      status := success;   (* info$stmt requires initialization of this flag *)
      info$stmt (proc_blk^.first_stmt, source_id, status);
      if status in severe then
	writ$str ('?')   (* a weak attempt at a diagnostic *)
      else
	dmp_source (source_id)
    end;

    (* Now the fun one, printout of the call location, which we do not
       do for the mainline, for obvious reasons.  The first step is to
       determine if an overlay switch may have occurred in the course of
       the call, and if so, we force the overlay containing the caller in. *)

    if not main$frame (stkframe) then begin
      ret$addr (stkframe, ret_addr, module_number);
      if module_number <> nil_ovl_modno then
	ld$ovl (module_number);
      writ$str (' CALL=');

      (* At this point we advance stkframe and proc_blk to the caller's frame *)

      stkframe := call$basis (stkframe);
      proc_blk := proc$blk (stkframe);

      (* And proceed to print the info *)

      if (proc_blk=nil) orif (proc_blk^.prog_blk=nil) orif
         (ord (proc_blk^.prog_blk^.last_file) = 0) then
	writ$int(ret_addr-1, octal_radix)   (* address of PUSHJ to routine *)
      else begin   (* caller in DEBUG mode, so printout source id *)

	 (* But first, we must find the STMT. call which precedes the
	    PUSHJ.  We determine the address of STMT. (which may be a
	    transfer vector address) by inspecting the "first statement"
	    pointed to from the procedure block. *)

	inst_ptr := ptr (ord (proc_blk^.first_stmt)-1);
	stmt_rtn_addr := inst_ptr^.inst_addr;   (* address in right halfword *)
	inst_ptr := ptr (ret_addr-1);   (* point one before the return address *)
	while inst_ptr^.inst_addr <> stmt_rtn_addr do
	   inst_ptr := ptr (ord (inst_ptr)-1);   (* and search backwards from it *)
	status := success;   (* now call info$stmt with the word after the call *)
	info$stmt (ptr (ord (inst_ptr)+1), source_id, status);
	if status in severe then
	  writ$str ('?')
	else
	  dmp_source (source_id)
      end (* caller in DEBUG mode *)
    end (* not main$frame *)
    else stkframe := nil;   (* terminate the dump arbitrarily at mainline *)
    writ$eol;   (* terminate the output line *)

  exit if ((curframe>=frames_to_dump) and (frames_to_dump>0)) or (stkframe=nil);

    curframe := curframe + 1;   (* increment count of frames dumped *)
    numframes := numframes - 1;   (* and decrement current frame number *)

  end (* loop *);

  (* Insure that the default module, i.e., the one containing the currently
     open scope is in if the program is overlaid before we leave. *)

  with db_static^ do
    if not def_mod_in then begin
      a$$ert (def_ovl_mod <> nil_ovl_modno);
      ld$ovl (def_ovl_mod)
    end
end (* dmp$stack *).
  