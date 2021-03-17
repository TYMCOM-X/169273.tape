$TITLE DEBSCP - Pascal Debugger scope stack routines
  
$HEADER debscp

module debsc$  options nocheck, special (coercions, word, ptr);

$INCLUDE debug.typ
  
$INCLUDE debsym.inc
$INCLUDE debio.inc
$INCLUDE debug.inc
$INCLUDE deblib.inc

const
    entrysize = 2;  (* size of link symboltable entries *)

$PAGE r50$asc
const
  r50period = 45b;
  r50dollar = 46b;
  r50percent = 47b;
  linknamesize = 6;

type
  r50chtype = 0..47b;

public function r50$asc (r50name: r50word): id_string;

  var
    text: packed array [1..linknamesize] of char;
    i:     1..linknamesize;
    temp: r50word;

  function convert_ch (r50ch: r50chtype): char;
    begin
      case r50ch of
	0:	    convert_ch := '?';
	1..12b:	    convert_ch := chr (r50ch+57b);
	13b..44b:   convert_ch := chr (r50ch+66b);
	r50period:  convert_ch := '.';
	r50dollar:  convert_ch := '$';
	r50percent: convert_ch := '_'  (* actually a percent, but underbar in pascal *)
      end
    end;

begin		    (* r50$asc *)
  temp := r50name;
  i := linknamesize + 1;
  repeat
    i := i - 1;
    text[i] := convert_ch (temp mod 50b);
    temp := temp div 50b
  until (temp = 0) or (i = 1 (* to be safe *));
  r50$asc := text [i:linknamesize - i + 1]
end (* r50$asc *);
$PAGE asctor50
function asctor50 (ascname: id_string): r50word;

var
  i:		1..linknamesize;
  cur$ch:	char;
  r50ch:	r50chtype;

begin
  asctor50 := 0;
  for i := 1 to min (linknamesize, length (ascname)) do begin	(* only look at first 6 chars *)
    cur$ch := ascname[i];
  exit if cur$ch = ' ';
    asctor50 := asctor50 * 50b;
    case cur$ch of
      'a'..'z':
	r50ch := ord (cur$ch) - 126b;
      'A'..'Z':
	r50ch := ord (cur$ch) - 66b;
      '0'..'9':
	r50ch := ord (cur$ch) - 57b;
      '.':
	r50ch := r50period;
      '$':
	r50ch := r50dollar;
      '_':
	r50ch := r50percent;	(* converts to percent *)
      others:
	r50ch := 0  (* includes blank *)
    end;	    
    asctor50 := asctor50 + r50ch
  end		    
end (* asctor50 *);
$PAGE first$module, next$module
const
  jbsym = 116b;

type
  linkhead_ptr = ^packed record
    linkstsize: half_word;	(* neg count of # wds in link st *)
    firstsymbol: ^linkentry
  end;
  
(* FIRST$MODULE - returns a pointer to the first (actually the last) symbol
   in the link symbol table.  *)
  
public function first$module: ^linkentry;

begin
  with linkhead_ptr (jbsym)^ do
    if linkstsize = 0 then
      first$module := nil
    else
      first$module := ptr (ord(firstsymbol) + (1000000b - linkstsize) - entrysize)
end (* first$module *);


  
(* NEXT$MODULE - returns a pointer to the next module entry, or nil if we have
   reached the end of the chain (or if a module's st size = 0).  *)
  
public function next$module (curmodule: ^linkentry): ^linkentry;

begin
  if curmodule <> nil then
    with curmodule^ do
      if stsize = 0 then
	next$module := nil
      else begin
	next$module := ptr (ord(curmodule) - (1000000b - stsize));
	if ord(next$module) < ord(linkhead_ptr (jbsym)^.firstsymbol) then
	  next$module := nil
      end
  else
    next$module := nil
end (* next$module *);
$PAGE mod$lookup
(* MOD$LOOKUPsearches through module entries in the link
   symboltable for name. Nil is returned if name is not found *)

public function mod$lookup (name: id_string): ^linkentry;


var
  r50name: r50word;

begin
  mod$lookup := first$module;
  r50name := asctor50 (name);
  while mod$lookup <> nil do
    with mod$lookup^ do
      if link_symtab_type = modtype then begin
	if modname = r50name then
	  return;
	mod$lookup := next$module (mod$lookup)
      end
end (* mod$lookup *);
$PAGE sym$lookup -- look up external symbol in link symbol table
(* SYM$LOOKUP searches the link symboltable for the specified symbol
   and returns the address of that symbol table entry if found, otherwise it
   returns nil. if modname = null string, the entire st is searched,
   otherwise the module is searched first, then that section is searched 
   for the symbol and the name of the module found in is returned. *)

public function sym$lookup (    name:		id_string;
			    var modname:	id_string): ^linkentry;



var
  modptr: ^linkentry;


  function search_module (name:		id_string;
			  curmodule:	^linkentry): ^linkentry;

  (* This routine searches the section of the link symboltable
     which contains symbols for that module, for the name specified.
     If found, core address of name is returned, else zero is returned *)

  var
    r50name: r50word;

  begin		    (* search_module *)
    search_module := next$module (curmodule);
    if search_module <> nil then begin
      search_module := ptr (ord(search_module) + entrysize);
      r50name := asctor50 (name);
      while (search_module <> curmodule) do
	with search_module^ do
	  if link_symtab_type <> modtype then begin
	    if symname = r50name then
	      return
	    else
	      search_module := ptr (ord(search_module) + entrysize)
	  end;
      search_module := nil;
    end
  end (* search_module *);


begin
  if modname <> '' then
    sym$lookup := search_module (name, mod$lookup(modname))
  else begin
    modptr := first$module;
    while modptr <> nil do begin
      sym$lookup := search_module (name, modptr);
    exit if sym$lookup <> nil;
      modptr := next$module (modptr);
    end;
    if sym$lookup <> nil then
      modname := r50$asc (modptr^.modname)
  end
end (* sym$lookup *);
$PAGE over$laid
(* OVER$LAID returns true if the overlay manager is present and returns
   false otherwise.  *)

public function over$laid: boolean;

var
  mod_name: id_string;

begin
  mod_name := '';
  over$laid := sym$lookup ('OVL.TV', mod_name) <> nil
end;
$PAGE open$deb$file
(* OPEN$DEB$FILE opens a symbol table file.  PROGRAM_BLOCK is a
   pointer to the program block of the corresponding .REL file.
   STATUS is set to DTIME_MISMATCH if the day/time word in
   the program block of the .REL file does not match that of
   the program or module block node in the .DEB file.    
   The program block pointer is assumed to be non-NIL.  *)

public procedure open$deb$file (    program_block:	^prog_block;
				var status:		status_code);

begin
  st$open (program_block^.symfile_name || ' .DEB');		(* open .DEB file *)
  (* first child of rootblock is module or program block *)
  
  if blk (deref$ptr (blk (deref$ptr (level0blk))^.children))^.comp_dtime
       <> program_block^.comp_date then begin
    status := dtime_mismatch;
    writ$nl ('Compilation times of REL file and DEB file do not match.');
    writ$nl ('REL file: ' || program_block^.symfile_name)
  end
end (* open$deb$file *);
$PAGE main$frame
(* MAIN_FRAME determines if a given stack frame is for the main
   routine.  STKFRAME is a pointer to the stack frame;  the function 
   value returned will be true if the frame is of the main routine
   and will be false otherwise.  *)

public function main$frame (stkframe: ^stack_frame): boolean;

begin
  main$frame := ord (stkframe^.caller_basis) = 0
end (* main$frame *);
$PAGE proc$blk
(* PROC$BLK takes a pointer to a stack frame as its parameter
   and returns a pointer to the procedure block for the routine
   corresponding to the stack frame.  The function value will
   be NIL if the routine was not compiled with the TRACE option.  *)

public function proc$blk (stkframe: ^stack_frame): ^procedure_block;

begin
  proc$blk := stkframe^.link_addr^.proc_blk
end (* proc$blk *);
$PAGE proc$name
(* PROC$NAME takes a stack frame pointer as its parameter and
   returns the name of the corresponding procedure.  The return
   value will be the null string it STKFRAME is nil or if the
   procedure was not compiled in TRACE mode.  Note that
   the module containing the procedure MUST be in core when
   this routine is called.  *)

public function proc$name (stkframe: ^stack_frame): id_string;

var
  proc_blk: ^procedure_block;

begin
  proc$name := '';
  if stkframe <> nil then begin
    proc_blk := proc$blk (stkframe);	(* get pointer to proc block *)
    if proc_blk <> nil then				(* if TRACE mode *)
      proc$name := proc_blk^.proc_name
  end
end;
$PAGE call$basis, parent_basis
(* CALL$BASIS takes a stack frame pointer and returns a pointer to
   the stack frame of the caller.  NIL is returned if the 
   parameter is a pointer to the stack frame of the main routine.  *)

public function call$basis (stkframe: ^stack_frame): ^stack_frame;

begin
  if main$frame (stkframe)
    then call$basis := nil
  else
    call$basis := stkframe^.caller_basis
end (* call$basis *);

  
  
  
(* PARENT_BASIS takes a stack frame pointer and returns the stack
   frame pointer of the parent routine.  NIL is returned if STKFRAME
   corresponds to a routine declared at lexic level 0 or 1.  
   NIL is also returned if no procedure block exists for STKFRAME. *)

function parent_basis (stkframe: ^stack_frame): ^stack_frame;

var
  procblk: ^procedure_block;

begin
  parent_basis := nil;
  procblk := proc$blk (stkframe);
  if (procblk <> nil) andif (procblk^.pflev >= 3) then
    parent_basis := stkframe^.parent_basis;
end (* parent_basis *);
$PAGE ret$addr
(* RET$ADDR returns the actual return address of a stack frame, taking
   into consideration the additional information which may appear above
   the frame due to the overlay manager and/or debugger (when stepping).
   It also returns the module number stored by the overlay manager in
   the same manner if present (if not, nil_ovl_modno is returned). *)

public procedure ret$addr (    stkframe:	^stack_frame;
			   var ret_addr:	unit_range;
			   var module_number:	ovl_mod_no);

type
  ovl_stk_word = packed record   (* format of word pushed on stack by manager *)
    module_number: half_word;
    ret_addr: half_word
  end;

var
  module_name: id_string;
  link_entry:  ^linkentry;
  ovrtn_addr:  unit_range;   (* address of return point through olay manager *)
  bstep_addr:  unit_range;   (* address debugger return point when stepping
				through procedure/function calls *)
  ovl_stack_word: ^ovl_stk_word;
  return_word: ^stack_frame;

begin
  if main$frame(stkframe) then begin   (* don't expect such calls, but in case *)
    ret_addr := 0;
    module_number := nil_ovl_modno
  end
  else begin

    if over$laid then begin
      module_name := '';   (* get addr (olay manager return point) *)
      link_entry := sym$lookup ('OPR.TV', module_name);
      a$$ert (link_entry <> nil);
      ovrtn_addr := link_entry^.symaddr
    end
    else
      ovrtn_addr := ord (nil);    (* an unlikely return address *)

    module_name := '';   (* get addr(debugger step return point) *)
    link_entry := sym$lookup ('BST.TV', module_name);
    a$$ert (link_entry <> nil);
    bstep_addr := link_entry^.symaddr;

    if stkframe^.rtn_addr = bstep_addr then
      return_word := ptr (ord (stkframe)-2)   (* debugger pushes 2 extra words *)
    else
      return_word := stkframe;

    if return_word^.rtn_addr = ovrtn_addr then begin
      ovl_stack_word := ptr (ord (return_word)-1);   (* olay mgr pushes 1 word *)
      module_number := ovl_stack_word^.module_number;
      ret_addr := ovl_stack_word^.ret_addr
    end
    else begin   (* no secret olay word *)
      module_number := nil_ovl_modno;
      ret_addr := return_word^.rtn_addr
    end

  end   (* non-main frame case *)
end (* ret$addr *);
$PAGE load$overlay
(* LOAD$OVERLAY takes a stack frame pointer and then, if necessary,
   brings the module corresponding to the stack frame into core.
   This routine should only be called if the program is in fact
   overlaid.  *)

public procedure load$overlay (stkframe: ^stack_frame);

var
  basis:		^stack_frame;
  ret_addr:		unit_range;
  mod_no_found:		boolean;
  current_frame:	^stack_frame;
  this_module_number,
  module_number:	ovl_mod_no;

begin
  basis := rt$base^.basis_rec.basis;		(* get ptr to topmost stack frame *)

  (* if stack frame is topmost on the stack then field CUR_STMT_IN 
     indicates whether or not the module is currently in core.  If 
     it's not in core then field CUR_OVL_MOD contains its overlay
     module number.  *)

  if stkframe = basis then
    with db$base^ do begin
      if not cur_stmt_in then begin
        a$$ert (cur_ovl_mod <> nil_ovl_modno);
        ld$ovl (cur_ovl_mod);
      end
    end

  (* Frame is not the topmost.  Find the first frame above STKFRAME
     in which the return address is to the overlay manager.  In the
     word below that return address is the module number of the desired
     overlay module.  If no return address to the overlay manager is
     found then the desired overlay module must be the same as the one
     which corresponds to the topmost stack frame.  Note that there will
     not be a return address to the overlay manager whenever the called
     procedure is in the same Pascal module as the calling procedure.
     Also note that if the procedure was being single stepped over (via
     the .S command) then 2 extra words were pushed on the stack and we
     must look 2 words below the nominal start of the stack frame for the
     overlay manager's return address.  *)

  else begin

    (* Starting at topmost frame, search down the stack until we
       find the frame we want.  While searching, note any
       frames returning to the overlay manager, indicating an overlay
       switch, and record the overlay module number. *)

    current_frame := basis;
    mod_no_found := false;
    while current_frame<>stkframe do begin
      ret$addr (current_frame, ret_addr, this_module_number);
      if this_module_number<>nil_ovl_modno then begin
	mod_no_found := true;
	module_number := this_module_number
      end;
      current_frame := call$basis (current_frame)
    end;

    (* Load module corresponding to last module number found.  If
       none found, load module corresponding to topmost stack frame. *)

    if mod_no_found then
      ld$ovl (module_number)
    else
      with db$base^ do
	if not cur_stmt_in then begin
	  a$$ert (cur_ovl_mod <> nil_ovl_modno);
	  ld$ovl (cur_ovl_mod)
	end

  end
end (* load$overlay *);
$PAGE open_module
(* OPEN_MODULE fills in the display record corresponding to a
   program or module.  The symbol table file for the module must
    be already open when this routine is called (if the module
   is in DEBUG mode).  *)

function open_module (pb: ^prog_block): display;

begin		    (* open_module *)
  with open_module do begin
    prog_blk := pb;
    stackbase := nil; (* true 'module' has no stack frame *)
    if pb <> nil then begin			(* module in debug mode *)
      staticbase := pb^.lowseg_base;
      blk_node := blk (deref$ptr (level0blk))^.children
    end
    else begin					(* module not in debug mode *)
      staticbase := 0;
      blk_node := ord (nil)
    end;
  end
end (* open_module *);
$PAGE open$stack
public procedure open$stack (    stkframe:	^stack_frame;
			     var scope_stack:	scope_type;
			     var status:	status_code);


var
  display_temp:	display;
  procblk:	^procedure_block;
  dl:		display_lvl_range;
$PAGE open_stack_frame - in open$stack
  (* OPEN_STACK_FRAME fills in as much of the display as possible for the stkframe passed.
     Assumes TRACE but not DEBUG.  *)

  function open_stack_frame (stkframe: ^stack_frame): display;

  var
    procblk: ^procedure_block;

  begin		    (* open_stack_frame *)
    with open_stack_frame do begin
      procblk := proc$blk (stkframe);
      a$$ert (procblk <> nil);		(* shouldn't have gotten here if PROCBLK = NIL *)
      if (procblk^.prog_blk <> nil) andif
         (ord (procblk^.prog_blk^.last_file) <> 0) then
        prog_blk := procblk^.prog_blk
      else
	prog_blk := nil;
      blk_node := procblk^.block_node;
      stackbase := stkframe;
      if prog_blk = nil then		(* if TRACE but not DEBUG *)
	staticbase := 0
      else
	staticbase := prog_blk^.lowseg_base;
    end
  end (* open_stack_frame *);
$PAGE open_parent - in open$stack
  (* OPEN_PARENT - create a display for the parent of an active procedure or function. *)

  function open_parent (child: display): display;

  var
    stkframe: ^stack_frame;

  begin    
    with child do begin
      stkframe := parent_basis (stackbase);
      if stkframe = nil then	(* must have been level 1 routine *)
	open_parent := open_module (prog_blk)
      else
	open_parent := open_stack_frame (stkframe);
    end
  end (* open_parent *);
$PAGE open$stack - body
begin
  
  if over$laid then
    load$overlay (stkframe);	(* if overlaid, then make sure overlay is in *)
  procblk := proc$blk (stkframe);
  if procblk = nil then begin
    status := noscope;
    return					(* <-- return if TRACE not on *)
  end;
  if (procblk^.prog_blk <> nil) andif 		(* proc blocks also emitted w/ TRACE option *)
     (ord(procblk^.prog_blk^.last_file) <> 0)  then
    open$deb$file (procblk^.prog_blk, status);
  with scope_stack do begin
    display_temp := open_stack_frame (stkframe);
    display_levels := procblk^.pflev;
    displays[display_levels] := display_temp;
    for dl := display_levels - 1 downto 1 do
      displays[dl] := open_parent (displays[dl + 1])
  end;
  
  if (procblk^.prog_blk = nil) orif (ord(procblk^.prog_blk^.last_file) = 0) then
    status := notdebug;
end (* open$stack *);
$PAGE open$frame
(* OPEN$FRAME opens scope_stack for stack frame n, where 1 is the first frame 
   on the stack, and basis points to the last one.   If n is > last frame
   on the stack then n is set to the number of the frame opened, i.e. the last frame. *)

public procedure open$frame (var n:		stack_level;
			     var scope_stack:	scope_type;
			     var status:	status_code);

var
  stkframe:	^stack_frame;
  lastframe,
  frame:	stack_level;

begin		    (* open$frame *)
  if n <= 0 then begin
    status := ill_frame_no;
    return
  end;
  
  with rt$base^ do begin
    stkframe := basis_rec.basis;		(* get ptr to topmost stack frame *)
    lastframe := 0when stkframe is main, callerbasis = nil *)
    while stkframe <> nil do begin   (* derive lastframe number *)
      lastframe := lastframe + 1;
      stkframe := call$basis (stkframe);
    end;

    if n > lastframe then
      n := lastframe;   (* open last possible frame *)
    stkframe := basis_rec.basis;
    for frame := 1 to lastframe - n do		(* find frame 'n' by counting down from the top *)
      stkframe := call$basis (stkframe);
    open$stack (stkframe, scope_stack, status)
  end		    
end (* open$frame *);
$PAGE open$routine
   (* The status code indicates the result, as follows:
      = notdefined ... the first name is not a module, or external
	 procedure/function.
      = notpascal ... the first name is not a pascal module,
	 procedure, or function as indicated by the call.
      = notdebug ... the module is not compiled in debug mode, and hence, no scope_stack
	 can be built for it.
      = badnest ... the names are not properly nested routines.  this is also
	 returned if a name is found but is not a procedure/function.  on this
	 return, the name_count in names is set to the index of the offending
	 name.
      = wronginvocation ... the desired invocation of the innermost routine could
	 not be found on the stack.  on this return, invocation is set to the
	 index of the invocation found, and name_count in names is set to the
	 index of the routine whose invocation was used.  the algorithm for finding
	 an invocation is as follows:
	   (1) try to find the desired invocation of the innermost routine.
	   (2) if this can not be found, use the deepest (earliest) invocation
	       of the routine.
	   (3) if no invocation exists, repeat (1) and (2) with all routine
	       names, moving outwards in scope_stack.
	   (4) if no invocation of any routine is found, set invocation to zero.
	       in this case, stackbase of all display entries will be nil. *)


public procedure open$routine (var scope_id:	scope_id_record;
			       var scope_stack:	scope_type;
			       var status:	status_code);

var
  ovl_modno: ovl_mod_no;			(* overlay module number of routine opened or
						   0 if opened module resident *)
$PAGE open_named_module - in open$routine
  (* OPEN_NAMED_MODULE - routine to open the module's symbol table and set up
     its display.  This routine must be called before any calls to deref$ptr,
     i.e., attempts to access the symbol table, are made.  Parameters are:

       module_name ... the module name from the names vector.
       status ... return code, possible values = (notpascal,notdebug)

     Function's value is display entry on successful return. *)

  function open_named_module (    module_name:	id_string;
			      var status:	status_code): display;

  var
    mod_st_entry: ^linkentry;

  begin
    mod_st_entry := mod$lookup(module_name); (* get link symbol table entry *)
    if mod_st_entry = nil then begin
      status := notdefined;   (* no such module name found *)
      return (* <-- return *)
    end;
  
    with mod_st_entry^ do

    (* Pascal module has name of module in radix-50 at first word
       of module's high segment to allow Pascal modules to be
       recognized.  *)

      if modname <> firstword^.r50_name then
	status := notpascal
      else if ord (firstword^.program_block.last_file) = 0 then
	status := notdebug
      else
	with firstword^ do begin
	  open$deb$file (address (program_block), status);	(* open symbol table file *)
	  if status in severe then return;	(* <-- error return *)
	  open_named_module := open_module(address (program_block));	(* fill in display entry *)
	end
  end (* open_named_module *);
$PAGE open_ovl_mod - in open$routine
(* OPEN_OVL_MOD creates a display for a public routine of an overlay
   module.  Parameter MOD_LINK_ENTRY is a pointer to the LINK symbol
   table entry for the routine.  Since the routine is in an overlay
   module, the address in the LINK symbol table is a transfer vector
   address.  The start address of the routine is found by getting the
   local transfer vector address from the LINK symbol table entry.
   The local transfer vector contains the routine's actual entry point.  The
   word following the entry point contains the procedure block pointer.
   The procedure block contains a pointer to the program block of the
   Pascal module containing the routine.  Given this program block
   pointer the module may be opened in the same manner as a module not
   in an overlay.
  
   Note also that the resident transfer vector contains the number 
   of the overlay module containing the routine.  This module number 
   is used to load the module into core, if necessary.  *)

function open_ovl_mod (    mod_link_entry:	^linkentry;
		       var scope_id:		scope_id_record;
		       var status:		status_code): display;

type
  proc_blk_link =	^procblklink;
  xfer_vector_entry = packed record		(* resident xfer vector entry for public routine *)
    op_code: 0..777b;				(* first 3 fields represent *)
    filler: 0..777b;				(* PUSHJ to overlay manager *)
    ovl_manager: half_word;
    module_number: half_word;			(* 2nd word - ovelay module number, and *)
    local_tv: ^local_tv_rec			(* address of local transfer vector entry *)
  end;

  local_tv_rec = packed record			(* local xfer vector entry for public routine *)
    jrst: half_word;				(* entry is JRST <entry point> instruction *)
    entry_addr: half_word
  end;

const
  pushj = 260b;					(* opcode for PUSHJ instr *)

var
  tv_entry:	^xfer_vector_entry;
  proc_block:	^procedure_block;
  prog_block:	^prog_block;

begin
  (* Get address of resident transfer vector entry for the public
     routine and make sure the entry contains a PUSHJ instruction.  *)

  tv_entry := ptr (mod_link_entry^.symaddr);
  if tv_entry^.op_code <> pushj then begin
    status := badnest;
    scope_id.name_count := 2			(* for error reporting *)
  end

  (* Get the overlay module number from the resident transfer vector
     entry and load the overlay if necessary.  Then get a pointer to
     the Pascal module's program block and open the module normally.  *)

  else begin 
    ovl_modno := tv_entry^.module_number;
    ld$ovl (ovl_modno); (* Force overlay in *)
    proc_block := proc_blk_link (ord (tv_entry^.local_tv^.entry_addr) + 1)^.proc_blk;
						(* block via the local xfer vector *)
    if (proc_block = nil) orif		(* make sure TRACE and *)
       (proc_block^.prog_blk = nil) orif	(* DEBUG are in effect *)
       (ord (proc_block^.prog_blk^.last_file) = 0) then
      status := notdebug
    else begin					(* finally, get prog block and open normally *)
      prog_block := proc_block^.prog_blk;
      open$deb$file (prog_block, status);
      if status in severe then return;
      open_ovl_mod := open_module (prog_block);
    end

  end
end (* open_ovl_mod *);
$PAGE open_mod - in open$routine
(* OPEN_MOD creates a display for the module of parameter SCOPE_ID.
   Three cases are distinguished.  First, if a module name is
   specified in SCOPE_ID then a display record is created for the
   module via OPEN_NAMED_MODULE.  If the first name is a public routine
   then the name is looked up in the link symbol table.  If the 
   program is overlaid and the symbol was found in the overlay 
   transfer vector then OPEN_OVL_MOD creates a display for the module.
   If the routine's name was not in an overlay transfer vector
   then OPEN_NAMED_MODULE is used to create the display using
   the module name returned by the Link symbol table lookup.  
   Whenever a module name was not given in the SCOPE_ID, then the
   routine names are shifted up one entry in the NAMES array.

   Note also that the open routines called by this routine open
   the symbol table file.  *)

function open_mod (var scope_id:	scope_id_record;
		   var status:		status_code): display;

var
  mod_name:	id_string;
  link_entry:	^linkentry;
  i:		lexic_lvl_range;

begin
  with scope_id do begin
    if delim = '@' then			(* module was specified *)
      open_mod := open_named_module (names[ 1 ], status)
    else begin					(* public routine specified *)
      mod_name := '';
      link_entry := sym$lookup (names[ 1 ], mod_name);
      if link_entry = nil then
        status := notdefined
      else begin
	for i := name_count downto 1 do	(* shift names up 1 to make room for module name *)
	  names[ i + 1 ] := names[ i ];
	names[ 1 ] := mod_name;
	name_count := name_count + 1;
        if over$laid andif			(* program is overlaid and *)
           (uppercase(mod_name) = 'PASTV.') then  (* routine is in an overlay *)
          open_mod := open_ovl_mod (link_entry, scope_id, status)
	else
	  open_mod := open_named_module (mod_name, status);	(* public routine not
								in an overlay module *)
      end
    end
  end
end (* open_mod *);
$PAGE open_kids - in open$routine
  (* OPEN_KIDS - Routine to open the nested routines within a module.
     parameter indicates status of operation, possible values = (badnest).
     routine uses parameters names and scope of module_scope, filling in scope,
     and on the badnest return, setting name_count in names to index of crummy
     name. *)

  procedure open_kids (var status: status_code);


    (* local routine to set up the display entry for a passed name.  the display
       entry for its parent is passed, so that a search of the routines defined
       within its parent can be made.
       The status parameter of open_kids is set to badnest
       when the name cannot be found in the symbol table. *)

    function open_child (parent: display; name: id_string): display;
    var
      block_addr:	blk;
      proc_name:	id_string;
      found:		boolean;

    begin
      with open_child do begin
        stackbase := nil;			(* filled in later if routine active *)
        prog_blk := parent.prog_blk;		(* assume children are in parent's module *)
        staticbase := parent.staticbase;	(* all statics in module allocated off same base *)
        
        (* Now find the routines block node offset in the symbol 
           table file. *)

        block_addr := deref$ptr (parent.blk_node);	(* get parent's block node *)
        blk_node := block_addr^.children;		(* first lower proc *)
        found := false;
      
        while blk_node <> ord (nil) do begin	(* search peer list *)
          block_addr := deref$ptr (blk_node);
          a$$ert (block_addr^.kind = subr_blk);
          with nam (deref$ptr (sym (deref$ptr (block_addr^.subr_sym))^.name))^ do
	    proc_name := uppercase (substr (text, 1, len));
        exit if proc_name = name do found := true;
          blk_node := blk (deref$ptr (blk_node))^.peer;
        end;
        if not found then
	  status := badnest
      end
    end (* open_child *);


  var
    i: display_lvl_range;

  begin		    (* open_kids *)
    with scope_id do
      for i := 2 to name_count do begin
	scope_stack.displays[i] := open_child (scope_stack.displays[i-1], names[i]);
      exit if status in severe do
	  name_count := i	(* remember bogus name *)
      end
  end (* open_kids *);
$PAGE subr_invocation - in open$routine
  (* SUBR_INVOCATION - Routine to find the desired invocation of a passed routine.
     If desired invocation cannot be found, deepest invocation is returned.  if no
     invocation is found, the function value is nil.
     Parameters are:

     subr -- the (undereferenced) pointer to the routine's symbol table entry.
     pb -- a pointer to the block of the program containing the routine.  this
       value along with subr serve to uniquely identify the routine.
     invoc_found -- returned index of the invocation found (if one was).

     The function value is the stack frame pointer of the invocation found, or
     nil if none was found. *)

  function subr_invocation (    subr:		intblk;
				pb:		^prog_block;
			    var invoc_found:	stack_level): ^stack_frame;

  var
     stkframe:		^stack_frame;
     examine_frame:	boolean;
     ret_addr:		unit_range;
     stack_ovl_modno:	ovl_mod_no;

  begin
    invoc_found := 0;	(* initialize for loop *)
    subr_invocation := nil;
    stkframe := rt$base^.basis_rec.basis;	(* fetch most recent stack frame *)
    if not main$frame (stkframe) then begin	(* if only main on stack, don't bother *)
      examine_frame := db$base^.cur_stmt_in;
      while (invoc_found < scope_id.stack_frm_no) and (stkframe<>nil) do begin
        if examine_frame then		(* DO NOT examine overlay modules which are nonresident *)
	  with stkframe^, link_addr^ do
	    if proc_blk<>nil then		(* if not TRACE mode, skip frame *)
	      with proc_blk^ do	(* now looking at procedure block *)
		if (prog_blk=pb) and (block_node=subr) then begin
		  invoc_found := invoc_found+1;
		  subr_invocation := stkframe
		end;
        ret$addr (stkframe, ret_addr, stack_ovl_modno);	(* get ovl modno of next frame *)
        if stack_ovl_modno <> nil_ovl_modno then	(* if ovl module is changing *)
	  examine_frame := ovl_modno = stack_ovl_modno;	(* examine next frame only if
						   it corresponds to routine in opened module *)
	stkframe := call$basis (stkframe);	(* move up the stack *)
        if main$frame (stkframe) then
	  stkframe := nil;	(* if at main, give up *)
      end   
    end	   
  end (* subr_invocation *);
$PAGE open$routine - body
var
  invoc_found:	stack_level;
  stkframe:	^stack_frame;
  i,
  rtn_invoked:	display_lvl_range;

begin		    (* open$routine *)

  (* open the module heading the list *)

  with scope_id do begin
    ovl_modno := 0;				(* overlay assummed to be the resident 
						   till proven otherwise *)
    scope_stack.displays[ 1 ] := open_mod (scope_id, status);
    if status in severe then return;	(* <-- did not get module open successfully *)
      
    (* Fill in display record for each nested routine. *)

    open_kids (status);
    if status in severe then return;   (* <-- all names were not kosher routines *)

    (* loop through each routine from inner to outer, looking for an invocation *)

    rtn_invoked := name_count;
    scope_stack.display_levels := rtn_invoked;	(* set count of displays filled *)
    stkframe := nil;
    while (rtn_invoked>=2) and (stkframe=nil) do begin	(* loop until invocation found
							   or all routine names searched for *)
      with scope_stack.displays[rtn_invoked] do
	stkframe := subr_invocation (blk_node, prog_blk, invoc_found);
      if stkframe=nil then
	rtn_invoked := rtn_invoked-1
    end;
    if stkframe=nil then begin    (* nary an invocation found *)
      stack_frm_no := 0;
      status := wronginvocation
    end
    else begin				(* some invocation of some routine was found *)
      if (rtn_invoked <> name_count) or (stack_frm_no <> invoc_found) then begin
	status := wronginvocation;
	stack_frm_no := invoc_found;
	name_count := rtn_invoked
      end;
      scope_stack.displays[rtn_invoked].stackbase := stkframe;	(* fill in stack frames *)
      for i := rtn_invoked-1 downto 2 do
	scope_stack.displays[i].stackbase :=
		     parent_basis (scope_stack.displays[i+1].stackbase)
    end
  end
end (* open$routine *).
 Y {ñ