$TITLE DEBBRK - Pascal Debugger breakpoint related routines
  
$HEADER debbrk

module debbr$  options nocheck, special (coercions, word, ptr);

$INCLUDE debug.typ
  
$INCLUDE debio.inc
$INCLUDE debug.inc
$INCLUDE debsym.inc
$INCLUDE debscp.inc
$INCLUDE deblib.inc
$PAGE get$mod$name
(* GET$MOD$NAME is given a program block pointer and returns the
   name of the module containing the program block.  The module
   name is obtained from the .DEB file rather than from the LINK
   symbol table, so that the routine will work with overlaid 
   programs.  If another .DEB file is open when this routine
   is called, then that file is first closed and then reopened
   before this routine returns.  Note, however, that the module
   containing the program block MUST be in core when this routine
   is called.  *)

public procedure get$mod$name (    prog_blk: ^prog_block;
			       var mod_name: id_string;
			       var status:   status_code);

var
  initial_file,
  mod_file:		file_name_string;
  not_open_initially:	boolean;
  mod_block:		blk;

begin
  a$$ert (prog_blk <> nil);			(* for those of us who are suspicious by nature *)
  initial_file := st$file;			(* get name of currently open file *)
  mod_file := prog_blk^.symfile_name || ' .DEB';
  not_open_initially := initial_file <> mod_file;
  if not_open_initially then
    open$deb$file (prog_blk, status);
  
  if not (status in severe) then begin	
  
    (* module or program block node is first child of root (level 0) block *)
  
    mod_block := deref$ptr (blk (deref$ptr (level0blk))^.children);
    with nam (deref$ptr (mod_block^.id))^ do	(* name node for module or program *)
      mod_name := substr (text, 1, len)
  end;

  if not_open_initially and (initial_file <> '') then (* restore orginal debsym file *)
    st$open (initial_file)
end (* get$mod$name *);
$PAGE info$stmt
(* INFO$STMT takes a statment block pointer, STMT_BLK, and returns a
   source_id_record describing the location of the statement.  All the
   fields in the source_id_record will be filled in.  Note that the
   module containing the statement block MUST be in core when this
   routine is called.  *)

public procedure info$stmt (    stmt_blk:  stmt_block_ptr;
			    var source_id: source_id_record;
			    var status:    status_code);

var
  stmt_ptr:	stmt_block_ptr;
  page_blk:	page_block_ptr;
  mod_name:	id_string;

begin
  with source_id do begin
    line_num := stmt_blk^.line_number;		(* fill in line number *)
    stmt_ptr := stmt_blk;
    while stmt_ptr^.line_number <> 0 do		(* find page block *)
      stmt_ptr := stmt_ptr^.previous_stmt;

    page_blk := page_block_ptr (stmt_ptr);	(* coerce pointer *)
    page_name := page_blk^.subtitle;		(* fill in page name and number *)
    page_num := page_blk^.page_number;

    while page_blk^.block_id = 0 do		(* find file block *)
      page_blk := page_blk^.previous_page;

    file_name := file_block_ptr (page_blk)^.file_name;		(* fill in file name and number *)
    file_num := file_block_ptr (page_blk)^.file_number;

    get$mod$name (file_block_ptr (page_blk)^.program_blk, mod_name, status); (* fill in module name *)
    if not (status in severe) then
      module_name := mod_name	(* type conversion *)
  end
end (* info$stmt *);
$PAGE get$prog$ptr
(* GET$PROG$PTR takes a module name and returns a pointer to the
   program block for the module.  Parameter STATUS will be set to
   MOD_NOT_FOUND if the module name is not found in the LINK symbol
   table.  Note that because a LINK symbol table lookup is used, this
   routine will not work for overlaid modules.  *)

public procedure get$prog$ptr (    mod_name:	 mod_string;
			       var prog_blk_ptr: ^prog_block;
			       var status:	 status_code);

var
  mod_link_entry: ^linkentry;			(* link symbol table entry *)

begin
  mod_link_entry := mod$lookup (mod_name);
  if mod_link_entry = nil then
    status := mod_not_found

    (* If not DEBUG, then the word which would normally begin the 
       program block (the second word of the module's high seg area)
       is zero.  The first word of the program block includes the
       LAST_FILE field, which in a DEBUG compilation, is either a 
       valid pointer or is NIL.  *)

  else if ord (mod_link_entry^.firstword^.program_block.last_file) = 0 then
    status := mod_not_debug
  else
    prog_blk_ptr := address (mod_link_entry^.firstword^.program_block)
end (* get$prog$ptr *);
$PAGE get$file$ptr
(* GET$FILE$PTR takes a program block pointer and either a file number
   or a file name and returns a pointer to the corresponding file block.
   If a file name is given then it will be used in the search and an exact
   match must be found.  If the file name is null, then the file number
   will be searched for.  If an exact match is not found then the user 
   will be queried to see if the next largest file number, if any, should
   be used.  Note that a module which does not generate any executable
   code will have a program block, but no file, page, or statement blocks.
   In this case STATUS will be set to NO_FILE_BLOCK.  *)

public procedure get$file$ptr (    prog_blk:  ^prog_block;
				   file_name: file_string;
				   file_num:  ext_file_range;
			       var file_ptr:  file_block_ptr;
			       var status:    status_code);


var
  instead_ptr:	file_block_ptr;
  instead_num:	ext_file_range;
  uc_file_name: file_string;

begin
  file_ptr := prog_blk^.last_file;		(* get last file of ring *)
  if file_ptr = nil then begin			(* no file blocks exist *)
    status := no_file_block;
    return  (* <-- return *)
  end;
  
  if file_name <> '' then begin			(* try to match on file name *)
    uc_file_name := uppercase (file_name);
    while (file_ptr^.previous_file <> nil) andif  (* not at end of ring *)
	   (uppercase (file_ptr^.file_name) <> uc_file_name) do	(* no match yet *)
      file_ptr := file_ptr^.previous_file; (* no match yet *)
    if uppercase (file_ptr^.file_name) <> uc_file_name then	(* error if no match on name *)
      status := file_not_found
  end
  
  else begin					(* try to match file numbers *)
    instead_ptr := nil;
    instead_num := max_file_no;
    while (file_ptr^.previous_file <> nil) andif  (* not at end of ring *)
	   (file_ptr^.file_number <> file_num) do begin
      with file_ptr^ do
	if (file_number > file_num) andif	(* if larger than specified number and *)
	   (file_number < instead_num) then begin  (* less than any other *)
	  instead_num := file_number;		(* then save in case no exact *)
	  instead_ptr := file_ptr		(* match is found *)
	end;
      file_ptr := file_ptr^.previous_file
    end;

    if file_num <> file_ptr^.file_number then	(* no exact match found *)
      if instead_ptr <> nil then begin		(* try next larger *)
	writ$str ('File ');
	writ$int (instead_num, decimal_radix);
	writ$str (' (' || instead_ptr^.file_name || ')');
	if query$ (' instead') then
	  file_ptr := instead_ptr
	else
	  status := no_instead
      end
      else				(* no larger file number found *)
	status := file_not_found
  end;
end (* get_file_ptr *);
$PAGE get$page$ptr
(* GET$PAGE$PTR takes a file block pointer and either a page name or
   a page number and returns a pointer to the corresponding page block.
   If a page name is given then it will be used in the search and an exact
   match must be found.  If the page name is null, then the page number is
   searched for.  If an exact match is not found then the user will be
   queried to see if the next largest page number, if one exists, should
   be used.  *)

procedure get$page$ptr (    file_ptr:  file_block_ptr;
			    page_name: page_string;
			    page_no:   ext_page_range;
			var page_ptr:  page_block_ptr;
			var status:    status_code);

var
  instead_ptr:	page_block_ptr;
  instead_num:	ext_page_range;
  uc_page_name: page_string;

begin
  page_ptr := file_ptr^.last_page;
  
  if page_name <> '' then begin			(* try to match on page name *)
    uc_page_name := uppercase (page_name);
    while (page_ptr^.block_id = 0) andif	(* not back at file block *)
          (uppercase (page_ptr^.subtitle) <> uc_page_name) do	(* no match yet *)
      page_ptr := page_ptr^.previous_page; (* no match yet *)
    if ord (page_ptr) = ord (file_ptr) then	(* back at file block *)
      status := page_not_found
  end
  
  else begin					(* try to match on page number *)
    instead_ptr := nil;
    instead_num := max_page_no;
    while (page_ptr^.block_id = 0) andif	(* not at end of ring yet *)
          (page_ptr^.page_number <> page_no) do begin  (* no match yet *)
      with page_ptr^ do			(* keep track of next larger numbered block *)
        if (page_number > page_no) and (page_number < instead_num) then begin
          instead_num := page_number;
          instead_ptr := page_ptr
        end;
      page_ptr := page_ptr^.previous_page
    end;
    if ord (page_ptr) = ord (file_ptr) then	(* no exact match found *)
      if instead_ptr <> nil then begin
        writ$str ('Page ');
        writ$int (instead_num, decimal_radix);
        writ$str (' (' || instead_ptr^.subtitle || ')');
        if query$ (' instead')  then
          page_ptr := instead_ptr
	else
	  status := no_instead
      end
      else				(* no larger numbered page exists *)
        status := page_not_found
  end
end (* get_page_ptr *);
$PAGE get$stmt$ptr
(* GET$STMT$PTR takes a page block pointer and a line number and returns
   a statement block pointer.  If an exact match is not found then the
   user is queried to see if the next larger line number should be used. *)

procedure get$stmt$ptr (    page_ptr: page_block_ptr;
			    line_no:  line_range;
			var stmt_ptr: stmt_block_ptr;
			var status:   status_code);

var
  instead_ptr: stmt_block_ptr;
  instead_num: line_range;
  max_ptr:     stmt_block_ptr;

begin
  stmt_ptr := page_ptr^.last_stmt;
  instead_ptr := nil;
  instead_num := max_line_no;
  max_ptr := stmt_ptr;
  while (stmt_ptr^.line_number <> 0) andif	(* not back at page block *)
        (stmt_ptr^.line_number <> line_no) do begin  (* no match yet *)
    with stmt_ptr^ do begin			(* keep track of next larger numbered statement *)
      if (line_number > line_no) and (line_number < instead_num) then begin
        instead_num := line_number;
        instead_ptr := stmt_ptr
      end;
      if line_number > max_ptr^.line_number then (* keep track of largest numbered stmt *)
        max_ptr := stmt_ptr
    end;
    stmt_ptr := stmt_ptr^.previous_stmt
  end;
  if ord (stmt_ptr) = ord (page_ptr) then begin  (* back at page block *)
    if instead_ptr = nil then 
      instead_ptr := max_ptr;		(* no larger found so use max *)
    writ$str ('Line ');
    writ$int (instead_ptr^.line_number, decimal_radix);
    if query$ (' instead') then
      stmt_ptr := instead_ptr
    else
      status := no_instead
  end
end (* get$stmt$ptr *);
$PAGE find$stmt$blk
(* FIND$STMT$BLK is given a source_id_record and returns a statement
   block pointer for the referenced statement.  Note that this routine
   will NOT work if the module name in the source_id_record is the name
   of a module in an overlay segment.  However, if the PROGRAM_BLOCK field of 
   AUG_SOURCE_ID is not NIL then the module name field is not used; instead
   the PROGRAM_BLOCK field is used directly.  Thus this routine can be used
   by overlaid programs to refer to the default module.
   The routines called to find file, page and statement pointers may query the
   user if the exact file/page/line number is not found.  *)

public procedure find$stmt$blk (    aug_source_id: augmntd_source_id;
				var stmt_blk:      stmt_block_ptr;
				var status:        status_code);

var
  prog_ptr: ^prog_block;
  file_ptr: file_block_ptr;
  page_ptr: page_block_ptr;

begin
  with aug_source_id, source_id do begin
    if program_block <> nil then
      prog_ptr := program_block
    else begin
      get$prog$ptr (module_name, prog_ptr, status);
      if status in severe then return
    end;

    get$file$ptr (prog_ptr, file_name, file_num, file_ptr, status);
    if status in severe then return;

    get$page$ptr (file_ptr, page_name, page_num, page_ptr, status);
    if status in severe then return;

    get$stmt$ptr (page_ptr, line_num, stmt_blk, status)
  end
end (* find$stmt$blk *);
$PAGE clr$all$brkpts

(* CLR$ALL$BRKPTS clears all entries in the breakpoint table by setting
   the statement block pointers to NIL *)

public procedure clr$all$brkpts;

var
  i: brkpt_range;

begin
  with rt$base^ do begin
    for i := minimum (brkpt_range) to maximum (brkpt_range) do
      brk_table[i].stmt_blk_addr := nil;
    max_active_brkpt := -1
  end
end (* clr$all$brkpts *);
$PAGE clr$brkpt

(* CLR$BRKPT clears a breakpoint in the breakpoint table.  It does
   so by setting the statment block pointer in the breakpoint table
   to NIL.  BRKPT_NO is the number of the breakpoint to clear.
   STATUS is the status code to be returned.  *)

public procedure clr$brkpt (    brkpt_no: brkpt_range;
			    var status:   status_code);

begin
  if (brkpt_no < minimum (brkpt_range)) orif (brkpt_no > maximum (brkpt_range)) then
    status := illegal_brkpt_num
  else
    with rt$base^, brk_table[ brkpt_no ] do
      if stmt_blk_addr = nil then
        status := no_such_brkpt
      else begin
        stmt_blk_addr := nil;
	if brkpt_no = max_active_brkpt then	(* Update max active brkpt number *)
	  repeat
	    max_active_brkpt := max_active_brkpt - 1;	(* in runtime static area *)
	  until (max_active_brkpt < 0) orif
		(brk_table[max_active_brkpt].stmt_blk_addr <> nil)
      end
end (* clr$brkpt *);
$PAGE set$brkpt

(* SET$BRKPT sets a breakpoint in the breakpoint table.  AUG_SOURCE_ID
   is a source_id_record representation of the source line where the
   breakpoint is to be set.  BRKPT_STRING is the (possibly null)
   breakpoint string.  The number of the breakpoint set is returned 
   in BRKPT_NO.  STATUS is the status code to be returned.  *)

public procedure set$brkpt (    aug_source_id:	augmntd_source_id;
				brkpt_string:	cmd_str;
			    var brkpt_no:	brkpt_range;
			    var status:		status_code);

var
  stmt_blk:	stmt_block_ptr;
  i:		brkpt_range;
  found:	boolean;

begin
  find$stmt$blk (aug_source_id, stmt_blk, status);	(* convert source_id to stmt_block_ptr *)
  if status in severe then return;
  
  found := false;
  with rt$base^, db$base^ do begin
    for i := minimum (brkpt_range) to maximum (brkpt_range) do
      with brk_table[ i ] do begin
	exit if stmt_blk_addr = nil do begin
	  found := true;
	  brkpt_no := i;
	  in_core := true;			(* has to be to set it *)
	  mod_no := nil_ovl_modno;			(* unknown *)
	  stmt_blk_addr := stmt_blk;
	  brk_strings[ i ] := brkpt_string
	end  (* exit *)
      end  (* with *);
    if not found then
      status := too_many_brkpts
    else
      max_active_brkpt := max(brkpt_no, max_active_brkpt);
  end
end (* set$brkpt *).
   