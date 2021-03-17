$TITLE pasrdr - source file reader
$LENGTH 42

module pasrdr;
$PAGE includes
$SYSTEM pascal
$SYSTEM pasist
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM paserr
$SYSTEM passw
$SYSTEM pasfil
$SYSTEM pasopn
$SYSTEM corout
$SYSTEM cmdutl
$PAGE declarations
static var
  in_directives: boolean;               (* true until first non directive line read *)
  listing: boolean;                     (* true => in listing pass *)


const tab = chr (11b);

public var
  reader: environment;  (* for coroutine READ_INPUT_LINES *)

type include_range = 0..max_include_level;

type directs =
      ( dir_autosource, dir_disable, dir_enable, dir_endif, dir_header,
        dir_if, dir_ifany, dir_ifnone, dir_ifnot, dir_include, dir_length,
        dir_nosource, dir_page, dir_source, dir_system, dir_title, dir_width );

const dirs: array[1..ord(maximum(directs))+1] of cmd_lookup_record =
        ( ( 'AUTOSOURCE', 5, ord (dir_autosource) ),
          ( 'DISABLE   ', 3, ord (dir_disable) ),
          ( 'ENABLE    ', 2, ord (dir_enable) ),
          ( 'ENDIF     ', 3, ord (dir_endif) ),
          ( 'HEADER    ', 4, ord (dir_header) ),
          ( 'IF        ', 2, ord (dir_if) ),
          ( 'IFANY     ', 3, ord (dir_ifany) ),
          ( 'IFNONE    ', 5, ord (dir_ifnone) ),
          ( 'IFNOT     ', 5, ord (dir_ifnot) ),
          ( 'INCLUDE   ', 3, ord (dir_include) ),
          ( 'LENGTH    ', 3, ord (dir_length) ),
          ( 'NOSOURCE  ', 3, ord (dir_nosource) ),
          ( 'PAGE      ', 4, ord (dir_page) ),
          ( 'SOURCE    ', 1, ord (dir_source) ),
          ( 'SYSTEM    ', 3, ord (dir_system) ),
          ( 'TITLE     ', 5, ord (dir_title) ),
          ( 'WIDTH     ', 3, ord (dir_width) )  );
$PAGE read_a_line
(* READ A LINE reads the next line of text from a designated file.  Fills in
   "line" and "literal_line" with text.  Increments cur_source counter.  At
   the end of file, the lines are set to blank.  The return value goes false
   at end of file. *)


function read_a_line (var f: text): boolean;

begin
  read_a_line := not eof (f);
  if read_a_line then begin (* if anything to be read *)
    readln (f, literal_line);
    line := uppercase (literal_line) || '  '; (* plus blanks for scanner *)
    cur_source.line_no := cur_source.line_no + 1;
    linect := linect + 1;
    if cur_source.file_no <> 0 then
      inclct := inclct + 1;
  end
  else begin
    line := '  ';
    literal_line := '';
  end;
end (* read_a_line *);
$PAGE new_file
(* NEW FILE advances to the src_id record for the file "f".  In the first pass,
   the record is created, initialized and appended to the master "file_list";
   in the second pass, the record created in the first pass is found and the 
   physical page assigned.  A pointer to the record is returned. *)

var last_file: source_ptr;

function new_file
  (  var f: text;                    (* file variable of file, assumed open *)
     file_number: file_range;           (* file number assigned to file *)
     include_level: include_range;   (* level of nesting of file *)
     in_system: boolean;          (* true => under control of system directive *)
     phys_page: page_range      (* physical page on which file is included *)
     ): source_ptr;

  var fname: file_name;

begin
  if not listing then begin      (* first pass *)
    fname := filename (f);
    new (new_file, length (fname));
    with new_file^ do begin
      file_no := file_number;
      pages := nil;
      next_file := nil;
      incl_level := include_level;
      system_file := in_system;
      file_name := fname;
    end;
    if file_list = nil
      then file_list := new_file          (* start of list *)
      else last_file^.next_file := new_file;
  end

  else begin                             (* second pass *)
    if last_file = nil
      then new_file := file_list
      else new_file := last_file^.next_file;
    new_file^.incl_page := phys_page;
  end;

  last_file := new_file;
end;
$PAGE new_page
(* NEW PAGE advances to the src_id record for the indicated page.  In the first
   pass, the record is created, initialized, and chained into the list of pages;
   in the second pass, the record created in the first pass is found and the 
   physical page number assigned.  A pointer to the record is returned. *)

var last_page: page_ptr;

function new_page
  ( file_ptr: source_ptr;           (* src_id of file containing page *)
    page_no: page_range;                (* page number to be assigned *)
    prev_page_ptr: page_ptr;        (* page_id of preceding page in file, or nil *)
    page_title: title_string;        (* title for page *)
    phys_page: page_range               (* actual page on which section begins *)
    ): page_ptr;            (* to record of new page *)

begin
  if not listing then begin              (* first pass *)
    new (new_page, length (page_title));
    with new_page^ do begin
      left_page := nil;
      right_page := nil;
      next_page := nil;
      following_page := nil;
      in_file := file_ptr;
      page_number := page_no;
      incl_page := 0;
      subtitle := page_title;
    end (* with *) ;
    if prev_page_ptr = nil
      then file_ptr^.pages := new_page
      else prev_page_ptr^.next_page := new_page;
    if last_page <> nil
      then last_page^.following_page := new_page;
  end

  else begin                     (* second pass *)
    if last_page = nil
      then new_page := file_list^.pages  (* page 0 of file 0 *)
      else new_page := last_page^.following_page;
    new_page^.incl_page := phys_page;
  end;

  last_page := new_page;
end;
$PAGE get_lines_from_file
(* GET LINES FROM FILE is called recursively to read the lines of the main
   and include (system) files.  When entered, it is assumes that the file has
   been opened.  Processing terminates normally when the end of file is
   detected by this routine.  Processing terminates abnormally (via a non-
   local goto) when an unbalanced if-end directive pair is detected by
   lower level routines.  This returns through a coroutine linkage each text
   or directive line to be listed.  Directive lines are interpreted by this
   routine. *)

procedure get_lines_from_file
  (  var f: text;                    (* current input file *)
     include_level: include_range;      (* level of nesting of this file *)
     in_system: boolean            );   (* indicates if file under control of system directive *)

  label 1 (* EOF abort *) ;

  var   cur_file: source_ptr;           (* src_id for file begin read *)
        cur_page: page_ptr;             (* page_id for page begin read *)


        (* This contains several nested internal routines:

              process_directive -             processes directive lines

                skip_blanks -                 parsing utilities
                cmd_eol (line, lindex)
                checkpunct
                number
                numeric_option
                identifier

                file_directive -              include, system, and header processing
                system
                header

                if_ifn_directive -            if/ifnot processing
                                                                                      *)
$PAGE process_directive
  (* PROCESS DIRECTIVE parses and interprets all directive lines.  Simple directives
     are handled directly in the body of this routine; others, by calls to internal
     routines. *)

  procedure process_directive;

    label 2 (* parse abort *) ;           (* escape on syntax error in directive *)

    var
      lindex: integer;             (* directive parsing cursor *)
      err_idx: integer;            (* error cursor location *)
      dir_listed: boolean;            (* indicates directive line has been listed;
                                         used to control listing of erroneous line *)
$PAGE dir_error - in process_directive
  (* DIR ERROR reports an error on a directive line;  the error cursor is taken
     from the global variable "err_idx" which is set by the parsing utilities
     below as the line is scanned.  Escapes back into process_directive. *)

  procedure dir_error (code: err_codes);
  begin
    if listing and (not dir_listed) then detach;
    err_print (code, cur_source, '', err_idx);
    goto 2 (* parse abort *) ;
  end;
$PAGE skip_blanks
procedure skip_blanks;
  var cmt_level: 0 .. 100;
      l: integer;
begin
  loop
    while (lindex <= length (line)) andif (line[lindex] <= ' ')
      do lindex := lindex + 1;
  exit if (lindex >= length (line)) orif (line[lindex] <> '(') orif
	  (line[lindex+1] <> '*');
    cmt_level := 1;
    err_idx := lindex;
    lindex := lindex + 2;
    repeat
      l := search (substr (line, lindex), ['(','*']);
      lindex := lindex + l - 1;
      if (l = 0) or (lindex >= length (line)) then dir_error (err_opt_comment);
      if (line[lindex] = '(') andif (line[lindex+1] = '*') then begin
	cmt_level := cmt_level + 1;
	lindex := lindex + 2;
      end
      else if (line[lindex] = '*') andif (line[lindex+1] = ')') then begin
	cmt_level := cmt_level - 1;
	lindex := lindex + 2;
      end
      else
	lindex := lindex + 1;
    until cmt_level = 0;
  end (* loop *);
  err_idx := lindex;
end;
$PAGE numeric_option
function numeric_option: integer;

var l: integer;
    paren: boolean;

begin
  skip_blanks;
  if cmd_check_punct (line, lindex, ':') orif cmd_check_punct (line, lindex, '=')
    then paren := false
    else paren := cmd_check_punct (line, lindex, '(');
  skip_blanks;
  l := lindex;                   (* remember start of number *)
  if not cmd_number (line, lindex, false, numeric_option) then
    dir_error (err_val_expected);
  if paren andif not cmd_check_punct (line, lindex, ')') then
    dir_error (err_opt_paren);
  skip_blanks;
  if not cmd_eol (line, lindex) then
    dir_error (err_dir_unterminated);
  err_idx := l;                 (* caller wants error at number *)
end;
$PAGE identifier
function identifier: parm_string;

begin
  skip_blanks;
  if not cmd_token (line, lindex, ['A'..'Z', '0'..'9', '_'], identifier) then
    dir_error (err_sw_expected);
  skip_blanks;
end;
$PAGE file_directive - in process_directive
(* FILE DIRECTIVE performs common processing for $include, $system or $header
   directive lines, and calls the specified processing routine.  The actions of
   this routine include:  pushing an include level, parsing the filename,
   opening the file, and stacking global values which must be restored after
   the file is read. *)

type
    file_proc = procedure ( var f: text;          (* like get_lines_from_file *)
			    include_level: include_range;
			    in_system: boolean            );
    ext_type = packed array [1..3] of char;

procedure file_directive (action: file_proc; ext: ext_type);

var fname: file_name;
    incl_file: text;
    old_source: source_id;            (* for stacking global values *)
    old_page_title: title_string;
    old_idx: line_index;
    old_line: line_string;

begin
  if include_level = max_include_level then begin
    error (err_incl_too_many);
    return;                         (* <---- exit early if error *)
  end;

  skip_blanks; (* up to file name *)
  if not cmd_file_name (line, lindex, true, fname) then begin
    err_idx := lindex;
    dir_error (err_inc_file_name);
  end;

  if not open_search (incl_file, '.' || ext || ' ' || fname) then begin
    if ext = 'HDR'
      then err_text (err_bad_hdr_fil, fname)
      else err_text (err_bad_file, fname);
    return;                       (* <---- exit on error *)
  end;

  old_source := cur_source;   (* stack values *)
  old_page_title := page_title;
  old_idx := err_idx;
  old_line := line;

  with cur_source do begin   (* init cur_source for this file *)
    no_files := no_files + 1; (* increment global counter *)
    file_no := no_files;
    page_no := 0;
    line_no := 0;
  end;

  action (incl_file, include_level + 1, in_system);

  cur_source := old_source;   (* restore values *)
  page_title := old_page_title;
  err_idx := old_idx;
  line := old_line;
  skip_blanks;
  if not cmd_eol (line, lindex) then
    dir_error (err_dir_unterminated);
end;
$PAGE system - in process_directive
(* SYSTEM processes a system file.  It causes the file (and any nested files)
   to be read in system mode. *)

procedure system (var f: text; include_level: include_range; in_system: boolean);

var old_src_on: boolean;

begin
  old_src_on := src_on;               (* suppress listing for duration of file *)
  src_on := src_on and prog_options.lsys_opt;
  get_lines_from_file (f, include_level, not prog_options.lsys_opt (* system mode *));
  src_on := old_src_on;
end;
$PAGE header - in process_directive
(* HEADER processes a header file.  In the reading pass, it simply creates a
   src_id record to remember that the file was there.  In the listing pass, it
   passes the lines of the file along without further processing. *)

procedure header (var f: text; include_level: include_range; in_system: boolean);

var cur_file: source_ptr;
    cur_page: page_ptr;

begin
  cur_file := new_file (f, cur_source.file_no, include_level, in_system, listfb.pageno);
  cur_page := new_page (cur_file, 0, nil, '', listfb.pageno);

  if listing then
    while read_a_line (f) do detach;

  close (f);
end;
$PAGE if_ifn_directive - in process_directive
(* IF_IFN_DIRECTIVE processes $IF, $IFANY, $IFNONE and $IFNOT directives,
   evaluating the switches to determine if the lines should be compiled
   or skipped. *)

procedure if_ifn_directive (dir: directs);

var switch_name: switch_string;
    was_enabled: boolean;
    paren: boolean;
    any_true, all_true, switch_true: boolean;
    start_src: source_id;            (* line on which if started *)

begin
  was_enabled := ln_enabled;        (* lambda close enabling value *)

  any_true := false;
  all_true := true;
  if cmd_check_punct (line, lindex, ':') orif cmd_check_punct (line, lindex, '=')
    then paren := false
    else paren := cmd_check_punct (line, lindex, '(');
  repeat
    switch_name := identifier ();
    switch_true := switch (prog_options.switches, switch_name);
    any_true := any_true or switch_true;
    all_true := all_true and switch_true;
  until not paren orif not cmd_check_punct (line, lindex, ',');
  if paren andif not cmd_check_punct (line, lindex, ')') then
    dir_error (err_opt_paren);
  if ln_enabled then begin (* $IF may disable, but never enable *)
    case dir of
      dir_if:     ln_enabled := all_true;
      dir_ifany:  ln_enabled := any_true;
      dir_ifnone: ln_enabled := not any_true;
      dir_ifnot:  ln_enabled := not all_true
    end;
  end;

  if (substr (line,lindex) <> '') then begin (* text on line? *)
    substr (line, 1, lindex - 1) := ''; (* blank out directive *)
    if ln_enabled or listing then detach;
  end

  else begin                        (* multi-line if-end *)
    if listing then detach;
    start_src := cur_source;
    loop
      if not read_a_line (f) then begin
	err_print (err_eof, start_src, '', 0);
	ln_enabled := true;
	goto 1 (* EOF abort *) ;
      end;
    exit if (length (line) >= 4) andif (substr (line, 1, 4) = '$END') do
      if listing then detach;
      if ln_enabled then begin              (* normal processing *)
	if line[1] = '$'
	  then process_directive
	  else begin
	    in_directives := false;
	    detach;
	  end;
      end
      else if (length (line) >= 3) andif (substr (line, 1, 3) = '$IF')
	then process_directive        (* to count nesting *)
      else if listing then detach;
    end; (* loop *)
  end;

  ln_enabled := was_enabled;
end;
$PAGE process_directive - main routine
var ix: integer;
    dir_ix: directs;
    val: integer;
    eval: boolean;
    switchname: switch_string;
    paren: boolean;

begin
  lindex := 2;        (* start scan past '$' *)
  err_idx := 2;
  dir_listed := false;                (* directive not yet listed *)

if not cmd_lookup (line, lindex, ['A'..'Z'], dirs, ix) then begin
    dir_error (err_bad_directive);
    return;   (* dir_error will actually do goto 2 *)
  end;

dir_ix := directs (ix);

  if not (dir_ix in [dir_if, dir_ifany, dir_ifnot, dir_ifnone, dir_page]) then begin
    if listing then detach;
    dir_listed := true;
  end;

  case dir_ix of

    dir_disable, dir_enable:
      begin
	eval := (dir_ix = dir_enable);                (* determine whether to enable or disable *)
	if cmd_check_punct (line, lindex, ':') orif cmd_check_punct (line, lindex, '=')
	  then paren := false
	  else paren := cmd_check_punct (line, lindex, '(');
	repeat
	  switchname := identifier ();
	  prog_options.switches := enable_switch (prog_options.switches, switchname, eval);
	until not paren orif not cmd_check_punct (line, lindex, ',');
	if paren andif not cmd_check_punct (line, lindex, ')') then dir_error (err_opt_paren);
	skip_blanks;
	if not cmd_eol (line, lindex) then dir_error (err_dir_unterminated);
      end;

    dir_endif:
      dir_error (err_unmatched_end);

    dir_if, dir_ifany, dir_ifnone, dir_ifnot:
      if_ifn_directive (dir_ix);

    dir_header:
      if src_on and (list_file <> '') then
	file_directive (header, 'HDR');

    dir_system:
      file_directive (system, 'INC');

    dir_include:
      file_directive (get_lines_from_file, 'INC');

    dir_length:
      begin
	val := numeric_option ();
	if (val < 4) or (val > 255)
	  then dir_error (err_bad_length)
	  else begin
	    listfb.plength := val;
	    if in_directives        (* establish page length on primary setting *)
	      then prog_options.page_length := listfb.plength;
	  end;
      end;

    dir_width:
      begin
	val := numeric_option ();
	if (val < 20) or (val > 255)
	  then dir_error (err_bad_width)
	  else begin
	    listfb.width := val;
	    if in_directives        (* establish page width on primary setting *)
	      then prog_options.page_width := lisidth;
	  end;
      end;

    dir_title:
      begin
	skip_blanks;
	global_title := substr (literal_line, lindex);
	if in_directives
	  then main_title := global_title;
      end;

    dir_page:
      if cmd_eol (line, lindex) then begin (* don't change title if no new one *)
	if listing then
	  detach;
      end
      else begin
	page_title := substr (literal_line, lindex);
	with cur_source do begin      (* update number before listing *)
	  page_no := page_no + 1;
	  line_no := 1;
	end;
	if listing then detach;       (* list first so as to get correct listfb.pageno *)
	cur_page := new_page (cur_file, cur_source.page_no, cur_page, page_title, listfb.pageno);
      end;

    dir_source, dir_nosource, dir_autosource:
      begin
	if in_system and (dir_ix = dir_source) then
	  dir_error (err_in_system) 
	else begin
	  src_on := (dir_ix = dir_source) or
		    ((dir_ix = dir_autosource) and (list_explicit));
	  if in_directives
	    then begin
	      src_selected := src_on;
	      if dir_ix = dir_source then
		prog_options.source_opt := opt_is_on
	      else if dir_ix = dir_nosource then
		prog_options.source_opt := opt_is_off
	      else
		prog_options.source_opt := opt_is_auto;
	    end
	    else src_selected := src_selected or src_on;
	end;
	if not cmd_eol (line, lindex) then dir_error (err_dir_unterminated);
      end
  end (* case *) ;

2 (* parse abort *) :         (* come here after syntax error detected by
				 parsing utilities. *)
end;
$PAGE get_lines_from_file - main routine

begin
  cur_file := new_file (f, cur_source.file_no, include_level, in_system, listfb.pageno);
  cur_page := new_page (cur_file, 0, nil, '', listfb.pageno);

  while read_a_line (f) do begin (* until eof *)
    if line [1] = '$' then
      process_directive
    else begin (* non directive text *)
      in_directives := false;
      detach;
    end;
  end;

1 (* EOF abort *) :
  close (f);
end;
$PAGE read_input_lines
(* READ INPUT LINES reads input lines for the first pass.  It is assumed that
   the main file is opened on the file "input".  It is also the responsibility
   of this routine to initialize all variables required by the reader and friends
   and to determine global option settings from the directive lines. *)

public procedure read_input_lines;
  var sw_start: switch_ptr;
begin
  cur_source := null_source;            (* initialization require both passes *)
  ln_enabled := true;
  end_of_file := false;
  src_on := (prog_options.source_opt = opt_is_on) or
            ((prog_options.source_opt = opt_is_auto) and list_explicit);
  last_file := nil;
  no_files := 0;

  file_list := nil;                     (* following variables are derived during first pass *)
  src_selected := src_on;
  main_title := filename (input);
  fin_source := last_source;
  global_title := main_title;     (* this are not used first pass, but keep kosher *)
  page_title := '';
  listfb.plength := prog_options.page_length;
  listfb.width := prog_options.page_width;
  in_directives := true;                (* permits setting of title, length and width *)
  detach;                               (* continue at first coroutine call *)

  listing := false;                     (* designates first pass *)
  sw_start := prog_options.switches;
  get_lines_from_file (input, 0, false);        (* returns only after entire file read *)
  pop_switches (prog_options.switches, sw_start);       (* delete switches created in passing *)
  prog_options.switches := sw_start;

  end_of_file := true;                  (* signal end of file until coroutine is killed *)
  line := '  ';
  literal_line := '';
  loop
    detach;
  end;
end;
$PAGE read_listing_lines
(* READ LISTING LINES reads input lines for the listing pass.  It is assumed that
   the main file is opened on the file "input".  It is the responsiblity of this
   routine to initialize all global variables which serve as the reader's output. *)

public procedure read_listing_lines;
  var sw_start: switch_ptr;
begin
  cur_source := null_source;            (* init global variables *)
  ln_enabled := true;
  end_of_file := false;
  src_on := (prog_options.source_opt = opt_is_on) or
            ((prog_options.source_opt = opt_is_auto) and list_explicit);
  last_file := nil;
  last_page := nil;
  no_files := 0;
  page_title := '';
  in_directives := false;               (* default directives not updated second pass *)
  detach;                               (* continue at first coroutine call *)

  listing := true;                      (* designates listing pass *)
  sw_start := prog_options.switches;
  get_lines_from_file (input, 0, false);        (* returns only after entire file read *)
  pop_switches (prog_options.switches, sw_start);       (* dispose switch nodes created in passing *)
  prog_options.switches := sw_start;

  end_of_file := true;                  (* signal end of file until coroutine is killed *)
  line := '  ';
  literal_line := '';
  loop
    detach;
  end;
end.
   i h