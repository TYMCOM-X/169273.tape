$TITLE pasrdr - source file reader
$LENGTH 42
MODULE pasrdr;
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
STATIC VAR
in_directives: BOOLEAN; (* true until first non directive line read *)
listing: BOOLEAN; (* true => in listing pass *)

CONST
tab = CHR (11b);
PUBLIC VAR
reader: co_routine; (* for coroutine READ_INPUT_LINES *)

TYPE
include_range = 0..max_include_level;

TYPE
directs = ( dir_autosource, dir_disable, dir_enable, dir_endif, dir_header,
  dir_if, dir_ifany, dir_ifnone, dir_ifnot, dir_include, dir_length,
  dir_nosource, dir_page, dir_source, dir_system, dir_title, dir_width );

CONST
dirs: ARRAY[1..ORD(MAXIMUM(directs))
  +1] OF cmd_lookup_record = ( ( 'AUTOSOURCE', 5, ORD (dir_autosource) )
  , ( 'DISABLE   ', 3, ORD (dir_disable) )
  , ( 'ENABLE    ', 2, ORD (dir_enable) ), ( 'ENDIF     ', 3, ORD (dir_endif)
  ), ( 'HEADER    ', 4, ORD (dir_header) ), ( 'IF        ', 2, ORD (dir_if) )
  , ( 'IFANY     ', 3, ORD (dir_ifany) ), ( 'IFNONE    ', 5, ORD (dir_ifnone)
  ), ( 'IFNOT     ', 5, ORD (dir_ifnot) )
  , ( 'INCLUDE   ', 3, ORD (dir_include) )
  , ( 'LENGTH    ', 3, ORD (dir_length) )
  , ( 'NOSOURCE  ', 3, ORD (dir_nosource) ), ( 'PAGE      ', 4, ORD (dir_page)
  ), ( 'SOURCE    ', 1, ORD (dir_source) )
  , ( 'SYSTEM    ', 3, ORD (dir_system) ), ( 'TITLE     ', 5, ORD (dir_title)
  ), ( 'WIDTH     ', 3, ORD (dir_width) ) );
$PAGE read_a_line
(* READ A LINE reads the next line of text from a designated file.  Fills in
   "line" and "literal_line" with text.  Increments cur_source counter.  At
   the end of file, the lines are set to blank.  The return value goes false
   at end of file. *)

FUNCTION read_a_line (VAR f: TEXT): BOOLEAN;
BEGIN
  read_a_line := NOT EOF (f);
  IF read_a_line THEN BEGIN (* if anything to be read *)
    READLN (f, literal_line);
    line := UPPERCASE (literal_line) || '  '; (* plus blanks for scanner *)
    cur_source.line_no := cur_source.line_no + 1;
    linect := linect + 1;
    IF cur_source.file_no <> 0 THEN inclct := inclct + 1;
  END
  ELSE BEGIN
    line := '  ';
    literal_line := '';
  END;
END (* read_a_line *);
$PAGE new_file
(* NEW FILE advances to the src_id record for the file "f".  In the first pass,
   the record is created, initialized and appended to the master "file_list";
   in the second pass, the record created in the first pass is found and the
   physical page assigned.  A pointer to the record is returned. *)

VAR
last_file: source_ptr;

FUNCTION new_file ( VAR f: TEXT; (* file variable of file, assumed open *)
file_number: file_range; (* file number assigned to file *)
include_level: include_range; (* level of nesting of file *)
in_system: BOOLEAN; (* true => under control of system directive *)
phys_page: page_range (* physical page on which file is included *)
): source_ptr;
VAR
fname: FILE_NAME;
BEGIN
  IF NOT listing THEN BEGIN (* first pass *)
    fname := FILENAME (f);
    NEW (new_file, LENGTH (fname));
    WITH new_file^ DO BEGIN
      file_no := file_number;
      pages := NIL;
      next_file := NIL;
      incl_level := include_level;
      system_file := in_system;
      FILE_NAME := fname;
    END;
    IF file_list = NIL THEN file_list := new_file (* start of list *)
    ELSE last_file^.next_file := new_file;
  END
  ELSE BEGIN (* second pass *)
    IF last_file = NIL THEN new_file := file_list
    ELSE new_file := last_file^.next_file;
    new_file^.incl_page := phys_page;
  END;
  last_file := new_file;
END;
$PAGE new_page
(* NEW PAGE advances to the src_id record for the indicated page.  In the first
   pass, the record is created, initialized, and chained into the list of pages;
   in the second pass, the record created in the first pass is found and the
   physical page number assigned.  A pointer to the record is returned. *)

VAR
last_page: page_ptr;

FUNCTION new_page ( file_ptr: source_ptr; (* src_id of file containing page *)
page_no: page_range; (* page number to be assigned *)
prev_page_ptr: page_ptr; (* page_id of preceding page in file, or nil *)
page_title: title_string; (* title for page *)
phys_page: page_range (* actual page on which section begins *)
): page_ptr; (* to record of new page *)
BEGIN
  IF NOT listing THEN BEGIN (* first pass *)
    NEW (new_page, LENGTH (page_title));
    WITH new_page^ DO BEGIN
      left_page := NIL;
      right_page := NIL;
      next_page := NIL;
      following_page := NIL;
      in_file := file_ptr;
      page_number := page_no;
      incl_page := 0;
      subtitle := page_title;
    END (* with *);
    IF prev_page_ptr = NIL THEN file_ptr^.pages := new_page
    ELSE prev_page_ptr^.next_page := new_page;
    IF last_page <> NIL THEN last_page^.following_page := new_page;
  END
  ELSE BEGIN (* second pass *)
    IF last_page = NIL THEN new_page := file_list^.pages (* page 0 of file 0 *)
    ELSE new_page := last_page^.following_page;
    new_page^.incl_page := phys_page;
  END;
  last_page := new_page;
END;
$PAGE get_lines_from_file
(* GET LINES FROM FILE is called recursively to read the lines of the main
   and include (system) files.  When entered, it is assumes that the file has
   been opened.  Processing terminates normally when the end of file is
   detected by this routine.  Processing terminates abnormally (via a non-
   local goto) when an unbalanced if-end directive pair is detected by
   lower level routines.  This returns through a coroutine linkage each text
   or directive line to be listed.  Directive lines are interpreted by this
   routine. *)

PROCEDURE get_lines_from_file ( VAR f: TEXT; (* current input file *)
include_level: include_range; (* level of nesting of this file *)
in_system: BOOLEAN ); (* indicates if file under control of system directive *)
LABEL
1 (* EOF abort *);
VAR
cur_file: source_ptr; (* src_id for file begin read *)
cur_page: page_ptr; (* page_id for page begin read *)
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

  PROCEDURE process_directive;
  LABEL
  2 (* parse abort *); (* escape on syntax error in directive *)
  VAR
  lindex: INTEGER; (* directive parsing cursor *)
  err_idx: INTEGER; (* error cursor location *)
  dir_listed: BOOLEAN; (* indicates directive line has been listed;
					   used to control listing of erroneous line *)
$PAGE dir_error - in process_directive
  (* DIR ERROR reports an error on a directive line;  the error cursor is taken
       from the global variable "err_idx" which is set by the parsing utilities
       below as the line is scanned.  Escapes back into process_directive. *)

    PROCEDURE dir_error (code: err_codes);
    BEGIN
      IF listing AND (NOT dir_listed) THEN detach;
      err_print (code, cur_source, '', err_idx);
      GOTO 2 (* parse abort *);
    END;
$PAGE skip_blanks

    PROCEDURE skip_blanks;
    VAR
    cmt_level: 0 .. 100;
    l: INTEGER;
    BEGIN
      LOOP
	WHILE (lindex <= LENGTH (line)) ANDIF (line[lindex] <= ' ')
	  DO lindex := lindex + 1;
	EXIT IF (lindex >= LENGTH (line)) ORIF (line[lindex] <> '(') ORIF
	  (line[lindex+1] <> '*');
	cmt_level := 1;
	err_idx := lindex;
	lindex := lindex + 2;
	REPEAT
	  l := SEARCH (SUBSTR (line, lindex), ['(','*']);
	  lindex := lindex + l - 1;
	  IF (l = 0) OR (lindex >= LENGTH (line))
	    THEN dir_error (err_opt_comment);
	  IF (line[lindex] = '(') ANDIF (line[lindex+1] = '*') THEN BEGIN
	    cmt_level := cmt_level + 1;
	    lindex := lindex + 2;
	  END
	  ELSE IF (line[lindex] = '*') ANDIF (line[lindex+1] = ')') THEN BEGIN
	    cmt_level := cmt_level - 1;
	    lindex := lindex + 2;
	  END
	  ELSE lindex := lindex + 1;
	UNTIL cmt_level = 0;
      END (* loop *);
      err_idx := lindex;
    END;
$PAGE numeric_option

    FUNCTION numeric_option: INTEGER;
    VAR
    l: INTEGER;
    paren: BOOLEAN;
    BEGIN
      skip_blanks;
      IF cmd_check_punct (line, lindex, ':') ORIF
	cmd_check_punct (line, lindex, '=') THEN paren := FALSE
      ELSE paren := cmd_check_punct (line, lindex, '(');
      skip_blanks;
      l := lindex; (* remember start of number *)
      IF NOT cmd_number (line, lindex, FALSE, numeric_option)
	THEN dir_error (err_val_expected);
      IF paren ANDIF NOT cmd_check_punct (line, lindex, ')')
	THEN dir_error (err_opt_paren);
      skip_blanks;
      IF NOT cmd_eol (line, lindex) THEN dir_error (err_dir_unterminated);
      err_idx := l; (* caller wants error at number *)
    END;
$PAGE identifier

    FUNCTION identifier: parm_string;
    BEGIN
      skip_blanks;
      IF NOT cmd_token (line, lindex, ['A'..'Z', '0'..'9', '_'], identifier)
	THEN dir_error (err_sw_expected);
      skip_blanks;
    END;
$PAGE file_directive - in process_directive
    (* FILE DIRECTIVE performs common processing for $include, $system or $header
       directive lines, and calls the specified processing routine.  The actions of
       this routine include:  pushing an include level, parsing the filename,
       opening the file, and stacking global values which must be restored after
       the file is read. *)
  TYPE
  file_proc =

    PROCEDURE ( VAR f: TEXT; (* like get_lines_from_file *)
    include_level: include_range; in_system: BOOLEAN );
    ext_type = PACKED ARRAY [1..3] OF CHAR;

      PROCEDURE file_directive (action: file_proc; ext: ext_type);
      VAR
      fname: FILE_NAME;
      incl_file: TEXT;
      old_source: source_id; (* for stacking global values *)
      old_page_title: title_string;
      old_idx: line_index;
      old_line: line_string;
      BEGIN
	IF include_level = max_include_level THEN BEGIN
	  error (err_incl_too_many);
	  RETURN; (* <---- exit early if error *)
	END;
	skip_blanks; (* up to file name *)
	IF NOT cmd_file_name (line, lindex, TRUE, fname) THEN BEGIN
	  err_idx := lindex;
	  dir_error (err_inc_file_name);
	END;
	IF NOT open_search (incl_file, '.' || ext || ' ' || fname) THEN BEGIN
	  IF ext = 'HDR' THEN err_text (err_bad_hdr_fil, fname)
	  ELSE err_text (err_bad_file, fname);
	  RETURN; (* <---- exit on error *)
	END;
	old_source := cur_source; (* stack values *)
	old_page_title := page_title;
	old_idx := err_idx;
	old_line := line;
	WITH cur_source DO BEGIN (* init cur_source for this file *)
	  no_files := no_files + 1; (* increment global counter *)
	  file_no := no_files;
	  page_no := 0;
	  line_no := 0;
	END;
	action (incl_file, include_level + 1, in_system);
	cur_source := old_source; (* restore values *)
	page_title := old_page_title;
	err_idx := old_idx;
	line := old_line;
	skip_blanks;
	IF NOT cmd_eol (line, lindex) THEN dir_error (err_dir_unterminated);
      END;
$PAGE system - in process_directive
      (* SYSTEM processes a system file.  It causes the file (and any nested files)
	 to be read in system mode. *)

      PROCEDURE system (VAR f: TEXT; include_level: include_range;
	in_system: BOOLEAN);
      VAR
      old_src_on: BOOLEAN;
      BEGIN
	old_src_on := src_on; (* suppress listing for duration of file *)
	src_on := src_on AND prog_options.lsys_opt;
	get_lines_from_file (f, include_level, NOT prog_options.lsys_opt (* system mode *)
	  );
	src_on := old_src_on;
      END;
$PAGE header - in process_directive
      (* HEADER processes a header file.  In the reading pass, it simply creates a
	 src_id record to remember that the file was there.  In the listing pass, it
	 passes the lines of the file along without further processing. *)

      PROCEDURE header (VAR f: TEXT; include_level: include_range;
	in_system: BOOLEAN);
      VAR
      cur_file: source_ptr;
      cur_page: page_ptr;
      BEGIN
	cur_file := new_file (f, cur_source.file_no, include_level, in_system,
	  listfb.pageno);
	cur_page := new_page (cur_file, 0, NIL, '', listfb.pageno);
	IF listing THEN WHILE read_a_line (f) DO detach;
	CLOSE (f);
      END;
$PAGE if_ifn_directive - in process_directive
      (* IF_IFN_DIRECTIVE processes $IF, $IFANY, $IFNONE and $IFNOT directives,
	 evaluating the switches to determine if the lines should be compiled
	 or skipped. *)

      PROCEDURE if_ifn_directive (dir: directs);
      VAR
      switch_name: switch_string;
      was_enabled: BOOLEAN;
      paren: BOOLEAN;
      any_true, all_true, switch_true: BOOLEAN;
      start_src: source_id; (* line on which if started *)
      BEGIN
	was_enabled := ln_enabled; (* lambda close enabling value *)
	any_true := FALSE;
	all_true := TRUE;
	IF cmd_check_punct (line, lindex, ':') ORIF
	  cmd_check_punct (line, lindex, '=') THEN paren := FALSE
	ELSE paren := cmd_check_punct (line, lindex, '(');
	REPEAT
	  switch_name := identifier ();
	  switch_true := switch (prog_options.switches, switch_name);
	  any_true := any_true OR switch_true;
	  all_true := all_true AND switch_true;
	UNTIL NOT paren ORIF NOT cmd_check_punct (line, lindex, ',');
	IF paren ANDIF NOT cmd_check_punct (line, lindex, ')')
	  THEN dir_error (err_opt_paren);
	IF ln_enabled THEN BEGIN (* $IF may disable, but never enable *)
	  CASE dir OF
	    dir_if: ln_enabled := all_true;
	    dir_ifany: ln_enabled := any_true;
	    dir_ifnone: ln_enabled := NOT any_true;
	    dir_ifnot: ln_enabled := NOT all_true
	  END;
	END;
	IF (SUBSTR (line,lindex) <> '') THEN BEGIN (* text on line? *)
	  SUBSTR (line, 1, lindex - 1) := ''; (* blank out directive *)
	  IF ln_enabled OR listing THEN detach;
	END
	ELSE BEGIN (* multi-line if-end *)
	  IF listing THEN detach;
	  start_src := cur_source;
	  LOOP
	    IF NOT read_a_line (f) THEN BEGIN
	      err_print (err_eof, start_src, '', 0);
	      ln_enabled := TRUE;
	      GOTO 1 (* EOF abort *);
	    END;
	    EXIT IF (LENGTH (line) >= 4) ANDIF (SUBSTR (line, 1, 4) = '$END')
	      DO IF listing THEN detach;
	    IF ln_enabled THEN BEGIN (* normal processing *)
	      IF line[1] = '$' THEN process_directive
	      ELSE BEGIN
		in_directives := FALSE;
		detach;
	      END;
	    END
	    ELSE IF (LENGTH (line) >= 3) ANDIF (SUBSTR (line, 1, 3) = '$IF')
	      THEN process_directive (* to count nesting *)
	    ELSE IF listing THEN detach;
	  END; (* loop *)
	END;
	ln_enabled := was_enabled;
      END;
$PAGE process_directive - main routine
    VAR
    ix: INTEGER;
    dir_ix: directs;
    val: INTEGER;
    eval: BOOLEAN;
    switchname: switch_string;
    paren: BOOLEAN;
    BEGIN
      lindex := 2; (* start scan past '$' *)
      err_idx := 2;
      dir_listed := FALSE; (* directive not yet listed *)
      IF NOT cmd_lookup (line, lindex, ['A'..'Z'], dirs, ix) THEN BEGIN
	dir_error (err_bad_directive);
	RETURN; (* dir_error will actually do goto 2 *)
      END;
      dir_ix := directs (ix);
      IF NOT (dir_ix IN [dir_if, dir_ifany, dir_ifnot, dir_ifnone, dir_page])
	THEN BEGIN
	IF listing THEN detach;
	dir_listed := TRUE;
      END;
      CASE dir_ix OF
	dir_disable, dir_enable: BEGIN
	  eval := (dir_ix = dir_enable); (* determine whether to enable or disable *)
	  IF cmd_check_punct (line, lindex, ':') ORIF
	    cmd_check_punct (line, lindex, '=') THEN paren := FALSE
	  ELSE paren := cmd_check_punct (line, lindex, '(');
	  REPEAT
	    switchname := identifier ();
	    prog_options.switches := enable_switch (prog_options.switches,
	      switchname, eval);
	  UNTIL NOT paren ORIF NOT cmd_check_punct (line, lindex, ',');
	  IF paren ANDIF NOT cmd_check_punct (line, lindex, ')')
	    THEN dir_error (err_opt_paren);
	  skip_blanks;
	  IF NOT cmd_eol (line, lindex) THEN dir_error (err_dir_unterminated);
	END;
	dir_endif: dir_error (err_unmatched_end);
	dir_if, dir_ifany, dir_ifnone, dir_ifnot: if_ifn_directive (dir_ix);
	dir_header: IF src_on AND (list_file <> '')
	  THEN file_directive (header, 'HDR');
	dir_system: file_directive (system, 'INC');
	dir_include: file_directive (get_lines_from_file, 'INC');
	dir_length: BEGIN
	  val := numeric_option ();
	  IF (val < 4) OR (val > 255) THEN dir_error (err_bad_length)
	  ELSE BEGIN
	    listfb.plength := val;
	    IF in_directives (* establish page length on primary setting *)
	    THEN prog_options.page_length := listfb.plength;
	  END;
	END;
	dir_width: BEGIN
	  val := numeric_option ();
	  IF (val < 20) OR (val > 255) THEN dir_error (err_bad_width)
	  ELSE BEGIN
	    listfb.width := val;
	    IF in_directives (* establish page width on primary setting *)
	    THEN prog_options.page_width := listfb.width;
	  END;
	END;
	dir_title: BEGIN
	  skip_blanks;
	  global_title := SUBSTR (literal_line, lindex);
	  IF in_directives THEN main_title := global_title;
	END;
	dir_page: IF cmd_eol (line, lindex) THEN BEGIN (* don't change title if no new one *)
	  IF listing THEN detach;
	END
	ELSE BEGIN
	  page_title := SUBSTR (literal_line, lindex);
	  WITH cur_source DO BEGIN (* update number before listing *)
	    page_no := page_no + 1;
	    line_no := 1;
	  END;
	  IF listing THEN detach; (* list first so as to get correct listfb.pageno *)
	  cur_page := new_page (cur_file, cur_source.page_no, cur_page,
	    page_title, listfb.pageno);
	END;
	dir_source, dir_nosource, dir_autosource: BEGIN
	  IF in_system AND (dir_ix = dir_source)
	    THEN dir_error (err_in_system)
	  ELSE BEGIN
	    src_on := (dir_ix = dir_source) OR ((dir_ix = dir_autosource) AND
	      (list_explicit));
	    IF in_directives THEN BEGIN
	      src_selected := src_on;
	      IF dir_ix = dir_source THEN prog_options.source_opt := opt_is_on
	      ELSdir_ix = dir_nosource THEN prog_options.source_opt :=
		opt_is_off
	      ELSE prog_options.source_opt := opt_is_auto;
	    END
	    ELSE src_selected := src_selected OR src_on;
	  END;
	  IF NOT cmd_eol (line, lindex) THEN dir_error (err_dir_unterminated);
	END
      END (* case *);
      2 (* parse abort *): (* come here after syntax error detected by
				       parsing utilities. *)
    END;
$PAGE get_lines_from_file - main routine
  BEGIN
    cur_file := new_file (f, cur_source.file_no, include_level, in_system,
      listfb.pageno);
    cur_page := new_page (cur_file, 0, NIL, '', listfb.pageno);
    WHILE read_a_line (f) DO BEGIN (* until eof *)
      IF line [1] = '$' THEN process_directive
      ELSE BEGIN (* non directive text *)
	in_directives := FALSE;
	detach;
      END;
    END;
    1 (* EOF abort *): CLOSE (f);
  END;
$PAGE read_input_lines
  (* READ INPUT LINES reads input lines for the first pass.  It is assumed that
     the main file is opened on the file "input".  It is also the responsibility
     of this routine to initialize all variables required by the reader and friends
     and to determine global option settings from the directive lines. *)
PUBLIC
  PROCEDURE read_input_lines;
  VAR
  sw_start: switch_ptr;
  BEGIN
    cur_source := null_source; (* initialization require both passes *)
    ln_enabled := TRUE;
    end_of_file := FALSE;
    src_on := (prog_options.source_opt = opt_is_on) OR
      ((prog_options.source_opt = opt_is_auto) AND list_explicit);
    last_file := NIL;
    no_files := 0;
    file_list := NIL; (* following variables are derived during first pass *)
    src_selected := src_on;
    main_title := FILENAME (INPUT);
    fin_source := last_source;
    global_title := main_title; (* this are not used first pass, but keep kosher *)
    page_title := '';
    listfb.plength := prog_options.page_length;
    listfb.width := prog_options.page_width;
    in_directives := TRUE; (* permits setting of title, length and width *)
    detach; (* continue at first coroutine call *)
    listing := FALSE; (* designates first pass *)
    sw_start := prog_options.switches;
    get_lines_from_file (INPUT, 0, FALSE); (* returns only after entire file read *)
    pop_switches (prog_options.switches, sw_start); (* delete switches created in passing *)
    prog_options.switches := sw_start;
    end_of_file := TRUE; (* signal end of file until coroutine is killed *)
    line := '  ';
    literal_line := '';
    LOOP
      detach;
    END;
  END;
$PAGE read_listing_lines
  (* READ LISTING LINES reads input lines for the listing pass.  It is assumed that
     the main file is opened on the file "input".  It is the responsiblity of this
     routine to initialize all global variables which serve as the reader's output. *)
PUBLIC
  PROCEDURE read_listing_lines;
  VAR
  sw_start: switch_ptr;
  BEGIN
    cur_source := null_source; (* init global variables *)
    ln_enabled := TRUE;
    end_of_file := FALSE;
    src_on := (prog_options.source_opt = opt_is_on) OR
      ((prog_options.source_opt = opt_is_auto) AND list_explicit);
    last_file := NIL;
    last_page := NIL;
    no_files := 0;
    page_title := '';
    in_directives := FALSE; (* default directives not updated second pass *)
    detach; (* continue at first coroutine call *)
    listing := TRUE; (* designates listing pass *)
    sw_start := prog_options.switches;
    get_lines_from_file (INPUT, 0, FALSE); (* returns only after entire file read *)
    pop_switches (prog_options.switches, sw_start); (* dispose switch nodes created in passing *)
    prog_options.switches := sw_start;
    end_of_file := TRUE; (* signal end of file until coroutine is killed *)
    line := '  ';
    literal_line := '';
    LOOP
      detach;
    END;
  END.
  W ñ