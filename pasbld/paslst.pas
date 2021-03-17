$TITLE PASLST.PAS, last modified 5/14/84, zw
PROGRAM paslst options special(word), storage(4000);
(*TYM-Pascal compiler listing generator*)
$PAGE includes and declarations
$SYSTEM pascal.inc
$SYSTEM ptmcon.inc
$SYSTEM pasist.inc
$SYSTEM pasfil.inc
$SYSTEM paslog.inc
$SYSTEM pasenv.inc
$SYSTEM corout.inc
$SYSTEM versio.inc
$SYSTEM prgdir.inc
$SYSTEM TIMUTL.inc
$SYSTEM infpac.inc
$SYSTEM RUNUTL.INC
$SYSTEM tmpnam.inc
$SYSTEM pasopn.inc
$SYSTEM pasrdr.inc
$SYSTEM paspt.typ
$SYSTEM paserr.inc
$SYSTEM pascv.inc
$SYSTEM pasopd.inc
external procedure xr_sym_call;
external var
  abort: co_routine;        (* fatal error action define in PASERR *)
public var
  mod_ident: string[12];                (* module identifier *)
  time_str: string[24];                 (* date/time reading *)
var
  num_files: file_range;        (* number of files in compilation *)
  ttyfb: file_block;                    (* for formatting errors on the tty *)
$PAGE error declarations
static var last_location: source_id;
const terminal_width: line_index := 72;
type  err_headers = array[1..3] of string[15];
const error_type: err_headers := ('Warning ','Error ','Fatal error ');
type
  err_ptr = ^err_record;
  err_record = packed record
    code: err_codes;
    column: line_index;
    source: source_id;
    number: integer; (* sequential error number *)
    extra_text: packed array[1..*] of char
  end;
  err_array = array [1..*] of err_ptr;
  err_file_array = array [0..*] of integer;
static var
  err_list: ^ err_array;
  err_file_list: ^ err_file_array; (* indices into err_list *)
  err_log_file: file of *;
  message_file: text;
  last_err_loc: source_id;
  err_cnt_line: string [30];
const
  pasmsg := 'PASERR.TXT';
$PAGE get_next_error
(* GET NEXT ERROR reads the next error record from the log file and creates an
   error record for it.  A pointer to the record is returned. *)
function get_next_error: err_ptr;
var ix: integer;
    tcode: err_codes;
    tsource: source_id;
    tcolumn: line_index;
    tlen: line_index;
begin
  read (err_log_file, ix);
  if ix = cursor (err_log_file) then begin
    read (err_log_file, tcode, tsource, tcolumn, tlen);
    with tsource do
      if (file_no = null_source.file_no) and (page_no = null_source.page_no)
	   and (line_no = null_source.line_no) then
	file_no := num_files + 1;
    new (get_next_error, tlen);
    with get_next_error^ do begin
      code := tcode;
      column := tcolumn;
      source := tsource;
      number := 0;
      if tlen <> 0 then
	read (err_log_file, extra_text: size (extra_text, tlen));
    end;
  end
  else begin
    new (get_next_error, 0);
    with get_next_error^ do begin
      code := err_file_error;
      column := 0;
      source := (num_files + 1, 0, 0);
      number := 0;
    end;
  end;
end;
$PAGE err_less
(* ERR LESS is a helper algorithm which is used in sorting the error list.
   Error A is less than error B if A is in an earlier file than B, or on an
   earlier page of the same file, or on an earlier line of the same page, or
   at an earlier column of the same line.  If two errors occurred at the same
   location, the one which was detected first is less than the other. *)
function err_less ( a, b: err_ptr ): boolean;
begin
       if a^.source.file_no < b^.source.file_no then
    err_less := true
  else if a^.source.file_no > b^.source.file_no then
    err_less := false
  else if a^.source.page_no < b^.source.page_no then
    err_less := true
  else if a^.source.page_no > b^.source.page_no then
    err_less := false
  else if a^.source.line_no < b^.source.line_no then
    err_less := true
  else if a^.source.line_no > b^.source.line_no then
    err_less := false
  else if a^.column < b^.column then
    err_less := true
  else if a^.column > b^.column then
    err_less := false
  else
    err_less := (a^.number < b^.number);
 end;
$PAGE err_adjust
(* ERR ADJUST is the heart of the heapsort algorithm, which is used to sort
   the error list.  It is called with the error list, List, an index, I, and
   a size, N.  We assume that List[2*I] and List[2*I+1] are heap roots, and
   combine their heaps with List[I] so that List[I] becomes a heap root.  If
   I = 1, then this results in the entire list being made into a heap.
   We say that List[J] is a heap root if J > [N/2], or if List[2*J] and
   List[2*J+1] are heap roots, and List[J] > List[2*j], and List[J] >
   List[2*J+1].
   The heap algorithms used here are taken from Fundamentals of Computer
   Algorithms, by Horowitz and Sahni. *)
procedure err_adjust ( var list: err_array; i, n: integer );
 var j: integer;
     item: err_ptr;
 begin
  j := 2 * i;
  item := list[i];
  while j <= n do begin
    if (j < n) and err_less (list[j], list[j+1]) then (* compare left and right children *)
      j := j + 1; (* j points to the larger child *)
  exit if not err_less (item, list[j]); (* place found for item *)
    list[j div 2] := list[j]; (* move the larger child up a level *)
    j := 2 * j;
  end (* while j <= n *);
  list[j div 2] := item;
 end;
$PAGE sort_error_log
(* SORT ERROR LOG reads the error log file and creates the err_list array containg
   a sorted list of errors, and a list giving the first error in each file. *)
procedure sort_error_log;
  var
    ix, last_ix: integer;
    fix: file_range;
    t: err_ptr;
begin
  new (err_list, err_count + 1);  (* create error info list *)
  new (err_file_list, num_files + 1);
  for fix := 0 to num_files + 1 do
    err_file_list^[fix] := 0;
  if err_count > 0 then begin           (* if any thing to read *)
    (* Read the error file and build the error list. *)
    reset (err_log_file, tempname ('ERR'));
    for ix := 1 to err_count do begin
      err_list^[ix] := get_next_error ();
      err_list^[ix]^.number := ix;
    end;
    close (err_log_file);
  end;
  new (err_list^[err_count+1], 0);
  err_list^[err_count+1]^.source.file_no := maximum (file_range);
  (* Sort the error list, using the heapsort algorithm. *)
  for ix := (err_count div 2) downto 1 do
    err_adjust (err_list^, ix, err_count);
  for ix := err_count downto 2 do begin
    t := err_list^[1];
    err_list^[1] := err_list^[ix];
    err_list^[ix] := t;
    err_adjust (err_list^, 1, ix - 1);
    err_file_list^[err_list^[ix]^.source.file_no] := ix;
  end;
  if err_count <> 0 then
    err_file_list^[err_list^[1]^.source.file_no] := 1;
  last_ix := err_count + 1;
  for fix := num_files + 1 downto 0 do begin
    if err_file_list^[fix] = 0
      then err_file_list^[fix] := last_ix
      else last_ix := err_file_list^[fix];
  end;
end;
$PAGE process_error
(* PROCESS ERROR displays all errors on the same source line as the next error
   in a file.  The error index is updated to the first error which is not on
   the same line. *)
var     (* inited in mainline *)
  last_page: page_ptr;                  (* page_id of page containing last error *)
procedure process_error
  ( err_file: file_range;       (* the file number of the error *)
    err_on_list: boolean;   (* if true => display error on listing *)
    src_listed: boolean;        (* if true => source line just printed on listing *)
    line_text: line_string  );      (* text of line on which error occurs if available *)
  (* Fetches page_id record for page denoted by the file and page of a source
     id record. *)
  function get_page (source: source_id): page_ptr;
  begin
    get_page := last_page;              (* search from last page to end first *)
    while get_page <> nil do begin
      if (get_page^.page_number = source.page_no) and
         (get_page^.in_file^.file_no = source.file_no)
        then return;                    (* <---- exit if found *)
      get_page := get_page^.following_page;
    end;
    get_page := file_list^.pages;       (* page 0 of file 0 *)
    while get_page <> nil do begin      (* search from start until found *)
      if (get_page^.page_number = source.page_no) and
         (get_page^.in_file^.file_no = source.file_no)
        then return;                    (* <---- exit when found *)
      get_page := get_page^.following_page;
    end;
  end;
$PAGE print_id, print_text - in process_error
  (* Prints file and page identification *)
  procedure print_id (var fb: file_block; page: page_ptr);
  begin
    fio_skip (fb);
    with page^ do begin
      if (last_page^.in_file <> in_file) and (in_file^.file_no <> 0) then begin
        with in_file^ do
          fio_line (fb, 'File ' || cv_int (file_no) || ':  ' || file_name);
      end;
      if (last_page <> page) and (page_number <> 0)
        then fio_line (fb, 'Section ' || cv_int (page_number) || ':  ' || subtitle);
    end;
  end;
  (* Prints the text of a line with its line number *)
  procedure print_text (var fb: file_block; err: err_ptr );
  begin
    fio_skip (fb);
    fio_write (fb, cvf_int (err^.source.line_no, 5));
    fio_tab (fb, 9);
    fio_line (fb, line_text);
  end;
$PAGE errout, errskip - in process_error
  (* Prints a string on the terminal and listing (if enabled).  The output is
     automatically filled so that output width restrictions are not overflowed. *)
  procedure errout (str: line_string);
  begin
    if err_on_list then begin
      if (listfb.column + length (str)) > listfb.width then begin
        fio_skip (listfb);
        fio_tab (listfb, listfb.c_column);
      end;
      fio_write (listfb, str);
    end;
    if (ttyfb.column + length (str)) > ttyfb.width then begin
      fio_skip (ttyfb);
      fio_tab (ttyfb, ttyfb.c_column);
    end;
    fio_write (ttyfb, str);
  end;
  (* Skips to a new line on the tty and listing (if enabled) *)
  procedure errskip;
  begin
    if err_on_list then fio_skip (listfb);
    fio_skip (ttyfb);
  end;
$PAGE print_message - in process_error
  procedure print_message (err: err_ptr);
    var word: line_string;
        error_number: user_range;
  begin
    reset (message_file, pasmsg || prgm_dir);
    if iostatus <> io_ok then begin
      rewrite (tty);
      writeln (tty, 'Catastrophic error: Pascal message file not found.');
      stop;
    end;
    loop    (* search for start of message *)
      repeat
        readln (message_file);
      until not eoln (message_file);
      read (message_file, error_number);
    exit if error_number = err_table[err^.code].user_number;
      repeat
        readln (message_file)
      until eoln (message_file);
    end;
    listfb.c_column := listfb.column;
    ttyfb.c_column := ttyfb.column;
    while not eoln (message_file) do begin    (* tokenize and output parts of message *)
      loop
        while (not eoln (message_file)) and (message_file^ = ' ')
          do get (message_file);              (* skip blanks *)
      exit if eoln (message_file);
        word := '';                 (* have non blank, get rest of token *)
        repeat
          if message_file^ = '$'
            then word := word || err^.extra_text    (* substitute supplied text for $ *)
            else word := word || message_file^;
          get (message_file);
        until message_file^ = ' ';
        errout (word || ' ');
      end;
      readln (message_file);
    end;
    listfb.c_column := 9;
    ttyfb.c_column := 9;
    close (message_file);
  end;
$PAGE process_error - main routine
  var
    page: page_ptr;
    err_index, ix: integer;
    err: err_ptr;
    err_source: source_id;
    tcol: line_index;
    err_level: level_range;
begin
  err_index := err_file_list^[err_file];
  err := err_list^[err_index];
  err_source := err^.source;
  (* Output file/page identification if not previously printed *)
  if err^.source.file_no = num_files + 1
    then page := get_page (null_source)
    else page := get_page (err_source);
  if page <> last_page then begin
    print_id (ttyfb, page);
    if err_on_list and (not src_listed) then print_id (listfb, page);
    last_page := page;  (* to suppress output next time around *)
  end;
  (* Display text of line on tty (and listing) with markers *)
  if (not prog_options.terse_opt) and (line_text <> '') and
     (err_source.file_no <> num_files + 1) then begin
    print_text (ttyfb, err); (* print text of line *)
    if err_on_list and not src_listed then print_text (listfb, err);
    ix := err_index; (* print the markers *)
    tcol := 0;
    while (err^.source.file_no = err_source.file_no) and
	  (err^.source.page_no = err_source.page_no) and
	  (err^.source.line_no = err_source.line_no) do begin
      if err^.column > tcol then begin
	tcol := err^.column;
	fio_tab (ttyfb, tcol + 8);
	fio_write (ttyfb, '^');
	if err_on_list then begin
	  fio_tab (listfb, tcol + 8);
	  fio_write (listfb, '^');
	end;
      end;
      ix := ix + 1;
      err := err_list^[ix];
    end;
    errskip;
  end
  else (* don't display text *) begin
    ix := err_index;
    with err_source do
      while (err_list^[ix]^.source.file_no = file_no) and
            (err_list^[ix]^.source.page_no = page_no) and
            (err_list^[ix]^.source.line_no = line_no) do
	ix := ix + 1;
  end;
  (* Display the messages for all errors on the line and increment the
     error index. *)
  err_level := 0;
  while err_index <> ix do begin
    err := err_list^[err_index];
    err_level := max (err_level, err_table[err^.code].level);
    with err_table [err^.code] do begin
      errout (error_type [level]);
      errout (cv_int (user_number));
    end;
    if (prog_options.terse_opt or (line_text = '')) and
       (err_source.file_no <> num_files + 1) then
      errout (' (on line ' || cv_source_id (err^.source) || ')');
    if not prog_options.terse_opt then begin
      errout (': ');
      print_message (err);
    end
    else if err^.extra_text <> '' then
      errout ('  "' || err^.extra_text || '"');
    break (ttyoutput); (* force the message to the tty *)
    errskip;
    err_index := err_index + 1;
  end (* while err_index <> ix *);
  with null_source do
    if ((last_err_loc.file_no <> file_no) or (last_err_loc.page_no <> page_no) or
	(last_err_loc.line_no <> line_no))
	   and err_on_list and src_selected then
      fio_line (listfb, 'Last error at line ' || cv_source_id (last_err_loc));
  last_err_loc := err_source;
  err_file_list^[err_file] := err_index;
  if err_level = 3 then (* fatal error, die now *)
    call (abort);
end;
$PAGE dump_errors
(* DUMP ERRORS displays all errors remaining in the error list. If "err_on_list"
   is true, the error messages are also written to the listing.  This is used
   to display errors without source - i.e., miscellaneouse errors at the end of
   the listing, errors left after a reader abort, or errors to be displayed in
   terse + nosource mode.  The error records are also deleted. *)
procedure dump_errors (err_on_list: boolean);
var ef: file_range;
    ix: integer;
begin
  for ef := 0 to num_files + 1 do begin
    while err_list^[err_file_list^[ef]]^.source.file_no = ef do
      process_error (ef, err_on_list, false, '');
  end;
  for ix := 1 to err_count + 1 do
    dispose (err_list^[ix]);
  dispose (err_list);
  dispose (err_file_list);
  if err_on_list then begin
    fio_skip (listfb);
    fio_line (listfb, err_cnt_line);
  end;
end;
$PAGE opt_header
(* OPT HEADER is called to print the "options in effect" on the header page *)
public procedure opt_header;
var first: boolean;     (* determines whether comma separator required *)
const
    header = 'Options in effect: ';
    indentation = 9;
    effective: array [boolean] of string [2] = ('NO','');
    separator: array [boolean] of string [2] = (', ','');
    autoeffective: array [option_mode] of string [4] = ('NO', 'AUTO', '');
    option_names: array [optionlist] of string [10] =
      ( 'ASSERTIONS', 'CASES', 'COMPATIBIL', 'FIELDS', 'FILES', 'INPUT',
	'POINTERS', 'STRINGS', 'SUBSCRIPTS', 'VALUES', 'STACK', 'COERCIONS',
	'PTR', 'WORD', 'MAP', 'SYMBOLS', 'CALLS', 'ASSEMBLY', 'XREF', 'TRACE',
	'QBLOCKS', 'OPTIMIZE' );
$PAGE out - in opt_header
(* OUT outputs a string to the listing file insuring that it does not overflow
   the maximum width of the listing line.  On overflow, a new line is started. *)
procedure out (str: parm_string);
begin
  fio_write (listfb, separator [first]);
  if (listfb.column + length (str) + 2 (* separator *)) > listfb.width then begin
    fio_skip (listfb);
    fio_tab (listfb, indentation);
  end;
  fio_write (listfb, str);
  first := false;
end;
$PAGE print_switches - in opt_header
(* PRINT SWITCHES formats and outputs a list of switch names. *)
procedure print_switches (head: switch_ptr; title: parm_string; pass: boolean);
var sw: switch_ptr;
    first_switch: boolean;
begin
  first_switch := true;
  sw := head;
  while sw <> nil do 
    with sw^ do begin
      if enabled = pass then begin
	if first_switch then begin
	  out (title || '(');
	  first := true (* no separator after ( *);
	  first_switch := false;
	end;
	out (name);
      end (* if enabled = pass *);
      sw := next_switch;
    end (* while not nil *);
  if not first_switch then begin (* if any printed *)
    first := true; (* no separator before ) *)
    out (')');
  end;
end (* print_switches *);
$PAGE opt_header - main routine
var opt: optionlist;
begin
  fio_line (listfb, header);
  fio_skip (listfb);
  fio_tab (listfb, indentation);
  first := true;
  with prog_options do begin
    if semantic_options * [ minimum(checklist)..maximum(checklist) ] =
                          [ minimum(checklist)..maximum(checklist) ] then
      out ('CHECK')
    else if semantic_options * [ minimum(checklist)..maximum(checklist) ] = [] then
      out ('NOCHECK')
    else begin
      out ('CHECK(');
      first := true;
      for opt := minimum (checklist) to maximum (checklist) do begin
        if opt in semantic_options then
	  out (option_names [opt]);
      end;
      first := true;
      out (')');
      out ('NOCHECK(');
      first := true;
      for opt := minimum (checklist) to maximum (checklist) do begin
        if not (opt in semantic_options) then
	  option_names [opt]);
      end;
      first := true;
      out (')');
    end;
    if semantic_options * [ minimum(speciallist)..maximum(speciallist) ] =
                          [ minimum(speciallist)..maximum(speciallist) ] then
      out ('SPECIAL')
    else if semantic_options * [ minimum(speciallist)..maximum(speciallist) ] = [] then
      out ('NOSPECIAL')
    else begin
      out ('SPECIAL(');
      first := true;
      for opt := minimum (speciallist) to maximum (speciallist) do begin
        if opt in semantic_options then
	  out (option_names [opt]);
      end;
      first := true;
      out (')');
      out ('NOSPECIAL(');
      first := true;
      for opt := minimum (speciallist) to maximum (speciallist) do begin
        if not (opt in semantic_options) then
	  out (option_names [opt]);
      end;
      first := true;
      out (')');
    end;
    for opt := succ (maximum (speciallist)) to maximum (optionlist) do
      out (effective [opt in semantic_options] || option_names [opt]);
    out (effective[code_opt] || 'CODE');
    out (effective[debug_opt] || 'DEBUG');
    out (effective[finish_opt] || 'FINISH');
    out (effective[global_opt] || 'GLOBAL');
    out (effective[mainseg_opt] || 'MAINSEG');
    out (effective[overlay_opt] || 'OVERLAY');
    out (autoeffective[quick_opt] || 'QUICK');
    out (autoeffective[source_opt] || 'SOURCE');
    out (effective[standard_opt] || 'STANDARD');
    out ('LENGTH ('|| cv_int(page_length) || ')');
    out ('WIDTH (' || cv_int(page_width) || ')');
    out ('STORAGE (' || cv_int (storage) || ')');
    out ('ALLOC (' || cv_int (alloc_mode) || ')');
    print_switches (switches, 'ENABLE', true);
    print_switches (switches, 'DISABLE', false);
    print_switches (dump_switches, 'DUMP', true);
    print_switches (dump_switches, 'NODUMP', false);
  end;
  fio_skip (listfb);
end (* opt_header *) ;
$PAGE big letter printing
const
  char_width = 5;
  ch_desc_size = 35;                          (* = char_width * char_height *)
  hspace = 3;                                 (* horizontal space between characters *)
  max_chars = 12;                             (* max characters / row *)
type
  prt_chars = ' ' .. '_';                     (* ascii columns 2 - 5 *)
  row = string [max_chars];
  ch_desc = packed array [1..ch_desc_size] of char;
  reptype = array [prt_chars] of ch_desc;
const rep: reptype :=
        (  '                                   ',
           ' $$   $$   $$   $$        $$   $$  ',
           ' $ $  $ $                          ',
           '      $ $ $$$$$ $ $ $$$$$ $ $      ',
           '  $   $$$ $ $   $$$   $ $ $$$   $  ',
           '$$   $$  $   $   $   $   $  $$   $$',
           ' $   $ $  $ $   $  $$ $ $$  $  $$ $',
           '   $   $                           ',
           '    $   $   $    $    $     $     $',
           '$     $     $    $    $   $   $    ',
           '     $ $ $ $$$ $$$$$ $$$ $ $ $     ',
           '       $    $  $$$$$  $    $       ',
           '                $$   $$    $    $  ',
           '               $$$$$               ',
           '                          $$   $$  ',
           '         $   $   $   $   $         ',
           '  $$  $  $ $  $ $  $ $  $ $  $  $$ ',
           '  $   $$    $    $    $    $   $$$ ',
           ' $$$ $   $    $ $$$ $    $    $$$$$',
           ' $$$ $   $    $  $$     $$   $ $$$ ',
           '   $   $$  $ $ $  $ $$$$$   $    $ ',
           '$$$$$$    $$$$     $    $$   $ $$$ ',
           '  $$  $   $    $ $$ $$  $$   $ $$$ ',
           '$$$$$    $   $   $   $   $    $    ',
           ' $$$ $   $$   $ $$$ $   $$   $ $$$ ',
           ' $$$ $   $$  $$ $$ $    $   $  $$  ',
           '      $$   $$        $$   $$       ',
           ' $$   $$        $$   $$    $   $   ',
           '    $   $   $   $     $     $     $',
           '          $$$$$     $$$$$          ',
           '$     $     $     $   $   $   $    ',
           ' $$$ $   $   $   $    $         $  ',
           ' $$$ $   $    $ $$ $$ $ $$ $$  $$  ',
           ' $$$ $   $$   $$$$$$$   $$   $$   $',
           '$$$$  $  $ $  $ $$$  $  $ $  $$$$$ ',
           ' $$$ $   $$    $    $    $   $ $$$ ',
           '$$$$  $  $ $  $ $  $ $  $ $  $$$$$ ',
           '$$$$$$    $    $$$  $    $    $$$$$',
           '$$$$$$    $    $$$  $    $    $    ',
           ' $$$$$    $    $ $$$$   $$   $ $$$ ',
           '$   $$   $$   $$$$$$$   $$   $$   $',
           ' $$$   $    $    $    $    $   $$$ ',
           '    $    $    $    $    $$   $ $$$ ',
           '$   $$  $ $ $  $$   $ $  $  $ $   $',
           '$    $    $    $    $    $    $$$$$',
           '$   $$$ $$$ $ $$ $ $$   $$   $$   $',
           '$   $$$  $$ $ $$  $$$   $$   $$   $',
           ' $$$ $   $$   $$   $$   $$   $ $$$ ',
           '$$$$ $   $$   $$$$$ $    $    $    ',
           ' $$$ $   $$   $$   $$ $ $$  $  $$ $',
           '$$$$ $   $$   $$$$$ $ $  $  $ $   $',
           ' $$$ $   $$     $$$     $$   $ $$$ ',
           '$$$$$  $    $    $    $    $    $  ',
           '$   $$   $$   $$   $$   $$   $ $$$ ',
           '$   $$   $$   $ $ $  $ $   $    $  ',
           '$   $$   $$   $$ $ $$ $ $$ $ $ $ $ ',
           '$   $$   $ $ $   $   $ $ $   $$   $',
           '$   $$   $ $ $   $    $    $    $  ',
           '$$$$$    $   $   $   $   $    $$$$$',
           '  $$$  $    $    $    $    $    $$$',
           '     $     $     $     $     $     ',
           '$$$    $    $    $    $    $  $$$  ',
           '  $   $ $ $   $                    ',
           '                              $$$$$'   );
$PAGE print_row -- put out one row of characters
procedure print_row (r:row);
  var
    ind: 0 .. ch_desc_size;
    filler: line_index;
    len: 0 .. max_chars;
    i: 1 .. max_chars;
begin
  len := length(r);
  if len = 0 then return;
  filler := (listfb.width + hspace - (char_width+hspace)*len) div 2;
  ind := 0;
  while ind <> ch_desc_size do begin
    fio_tab (listfb, listfb.column + filler);
    for i := 1 to len do begin
      fio_write (listfb, substr (rep[r[i]],ind+1,char_width));
      if i <> len then fio_tab (listfb, listfb.column + hspace);
    end;
    fio_skip (listfb);
    ind := ind + char_width;
  end;
end;
$PAGE make_header
(* MAKE HEADER writes a big letter header to the listing file. *)
procedure make_header;
  var
    i: line_index;
    title_line: line_string;
begin
  title_line := 'File ' || file_list^.file_name || '   Compiled ' ||
                substr (dc_ext (root_block^.children^.comp_dtime), 1, 15) ||
                '   Pascal, Version ' || version ();
  for i := 1 to (listfb.width - length (title_line)) div 2 do
    title_line := ' ' || title_line;
  for i := 1 to 4 do begin
    fio_line (listfb, title_line);
    fio_skip (listfb);
  end;
  for i := 1 to 4 do fio_skip (listfb);
  print_row (mod_ident);
  fio_skip (listfb);
  fio_skip (listfb);
  fio_tab (listfb, max (1, (listfb.width - length (main_title)) div 2));
  fio_line (listfb, main_title);
  fio_skip (listfb);
  fio_skip (listfb);
  opt_header;
  for i := 1 to 4 do fio_skip (listfb);
  for i := 1 to 4 do begin
    fio_line (listfb, title_line);
    fio_skip (listfb);
  end;
  fio_page (listfb);
end;
$PAGE page_header
(* PAGE HEADER is called as a subroutine variable by the FIO package whenever
   a page eject is required.  It prints the title lines on the listing *)
procedure page_header (var fb: file_block);
  var
    idx: line_index;
    buffer: packed array[1..line_length] of char;
    pagestr: string[10];
begin
  with listfb do begin
    buffer [1:width] := 'MODULE';
    buffer [9:12] := mod_ident;
    idx := max (22, width - length (time_str) + 1);     (* get column in which to put time *)
    buffer[idx: length (time_str)] := time_str;
    buffer[22 : max (0, (idx-2)-22+1)] := global_title;
    fio_line (listfb, substr (buffer, 1, width));
    buffer [1:width] := 'SECTION';
    buffer [9:12] := cv_fp (cur_source);
    pagestr := 'PAGE ' || cv_int (pageno);
    idx := max (22, width - length (pagestr) + 1);
    buffer [idx: length (pagestr)] := pagestr;
    buffer [22: max (0, (idx-2)-22+1)] := page_title;
    fio_line (listfb, substr (buffer, 1, width));
    fio_skip (listfb);
  end;
end;
$PAGE make_listing
(* MAKE LISTING generates a source listing with interspersed errors. *)
procedure make_listing;
  label 100 (* fatal abort *) ;
  procedure abt_src_listing;                 (* fatal error handler *)
  begin
    detach;
    goto 100 (* fatal abort *) ;
  end;
  var listing_pass: co_routine;
begin
  listing_pass := create (read_listing_lines, 2000);
  if open_search (input, '.PAS ' || main_file) then ;
  abort := create (abt_src_listing, 10);
  listfb.page_header := page_header;
  listfb.c_column := 9; (* source line continuation, not truncation *)
  fio_page (listfb);                    (* print initial title *)
  call (listing_pass);
  while not end_of_file do begin
    if src_on then begin
      if (line[1] = '$') andif ln_enabled andif
         (length (line) >= 5) andif (substr (line, 2, 4) = 'PAGE') then
        fio_page (listfb)
      else begin
        fio_write (listfb, cvf_int (cur_source.line_no, 5));
        if not ln_enabled then fio_write (listfb, ' *');
        fio_tab (listfb, 9);
        fio_line (listfb, literal_line);
      end;
    end;
    with err_list^[err_file_list^[cur_source.file_no]]^.source do
      if (file_no = cur_source.file_no) and (page_no = cur_source.page_no)
            and (line_no = cur_source.line_no) then begin
	process_error (cur_source.file_no, true, src_on, literal_line);
	fio_skip (listfb);
      end;
    call (listing_pass);
  end (* while not end_of_file *);
100 (* fatal abort *) :
  dispose (listing_pass);
  dispose (abort);
  dump_errors (true);                   (* dump any remaining errors on listing *)
  with null_source do
    if (last_err_loc.file_no <> file_no) or (last_err_loc.page_no <> page_no) 
         or (last_err_loc.line_no <> line_no) then (* print the last error location *)
      fio_line (listfb, 'Last error at line ' || cv_source_id (last_err_loc));
  listfb.page_header := fio_nop;      (* don't leave bad proc lying around *)
  listfb.c_column := 0; (* back to normal *)
end;
$PAGE make_file_xref
(* MAKE FILE XREF prints a listing of all files comprising the source program. *)
procedure make_file_xref;
  procedure file_xref_header (var fb: file_block);
  begin
    with fb do
    fio_line (fb, 'File   level   on page     name');
    fio_skip (fb);
  end;
  var f: source_ptr;
begin
  listfb.page_header := file_xref_header;
  fio_page (listfb);
  f := file_list;
  while f <> nil do begin
    with f^ do begin
      fio_write (listfb, cvf_int (file_no, 3));
      fio_tab (listfb, 10);
      fio_write (listfb, cvf_int (incl_level, 1));
      fio_tab (listfb, 17);
      fio_write (listfb, cvf_int (incl_page, 5));
      fio_tab (listfb, 25);
      fio_line (listfb, file_name);
      f := next_file;
    end;
  end (* while *) ;
  listfb.page_header := fio_nop;
end;
$PAGE make_page_xref
(* MAKE PAGE XREF prints an alphabetized list of page titles, cross referenced
   with the page on which they appear. *)
procedure make_page_xref;
  var root: page_ptr;                   (* base of unbalanced binary tree used to alphabetize the titles *)
  procedure enter_page (pt: page_ptr);  (* adds page to tree *)
    var cp, lp: page_ptr;
  begin
    if root = nil
      then root := pt
      else begin
        cp := root;
        while cp <> nil do begin
          lp := cp;
          if uppercase (pt^.subtitle) < uppercase (cp^.subtitle)
            then cp := cp^.left_page
            else cp := cp^.right_page
        end;
        if uppercase (pt^.subtitle) < uppercase (lp^.subtitle)
          then lp^.left_page := pt
          else lp^.right_page := pt;
      end;
  end;
  procedure print_page (pt: page_ptr);         (* traverse tree listing pages *)
  begin
    if pt <> nil then begin
      with pt^ do begin
        print_page (left_page);
        fio_write (listfb, cvf_int (page_number, 6));
        fio_tab (listfb, 13);
        fio_write (listfb, cvf_int (in_file^.file_no, 3));
        fio_tab (listfb, 22);
        fio_write (listfb, cvf_int (incl_page, 5));
        fio_tab (listfb, 31);
        fio_line (listfb, subtitle);
        print_page (right_page);
      end;
    end;
  end;
  procedure page_xref_header (var fb: file_block);       (* writes out xref title *)
  begin
    fio_line (fb, 'Section   in file   on page     title');
    fio_skip (fb);
  end;
  var p: page_ptr;
      i: 0..4;
begin
  root := nil;                          (* enter pages in tree *)
  p := file_list^.pages;                (* 0-0/0 *)
  while p <> nil do begin
    if p^.subtitle <> ''
      then enter_page (p);
    p := p^.following_page;
  end;
  if root <> nil then begin             (* if there are pages with titles, print xref *)
    listfb.page_header := page_xref_header;
    if (listfb.plength <> 0) andif ((listfb.lineno + 8) <= listfb.plength)
      then begin        (* attempt to put list on same page as file list *)
        for i:= 1 to 3 do fio_skip (listfb);
        page_xref_header (listfb);
      end
      else fio_page (listfb);
    print_page (root);
    listfb.page_header := fio_nop;
  end;
end;
$PAGE list_error_lines
(* LIST ERROR LINES displays error messages with the text of the lines on which
   they occured on the terminal. *)
procedure list_error_lines;
  label 100 (* fatal abort *) ;
  procedure abt_err_listing;                 (* fatal error handler *)
  begin
    detach;
    goto 100 (* fatal abort *) ;
  end;
  var listing_pass: co_routine;
      err: err_ptr;
      to_list_file: boolean;
begin
  listing_pass := create (read_listing_lines, 4000);
  if open_search (input, '.PAS ' || main_file) then ;
  abort := create (abt_err_listing, 10);
  to_list_file := prog_options.errors_opt and (list_file <> '');
  call (listing_pass);
  while not end_of_file do begin
    with err_list^[err_file_list^[cur_source.file_no]]^.source do
      if (file_no = cur_source.file_no) and (page_no = cur_source.page_no)
           and (line_no = cur_source.line_no) then
	process_error (cur_source.file_no, to_list_file, false, literal_line);
    call (listing_pass);
  end;
100 (* fatal abort *) :
  dispose (listing_pass);
  dispose (abort);
  dump_errors (to_list_file);                   (* dump any remaining errors on listing *)
end;
$PAGE pass3 - main program
var
  start_time: integer;
  count: string[10];
  next: packed array [1..6] of char;
  segstuff: segrecd;
  errors: integer;
  temp_file: text;
begin
  start_time := runtime;
  if not rdpas(tempname ('PAS'), true) then begin
    rewrite (tty);
    writeln (tty, '?Compiler temporary file PAS lost');
    stop;
  end;
  num_files := no_files;                (* remember number read 1st pass *)
  rewrite (tty);
  fio_attach (ttyfb, ttyoutput);
  ttyfb.width := 80;
  ttyfb.c_column := 9;
  sort_error_log;
  last_page := nil;                     (* for process error *)
  (*  Initialize variables required by listing pass.  *)
  if root_block^.children <> nil then begin
    with root_block^.children^ do begin
      time_str := substr (dc_ext (comp_dtime), 1, 15);
      if id <> nil
	then mod_ident := id^.text
        else mod_ident := '??????';
    end;
  end
  else if env_name <> nil then begin (* must be an environment compilation *)
    time_str := substr (dc_ext (env_dtime), 1, 15);
    mod_ident := env_name^.text;
  end
  else begin (* who knows? *)
    time_str := substr (dc_ext (daytime ()), 1, 15);
    mod_ident := '??????';
  end;
  if opts_listing or prog_options.errors_opt then begin
    fio_open (listfb, '.LST ' || list_file);
    if iostatus (listfb.file_var) = io_ok then begin
      listfb.width := prog_options.page_width;    (* set up listing parameters *)
      listfb.plength := prog_options.page_length;
      global_title := main_title;
      lf_status := now_open;
      if prog_options.banner_opt and opts_listing then begin
        make_header;
        make_header;
      end;
      listfb.pageno := 1;         (* reset counter *)
    end
    else begin (* Bad listing file. *)
      writeln (tty, '?Unable to open listing file ', list_file);
      list_file := '';
    end;
  end;
  (*  If there are any errors, then construct the error summary line.  *)
  if err_count = 0 then
    err_cnt_line := ''
  else begin
    if not prog_options.finish_opt then
      warnings := 0;
    errors := err_count - warnings;
    if errors = 0 then
      err_cnt_line := ''
    else if errors = 1 then
      err_cnt_line := 'One error'
    else
      err_cnt_line := cv_int (errors) || ' errors';
    if warnings = 0 then
      (* no message *)
    else if warnings = 1 then begin
      if errors = 0
        then err_cnt_line := 'One warning'
        else err_cnt_line := err_cnt_line || ', one warning';
    end
    else begin
      if errors <> 0 then
        err_cnt_line := err_cnt_line || ', ';
      err_cnt_line := err_cnt_line || cv_int (warnings) || ' warnings';
    end;
  end;
  last_err_loc := null_source;
  if src_selected and (list_file <> '') then begin
    make_listing;
    make_file_xref;
    make_page_xref;
  end
  else if err_count <> 0 then begin
    if prog_options.terse_opt
      then dump_errors (prog_options.errors_opt and (list_file <> ''))
      else list_error_lines;
  end;
  if ([symbols_opt, xref_opt, calls_opt] * all_opts <> []) and (list_file <> '') then
    xr_sym_call;
  if lf_status = now_open then begin
    fio_close (listfb);
    lf_status := prev_opened;
  end;
  if err_count <> 0 then begin
    writeln (tty);
    if prog_options.finish_opt and (max_severity = 1)
      then writeln (tty, '% ', err_cnt_line)
      else writeln (tty, '? ', err_cnt_line);
  end;
  reset (temp_file, tempname ('XRF'));
  scratch (temp_file);
  reset (temp_file, tempname ('ERR'));
  scratch (temp_file);
  if prog_options.statistics_opt then begin
    seginfo (segstuff);
    writeln (tty, '[Pass 3: ', (runtime - start_time) / 1000.0:8:3, ' seconds, ',
                          (segstuff.lowlen+511) div 512: 3, '+',
                          (segstuff.highlen+511) div 512: 3, 'P]');
  end;
  if finish and prog_options.code_opt then begin
    if quick
      then next := tmprefix || 'CCG'
      else next := tmprefix || 'OCG';
    IF NOT wrpas(tempname ('PAS')) THEN STOP;
  end
  else begin
    next := 'PASCAL';
    log_write;
  end;
  if runoff <> 0 then begin
    IF NOT runprg(next || prgm_dir (), 1) THEN BEGIN
      rewrite (tty);
      writeln (tty, '?Unable to run ', next)
    END
  end;
end.
    r@9Ì