$WIDTH=100
$LENGTH=55
$TITLE PASS3.PAS, last modified 1/9/84, zw
PROGRAM pass3 OPTIONS STORAGE(4000), SPECIAL(WORD);
(*TYM-Pascal compiler pass 3 -- error reporting and listing generation*)

$PAGE system modules
$SYSTEM PASCAL.INC
$SYSTEM PTMCON.INC
$SYSTEM PASIST.INC
$SYSTEM PASFIL.INC
$SYSTEM PASLOG.INC
$SYSTEM COROUT.INC
$SYSTEM UTLPDR.INC
$SYSTEM UTLSW.INC
$SYSTEM DTIME.INC
$SYSTEM INFPAC.INC
$SYSTEM UTLOPN.INC
$SYSTEM PASRDR.INC
$SYSTEM PASPT.TYP
$SYSTEM PASERR.INC
$SYSTEM PASCV.INC
$SYSTEM PASOPD.INC

$PAGE definitions

EXTERNAL PROCEDURE xr_sym_call; (*where defined?*)
EXTERNAL VAR
auto_run: 0 .. 2; (* 0 = manual, 1 or 2 = automatic *)

PUBLIC VAR
mod_ident: STRING[12]; (* module identifier *)
time_str: STRING[24]; (* date/time reading *)

VAR
num_files: file_range; (* number of files in compilation *)
ttyfb: file_block; (* for formatting errors on the tty *)

$PAGE error declarations

STATIC VAR last_location: source_id;
CONST terminal_width: line_index := 72;

TYPE  err_headers = array[1..3] of string[15];
CONST error_type: err_headers := ('Warning ','Error ','Fatal error ');

TYPE
err_ptr = ^err_record;
err_record = PACKED RECORD
  code: err_codes;
  column: line_index;
  source: source_id;
  number: INTEGER; (* sequential error number *)
  extra_text: PACKED ARRAY [1..*] OF CHAR
END;
err_array = ARRAY[1..*] OF err_ptr;
err_file_array = ARRAY[0..*] OF INTEGER;

STATIC VAR
err_list: ^err_array;
err_file_list: ^err_file_array; (* indices into err_list *)
err_log_file: FILE OF *;
message_file: TEXT;
last_err_loc: source_id;
err_cnt_line: STRING[30];

$PAGE get_next_error
FUNCTION get_next_error: err_ptr;
(*read the next error record from the log file and create an
  error record for it.  A pointer to the record is returned.*)
VAR ix: INTEGER; tcod: err_codes; tsrc: source_id; tcol, tlen: line_index;
BEGIN
  ASSERT(NOT EOF(err_log_file)); READ(err_log_file, ix);
  IF ix = CURSOR(err_log_file) THEN BEGIN
    READ(err_log_file, tcod, tsrc, tcol, tlen);
    WITH tsrc DO
      IF (file_no = null_source.file_no) AND (page_no = null_source.page_no)
	AND (line_no = null_source.line_no)
      THEN file_no := num_files + 1;
    NEW(get_next_error, tlen);
    WITH get_next_error^ DO BEGIN
      code := tcod; column := tcol; source := tsrc; number := 0;
      IF tlen <> 0 THEN	READ(err_log_file, extra_text: SIZE(extra_text, tlen))
    END
  END
  ELSE BEGIN (*??*)
    NEW(get_next_error, 0);
    WITH get_next_error^ DO BEGIN
      code := err_file_error; column := 0;
      source := (num_files + 1, 0, 0); number := 0
    END
  END
END;

$PAGE err_less

FUNCTION err_less(a, b: err_ptr): BOOLEAN;
(*a helper algorithm which is used in sorting the error list.
   Error A is less than error B if A is in an earlier file than B, or on an
   earlier page of the same file, or on an earlier line of the same page, or
   at an earlier column of the same line.  If two errors occurred at the same
   location, the one which was detected first is less than the other. *)
BEGIN
  IF a^.source.file_no < b^.source.file_no THEN err_less := TRUE
  ELSE IF a^.source.file_no > b^.source.file_no THEN err_less := FALSE
  ELSE IF a^.source.page_no < b^.source.page_no THEN err_less := TRUE
  ELSE IF a^.source.page_no > b^.source.page_no THEN err_less := FALSE
  ELSE IF a^.source.line_no < b^.source.line_no THEN err_less := TRUE
  ELSE IF a^.source.line_no > b^.source.line_no THEN err_less := FALSE
  ELSE IF a^.column < b^.column THEN err_less := TRUE
  ELSE IF a^.column > b^.column THEN err_less := FALSE
  ELSE err_less := (a^.number < b^.number)
END;

$PAGE err_adjust

PROCEDURE err_adjust(VAR list: err_array; i, n: INTEGER);
(*the heart of the heapsort algorithm, which is used to sort
  the error list.  It is called with the error list, List, an index, I, and
  a size, N.  We assume that List[2*I] and List[2*I+1] are heap roots, and
  combine their heaps with List[I] so that List[I] becomes a heap root.  If
  I = 1, then this results in the entire list being made into a heap.
  We say that List[J] is a heap root if J > [N/2], or if List[2*J] and
  List[2*J+1] are heap roots, and List[J] > List[2*j], and List[J] >
  List[2*J+1].
  The heap algorithms used here are taken from Fundamentals of Computer
  Algorithms, by Horowitz and Sahni. *)
VAR j: INTEGER; item: err_ptr;
BEGIN
  j := 2 * i; item := list[i];
  WHILE j <= n DO BEGIN
    IF (j < n) AND err_less(list[j], list[j+1]) (*compare left/right children*)
    THEN j := j + 1; (*j points to the larger child*)
    EXIT IF NOT err_less(item, list[j]); (*place found for item*)
    list[j div 2] := list[j]; (*move the larger child up a level*)
    j := 2 * j
  END;
  list[j div 2] := item
 END;

$PAGE sort_error_log

PROCEDURE sort_error_log;
(*read the error log file and creates the err_list array containing
  a sorted list of errors, and a list giving the first error in each file.*)
VAR ix, last_ix: INTEGER; fix: file_range; t: err_ptr;
BEGIN
  NEW(err_list, err_count + 1); (*create error info list*)
  NEW(err_file_list, num_files + 1);
  FOR fix := 0 TO num_files + 1 DO err_file_list^[fix] := 0;
  IF err_count > 0 THEN BEGIN (*if any thing to read*)
    (*read the error file and build the error list*)
    RESET(err_log_file, err_tmp); ASSERT(NOT EOF(err_log_file));
    FOR ix := 1 TO err_count DO BEGIN
      err_list^[ix] := get_next_error(); err_list^[ix]^.number := ix
    END;
    CLOSE(err_log_file)
  END;
  NEW(err_list^[err_count+1], 0);
  err_list^[err_count+1]^.source.file_no := MAXIMUM(file_range);
  (*sort the error list, using the heapsort algorithm*)
  FOR ix := (err_count div 2) DOWNTO 1 DO err_adjust(err_list^, ix, err_count);
  FOR ix := err_count DOWNTO 2 DO BEGIN
    t := err_list^[1]; err_list^[1] := err_list^[ix];
    err_list^[ix] := t; err_adjust(err_list^, 1, ix - 1);
    err_file_list^[err_list^[ix]^.source.file_no] := ix
  END;
  IF err_count <> 0
  THEN err_file_list^[err_list^[1]^.source.file_no] := 1;
  last_ix := err_count + 1;
  FOR fix := num_files + 1 DOWNTO 0 DO BEGIN
    IF err_file_list^[fix] = 0
    THEN err_file_list^[fix] := last_ix
    ELSE last_ix := err_file_list^[fix]
  END
END;

$PAGE process_error

VAR last_page: page_ptr := NIL; (*page_id of page containing last error*)

PROCEDURE process_error(
  err_file: file_range; (*the file number of the error*)
  err_on_list: BOOLEAN; (*if true => display error on listing*)
  src_listed: BOOLEAN; (*if true => source line just printed on listing*)
  line_text: line_string); (*text of line where error occurs if available*)
(*display all errors on the same source line as the next error
  in a file.  The error index is updated to the first error which is not on
  the same line.*)
  FUNCTION get_page(source: source_id): page_ptr;
  (*fetches page_id record for page denoted by the file and page of a source
    id record*)
  BEGIN
    get_page := last_page; (*search from last page to end first*)
    WHILE get_page <> NIL DO BEGIN
      IF (get_page^.page_number = source.page_no) AND
         (get_page^.in_file^.file_no = source.file_no)
      THEN RETURN; (*exit if found*)
      get_page := get_page^.following_page
    END;
    get_page := file_list^.pages; (*page 0 of file 0*)
    WHILE get_page <> NIL DO BEGIN (*search from start until found *)
      IF (get_page^.page_number = source.page_no) AND
         (get_page^.in_file^.file_no = source.file_no)
      THEN RETURN; (*exit when found*)
      get_page := get_page^.following_page
    END
  END;

$PAGE print_id, print_text - in process_error

  PROCEDURE print_id(VAR fb: file_block; page: page_ptr);
  (*prints file and page identification*)
  BEGIN
    fio_skip(fb); ASSERT(page <> NIL);
    WITH page^ DO BEGIN
      ASSERT(in_file <> NIL);
      IF ((last_page = NIL) ORIF (last_page^.in_file <> in_file)) AND
        (in_file^.file_no <> 0)
      THEN WITH in_file^
      DO fio_line(fb, 'File ' || cv_int(file_no) || ':  ' || file_name);
      IF (last_page <> page) AND (page_number <> 0)
      THEN fio_line(fb, 'Section ' || cv_int(page_number) || ':  ' || subtitle)
    END
  END;
  PROCEDURE print_text(VAR fb: file_block; err: err_ptr);
  (*prints the text of a line with its line number*)
  BEGIN
    ASSERT(err <> NIL);
    fio_skip(fb); fio_write(fb, cvf_int(err^.source.line_no, 5));
    fio_tab(fb, 9); fio_line(fb, line_text)
  END;

$PAGE errout, errskip - in process_error

  PROCEDURE ERROUT(str: line_string);
  (*print a string on the terminal and listing (if enabled).  The output is
    automatically filled so that output width restrictions not overflowed.*)
  BEGIN
    IF err_on_list THEN BEGIN
      IF (listfb.column + length (str)) > listfb.width THEN BEGIN
        fio_skip(listfb); fio_tab(listfb, listfb.c_column)
      END;
      fio_write(listfb, str)
    END;
    IF (ttyfb.column + length (str)) > ttyfb.width THEN BEGIN
      fio_skip(ttyfb); fio_tab(ttyfb, ttyfb.c_column)
    END;
    fio_write(ttyfb, str)
  END;
  PROCEDURE errskip;
  (*skip to a new line on the tty and listing (if enabled)*)
  BEGIN
    IF err_on_list THEN fio_skip(listfb);
    fio_skip(ttyfb)
  END;

$PAGE print_message - in process_error

  PROCEDURE print_message(err: err_ptr);
  VAR word: line_string; error_number: user_range;
  BEGIN
    RESET(message_file, pdr(err_txt));
    IF IOSTATUS <> IO_OK THEN BEGIN
      REWRITE(TTY);
      WRITELN(TTY, '? error message file not found');
      STOP
    END;
    LOOP (*search for start of message*)
      REPEAT READLN(message_file)
      UNTIL EOF(message_file) ORIF NOT EOLN(message_file);
      EXIT IF EOF(message_file);
      READ(message_file, error_number);
      EXIT IF error_number = err_table[err^.code].user_number;
      REPEAT READLN(message_file)
      UNTIL EOF(message_file) ORIF EOLN(message_file);
      EXIT IF EOF(message_file)
    END;
    IF EOF(message_file) THEN BEGIN
      REWRITE(TTY);
      WRITELN(TTY, '? error message not found, #', error_number: 0);
      STOP
    END;
    listfb.c_column := listfb.column;
    ttyfb.c_column := ttyfb.column;
    WHILE NOT EOLN(message_file) DO BEGIN (*tokenize, output parts of message*)
      LOOP
        WHILE (NOT EOLN(message_file)) AND (message_file^ = ' ')
        DO GET(message_file); (*skip blanks*)
        EXIT IF EOLN(message_file);
        word := ''; (*have non blank, get rest of token*)
        REPEAT
          IF message_file^ = '$'
          THEN word := word || err^.extra_text (*substitute for $*)
          ELSE word := word || message_file^;
          GET(message_file)
        UNTIL message_file^ = ' ';
        errout(word || ' ')
      END;
      READLN(message_file)
    END;
    listfb.c_column := 9;
    ttyfb.c_column := 9;
    CLOSE(message_file)
  END;

$PAGE process_error - main routine

VAR page: page_ptr; err_index, ix: INTEGER; err: err_ptr;
  err_source: source_id; tcol: line_index; err_level: level_range;
BEGIN
  ASSERT(err_file_list <> NIL); err_index := err_file_list^[err_file];
  ASSERT(err_list <> NIL); err := err_list^[err_index];
  ASSERT(err <> NIL); err_source := err^.source;
  (*output file/page identification if not previously printed*)
  IF err^.source.file_no = num_files + 1
  THEN page := get_page(null_source)
  ELSE page := get_page(err_source);
  IF page <> last_page THEN BEGIN
    print_id(ttyfb, page);
    IF err_on_list AND NOT src_listed THEN print_id(listfb, page);
    last_page := page (*to suppress output next time around*)
  END;
  (*display text of line on tty (and listing) with markers*)
  IF (NOT prog_options.terse_opt) AND (line_text <> '') AND
     (err_source.file_no <> num_files + 1) 
  THEN BEGIN
    print_text(ttyfb, err);
    IF err_on_list AND NOT src_listed THEN print_text(listfb, err);
    ix := err_index; tcol := 0; (*print the markers*)
    WHILE (err^.source.file_no = err_source.file_no) AND
      (err^.source.page_no = err_source.page_no) AND
      (err^.source.line_no = err_source.line_no)
    DO BEGIN
      IF err^.column > tcol THEN BEGIN
	tcol := err^.column; fio_tab(ttyfb, tcol + 8);
	fio_write(ttyfb, '^');
	IF err_on_list THEN BEGIN
	  fio_tab(listfb, tcol + 8);
	  fio_write(listfb, '^')
	END
      END;
      ix := ix + 1; err := err_list^[ix]; ASSERT(err <> NIL)
    END;
    errskip
  END
  ELSE BEGIN (*don't display text*)
    ix := err_index;
    WITH err_source DO BEGIN
      ASSERT(err_list^[ix] <> NIL);
      WHILE (err_list^[ix]^.source.file_no = file_no) AND
            (err_list^[ix]^.source.page_no = page_no) AND
            (err_list^[ix]^.source.line_no = line_no)
      DO BEGIN ix := ix + 1; ASSERT(err_list^[ix] <> NIL) END
    END
  END;
  (*display the messages for all errors on the line and increment error index*)
  err_level := 0;
  WHILE err_index <> ix DO BEGIN
    err := err_list^[err_index]; ASSERT(err <> NIL);
    err_level := MAX(err_level, err_table[err^.code].level);
    WITH err_table[err^.code] DO BEGIN
      errout(error_type[level]); errout(cv_int(user_number))
    END;
    IF (prog_options.terse_opt or (line_text = '')) AND
       (err_source.file_no <> num_files + 1)
    THEN errout(' (at line ' || cv_source_id(err^.source) || ')');
    IF NOT prog_options.terse_opt THEN BEGIN
      errout(': '); print_message(err)
    END
    ELSE IF err^.extra_text <> ''
      THEN errout('  "' || err^.extra_text || '"');
    errskip;
    BREAK(TTYOUTPUT); (*force the message to the tty*)
    err_index := err_index + 1
  END; (*while err_index <> ix*)
  WITH null_source DO IF ((last_err_loc.file_no <> file_no) OR
    (last_err_loc.page_no <> page_no) OR
    (last_err_loc.line_no <> line_no)) AND
    err_on_list AND src_selected
  THEN fio_line(listfb, 'last error at line ' || cv_source_id(last_err_loc));
  last_err_loc := err_source;
  err_file_list^[err_file] := err_index;
  IF err_level >= 3 THEN abort (*fatal error, die now*)
END;

$PAGE dump_errors

PROCEDURE dump_errors(err_on_list: BOOLEAN);
(* display all errors remaining in the error list. If "err_on_list"
   is true, the error messages are also written to the listing.  This is used
   to display errors without source - i.e., miscellaneouse errors at the end of
   the listing, errors left after a reader abort, or errors to be displayed in
   terse + nosource mode.  The error records are also deleted. *)
VAR ef: file_range; ix: INTEGER;
BEGIN
  FOR ef := 0 TO num_files + 1 DO BEGIN
    WHILE err_list^[err_file_list^[ef]]^.source.file_no = ef DO
      process_error(ef, err_on_list, false, '')
  END;
  FOR ix := 1 TO err_count + 1 DO DISPOSE(err_list^[ix]);
  DISPOSE(err_list); DISPOSE(err_file_list);
  IF err_on_list THEN BEGIN
    fio_skip(listfb);
    fio_line(listfb, err_cnt_line)
  END
END;

$PAGE opt_header

PUBLIC PROCEDURE opt_header;
(*print the "options in effect" on the header page*)
VAR first: BOOLEAN; (*signals whether comma separator required*)
CONST
  header = 'Options in effect: ';
  indentation = 9;
  effective: ARRAY [BOOLEAN] OF STRING[2] = ('NO','');
  separator: ARRAY [BOOLEAN] OF STRING[2] = (', ','');
  autoeffective: ARRAY [option_mode] OF STRING[4] = ('NO', 'AUTO', '');
  option_names: array [optionlist] of string [10] =
    ('ASSERTIONS', 'CASES', 'COMPATIBIL', 'FIELDS', 'FILES', 'INPUT',
     'POINTERS', 'STRINGS', 'SUBSCRIPTS', 'VALUES', 'COERCIONS', 'PTR',
     'WORD', 'MAP', 'SYMBOLS', 'CALLS', 'ASSEMBLY', 'XREF', 'TRACE',
     'QBLOCKS', 'OPTIMIZE');

$PAGE out - in opt_header

  PROCEDURE out(str: parm_string);
  (*output a string to the listing file insuring that it does not overflow
    maximum width of the listing line.  On overflow, a new line is started.*)
  BEGIN
    fio_write(listfb, separator[first]);
    IF (listfb.column + length (str) + 2 (* separator *)) > listfb.width
    THEN BEGIN
      fio_skip(listfb); fio_tab(listfb, indentation)
    END;
    fio_write(listfb, str); first := false
  END;

$PAGE print_switches - in opt_header

  PROCEDURE print_switches(head: sw_ptr; title: parm_string; pass: BOOLEAN);
  (*format and output a list of switch names*)
  VAR swtch: sw_ptr; first_switch: BOOLEAN;
  BEGIN
    first_switch := TRUE; swtch := head;
    WHILE swtch <> NIL DO WITH swtch^ DO BEGIN
      IF ena = pass THEN BEGIN
	IF first_switch THEN BEGIN
	  out(title || '('); first := TRUE; (*no separator after '('*)
	  first_switch := FALSE
	END;
	out(nam)
      END;
      swtch := nxt
    END;
    IF NOT first_switch THEN BEGIN (*if any printed*)
      first := TRUE; out(')') (*no separator before ')'*)
    END
  END;

$PAGE opt_header - main routine

VAR opt: optionlist;
BEGIN
  fio_line(listfb, header); fio_skip(listfb); fio_tab(listfb, indentation);
  first := true;
  WITH prog_options DO BEGIN
    IF (semantic_options * [MINIMUM(checklist)..MAXIMUM(checklist)]) =
      [MINIMUM(checklist)..MAXIMUM(checklist)] THEN out('CHECK')
    ELSE IF (semantic_options * [MINIMUM(checklist)..MAXIMUM(checklist)]) = []
      THEN out('NOCHECK')
    ELSE BEGIN
      out('CHECK('); first := TRUE;
      FOR opt := MINIMUM(checklist) TO MAXIMUM(checklist)
      DO IF opt IN semantic_options THEN out(option_names[opt]);
      first := TRUE; out(')');
      out('NOCHECK('); first := TRUE;
      FOR opt := MINIMUM(checklist) TO MAXIMUM(checklist)
      DO IF NOT (opt IN semantic_options) THEN out(option_names[opt]);
      first := TRUE; out(')')
    END;
    IF (semantic_options * [MINIMUM(speciallist)..MAXIMUM(speciallist)]) =
      [MINIMUM(speciallist)..MAXIMUM(speciallist)] THEN out('SPECIAL')
    ELSE IF (semantic_options * [MINIMUM(speciallist)..MAXIMUM(speciallist)]) =
      [] THEN out('NOSPECIAL')
    ELSE BEGIN
      out('SPECIAL('); first := TRUE;
      FOR opt := MINIMUM(speciallist) TO MAXIMUM(speciallist) 
      DO IF opt IN semantic_options THEN out(option_names[opt]);
      first := TRUE; out(')');
      out('NOSPECIAL('); first := TRUE;
      FOR opt := MINIMUM(speciallist) TO MAXIMUM(speciallist) 
      DO IF NOT (opt mantic_options) THEN out(option_names[opt]);
      first := TRUE; out(')')
    END;
    FOR opt := SUCC (MAXIMUM(speciallist)) TO MAXIMUM(optionlist)
    DO out(effective[opt IN semantic_options] || option_names [opt]);
    out(effective[code_opt] || 'CODE');
    out(effective[debug_opt] || 'DEBUG');
    out(effective[finish_opt] || 'FINISH');
    out(effective[global_opt] || 'GLOBAL');
    out(effective[mainseg_opt] || 'MAINSEG');
    out(effective[overlay_opt] || 'OVERLAY');
    out(autoeffective[quick_opt] || 'QUICK');
    out(autoeffective[source_opt] || 'SOURCE');
    out(effective[standard_opt] || 'STANDARD');
    out('LENGTH (' || cv_int(page_length) || ')');
    out('WIDTH (' || cv_int(page_width) || ')');
    out('STORAGE (' || cv_int (storage) || ')');
    out('ALLOC (' || cv_int (alloc_mode) || ')');
    print_switches(switches, 'ENABLE', true);
    print_switches(switches, 'DISABLE', false);
    print_switches(dump_switches, 'DUMP', true);
    print_switches(dump_switches, 'NODUMP', false)
  END;
  fio_skip(listfb)
END;

$PAGE big letter printing

CONST
  char_width = 5;
  ch_desc_size = 35; (*char_width * char_height*)
  hspace = 3; (*horizontal space between characters*)
  max_chars = 12; (*max characters / row*)
TYPE
  prt_chars = ' ' .. '_'; (*ascii columns 2 - 5*)
  row = STRING[max_chars];
  ch_desc = PACKED ARRAY [1..ch_desc_size] OF CHAR;
  reptype = ARRAY [prt_chars] OF ch_desc;
CONST rep: reptype :=
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

$PAGE print_row

PROCEDURE print_row(r: row);
(*print one row of characters*)
VAR ind: 0 .. ch_desc_size; filler: line_index;
  len: 0 .. max_chars; i: 1 .. max_chars;
BEGIN
  len := LENGTH(r);
  IF len = 0 THEN RETURN;
  filler := (listfb.width + hspace - (char_width + hspace) * len) div 2;
  ind := 0;
  WHILE ind <> ch_desc_size DO BEGIN
    fio_tab(listfb, listfb.column + filler);
    FOR i := 1 TO len DO BEGIN
      fio_write(listfb, SUBSTR(rep[r[i]], ind + 1, char_width));
      IF i <> len THEN fio_tab(listfb, listfb.column + hspace)
    END;
    fio_skip(listfb);
    ind := ind + char_width
  END
END;

$PAGE make_header

PROCEDURE make_header;
(*write a big letter header to the listing file*)
VAR i: line_index; title_line: line_string;
BEGIN
  title_line := 'File ' || file_list^.file_name || '   Compiled ' ||
    SUBSTR(dc_ext(root_block^.children^.comp_dtime), 1, 15) || '   ' ||
    version;
  FOR i := 1 TO (listfb.width - LENGTH(title_line)) DIV 2
  DO title_line := ' ' || title_line;
  FOR i := 1 TO 4 DO BEGIN
    fio_line(listfb, title_line); fio_skip(listfb)
  END;
  FOR i := 1 TO 4 DO fio_skip(listfb);
  print_row(mod_ident);
  fio_skip(listfb); fio_skip(listfb);
  fio_tab(listfb, MAX(1, (listfb.width - LENGTH(main_title)) DIV 2));
  fio_line(listfb, main_title);
  fio_skip(listfb); fio_skip(listfb);
  opt_header;
  FOR i := 1 TO 4 DO fio_skip(listfb);
  FOR i := 1 TO 4 DO BEGIN
    fio_line(listfb, title_line); fio_skip(listfb)
  END;
  fio_page(listfb)
END;

$PAGE page_header

PROCEDURE page_header(VAR fb: file_block);
(*called as a subroutine variable by the FIO package whenever
  a page eject is required.  It prints the title lines on the listing.*)
VAR idx: line_index;
    buffer: PACKED ARRAY[1..line_length] OF CHAR;
    pagestr: STRING[10];
BEGIN
  WITH listfb DO BEGIN
    buffer[1 : width] := 'MODULE';
    buffer[9 : 12] := mod_ident;
    idx := MAX(22, width - LENGTH(time_str) + 1); (*get column for time*)
    buffer[idx : LENGTH(time_str)] := time_str;
    buffer[22 : MAX(0, (idx - 2) - 22 + 1)] := global_title;
    fio_line(listfb, SUBSTR(buffer, 1, width));
    buffer[1 : width] := 'SECTION';
    buffer[9 : 12] := cv_fp(cur_source);
    pagestr := 'PAGE ' || cv_int(pageno);
    idx := MAX(22, width - LENGTH(pagestr) + 1);
    buffer[idx : LENGTH(pagestr)] := pagestr;
    buffer[22 : MAX(0, (idx - 2) - 22 + 1)] := page_title;
    fio_line(listfb, SUBSTR(buffer, 1, width));
    fio_skip(listfb)
  END
END;

$PAGE make_listing

PROCEDURE make_listing;
(*generates a source listing with interspersed errors. *)
LABEL 100; (*fatal abort*)
VAR abt_rdy: BOOLEAN; (*will abort only if TRUE*)
PROCEDURE abt_src_listing;
BEGIN IF abt_rdy THEN GOTO 100 ELSE abt_rdy := TRUE END;
VAR listing_pass: environment;
BEGIN
  listing_pass := create(read_listing_lines, 2000);
  IF opnsrc(prog_options.search_list, INPUT, '.PAS ' || main_file) THEN;
  abort := abt_src_listing; abt_rdy := FALSE;
  listfb.page_header := page_header;
  listfb.c_column := 9; (*source line continuation, not truncation*)
  fio_page(listfb); (*print initial title*)
  call(listing_pass);
  WHILE NOT end_of_file DO BEGIN
    IF src_on THEN BEGIN
      IF (line[1] = '$') ANDIF ln_enabled ANDIF
         (LENGTH(line) >= 5) ANDIF (substr (line, 2, 4) = 'PAGE')
      THEN fio_page(listfb)
      ELSE BEGIN
        fio_write(listfb, cvf_int(cur_source.line_no, 5));
        IF NOT ln_enabled THEN fio_write(listfb, ' *');
        fio_tab(listfb, 9); fio_line(listfb, literal_line)
      END
    END;
    WITH err_list^[err_file_list^[cur_source.file_no]]^.source
    DO IF (file_no = cur_source.file_no) AND (page_no = cur_source.page_no)
      AND (line_no = cur_source.line_no)
    THEN BEGIN
      process_error(cur_source.file_no, TRUE, src_on, literal_line);
      fio_skip(listfb)
    END;
    call(listing_pass);
  END; (*while not end_of_file*)
  100: (*fatal abort*)
  DISPOSE(listing_pass);
  dump_errors(TRUE); (*dump any remaining errors on listing*)
  WITH null_source
  DO IF (last_err_loc.file_no <> file_no) OR (last_err_loc.page_no <> page_no) 
    OR (last_err_loc.line_no <> line_no)
  THEN fio_line(listfb, 'last error at line ' || cv_source_id (last_err_loc));
  listfb.page_header := fio_nop; (*don't leave bad proc lying around*)
  listfb.c_column := 0 (*back to normal*)
END;

$PAGE make_file_xref

PROCEDURE make_file_xref;
(*print a listing of all files comprising the source program*)
  PROCEDURE file_xref_header(VAR fb: file_block);
  BEGIN
    fio_line(fb, 'File   level   on page     name'); fio_skip(fb)
  END;
VAR f: source_ptr;
BEGIN
  listfb.page_header := file_xref_header;
  fio_page(listfb);
  f := file_list;
  WHILE f <> NIL DO WITH f^ DO BEGIN
    fio_write(listfb, cvf_int(file_no, 3));
    fio_tab(listfb, 10);
    fio_write(listfb, cvf_int(incl_level, 1));
    fio_tab(listfb, 17);
    fio_write(listfb, cvf_int(incl_page, 5));
    fio_tab(listfb, 25);
    fio_line(listfb, file_name);
    f := next_file;
  END;
  listfb.page_header := fio_nop
END;

$PAGE make_page_xref

PROCEDURE make_page_xref;
(*print an alphabetized list of page titles, cross referenced
  with the page on which they appear*)
VAR root: page_ptr; (*base of unbalanced binary tree, alphabetizes the titles*)
  PROCEDURE enter_page(pt: page_ptr); (*adds page to tree*)
  VAR cp, lp: page_ptr;
  BEGIN
    IF root = NIL THEN root := pt
    ELSE BEGIN
      cp := root;
      WHILE cp <> NIL DO BEGIN
        lp := cp;
        IF UPPERCASE(pt^.subtitle) < UPPERCASE(cp^.subtitle)
	THEN cp := cp^.left_page
        ELSE cp := cp^.right_page
      END;
      IF UPPERCASE(pt^.subtitle) < UPPERCASE(lp^.subtitle)
      THEN lp^.left_page := pt
      ELSE lp^.right_page := pt
    END
  END;
  PROCEDURE print_page(pt: page_ptr); (*traverse tree listing pages*)
  BEGIN
    IF pt <> NIL THEN WITH pt^ DO BEGIN
      print_page(left_page);
      fio_write(listfb, cvf_int(page_number, 6));
      fio_tab(listfb, 13);
      fio_write(listfb, cvf_int(in_file^.file_no, 3));
      fio_tab(listfb, 22);
      fio_write(listfb, cvf_int(incl_page, 5));
      fio_tab(listfb, 31);
      fio_line(listfb, subtitle);
      print_page(right_page)
    END
  END;
  PROCEDURE page_xref_header(VAR fb: file_block);
  BEGIN
    fio_line(fb, 'Section   in file   on page     title');
    fio_skip(fb)
  END;
VAR p: page_ptr; i: 0..4;
BEGIN
  root := NIL; (*enter pages in tree*)
  ASSERT(file_list <> NIL);
  p := file_list^.pages; (*0-0/0*)
  WHILE p <> NIL DO BEGIN
    IF p^.subtitle <> '' THEN enter_page(p);
    p := p^.following_page
  END;
  IF root <> NIL THEN BEGIN (*if there are pages with titles, print xref*)
    listfb.page_header := page_xref_header;
    IF (listfb.plength <> 0) ANDIF ((listfb.lineno + 8) <= listfb.plength)
    THEN BEGIN (*attempt to put list on same page as file list*)
      for i:= 1 to 3 do fio_skip(listfb); page_xref_header(listfb)
    END
    ELSE fio_page(listfb);
    print_page(root);
    listfb.page_header := fio_nop
  END
END;

$PAGE list_error_lines

PROCEDURE list_error_lines;
(*display error messages with the text of the lines on which
   they ocured on the terminal*)
LABEL 100; (*fatal abort*)
VAR abt_rdy: BOOLEAN; (*will abort only if TRUE*)
PROCEDURE abt_err_listing;
BEGIN IF abt_rdy THEN GOTO 100 ELSE abt_rdy := TRUE END;
VAR listing_pass: environment; err: err_ptr; to_list_file: BOOLEAN;
BEGIN
  listing_pass := create(read_listing_lines, 4000);
  IF opnsrc(prog_options.search_list, INPUT, '.PAS ' || main_file) THEN;
  abort := abt_err_listing; abt_rdy := FALSE;
  to_list_file := prog_options.errors_opt AND (list_file <> '');
  call(listing_pass);
  WHILE NOT end_of_file DO BEGIN
    WITH err_list^[err_file_list^[cur_source.file_no]]^.source
    DO IF (file_no = cur_source.file_no) AND (page_no = cur_source.page_no)
      AND (line_no = cur_source.line_no)
    THEN process_error(cur_source.file_no, to_list_file, false, literal_line);
    call (listing_pass)
  END;
  100: (*fatal abort*)
  DISPOSE(listing_pass);
  dump_errors(to_list_file) (*dump any remaining errors on listing*)
END;

$PAGE pass3 - main program

VAR start_time: INTEGER; count: STRING[10]; next: PACKED ARRAY [1..6] OF CHAR;
  segstuff: segrecd; errors: INTEGER; temp_file: TEXT;

BEGIN
  start_time := RUNTIME;
  unchain;
  num_files := no_files; (*remember number read 1st pass*)
  REWRITE(TTY);
  fio_attach(ttyfb, TTYOUTPUT);
  ttyfb.width := 80; ttyfb.c_column := 9;
  sort_error_log;
  last_page := NIL; (*for process error*)
  (*initialize variables required by listing pass*)
  ASSERT(root_block <> NIL);
  IF root_block^.children <> NIL THEN WITH root_block^.children^ DO BEGIN
    time_str := SUBSTR(dc_ext(comp_dtime), 1, 15);
    IF id <> NIL THEN mod_ident := id^.text ELSE mod_ident := '??????'
  END
  ELSE IF env_name <> NIL THEN BEGIN (*must be an environment compilation*)
    time_str := SUBSTR(dc_ext(env_dtime), 1, 15); mod_ident := env_name^.text
  END
  ELSE BEGIN (*unknown*)
    time_str := SUBSTR(dc_ext(daytime()), 1, 15); mod_ident := '??????'
  END;
  IF opts_listing OR prog_options.errors_opt THEN BEGIN
    fio_open(listfb, '.LST ' || list_file);
    IF IOSTATUS(listfb.file_var) = IO_OK THEN BEGIN
      lf_status := now_open;
      listfb.width := prog_options.page_width; (*set up listing parameters*)
      listfb.plength := prog_options.page_length;
      global_title := main_title;
      IF prog_options.banner_opt AND opts_listing THEN BEGIN
        make_header; make_header
      END;
      listfb.pageno := 1 (*reset page counter after making header*)
    END
    ELSE BEGIN
      ttymsg('?unable to open listing file: ' || list_file);
      lf_status := unopened; list_file := ''
    END
  END;
  (*if there are any errors, then construct the error summary line*)
  IF err_count = 0 THEN err_cnt_line := ''
  ELSE BEGIN
    IF NOT prog_options.finish_opt THEN warnings := 0;
    errors := err_count - warnings;
    IF errors = 0 THEN err_cnt_line := ''
    ELSE IF errors = 1 THEN err_cnt_line := 'one error'
    ELSE err_cnt_line := cv_int(errors) || ' errors';
    IF warnings = 0 THEN (*no message*)
    ELSE IF warnings = 1 THEN BEGIN
      IF errors = 0 THEN err_cnt_line := 'one warning'
      ELSE err_cnt_line := err_cnt_line || ', one warning'
    END
    ELSE BEGIN
      IF errors <> 0 THEN err_cnt_line := err_cnt_line || ', ';
      err_cnt_line := err_cnt_line || cv_int(warnings) || ' warnings'
    END
  END;
  last_err_loc := null_source;
  IF src_selected AND (list_file <> '') THEN BEGIN
    make_listing; make_file_xref; make_page_xref
  END
  ELSE IF err_count <> 0 THEN BEGIN
    IF prog_options.terse_opt
    THEN dump_errors(prog_options.errors_opt AND (list_file <> ''))
    ELSE list_error_lines
  END;
  IF (([symbols_opt, xref_opt, calls_opt] * all_opts) <> []) AND
    (list_file <> '') THEN xr_sym_call;
  IF lf_status = now_open THEN BEGIN
    fio_close(listfb); lf_status := prev_opened
  END;
  IF err_count <> 0 THEN BEGIN
    WRITELN(TTY);
    IF prog_options.finish_opt AND (max_severity = 1)
    THEN WRITELN(TTY, '% ', err_cnt_line)
    ELSE WRITELN(TTY, '? ', err_cnt_line)
  END;
  RESET(temp_file, xrf_tmp); SCRATCH(temp_file);
  RESET(temp_file, err_tmp); SCRATCH(temp_file);
  IF prog_options.statistics_opt THEN BEGIN
    seginfo(segstuff);
    WRITELN(TTY, '[Pass 3: ',
      (runtime - start_time) / 1000.0:8:3, ' seconds, ',
      (segstuff.lowlen + 511) DIV 512: 3, '+',
      (segstuff.highlen + 511) DIV 512: 3, 'P]')
  END;
  IF finish AND prog_options.code_opt THEN BEGIN
    IF quick THEN next := tmprefix || 'CCG'
    ELSE next := tmprefix || 'OCG'
  END
  ELSE next := 'PASCAL';
  chain(next)
END.
    ] 
1	