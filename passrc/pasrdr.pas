$WIDTH=100
$LENGTH=55
$TITLE PASRDR.PAS, last modified 3/21/84, zw
MODULE pasrdr OPTIONS SPECIAL(WORD);
(*TYM-Pascal compiler -- source file reader / directive processor*)

$PAGE system modules
$SYSTEM UTLTYP.TYP
$SYSTEM UTLKEY.INC
$SYSTEM UTLIO.INC
$SYSTEM UTLSRC.INC
$SYSTEM UTLRDR.INC
$SYSTEM UTLSW.INC
$SYSTEM UTLLST.INC
$SYSTEM PASCAL.INC
$SYSTEM PASERR.INC

$INCLUDE PASRDR.TYP

$PAGE declarations, reader_error
EXTERNAL VAR (*declared public in PASENV.PAS*)
maittl: title_string; (*first title which appears in input*)
glbttl: title_string; (*current title*)
pagttl: title_string; (*currently applicable $PAGE title*)
slncnt: positive_integer; (*source line counter*)
ilncnt: positive_integer; (*include line counter*)
srclst: source_pointer; (*start of list of files referenced*)
paglst: page_pointer; (*start of list of pages referenced*)
filnum: source_range; (*number of current file*)
lstpos: source_position; (*last source position*)
srcpos: source_position; (*current source position*)
srclin: source_line_string; (*uppercased input line + 2 blanks*)
litlin: source_line_string; (*input line as read*)
cmplok: yes_no; (*source line can be compiled?*)
listok: yes_no; (*source line can be listed?*)
srceof: yes_no; (*last source line been read?*)
srcsel: yes_no; (*source on somewhere in program?*)
explst: yes_no; (*list file name from ",list=source"*)

TYPE file_kind = (main_file, include_file, system_file, header_file);
VAR
local_switches: switch_pointer; (*mark any switches local to current program*)
listing: yes_no; (*in listing pass?*)
initial_directives: yes_no; (*processing initial directives?*)
system_count: positive_integer; (*counts nesting of system files*)
if_count: positive_integer; (*counts nesting of multi-line IF directives*)
line_too_long: yes_no; (*was source line just read too long?*)
end_if: yes_no; (*ENDIF directive processed?*)
current_source: source_pointer; (*points to current source file record*)
current_page: page_pointer; (*points to current page record*)

PUBLIC PROCEDURE rdsrc; FORWARD;

PROCEDURE reader_error(source_cursor: source_line_cursor; code: error_code);
(*report reader error on source line*)
BEGIN
  (*only report errors on the first pass, not the listing pass*)
  IF NOT listing THEN errprt(code, srcpos, '', source_cursor)
END;

$PAGE new_file, new_page
FUNCTION new_file
  (number: source_range; include_level: positive_integer;
   physical_page: positive_integer): source_pointer;
(*Return a pointer to the source record for the current source INPUT file
  specified by number.  If the record does not exist, one is created and
  added to the source file list (list head is "srclst").  In any case, the
  field "physical_page" is updated in the source record.*)
VAR name: file_name; current: source_pointer;
BEGIN
  (*Search for an existing source record in the source file list.*)
  current := srclst; new_file := NIL;
  WHILE (current <> NIL) AND (new_file = NIL) DO BEGIN
    IF current^.number = number THEN new_file := current
    ELSE current := current^.next_source
  END;
  (*If the record was not found then create a new one.*)
  IF new_file = NIL THEN BEGIN
    name := FILENAME(INPUT); NEW(new_file, LENGTH(name));
    new_file^.number := number;
    new_file^.include_level := include_level;
    new_file^.next_source := srclst;
    new_file^.page_list := NIL;
    new_file^.previous_source := NIL;
    new_file^.name := name;
    srclst := new_file
  END;
  (*Update the "physical_page".*)
  new_file^.physical_page := physical_page
END;

FUNCTION new_page
  (number: source_page_range;
   subtitle: title_string; physical_page: positive_integer): page_pointer;
(*Return a pointer to the page record in the specified source for the page
  specified by number.  If such a record does not exist, one is created.
  In any case, the physical page number is updated.*)
VAR current: page_pointer;
BEGIN
  (*Search for the page record in the source file's page list.*)
  current := current_source^.page_list; new_page := NIL;
  WHILE (current <> NIL) AND (new_page = NIL) DO BEGIN
    IF current^.number = number THEN new_page := current
    ELSE current := current^.next_in_file
  END;
  (*If the record was not found then create a new one.*)
  IF new_page = NIL THEN BEGIN
    NEW(new_page, LENGTH(subtitle));
    new_page^.number := number;
    new_page^.source_file := current_source;
    new_page^.next_in_file := current_source^.page_list;
    new_page^.next_in_list := paglst;
    new_page^.left_page := NIL; new_page^.right_page := NIL;
    new_page^.subtitle := subtitle;
    current_source^.page_list := new_page;
    paglst := new_page
  END;
  (*Update the "physical_page".*)
  new_page^.physical_page := physical_page
END;

$PAGE pop_file, push_file
PROCEDURE pop_file;
(*Close the current source INPUT file and pop the file stack to get the
  previous source INPUT file*)
BEGIN
  (*Pop the actual INPUT file.  Reopen the previous one.*)
  assume((current_source <> NIL) ANDIF popin, '? Can not "POP" source file.');
  (*Adjust system file count if current file was a system file.*)
  IF current_source^.is_system THEN system_count := system_count - 1;
  (*Ok to list if out of outer system file.*)
  IF current_source^.is_system AND (system_count = 0) THEN listok := yes;
  (*Return to previous source file record*)
  current_source := current_source^.previous_source;
  IF current_source <> NIL THEN ilncnt := current_source^.include_line_count
  ELSE ilncnt := 0
END;

PROCEDURE push_file
  (source_cursor: source_line_cursor; name: file_name; kind: file_kind);
(*Push the current source file onto the stack the prepare the specified
  source file to be the current source INPUT file*)
VAR previous_source: source_pointer; next_number: positive_integer;
BEGIN
  IF NOT pushin(name) THEN reader_error(source_cursor, err1)
  ELSE IF (kind = header_file) AND NOT listing
  THEN assume(popin, '? Can not pop file just pushed in "push_file".')
  ELSE BEGIN (*The new file is open for input.*)
    (*Make current source into previous source, save include line counter.*)
    previous_source := current_source;
    IF previous_source <> NIL
    THEN previous_source^.include_line_count := ilncnt;
    (*determine next source file number*)
    IF kind = main_file THEN next_number := 0
    ELSE next_number := previous_source^.number + 1;
    (*Establish new source file to be current source file.*)
    current_source := new_file(next_number, current_source^.include_level + 1,
      lstfcb.page_number);
    current_source^.previous_source := previous_source;
    (*Keep count of system file nesting depth.*)
    current_source^.is_system := (kind = system_file);
    IF current_source^.is_system THEN system_count := system_count + 1
    (*Not ok to list if in system file.*)
    IF current_source^.is_system THEN listok := no
  END
END;

$PAGE read_line
FUNCTION read_line(VAR line: generic_string): yes_no;
(*Try to read the next line from a source file.  Set blank line if failure.*)
BEGIN
  (*try to get next literal line*)
  LOOP
    EXIT IF rdlin(litlin);
    (*If at the end of the current source file then pop any previous file.*)
    pop_file;
    (*If there is not a previous file, signal end of source file.*)
    EXIT IF current_source = NIL DO srceof := TRUE
  END;
  (*Return success if not at the end of the source file.*)
  read_line := NOT srceof;
  (*If successful then prepare line for processing else set a blank line.*)
  IF read_line THEN BEGIN
    lstpos := srcpos;
    WITH srcpos DO BEGIN
      source_number := current_source^.number;
      page_number := current_page^.number;
      line_number := line_number + 1;
    END;
    slncnt := slncnt + 1;
    IF current_source^.include_level > 0 THEN ilncnt := ilncnt + 1
  END
  ELSE litlin := '';
  line_too_long := LENGTH(litlin) > (UPPERBOUND(srclin) - 2);
  IF NOT line_too_long THEN srclin := UPPERCASE(litlin) || '  '
  ELSE srclin := UPPERCASE(SUBSTR(litlin, 1, UPPERBOUND(srclin) - 2));
  line := SUBSTR(litlin, 1, MIN(UPPERBOUND(line), LENGTH(litlin)))
END;

$PAGE scan_spaces
PROCEDURE scan_spaces(VAR source_cursor: source_line_cursor);
(*Scan any spaces or comments from the source line.*)
VAR next_comment, error_cursor, comment_level: positive_integer;
BEGIN
  (*Loop until no more spaces or comments can be found.*)
  LOOP
    (*Advance past spaces and non-printing characters.*)
    WHILE (source_cursor <= LENGTH(srclin)) ANDIF
      (srclin[source_cursor] <= ' ') DO source_cursor := source_cursor + 1;
    (*Exit loop if at the end of the source line.*)
    EXIT IF source_cursor >= LENGTH(srclin);
    (*Exit loop if there is no comment.*)
    EXIT IF (srclin[source_cursor] <> '(') ORIF
      (srclin[source_cursor + 1] <> '*');
    (*Prepare to scan a comment.*)
    comment_level := 1; (*counts nesting depth*)
    error_cursor := source_cursor; (*points to beginning of comment*)
    (*Scan comments until there are no more.*)
    REPEAT
      (*Advance to next nested comment, if any.*)
      next_comment := SEARCH(SUBSTR(srclin, source_cursor), ['(', '*']);
      source_cursor := source_cursor + next_comment - 1;
      IF next_comment = 0 THEN BEGIN (*Error if un-closed comment.*)
        reader_error(error_cursor, err12);
	comment_level := 0
      END
      ELSE IF (srclin[source_cursor] = '(') ANDIF
        (srclin[source_cursor + 1] = '*') THEN BEGIN (*Open comment.*)
	comment_level := comment_level + 1;
	source_cursor := source_cursor + 2
      END
      ELSE IF (srclin[source_cursor] = '*') ANDIF
        (srclin[source_cursor + 1] = ')') THEN BEGIN (*Close comment.*)
	comment_level := comment_level - 1;
	source_cursor := source_cursor + 2
      END
      ELSE source_cursor := source_cursor + 1; (*Skip single "(" or "*".*)
    UNTIL comment_level = 0
  END
END;

$PAGE scan_paren, scan_file, scan_switch, scan_number, scan_comma, scan_eol
FUNCTION scan_paren(VAR source_cursor: source_line_cursor): yes_no;
(*Try to scan a left paren (or ':' or '=') and return yes if ')' was scanned.*)
VAR key: STRING[1];
BEGIN
  scan_spaces(source_cursor);
  scan_paren := scnkey(srclin, source_cursor, ['(', ':', '='], key);
  IF scan_paren THEN scan_paren := (key = '(')
END;

FUNCTION scan_file(VAR source_cursor: source_line_cursor): file_name;
(*Scan a file name from the current source line.*)
BEGIN
  scan_spaces(source_cursor);
  IF NOT scnkey(srclin, source_cursor, ['A'..'Z', '0'..'9', '.'], scan_file)
  THEN reader_error(source_cursor, err2)
END;

FUNCTION scan_switch(VAR source_cursor: source_line_cursor): switch_name;
(*Scan a switch name from the current source line.*)
BEGIN
  scan_spaces(source_cursor);
  IF NOT scnkey(srclin, source_cursor, ['A'..'Z', '0'..'9', '_'], scan_switch)
  THEN reader_error(source_cursor, err3)
END;

FUNCTION scan_number(VAR source_cursor: source_line_cursor): positive_integer;
(*Scan a number from the current source line.*)
VAR digit_string: STRING[11];
BEGIN
  scan_spaces(source_cursor);
  IF scnkey(srclin, source_cursor, ['0' .. '9'], digit_string)
  THEN reader_error(source_cursor, err13)
  ELSE GETSTRING(digit_string, scan_number);
END;

FUNCTION scan_comma(VAR source_cursor: source_line_cursor): yes_no;
(*Try to scan a comma from the current source line.*)
VAR comma: STRING[1];
BEGIN
  scan_spaces(source_cursor);
  scan_comma := scnkey(srclin, source_cursor, [','], comma)
END;

FUNCTION scan_eol(VAR source_cursor: source_line_cursor): yes_no;
(*Try to scan end of the current source line.*)
BEGIN
 scan_eol := source_cursor > LENGTH(srclin)
END;

$PAGE check_paren, munch_directive, skip_source_lines
PROCEDURE check_paren(VAR source_cursor: source_line_cursor);
(*Check that a right paren is scanned.*)
VAR paren: STRING[1];
BEGIN
  IF NOT scnkey(srclin, source_cursor, [')'], paren)
  THEN reader_error(source_cursor, err4)
END;

PROCEDURE munch_directive(VAR source_cursor: source_line_cursor);
(*Munch directive line after it has been processed then read the next line.*)
BEGIN
  IF NOT scan_eol(source_cursor)
  THEN reader_error(source_cursor, err5);
  rdsrc
END;

PROCEDURE skip_source_lines(VAR source_cursor: source_line_cursor);
(*Skip any source lines as directed by IF directive.*)
BEGIN
  IF NOT listing THEN BEGIN (*Don't skip anything if listing.*)
    IF NOT scan_eol(source_cursor) THEN BEGIN (*controls remainder of line*)
      IF NOT cmplok THEN rdsrc (*fails -- skip remainder of line*)
      ELSE BEGIN (*succeeds -- pass remainder of line*)
        srclin := SUBSTR(srclin, source_cursor);
        litlin := SUBSTR(litlin, source_cursor);
        source_cursor := 1
      END
    END
    ELSE BEGIN (*controls multiple lines*)
      if_count := if_count + 1; (*the ENDIF will decrement*)
      IF cmplok THEN rdsrc (*succeeds -- pass lines following*)
      ELSE BEGIN (*fails -- skip lines until corresponding ENDIF*)
        end_if := no;
        REPEAT rdsrc UNTIL end_if OR srceof;
        end_if := no
      END
    END
  END
END;

$PAGE process_if, process_enable
PROCEDURE process_if
  (VAR source_cursor: source_line_cursor;
   kind: (if_all, if_any, if_none, if_not));
(*Process IF directives: all, any, or none.*)
VAR switch_count: ARRAY[yes_no] OF positive_integer; paren, value: yes_no;
BEGIN
  switch_count := (0, 0);
  paren := scan_paren(source_cursor);
  REPEAT
    value := sw(glbopts.switches, scan_switch(source_cursor));
    switch_count[value] := switch_count[value] + 1;
  UNTIL NOT paren ORIF NOT scan_comma(source_cursor);
  IF paren THEN check_paren(source_cursor);
  IF switch_count[yes] + switch_count[no] = 0
  THEN reader_error(source_cursor, err6);
  CASE kind OF
    if_all: cmplok := switch_count[no] = 0;
    if_any: cmplok := switch_count[yes] > 0;
    if_none: cmplok := switch_count[yes] = 0;
    if_not: cmplok := switch_count[no] > 0
  END;
  skip_source_lines(source_cursor)
END;

PROCEDURE process_enable(VAR source_cursor: source_line_cursor; value: yes_no);
(*Process ENABLE/DISABLE directive.*)
VAR paren: yes_no; name: switch_name;
BEGIN
  paren := scan_paren(source_cursor);
  REPEAT
    name := scan_switch(source_cursor);
    glbopts.switches := enasw(glbopts.switches, name, value);
  UNTIL NOT paren ORIF NOT scan_comma(source_cursor);
  IF paren THEN check_paren(source_cursor);
  munch_directive(source_cursor)
END;

$PAGE do_if, do_ifnot, do_ifany, do_ifnone, do_endif
PROCEDURE do_if(VAR source_cursor: source_line_cursor);
(*Handle IF directive.*)
BEGIN
  process_if(source_cursor, if_all)
END;

PROCEDURE do_ifnot(VAR source_cursor: source_line_cursor);
(*Handle IFNOT directive.*)
BEGIN
  process_if(source_cursor, if_not)
END;

PROCEDURE do_ifany(VAR source_cursor: source_line_cursor);
(*Handle IFANY directive.*)
BEGIN
  process_if(source_cursor, if_any)
END;

PROCEDURE do_ifnone(VAR source_cursor: source_line_cursor);
(*Handle IFNONE directive.*)
BEGIN
  process_if(source_cursor, if_none)
END;

PROCEDURE do_endif(VAR source_cursor: source_line_cursor);
(*Handle ENDIF directive.*)
BEGIN
  IF if_count > 0 THEN BEGIN
    end_if := yes;
    if_count := if_count - 1
  END
  ELSE reader_error(source_cursor, err7);
  munch_directive(source_cursor)
END;

$PAGE do_disable, do_enable, do_header, do_system, do_include
PROCEDURE do_disable(VAR source_cursor: source_line_cursor);
(*Handle DISABLE directive.*)
BEGIN
  process_enable(source_cursor, no)
END;

PROCEDURE do_enable(VAR source_cursor: source_line_cursor);
(*Handle ENABLE directive.*)
BEGIN
  process_enable(source_cursor, yes)
END;

PROCEDURE do_header(VAR source_cursor: source_line_cursor);
(*Handle HEADER directive.*)
VAR name: file_name;
BEGIN
  name := scan_file(source_cursor);
  push_file(source_cursor, name, header_file);
  munch_directive(source_cursor)
END;

PROCEDURE do_system(VAR source_cursor: source_line_cursor);
(*Handle SYSTEM directive.*)
VAR name: file_name;
BEGIN
  name := scan_file(source_cursor);
  IF cmplok THEN push_file(source_cursor, name, system_file);
  munch_directive(source_cursor)
END;

PROCEDURE do_include(VAR source_cursor: source_line_cursor);
(*Handle INCLUDE directive.*)
VAR name: file_name;
BEGIN
  name := scan_file(source_cursor);
  IF cmplok THEN push_file(source_cursor, name, include_file);
  munch_directive(source_cursor)
END;

$PAGE do_length, do_width
PROCEDURE do_length(VAR source_cursor: source_line_cursor);
(*Handle LENGTH directive.*)
VAR paren: yes_no; value: positive_integer;
BEGIN
  paren := scan_paren(source_cursor);
  value := scan_number(source_cursor);
  IF paren THEN check_paren(source_cursor);
  IF NOT (value IN [4 .. 255]) THEN reader_error(source_cursor, err14)
  ELSE BEGIN
    lstfcb.page_length := value;
    IF initial_directives THEN glbopts.page_length := lstfcb.page_length
  END;
  munch_directive(source_cursor)
END;

PROCEDURE do_width(VAR source_cursor: source_line_cursor);
(*Handle WIDTH directive.*)
VAR paren: yes_no; value: positive_integer;
BEGIN
  paren := scan_paren(source_cursor);
  value := scan_number(source_cursor);
  IF paren THEN check_paren(source_cursor);
  IF NOT (value IN [20 .. 255]) THEN reader_error(source_cursor, err15)
  ELSE BEGIN
    lstfcb.page_width := value;
    IF initial_directives THEN glbopts.page_width := lstfcb.page_width
  END;
  munch_directive(source_cursor)
END;

$PAGE do_title, do_page
PROCEDURE do_title(VAR source_cursor: source_line_cursor);
(*Handle TITLE directive.*)
BEGIN
  scan_spaces(source_cursor);
  IF scan_eol(source_cursor) THEN reader_error(source_cursor, err8)
  ELSE BEGIN
    glbttl := SUBSTR(litlin, source_cursor,
      MIN(UPPERBOUND(glbttl), LENGTH(litlin) - source_cursor));
    source_cursor := LENGTH(litlin) + 1;
    IF initial_directives THEN maittl := glbttl
  END;
  munch_directive(source_cursor)
END;

PROCEDURE do_page(VAR source_cursor: source_line_cursor);
(*Handle PAGE directive.*)
BEGIN
  IF NOT scan_eol(source_cursor) THEN BEGIN
    pagttl := SUBSTR(litlin, source_cursor,
      MIN(UPPERBOUND(pagtLENGTH(litlin) - source_cursor));
    source_cursor := LENGTH(litlin) + 1;
    WITH srcpos DO BEGIN
      page_number := page_number + 1;
      line_number := 1
    END;
    current_page := new_page(srcpos.page_number, pagttl,
      lstfcb.page_number)
  END;
  IF listing THEN lstpag(lstfcb);
  munch_directive(source_cursor)
END;

$PAGE do_source, do_nosource, do_autosource
PROCEDURE do_source(VAR source_cursor: source_line_cursor);
(*Handle SOURCE directive.*)
BEGIN
  (*list lines, source has been selected*)
  IF system_count > 0 THEN reader_error(source_cursor, err9);
  listok := yes;
  IF NOT initial_directives THEN srcsel := srcsel OR listok
  ELSE BEGIN
    srcsel := listok;
    glbopts.source_option := option_is_on
  END;
  munch_directive(source_cursor)
END;

PROCEDURE do_nosource(VAR source_cursor: source_line_cursor);
(*Handle NOSOURCE directive.*)
BEGIN
  (*no list lines, source may have been selected*)
  listok := no;
  IF NOT initial_directives THEN srcsel := srcsel OR listok
  ELSE BEGIN
    srcsel := listok;
    glbopts.source_option := option_is_off
  END;
  munch_directive(source_cursor)
END;

PROCEDURE do_autosource(VAR source_cursor: source_line_cursor);
(*Handle AUTOSOURCE directive.*)
BEGIN
  (*list if explicit, source may have been selected*)
  listok := lstexp;
  IF NOT initial_directives THEN srcsel := srcsel OR listok
  ELSE BEGIN
    srcsel := listok;
    glbopts.source_option := option_is_auto
  END;
  munch_directive(source_cursor)
END;

$PAGE error_not_key, error_not_directive, directives
PROCEDURE error_not_key(VAR source_cursor: source_line_cursor);
(*Handle reader error -- not a key.*)
BEGIN
  reader_error(source_cursor, err10)
END;

PROCEDURE error_not_directive(VAR source_cursor: source_line_cursor);
(*Handle reader error -- not a directive.*)
BEGIN
  reader_error(source_cursor, err1)
END;

FUNCTION directives: ARRAY[1 .. 17] OF reader_record;
(*Return list of TYM-Pascal reader directives.*)
BEGIN
  directives[1] := ('IF', 2, do_if);
  directives[2] := ('IFANY', 3, do_ifany);
  directives[3] := ('IFNONE', 5, do_ifnone);
  directives[4] := ('IFNOT', 5, do_ifnot);
  directives[5] := ('ENDIF', 3, do_endif);
  directives[6] := ('AUTOSOURCE', 5, do_autosource);
  directives[7] := ('DISABLE', 3, do_disable);
  directives[8] := ('ENABLE', 2, do_enable);
  directives[9] := ('HEADER', 4, do_header);
  directives[10] := ('INCLUDE', 3, do_include);
  directives[11] := ('LENGTH', 3, do_length);
  directives[12] := ('NOSOURCE', 3, do_nosource);
  directives[13] := ('PAGE', 4, do_page);
  directives[14] := ('SOURCE', 1, do_source);
  directives[15] := ('SYSTEM', 3, do_system);
  directives[16] := ('TITLE', 5, do_title);
  directives[17] := ('WIDTH', 3, do_width)
END;

$PAGE prepare_reader
PROCEDURE prepare_reader(name: file_name);
(*Make preparations common to source and listing readers.*)
BEGIN
  listing := no;
  initial_directives := yes;
  cmplok := yes;
  srceof := no;
  CASE glbopts.source_option OF
    option_is_on: listok := yes;
    option_is_auto: listok := lstexp;
    option_is_off: listok := no
  END;
  maittl := '';
  glbttl := '';
  pagttl := '';
  if_count := 0;
  system_count := 0;
  local_switches := glbopts.switches;
  lstpos := last_source_position;
  srcpos := null_source_position;
  current_source := NIL;
  current_page := NIL;
  bgnrdr(read_line, error_not_key, error_not_directive, directives);
  slncnt := 0; (*zero source line counter*)
  ilncnt := 0; (*zero include line counter*)
  push_file(1, name, main_file);
  current_page := new_page(0, glbttl, 1)
END;

$PAGE rdysrc, rdylst, rdsrc
PUBLIC PROCEDURE rdysrc(name: file_name);
(*Make preparations to read source lines from file.*)
BEGIN
  srclst := NIL;
  paglst := NIL;
  prepare_reader(name);
  srcsel := listok;
  maittl := FILENAME(INPUT);
  glbttl := maittl;
  pagttl := maittl;
  lstfcb.page_length := glbopts.page_length;
  lstfcb.page_width := glbopts.page_width
END;

PUBLIC PROCEDURE rdylst(name: file_name);
(*Make preparations to read listing lines from file.*)
BEGIN
  prepare_reader(name);
  listing := yes;
  initial_directives := no
END;

PUBLIC PROCEDURE rdsrc;
(*Read next source/listing line, if any.*)
BEGIN
  IF NOT srceof THEN BEGIN
    srcrdr;
    IF line_too_long THEN reader_error(1, err17);
    initial_directives := no;
    IF srceof THEN BEGIN
      IF if_count > 0 THEN reader_error(1, err16);
      endrdr;
      srclin := '  ';
      litlin := '';
      popsw(glbopts.switches, local_switches)
    END
  END
END.
x*R-"