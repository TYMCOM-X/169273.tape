$TITLE FMT.PAS, lst modified 4/30/84, zw
PROGRAM fmt;
(* Program to format PASCAL source programs for ease of reading and editing.
   Commands of the following format are accepted:

	[ <output file> = ]  [ <input file> ]  [ / <options> ]

   The <input file> is the title of the pascal source program. An extension
   of '.PAS' is assumed.

  The <output file> gives components of the name of the file to which the
  formatted output is to be written.  For example, if '.LPT' is specified,
  the output file will be 'DSK:name.LPT[]' where name is the default
  output name (the input file name);  if [n,m] is specified, the output
  file will be placed in the specified directory; etc.  Note that if 'LPT:'
  is specified for the output file, the output is written to a temporary
  file which is QUEUEd for printing at the end of processing.  Multiple
  files written to 'LPT:' are put in the same print request.

   The <options> are described below.  Any of the options may be omitted,
   and the defaults will be chosen from:

      /LIST,UPPERCASE,NODECAP,COLUMN=49,LENGTH=45,NOXREF,NOAPPEND,NOHEAD

   If an <input file> is omitted, the list of options given becomes the default
   for the rest of the job.  This applies to the <output file> as well.  For
   example, if '.LST' is specified, all output will be placed in '.LST' files.

   The mode options specify what is to be done with the formatted output.
   If LIST is specified, a line numbered listing file with headers is
   created.  The text of the program is formatted in such a listing.  If
   EDIT or NEW is specified, text is simply formatted, that is left in a
   compilable form.  Unless overriden by the <output file> specification,
   LIST implies '.LST'; EDIT, '.PAS' (that is, it overwrites the original
   input file); and NEW, '.TMP'.

   When /LIST is selected, a form-feed is inserted every n lines if the
   if the <length> option is also selected, LENGTH=n, and n is nonzero.
   The <case> specifies the capitalization of all text not in comments,
   literal strings, or directive lines.  Nested comments are handled, and
   so are conditional comments; i.e., text within a comment containing $X or $Y
   is formatted as though it were ordinary text.  The <case>s are: UPPERCASE,
   LOWERCASE, and PASS (to suppress the mapping altogether).

   Comments in the right-hand margin are aligned in a column specfied in the
   <column>. If such a comment is split across a line boundary, the
   following lines are also aligned so that the same relative position as in
   the original is maintained. The format of this option is: COLUMN=<value>.

   Presence of a <decap> indicates that the case of text within comments and
   directive lines is to be modified as follows: whenever an at-sign (@) is
   encountered, the following character is uppercased, and the at-sign is
   deleted from the text.  Otherwise, characters are lowercased.  This option
   allows upper/lower case comments and/or directives to be entered on
   terminals without lower case, or without explicitly entering uppercase.
   If DECAP is entered, this processing is performed; if NODECAP is entered,
   it is not.

   It should be noted that this program always uppercases directive keywords.
   This insures that, in the case of $PAGE, that the QED MARK string search
   always matches it.

   If XREF is specified (and LIST is in effect), section and page numbers are
   printed on each physical page along with the title given on the last $PAGE
   directive, and an alphabetized list of $PAGE titles are placed at the end
   of the listing. The section number denotes logical pages (i.e., $PAGE
   directives), and corresponds to the numbering used by the debugger; the page
   number is the actual physical page in the listing. The page length is
   implicitly decreased by two to allow for the the page header. NOXREF
   suppresses this processing.

   The formatted output may be appended to the chosen output file by specifying
   APPEND;  if NOAPPEND is specified, the output replaces the old file.

   The <queue> option allows specification of options to be passed to the QUEUE
   program when 'LPT:' has been specified as the output device.  As all files
   written to LPT: are output together, the <queue> option is not modal. That
   is, only one setting (the last) will be used.  The format of the option is
   QUEUE:"string of queue options".

   If QUIT is specified, the program exits immediately. The command line in
   which it appears is not processed. The program also exits if a blank line
   is entered.

   If INCLUDE or HEADER is given, the the files specified in $INCLUDE and
   $HEADER directives are printed in the formatted listing. Default file
   extensions are '.INC' and '.HDR', respectively. Included file line numbers
   have a '*' appended to them.

   ECHO will cause the command lines in a @<cmd file> specification to be
   output to the terminal, enclosed in square brackets. Blank lines in command
   files are completely ignored.

   Note: this program always canonicalizes leading white space into minimal
   tab-blank form.  *)
$PAGE includes
$INCLUDE VERSIO.INC
$INCLUDE cmdutl.typ
$INCLUDE lookup.typ
$INCLUDE filutl.inc
$INCLUDE TIMUTL.INC
$INCLUDE pasdir.typ
$INCLUDE pasdir.inc
$INCLUDE swini.inc
$INCLUDE RUNUTL.INC
EXTERNAL
FUNCTION jobnum: STRING[3];
$PAGE declarations

LABEL
100, (* EOF abort point *)
150; (* next command *)

CONST
cr=CHR(#o15); (* "character" returned at EOLN by READCH *)
tab=CHR(#o11);
maxtitle_length = 64;
eoln_chars : SET OF CHAR := [CHR(#o15), CHR(#o33), CHR(#o32), CHR(#o12)
  , CHR(#o7)];
max_inc_file_nesting = 3; (* same as current TYM-Pascal compiler*)

TYPE (* processing modes *)
modes = (intext, incomment, inliteral, indirective, inheader, ininclude);
title_type = STRING [maxtitle_length];
file_type = (main, include); (* processing main or an include? *)
cmdfile_type = (from_tty, from_cmd_file, from_auto_file);
directive_type = (no_direct, page_direct, header_direct, include_direct);
inc_file_rec = RECORD (* record used in include file nesting *)
  inc_file : TEXT;
  save_line : STRING[ 255 ]; (* saves preceeding levels input line *)
  save_idx : 1..256; (* save index into saved input line *)
END;

VAR
ch: CHAR; (* current character, returned by READCH - NEXTCH *)
curmode: modes; (* current processing mode *)
curfile: file_type; (* type of file being processed *)
cmdmode, oldcmdmode: cmdfile_type; (* source of command inputs *)
commentlvl: 0..100; (* count of nested comments *)
buffer: PACKED ARRAY[1..255] OF CHAR; (* line output buffer *)
inputline: STRING[255]; (* the input line *)
inputidx: 1..256; (* index into the above *)
lineno: 0..99999; (* line number in main file *)
inc_lineno: ARRAY [1..max_inc_file_nesting] OF 0..99999;
(* keeps include file line numbers *)
inc_files : ARRAY [1..max_inc_file_nesting] OF inc_file_rec;
(* array used in handling include file nesting *)
inc_file_level : 0..max_inc_file_nesting; (* current include file nesting level *)
page_lineno: 0..99999; (* line number reset every page *)
phys_page: 0..99999; (* count of physical pages *)
dir_page: 0..99999; (* count of logical pages *)
in_page: 0..99999; (* physical pages within logical page *)
title_heading: title_type; (* current title from most recent page directive *)
saved_title: title_type := ''; (* current title to be printed *)
dir_type: directive_type; (* PAGE,HEADER,or INCLUDE directive? *)
cmdfile, oldcmdfile: TEXT; (* non-user command file *)
(* below are reset on a per line basis *)
bufidx: 0..255; (* index within buffer of CH *)
col: 0..255; (* leftmost logical text column position of CH *)
firstidx: 0..255; (* index of first non-white character *)
firstcol: 0..255; (* logical column position of same *)
textidx: 0..255; (* index of last char of program text or literal *)
textcol: 0..255; (* logical column of same *)
commentidx: 0..255; (* index of start "(" of last comment on line *)
commentcol: 0..255; (* column position of same *)
(* state vars controlling processing of multi-line comments *)
splitcomment: BOOLEAN; (* set when eoln appears within comment *)
commentadjustment: -255..255; (* number of columns to offset subsequent lines of comment
						   if column position of first is changed *)
$PAGE command declarations

TYPE (* oprocessing options *)
outputopts = ( list, (* output to .LST file *)
edit, (* replace original *)
newop ); (* output to .TMP file *)
caseopts = ( upper, (* uppercase program text *)
lower, (* lowercase it *)
pass ); (* leave it alone *)
options_type = RECORD (* complete option list *)
  caseopt: caseopts;
  outputopt: outputopts;
  column: 0..255; (* column in which to align comments *)
  decapflag: BOOLEAN; (* flags decapitalization processing in comments *)
  appendflag: BOOLEAN; (* append output to file *)
  xref_flag: BOOLEAN; (* generate cross reference, if listing *)
  pagelength: 0..255; (* length of listing page *)
  list_headers: BOOLEAN; (* list $HEADER file contents *)
  list_includes: BOOLEAN; (* list $INCLUDE file contents *)
  echo_cmds: BOOLEAN; (* echo the commands in CMD files *)
  fid_qualifier: file_id; (* default extension, ppn, etc. *)
  input_defaults: file_id (* default input file name components *)
END;

VAR
line: cmdline; (* command line to interpret *)
f_name: file_id; (* file to process *)
inc_filename: file_id; (* include file name *)
quit_flag, error_flag: BOOLEAN; (* returned by command line processor *)
defaultoptions: options_type; (* default option set *)
curoptions: options_type; (* options_type in effect for processing of a single file *)
file_queued: BOOLEAN; (* indicates output to 'LPT:' *)
queue_file: file_id; (* name of queue file *)
queue_options: STRING; (* special options_type for queue *)
switch_line: sw_ini_string; (* receives text from SWITCH.INI lines *)

CONST
initial_defaults: options_type = ( upper, list, 49, FALSE, FALSE, FALSE, 45,
  FALSE, FALSE, FALSE, '', '.PAS' );
$PAGE command name list

TYPE
commands = ( listcmd, editcmd, newcmd, uppercmd, lowercmd, passcmd, columncmd,
  lengthcmd, quitcmd, decapcmd, nodecapcmd, xrefcmd, noxrefcmd, appendcmd,
  noappendcmd, queueopt, headercmd, noheadcmd, optioncmd, nooptcmd, defcmd,
  includecmd, noincludecmd, echocmd, noechocmd );
cmdnames = ARRAY[commands] OF cmdlist;

VAR
cmd: commands; (* command to dispatch on *)

CONST
cmdnamelist: cmdnames := ( ( 'LIST', 4 ), ( 'EDIT', 4 ), ( 'NEW', 3 )
  , ( 'UPPERCASE', 5 ), ( 'LOWERCASE', 5 ), ( 'PASS', 4 ), ( 'COLUMN', 3 )
  , ( 'LENGTH', 3 ), ( 'QUIT', 4 ), ( 'DECAP', 1 ), ( 'NODECAP', 3 )
  , ( 'XREF', 1 ), ( 'NOXREF', 3 ), ( 'APPEND', 1 ), ( 'NOAPPEND', 3 )
  , ( 'QUEUE', 3 ), ( 'HEADER', 4 ), ( 'NOHEADER', 3 ), ( 'OPTION', 3 )
  , ( 'NOOPTION', 3 ), ( 'DEFAULT', 3 ), ( 'INCLUDE', 3 ), ( 'NOINCLUDE', 3 )
  , ( 'ECHO', 3 ), ( 'NOECHO', 3 ) );
EXTERNAL
FUNCTION lookup_cmdnames (* specific declaration for this scalar type *)
( line: cmdline; VAR idx: cmdlineidx; list: cmdnames; maxcommands: commands;
  VAR nameidx: commands ): BOOLEAN;
$PAGE big letter printing

CONST
page_width = 100;
char_width = 5;
ch_desc_size = 35; (* = char_width * char_height *)
hspace = 3; (* horizontal space between characters *)
max_chars = 12; (* max characters / row *)

TYPE
prt_chars = ' ' .. '_'; (* ascii columns 2 - 5 *)
row = STRING [max_chars];
ch_desc = PACKED ARRAY [1..ch_desc_size] OF CHAR;
reptype = ARRAY [prt_chars] OF ch_desc;

CONST
rep: reptype := ( '                                   ',
  ' $$   $$   $$   $$        $$   $$  ', ' $ $  $ $                          '
  , '      $ $ $$$$$ $ $ $$$$$ $ $      ',
  '  $   $$$ $ $   $$$   $ $ $$$   $  ', '$$   $$  $   $   $   $   $  $$   $$'
  , ' $   $ $  $ $   $  $$ $ $$  $  $$ $',
  '   $   $                           ', '    $   $   $    $    $     $     $'
  , '$     $     $    $    $   $   $    ',
  '     $ $ $ $$$ $$$$$ $$$ $ $ $     ', '       $    $  $$$$$  $    $       '
  , '                $$   $$    $    $  ',
  '               $$$$$               ', '                          $$   $$  '
  , '         $   $   $   $   $         ',
  '  $$  $  $ $  $ $  $ $  $ $  $  $$ ', '  $   $$    $    $    $    $   $$$ '
  , ' $$$ $   $    $ $$$ $    $    $$$$$',
  ' $$$ $   $    $  $$     $$   $ $$$ ', '   $   $$  $ $ $  $ $$$$$   $    $ '
  , '$$$$$$    $$$$     $    $$   $ $$$ ',
  '  $$  $   $    $ $$ $$  $$   $ $$$ ', '$$$$$    $   $   $   $   $    $    '
  , ' $$$ $   $$   $ $$$ $   $$   $ $$$ ',
  ' $$$ $   $$  $$ $$ $    $   $  $$  ', '      $$   $$        $$   $$       '
  , ' $$   $$        $$   $$    $   $   ',
  '    $   $   $   $     $     $     $', '          $$$$$     $$$$$          '
  , '$     $     $     $   $   $   $    ',
  ' $$$ $   $   $   $    $         $  ', ' $$$ $   $    $ $$ $$ $ $$ $$  $$  '
  , ' $$$ $   $$   $$$$$$$   $$   $$   $',
  '$$$$  $  $ $  $ $$$  $  $ $  $$$$$ ', ' $$$ $   $$    $    $    $   $ $$$ '
  , '$$$$  $  $ $  $ $  $ $  $ $  $$$$$ ',
  '$$$$$$    $    $$$  $    $    $$$$$', '$$$$$$    $    $$$  $    $    $    '
  , ' $$$$$    $    $ $$$$   $$   $ $$$ ',
  '$   $$   $$   $$$$$$$   $$   $$   $', ' $$$   $    $    $    $    $   $$$ '
  , '    $    $    $    $    $$   $ $$$ ',
  '$   $$  $ $ $  $$   $ $  $  $ $   $', '$    $    $    $    $    $    $$$$$'
  , '$   $$$ $$$ $ $$ $ $$   $$   $$   $',
  '$   $$$  $$ $ $$  $$$   $$   $$   $', ' $$$ $   $$   $$   $$   $$   $ $$$ '
  , '$$$$ $   $$   $$$$$ $    $    $    ',
  ' $$$ $   $$   $$   $$ $ $$  $  $$ $', '$$$$ $   $$   $$$$$ $ $  $  $ $   $'
  , ' $$$ $   $$     $$$     $$   $ $$$ ',
  '$$$$$  $    $    $    $    $    $  ', '$   $$   $$   $$   $$   $$   $ $$$ '
  , '$   $$   $$   $ $ $  $ $   $    $  ',
  '$   $$   $$   $$ $ $$ $ $$ $ $ $ $ ', '$   $$   $ $ $   $   $ $ $   $$   $'
  , '$   $$   $ $ $   $    $    $    $  ',
  '$$$$$    $   $   $   $   $    $$$$$', '  $$$  $    $    $    $    $    $$$'
  , '     $     $     $     $     $     ',
  '$$$    $    $    $    $    $  $$$  ', '  $   $ $ $   $                    '
  , '                              $$$$$' );
$PAGE print_row -- put out one row of characters

PROCEDURE print_row (r:row);
VAR
ind: 0 .. ch_desc_size;
hfill: 1 .. page_width;
len: 0 .. max_chars;
i: 1 .. max_chars;
BEGIN
  len := LENGTH(r);
  IF len = 0 THEN RETURN;
  hfill := (page_width + hspace - (char_width+hspace)*len) DIV 2;
  ind := 0;
  WHILE ind <> ch_desc_size DO BEGIN
    WRITE (OUTPUT,' ':hfill);
    FOR i := 1 TO len DO BEGIN
      WRITE (OUTPUT,SUBSTR(rep[r[i]],ind+1,char_width));
      IF i <> len THEN WRITE (OUTPUT,' ':hspace);
    END;
    WRITELN (OUTPUT);
    ind := ind + char_width;
  END;
END;
$PAGE list_header

PROCEDURE list_header;
TYPE
day_array = ARRAY [week_day] OF STRING [3];
CONST
day_name: day_array := ( 'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat' );
VAR
fname: file_id;
i: 1 .. 4;
ecode: dir_errors;
file_desc: dir_attrs;
title_line: STRING [105];
BEGIN
  f_name := FILENAME (INPUT); (* get actual name of file *)
  dir_attr (ecode, f_name, file_desc);
  fname := SUBSTR (f_name, INDEX (f_name, ':') + 1); (* strip dev and ppn *)
  fname := SUBSTR (fname, 1, INDEX (fname, '[') - 1);
  title_line := 'File ' || f_name || '   ' || day_name [day_of_week (extr_date
    (file_desc.creation))] || ' ' || SUBSTR (dc_ext (file_desc.creation)
    , 1, 15) || '   TYM-Pascal Source File Formatter, Version ' || version();
  FOR i := 1 TO 4 DO BEGIN
    WRITELN (OUTPUT, title_line);
    WRITELN (OUTPUT)
  END;
  FOR i := 1 TO 4 DO WRITELN (OUTPUT);
  print_row (fname);
  FOR i := 1 TO 4 DO WRITELN (OUTPUT);
  FOR i := 1 TO 4 DO BEGIN
    WRITELN (OUTPUT, title_line);
    WRITELN (OUTPUT)
  END;
END;
$PAGE startline

PROCEDURE startline; (* init per line information *)
BEGIN
  bufidx := 0;
  col := 1;
  firstidx := 0;
  firstcol := 0;
  textidx := 0;
  textcol := 0;
  commentidx := 0;
  commentcol := 0;
END;
$PAGE cvt

TYPE
page_num = 0 .. 99999;
cvt_str = STRING [5];

FUNCTION cvt(page: page_num): cvt_str;
BEGIN
  IF page = 0 THEN cvt := ''
  ELSE cvt := cvt(page DIV 10) || CHR(ORD('0') + (page MOD 10));
END;
$PAGE do_header, do_page

PROCEDURE do_header(title: title_type);
BEGIN
  WRITE (OUTPUT, 'SECTION ', dir_page:4);
  IF in_page = 0 THEN WRITE (OUTPUT, ' ':4)
  ELSE WRITE (OUTPUT, '-', cvt(in_page) || '   ':3);
  WRITELN (OUTPUT, ' ', title, ' ':90-8-4-4-1-LENGTH(title)
    -5-4, 'PAGE ', phys_page:4);
  WRITELN (OUTPUT);
END;

PROCEDURE do_phys_page(title: title_type);
BEGIN
  page (OUTPUT);
  phys_page := phys_page + 1;
  IF phys_page <> 1 THEN in_page := in_page + 1;
  IF curoptions.xref_flag THEN do_header(title)
END;

PROCEDURE do_dir_page(title: title_type);
BEGIN
  page(OUTPUT);
  dir_page := dir_page + 1;
  phys_page := phys_page + 1;
  in_page := 0;
  IF curoptions.xref_flag THEN do_header(title)
END;
$PAGE record_title
(* These routines keep an alphabetized list of all of the "$page" titles seen,
   along with their physical page number.  When XREF is selected this list is
   dumped at the end of the listing. *)

TYPE
title = RECORD
  phys_page_num: 0..999999;
  dir_page_num: 0..999999;
  next: ^ title;
  TEXT: title_type
END;

VAR
first_title: ^ title;
(* RECORD TITLE appends a title to the list.  The page number is taking from the
   current page numbers in phys_page and dir_page. *)

PROCEDURE record_title ( title_text: STRING );
VAR
ntitle, lt, t: ^ title;
BEGIN
  IF title_text = '' THEN RETURN;
  NEW (ntitle);
  ntitle^.TEXT := title_text;
  ntitle^.phys_page_num := phys_page;
  ntitle^.dir_page_num := dir_page;
  t := first_title;
  lt := NIL;
  WHILE t <> NIL DO BEGIN
    EXIT IF title_text < t^.TEXT;
    lt := t;
    t := t^.next;
  END;
  ntitle^.next := t;
  IF lt = NIL THEN first_title := ntitle
  ELSE lt^.next := ntitle;
END;
$PAGE dump_titles
(* DUMP TITLES is called after the entire file has been listed, and prints
   the alphabetized list of titles, with their page numbers. *)

PROCEDURE dump_titles;
VAR
n, title;
BEGIN
  WRITELN (OUTPUT, 'SECTION  ', 'TITLE':10, ' ':60, '  PAGE');
  WRITELN (OUTPUT);
  lineno := 3;
  t := first_title;
  WHILE t <> NIL DO BEGIN
    WRITELN (OUTPUT, t^.dir_page_num:6, ' ':3, t^.TEXT, t^.phys_page_num:6);
    lineno := lineno + 1;
    IF (lineno MOD curoptions.pagelength) = 1 THEN BEGIN
      page(OUTPUT);
      lineno := 1
    END;
    n := t^.next;
    DISPOSE (t);
    t := n;
  END;
  page (OUTPUT);
END;
$PAGE flush
(* FLUSH massages the last line read then outputs it. It canonicalizes
     leading white space, aligns trailing comments in a designated column,
     and lines up multi-line comments under the first line. *)

PROCEDURE flush;
VAR
pad: STRING[255];
blanks: 0..8;
newcol: 0..255;
newlen: 0..255;
idx: 0..255;
title_length: 0..255;
i: 0..255;
BEGIN
(* If we have a multi-line comment, align subsequent lines up under the
       the first line of the comment, by forcing FIRSTCOL to the desired
       value; canonicalization then aligns the text properly. Note: this is
       only done to trailing comments that are split across lines, since
       other comments are never moved. *)
  IF splitcomment AND (firstcol <> 0) THEN BEGIN
    firstcol := firstcol + commentadjustment;
    IF firstcol <= 0 THEN firstcol := 1; (* avoid blowing up on a sick looking program *)
  END;
  IF curmode <> incomment THEN splitcomment := FALSE; (* turn off adjustment when comment done *)
  (* Canonicalize leading white space *)
  IF firstcol > 0 THEN BEGIN (* only work on non-blank line *)
    pad := ''; (* get minimal tab-blank form *)
    FOR idx := 1 TO ((firstcol - 1) DIV 8) DO pad := pad || tab; (* use as many tabs as possible *)
    pad := pad || SUBSTR ('        ', 1, (firstcol - 1) MOD 8);
    (* then blanks *)
    newlen := LENGTH (pad) + (bufidx - firstidx + 1);
    IF newlen <= LENGTH (buffer) THEN BEGIN (* operate if there is room *)
      buffer := pad || SUBSTR (buffer, firstidx, (bufidx - firstidx + 1));
      IF textidx > 0 THEN (* adjust idx's to reflect change *)
      textidx := textidx - (firstidx - LENGTH (pad) - 1);
      IF commentidx > 0 THEN commentidx := commentidx - (firstidx - LENGTH (
	pad) - 1);
      bufidx := newlen (* update buffer length *)
    END
  END
  ELSE bufidx := 0; (* effective null, truncate *)
  (* Align a trailing comment if it is not the only thing on the line. *)
  IF commentidx > textidx (* comment follows last text *)
  THEN IF textidx > 0 (* comment is not alone on the line *)
  THEN BEGIN
    newcol := curoptions.column; (* select column in which to align comment *)
    WHILE newcol <= (textcol + 1) (* use standard or first MOD 4 stop after text *)
    DO newcol := newcol + 4;
    pad := ''; (* get chars to pad between text and comment *)
    blanks := 0;
    textcol := textcol + 1; (* pad chars begin in column after last text *)
    REPEAT
      textcol := textcol + 1;
      blanks := blanks + 1;
      IF (textcol MOD 8) = 1 THEN BEGIN (* can use TAB *)
	pad := pad || tab;
	blanks := 0; (* spaces aren't needed *)
      END
    UNTIL textcol = newcol;
    pad := pad || SUBSTR ('        ', 1, blanks);
    newlen := textidx + LENGTH (pad) + (bufidx - commentidx + 1);
    IF newlen <= LENGTH (buffer) THEN BEGIN (* if adjusted line not too long, adjust *)
      buffer := SUBSTR (buffer, 1, textidx)
	|| pad || SUBSTR (buffer, commentidx, (bufidx - commentidx + 1));
      bufidx := newlen
    END;
    IF curmode = incomment THEN BEGIN (* comment extends across line boundary *)
      splitcomment := TRUE; (* set to line up following lines *)
      commentadjustment := newcol - commentcol
    END;
  END (* alignment code *);
  (* Output processed line *)
  IF curoptions.outputopt = list THEN BEGIN
    dir_type := no_direct; (* assume not a directive line until check *)
    IF (bufidx >= 5) ANDIF (SUBSTR (buffer, 1, 5) = '$PAGE') THEN BEGIN
      dir_type := page_direct;
      title_length := bufidx - 5;
      title_heading := SUBSTR(buffer, 6);
      FOR i := 1 TO LENGTH(title_heading) DO (* get rid of tabs *)
      IF title_heading[i] = tab THEN title_heading[i] := ' ';
      IF VERIFY (title_heading, [' ']) <> 0 THEN BEGIN (* make sure there is a title *)
	saved_title := title_heading;
	do_dir_page(title_heading);
	record_title (title_heading);
	IF curfile = main THEN lineno := 1
	ELSE inc_lineno[ inc_file_level ] := 1
      END
      ELSE BEGIN (* no title -- simple page eject *)
	do_phys_page (saved_title);
	IF curfile = main THEN lineno := lineno + 1
	ELSE inc_lineno[ inc_file_level ] := inc_lineno[ inc_file_level ] + 1
      END;
      page_lineno := 0
    END
    ELSE BEGIN
      IF curfile = main THEN lineno := lineno + 1
      ELSE inc_lineno[ inc_file_level ] := inc_lineno[ inc_file_level ] + 1;
      page_lineno := page_lineno + 1
    END;
    IF inc_file_level < max_inc_file_nesting THEN BEGIN
      IF (bufidx >= 7) ANDIF (SUBSTR(buffer,1,7) = '$HEADER')
	THEN dir_type := header_direct
      ELSE IF (bufidx >=8) ANDIF (SUBSTR(buffer,1,8) = '$INCLUDE')
	THEN dir_type := include_direct;
      inc_filename := SUBSTR(buffer, INDEX(buffer,' ',LENGTH(buffer)) )
    END;
    IF page_lineno > 1 THEN IF curoptions.pagelength <> 0 THEN IF (page_lineno
      MOD curoptions.pagelength) = 1 THEN do_phys_page(title_heading);
    IF dir_type <> page_direct THEN IF curfile = main THEN WRITE (lineno:5,
      tab)
    ELSE WRITE (inc_lineno[ inc_file_level ]:5, '*', tab)
  END;
  IF dir_type <> page_direct THEN WRITELN (buffer:bufidx)
END;
$PAGE readch
(* READCH simply reads the next character from the file and assigns
     its buffer position. *)

PROCEDURE readch;
BEGIN
  IF (inputidx > LENGTH(inputline)) ORIF (inputline[inputidx] IN eoln_chars)
    THEN ch := cr (* use CR to signify end of line *)
  ELSE BEGIN
    ch := inputline[inputidx];
    inputidx := inputidx + 1;
    (* note that we are not really in comment unless curmode=incomment
	      and commentlvl>0.  This is because code sets curmode when
	      examining the second character of a possible comment terminator
	      or initiator for reasons documented elsewhere.  However, it
	      sets commentlvl appropriately when it is sure it is in or out. *)
    IF curoptions.decapflag AND ( (curmode=indirective) OR (commentlvl>0) (* implying curmode=incomment *)
      ) THEN IF ch<>'@' THEN ch:= LOWERCASE(ch)
    ELSE IF (inputidx > LENGTH(inputline)) ORIF
      (inputline[inputidx] IN eoln_chars) THEN ch := cr (* implicitly eating the at-sign *)
    ELSE BEGIN
      ch := inputline[inputidx];
      inputidx := inputidx + 1;
      ch:= UPPERCASE(ch)
    END;
    IF ch<>cr THEN bufidx := bufidx + 1
  END
END;
$PAGE nextch
(* NEXTCH processes the current character, CH, then reads the next
     character from the file. The processing of a character is done after
     it is read and passed up to the mode setting loop, so that it is
     treated for what it is recognized to be, not for what the previous
     mode was. Calculation of the new logical column position must also
     be done after column values have been recorded for the current char. *)

PROCEDURE nextch;
BEGIN
  IF ch <> cr THEN BEGIN
    IF bufidx <= LENGTH (buffer) (* save char just processed *)
    THEN buffer[bufidx] := ch;
    (* Record last text position if char is non-white and in program text or
	     literal. Note that blanks in literals are not recorded, but since they
	     must be followed by an ending "'", everything works out. *)
    IF curmode IN [intext, inliteral] THEN IF ORD (ch) > ORD (' ') THEN BEGIN
      textcol := col;
      textidx := bufidx
    END;
    (* Record first non-white character position if applicable. *)
    IF ORD (ch) > ORD (' ') (* char is non-white *)
    THEN IF firstcol = 0 THEN BEGIN (* and is first non-white *)
      firstidx := bufidx;
      firstcol := col
    END;
    (* Update logical column position by advancing COL by the number of
	     columns occupied by this character. *)
    col := col + 1; (* assume it is non-control char *)
    IF ORD (ch) < ORD (' ') (* check for control chars *)
    THEN BEGIN
      IF ch = tab THEN WHILE (col MOD 8) <> 1 DO col := col + 1
      ELSE col := col - 1 (* we had allowed one column for char, undo *)
    END
  END
  ELSE BEGIN (* CR last char, get next line *)
    flush;
    IF curfile = main THEN BEGIN
      READLN(INPUT);
      READ(INPUT, inputline)
    END
    ELSE BEGIN (* we are in an include file *)
      READLN(inc_files[ inc_file_level ].inc_file);
      READ(inc_files[ inc_file_level ].inc_file, inputline)
    END;
    inputidx := 1;
    startline;
    IF ( (curfile = main) ANDIF EOF(INPUT) ) THEN BEGIN (* cleanup *)
      IF list = curoptions.outputopt THEN page (OUTPUT);
      GOTO 100 (* exit mainline while not EOF loop *)
    END;
  END;
  IF dir_type IN [no_direct, page_direct] THEN readch (* get the next character *)
END (*NEXTCH*);
$PAGE read_command_line

PROCEDURE read_command_line (VAR line: cmdline);
VAR
startidx: cmdlineidx;
BEGIN
  LOOP
    LOOP
      EXIT IF cmdmode = from_tty DO BEGIN
	WRITE (TTY, '*');
	BREAK;
	READLN (TTY);
	READ (TTY, line);
      END;
      line := '';
      EXIT IF (cmdmode = from_auto_file) AND EOF (cmdfile)
	DO SCRATCH (cmdfile);
      IF EOF (cmdfile) THEN BEGIN (* must be from real command file *)
	CLOSE (cmdfile);
	cmdfile := oldcmdfile;
	cmdmode := oldcmdmode;
      END
      ELSE BEGIN
	REPEAT
	  READLN (cmdfile, line);
	UNTIL (line <> '') OR EOF (cmdfile);
      END;
      EXIT IF line <> '' DO IF curoptions.echo_cmds THEN BEGIN
	WRITELN (TTY, '[', line, ']');
	BREAK
      END;
    END (* loop *);
    startidx := VERIFY (line, [' ']);
    EXIT IF (startidx = 0) ORIF (line[startidx] <> '@');
    IF cmdmode = from_cmd_file THEN WRITELN (TTY,
      'Nested command files not allowed')
    ELSE BEGIN
      line := SUBSTR (line, startidx + 1);
      oldcmdmode := cmdmode;
      oldcmdfile := cmdfile;
      cmdmode := from_cmd_file;
      RESET (cmdfile, '.CCL ' || line);
      IF EOF(cmdfile) THEN BEGIN
	CLOSE(cmdfile);
	RESET (cmdfile, '.CMD ' || line);
	IF EOF(cmdfile) THEN BEGIN
	  CLOSE(cmdfile);
	  cmdmode := oldcmdmode;
	  cmdfile := oldcmdfile;
	  WRITELN (TTY, 'Unable to open command file ', line);
	END;
      END;
    END;
  END (* loop *);
END;
$PAGE process_command_line

(* PROCESS COMMAND LINE is called with a command line.  It processes any
   switches in the input line, and returns the input file name, a quit
   flag, and an error flag. *)

PROCEDURE process_command_line ( line: cmdline; VAR f_name: file_id;
  VAR quit, error: BOOLEAN );
VAR
lindex, idx: cmdlineidx;
$PAGE command utilities

  PROCEDURE skipblanks;
  BEGIN
    WHILE (lindex <= LENGTH (line)) ANDIF (ORD (line[lindex]) <= ORD (' '))
      DO lindex := lindex + 1
  END;

  FUNCTION checkpunct ( ch: CHAR ): BOOLEAN;
  BEGIN
    skipblanks;
    checkpunct := FALSE;
    IF (lindex <= LENGTH (line)) ANDIF (line[lindex] = ch) THEN BEGIN
      lindex := lindex + 1;
      checkpunct := TRUE
    END
  END;
TYPE
getnumtype = 0..255;

  FUNCTION getnum ( VAR num: getnumtype ): BOOLEAN;
  BEGIN
    num := 0;
    getnum := FALSE;
    skipblanks;
    WHILE (lindex <= LENGTH (line)) ANDIF (line[lindex] IN ['0'..'9'])
      DO BEGIN
      getnum := TRUE;
      num := num * 10 + (ORD (line[lindex]) - ORD ('0'));
      lindex := lindex + 1
    END
  END;
TYPE
getnametype = STRING [6];

  FUNCTION getname ( VAR name: getnametype ): BOOLEAN;
  BEGIN
    name := '';
    getname := FALSE;
    skipblanks;
    WHILE (lindex <= LENGTH(line)) ANDIF (UPPERCASE(line[lindex])
      IN ['A'..'Z', '0'..'9']) DO BEGIN
      getname := TRUE;
      name := name || UPPERCASE (line[lindex]);
      lindex := lindex + 1;
    END;
  END;
$PAGE process_command_line mainline
LABEL
150,
200;
VAR
opt_name: sw_ini_name;
end_quote: CHAR;
switch_line: sw_ini_string;
fid_specified: BOOLEAN;
filetemp, fidtemp: file_id;
defaultname: file_id;
BEGIN
  error := FALSE;
  quit := FALSE;
  IF line = '' THEN GOTO 200;
  curoptions := defaultoptions; (* assume the defaults and look for options *)
  lindex := INDEX (line, '/', LENGTH (line) + 1); (* get filename *)
  f_name := SUBSTR (line, 1, lindex - 1);
  idx := INDEX (f_name, '='); (* see if there is output info *)
  IF idx = 0 THEN fid_specified := FALSE
  ELSE BEGIN
    f_name := SUBSTR (line, idx + 1, lindex-idx-1);
    curoptions.fid_qualifier := SUBSTR (line, 1, idx - 1);
    fid_specified := TRUE;
  END;
  IF lindex <= LENGTH (line) THEN BEGIN (*  a '/' was found *)
    lindex := lindex + 1; (* position after the slash *)
    WHILE lindex <= LENGTH (line) DO BEGIN (* loop over options *)
      IF NOT lookup_cmdnames (line, lindex, cmdnamelist, MAXIMUM (commands)
	, cmd) THEN BEGIN
	WRITELN (TTY, 'Bad option.');
	GOTO 150
      END
      ELSE CASE cmd OF
	decapcmd: curoptions.decapflag := TRUE;
	nodecapcmd: curoptions.decapflag:= FALSE;
	quitcmd: GOTO 200;
	listcmd: curoptions.outputopt := list;
	editcmd: curoptions.outputopt := edit;
	newcmd: curoptions.outputopt := newop;
	uppercmd: curoptions.caseopt := upper;
	lowercmd: curoptions.caseopt := lower;
	passcmd: curoptions.caseopt := pass;
	appendcmd: curoptions.appendflag := TRUE;
	noappendcmd:curoptions.appendflag := FALSE;
	xrefcmd: curoptions.xref_flag := TRUE;
	noxrefcmd: curoptions.xref_flag := FALSE;
	headercmd: curoptions.list_headers := TRUE;
	noheadcmd: curoptions.list_headers := FALSE;
	includecmd: curoptions.list_includes := TRUE;
	noincludecmd: curoptions.list_includes := FALSE;
	echocmd: curoptions.echo_cmds := TRUE;
	noechocmd: curoptions.echo_cmds := FALSE;
	defcmd: BEGIN
	  IF checkpunct (':') ORIF checkpunct ('=') THEN;
	  IF checkpunct ('"') THEN end_quote := '"'
	  ELSE IF checkpunct ('''') THEN end_quote := ''''
	  ELSE BEGIN
	    WRITELN (TTY, 'File name defaults expected.');
	    GOTO 150;
	  END;
	  idx := INDEX (SUBSTR (line, lindex), end_quote);
	  IF idx = 0 THEN BEGIN
	    WRITELN (TTY, 'File name defaults not terminated.');
	    GOTO 150;
	  END;
	  curoptions.input_defaults := SUBSTR (line, lindex, idx-1);
	  lindex := lindex + idx;
	END;
	queueopt: BEGIN
	  IF checkpunct (':') ORIF checkpunct ('=') THEN ;
	  IF checkpunct ('"') THEN end_quote := '"'
	  ELSE IF checkpunct ('''') THEN end_quote := ''''
	  ELSE end_quote := ' ';
	  IF end_quote = ' ' THEN BEGIN
	    IF NOT getname (opt_name) THEN BEGIN
	      WRITELN (TTY, 'Queue options expected.');
	      GOTO 150;
	    END;
	    IF sw_ini ('PRINT', opt_name, switch_line) ORIF
	      sw_ini ('QUEUE', opt_name, switch_line)
	      THEN queue_options := switch_line
	    ELSE BEGIN
	      WRITELN (TTY, 'Print option ', opt_name, ' not in SWITCH.INI');
	      GOTO 150;
	    END;
	  END
	  ELSE BEGIN
	    idx := INDEX (SUBSTR (line, lindex), end_quote);
	    IF idx = 0 THEN BEGIN
	      WRITELN (TTY, 'Quote options not terminated.');
	      GOTO 150;
	    END;
	    queue_options := SUBSTR (line, lindex, idx-1);
	    lindex := lindex + idx;
	  END;
	END;
	optioncmd: BEGIN
	  IF checkpunct (':') ORIF checkpunct ('=') THEN ;
	  IF NOT getname (opt_name) THEN BEGIN
	    WRITELN (TTY, 'Option name expected.');
	    GOTO 150;
	  END;
	  IF sw_ini ('FORMAT', opt_name, switch_line) THEN BEGIN
	    fidtemp := curoptions.fid_qualifier;
	    process_command_line (switch_line, filetemp, quit, error);
	    IF fid_specified THEN curoptions.fid_qualifier := fidtemp;
	    IF error THEN WRITELN (TTY, '  -- in SWITCH.INI:', opt_name);
	    IF error OR quit THEN RETURN;
	  END
	  ELSE BEGIN
	    WRITELN (TTY, 'FORMAT option ', opt_name, 'not in SWITCH.INI');
	    GOTO 150;
	  END;
	END;
	nooptcmd: BEGIN
	  fidtemp := curoptions.fid_qualifier;
	  curoptions := initial_defaults;
	  IF fid_specified THEN curoptions.fid_qualifier := fidtemp;
	  IF sw_ini ('PRINT', '', switch_line) ORIF
	    sw_ini ('QUEUE', '', switch_line)
	    THEN queue_options := switch_line
	  ELSE queue_options := '';
	END;
	lengthcmd: BEGIN
	  IF checkpunct (':') THEN ;
	  IF checkpunct ('=') THEN ;
	  IF getnum ( curoptions.pagelength ) THEN BEGIN
	    IF curoptions.pagelength <= 10 THEN BEGIN
	      WRITELN (TTY, 'Value too small.');
	      GOTO 150
	    END
	  END
	  ELSE BEGIN (* no value given *)
	    WRITELN (TTY, 'Page length expected.');
	  END
	END;
	columncmd: BEGIN
	  IF checkpunct (':') THEN ;
	  IF checkpunct ('=') THEN ;
	  IF getnum ( curoptions.column ) THEN BEGIN (* check for reasonable value *)
	    IF curoptions.column <= 20 THEN BEGIN
	      WRITELN (TTY, 'Value too small.');
	      GOTO 150
	    END
	    ELSE IF (curoptions.column MOD 8) <> 1 THEN BEGIN
	      WRITELN (TTY, 'Warning - not a tab stop.');
	      BREAK
	    END
	  END
	  ELSE BEGIN (* no column value *)
	    WRITELN (TTY, 'Column position expected.');
	    GOTO 150
	  END
	END
      END (* case cmd *);
      IF checkpunct (',') THEN ;
      IF checkpunct ('/') THEN ;
    END (* option loop *)
  END;
  RETURN;
  150: error := TRUE;
  RETURN;
  200: quit := TRUE;
  RETURN;
END;
$PAGE list mainline

BEGIN
  OPEN(TTY);
  REWRITE(TTY);
  WRITELN (TTY, 'TYM-Pascal Source File Formatter, Version ', version());
  WRITELN(TTY);
  title_heading := '                             ';
  defaultoptions := initial_defaults;
  file_queued := FALSE;
  IF sw_ini ('PRINT', '', switch_line) ORIF sw_ini ('QUEUE', '', switch_line)
    THEN queue_options := switch_line
  ELSE queue_options := '';
  IF sw_ini ('FORMAT', '', switch_line) THEN BEGIN
    process_command_line (switch_line, f_name, quit_flag, error_flag);
    IF error_flag THEN WRITELN (TTY, '  -- in SWITCH.INI');
    defaultoptions := curoptions;
  END;
  cmdmode := from_tty;
  IF runoff > 0 THEN BEGIN
    RESET (cmdfile, jobnum || 'FMT.TMP');
    IF NOT EOF (cmdfile) THEN cmdmode := from_auto_file;
  END;
  LOOP (* through files *)
    150 (* next command *): read_command_line (line);
    process_command_line (line, f_name, quit_flag, error_flag);
    EXIT IF quit_flag;
    IF error_flag THEN GOTO 150;
    IF f_name = '' THEN BEGIN (* default option setting *)
      defaultoptions := curoptions;
      GOTO 150
    END;
    (* Have filename and options - go to it *)
    RESET(INPUT, curoptions.input_defaults||' '||f_name, [ASCII]);
    IF EOF (INPUT) THEN BEGIN (* non existent file *)
      WRITELN (TTY, 'File ',SUBSTR(f_name,1,LENGTH(f_name))
	, ' is null or nonexistent.');
      GOTO 150
    END;
    READ (INPUT, inputline);
    inputidx := 1;
    IF UPPERCASE (curoptions.fid_qualifier) = 'LPT:' THEN BEGIN (* queue result to printer *)
      curoptions.fid_qualifier := 'DSK:' || jobnum || 'FMT.LST';
      curoptions.appendflag := TRUE;
      IF NOT file_queued THEN BEGIN
	REWRITE (OUTPUT, curoptions.fid_qualifier);
	queue_file := FILENAME (OUTPUT);
	CLOSE (OUTPUT);
      END;
      file_queued := TRUE;
    END;
    CASE curoptions.outputopt OF (* get suffix for output file *)
      list: curoptions.fid_qualifier := '.lst[] ' || curoptions.fid_qualifier;
      edit: ; (* will go in original *)
      newop: curoptions.fid_qualifier := '.tmp[] ' || curoptions.fid_qualifier
	;
      OTHERS:
    END;
    IF curoptions.appendflag THEN REWRITE(OUTPUT, '.PAS ' ||f_name|| ' ' ||
      curoptions.fid_qualifier, [PRESERVE])
    ELSE REWRITE(OUTPUT, '.PAS ' ||f_name|| ' ' || curoptions.fid_qualifier);
    (* set up recording of pages if in xref mode *)
    dir_page := 0;
    phys_page := 0;
    in_page := 0;
    title_heading := '';
    curfile := main;
    IF curoptions.xref_flag THEN BEGIN
      IF curoptions.pagelength > 2 THEN curoptions.pagelength := curoptions.
	pagelength - 2; (* adjust for page header *)
    END;
    (* If listing generate a header page *)
    IF curoptions.outputopt = list THEN BEGIN
      list_header;
      page (OUTPUT);
      list_header;
      do_phys_page(title_heading);
    END;
    commentlvl:= 0;
    curmode:= intext;
    startline;
    readch; (* read first char *)
    lineno := 0;
    inc_file_level := 0; (* level zero means in main file *)
    page_lineno := 0;
    splitcomment := FALSE;
    first_title := NIL;
    WHILE ( NOT EOF(INPUT) ) ORIF ( curfile = include ) DO BEGIN
      CASE curmode OF
	intext: IF ch='''' THEN BEGIN
	  curmode:= inliteral; (* go into literal mode *)
	  nextch
	END
	ELSE IF ch='(' THEN BEGIN
	  curmode := incomment; (* assume so that char not recorded as text *)
	  nextch;
	  IF ch='*' THEN BEGIN (* go into comment mode *)
	    commentidx := bufidx - 1;
	    commentcol := col - 1;
	    commentlvl:= 1;
	    nextch;
	    IF ch = '$' THEN BEGIN
	      nextch;
	      IF UPPERCASE (ch) IN ['X', 'Y'] THEN BEGIN
		curmode := intext;
		textidx := bufidx - 3;
	      END;
	    END;
	  END
	  ELSE BEGIN (* paren really was text *)
	    curmode := intext;
	    textidx := bufidx - 1
	  END
	END
	ELSE IF (ch = '$') AND (bufidx = 1) THEN BEGIN
	  REPEAT (* make sure directive keyword is in uppercase *)
	    ch:= UPPERCASE(ch);
	    nextch
	  UNTIL NOT (UPPERCASE(ch) IN ['A'..'Z']);
	  curmode:= indirective (* now go into directive mode *)
	END
	ELSE BEGIN (* uppercase the character *)
	  CASE curoptions.caseopt OF
	    upper: ch := UPPERCASE (ch);
	    lower: ch := LOWERCASE (ch);
	    OTHERS:
	  END;
	  nextch (* and get another for inspection *)
	END;
	inliteral: IF ch='''' THEN BEGIN (* must check for double prime *)
	  nextch;
	  IF ch='''' THEN nextch
	  ELSE curmode:= intext (* and save this character *)
	END
	ELSE nextch;
	incomment: IF ch='(' THEN BEGIN (* check for nested comment *)
	  nextch;
	  IF ch='*' THEN BEGIN
	    commentlvl:= commentlvl+1;
	    nextch
	  END (* if not nested, save character *)
	END
	ELSE IF ch='*' THEN BEGIN
	  nextch; (* check for end of comment *)
	  IF ch=')' THEN BEGIN
	    commentlvl:= commentlvl-1; (* insure decap works right *)
	    nextch; (* do this before setting curmode, so that cur char
							       is not seen as text *)
	    IF commentlvl<=0 THEN curmode:= intext;
	  END (* else, save this character *)
	END
	ELSE nextch;
	indirective: BEGIN
	  IF ch = cr THEN BEGIN (* assume directive not in middle *)
	    curmode := intext;
	    nextch;
	    IF inc_file_level < max_inc_file_nesting THEN IF (dir_type =
	      header_direct) AND (curoptions.list_headers)
	      THEN curmode := inheader
	    ELSE IF (dir_type = include_direct) AND (curoptions.list_includes)
	      THEN curmode := ininclude
	    ELSE IF NOT (dir_type IN [page_direct,no_direct]) THEN readch;
	    dir_type := no_direct (* reset the directive type *)
	  END
	  ELSE nextch
	END;
	inheader, ininclude: BEGIN
	  inc_file_level := inc_file_level + 1; (* increment inc_file_level *)
	  IF curmode = inheader THEN RESET (inc_files[ inc_file_level ].
	    inc_file, '.HDR ' || inc_filename, [ASCII])
	  ELSE RESET (inc_files[ inc_file_level ].inc_file, '.INC ' ||
	    inc_filename, [ASCII]);
	  IF EOF(inc_files[ inc_file_level ].inc_file) THEN BEGIN
	    WRITELN(TTY, 'File ',SUBSTR(inc_filename,1,LENGTH(inc_filename))
	      , ' is null or nonexistent.');
	    inc_file_level := inc_file_level - 1; (* increment inc_file_level *)
	    readch
	  END
	  ELSE BEGIN
	    inc_files[ inc_file_level ].save_line := inputline; (* save current line *)
	    inc_files[ inc_file_level ].save_idx := inputidx;
	    READ (inc_files[ inc_file_level ].inc_file, inputline); (* get the INCLUDE input *)
	    inputidx := 1;
	    curfile := include;
	    inc_lineno[ inc_file_level ] := 0;
	    startline;
	    readch (* get a new char from the include file *)
	  END;
	  curmode := intext;
	END
      END (* CASE *);
      IF (curfile = include) ANDIF EOF(inc_files[ inc_file_level ].inc_file)
	THEN BEGIN
	inc_lineno[ inc_file_level ] := 0;
	WITH inc_files[ inc_file_level ] DO BEGIN
	  CLOSE(inc_file);
	  inputline := save_line;
	  inputidx := save_idx;
	END (* with *);
	inc_file_level := inc_file_level -1;
	IF inc_file_level = 0 THEN curfile := main; (* current file is back to main file *)
	IF NOT EOF( INPUT ) THEN readch (* get a CH from the main file *)
      END
    END (* WHILE loop *);
    100 (* EOF abort *): IF (curoptions.outputopt = list) AND
      (curoptions.xref_flag) THEN dump_titles; (* print xref of page titles *)
    IF OUTPUT <> TTYOUTPUT THEN CLOSE (OUTPUT); (* close up this file *)
    CLOSE (INPUT);
  END; (* and go ask for another *)
  IF file_queued THEN BEGIN (* queue temp list file to printer *)
    REWRITE (OUTPUT, jobnum || 'QUE.TMP');
    WRITELN (OUTPUT, 'lpt:=', queue_file, queue_options);
    CLOSE (OUTPUT);
    IF NOT runprg ('sys:queue', 1) THEN BEGIN
      REWRITE(TTY);
      WRITELN (TTY, 'QUEUE fails.');
      BREAK;
    END
  END;
END.
   n û