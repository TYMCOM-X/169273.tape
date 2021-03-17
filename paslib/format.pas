program format;

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
$INCLUDE cmdutl.typ[31024,320156]
$INCLUDE lookup.typ[31024,320156]
$INCLUDE filutl.inc[31024,320156]
$INCLUDE dtime.typ[31024,320156]
$INCLUDE dtime.inc[31024,320156]
$INCLUDE pasdir.typ[31024,320156]
$INCLUDE pasdir.inc[31024,320156]
$INCLUDE swini.inc[31024,320226]
$INCLUDE RUN.INC[31024,320156]

EXTERNAL VAR AUTO_RUN: BOOLEAN;

external function jobnum: string[3];
$PAGE declarations
label 100,					(* EOF abort point *)
      150;					(* next command *)

const cr=chr(#O15);				(* "character" returned at EOLN by READCH *)
      tab=chr(#O11);
      maxtitle_length = 64;
      eoln_chars : set of char :=
                  [chr(#O15), chr(#O33), chr(#O32), chr(#O12), chr(#O7)];
      max_inc_file_nesting = 3;  (* same as current MDSI compiler - 5/19/82 *)

type						(* processing modes *)
  modes = (intext, incomment, inliteral, indirective, inheader, ininclude);
  title_type =  string [maxtitle_length];
  file_type = (main, include);			(* processing main or an include? *)
  cmdfile_type = (from_tty, from_cmd_file, from_auto_file);
  directive_type = (no_direct, page_direct, header_direct, include_direct);
  inc_file_rec = record    (* record used in include file nesting *)
                   inc_file : text;
                   save_line : string[ 255 ]; (* saves preceeding levels input line *)
                   save_idx  : 1..256;  (* save index into saved input line *)
                 end;

var
  ch: char;					(* current character, returned by READCH - NEXTCH *)
  curmode: modes;				(* current processing mode *)
  curfile: file_type;				(* type of file being processed *)
  cmdmode, oldcmdmode: cmdfile_type;			(* source of command inputs *)
  commentlvl: 0..100;				(* count of nested comments *)
  buffer: packed array[1..255] of char;		(* line output buffer *)
  inputline: string[255];			(* the input line *)
  inputidx: 1..256;				(* index into the above *)
  lineno: 0..99999;				(* line number in main file *)
  inc_lineno: array [1..max_inc_file_nesting] of 0..99999;
                                                (* keeps include file line numbers *)
  inc_files : array [1..max_inc_file_nesting] of inc_file_rec;
                                                (* array used in handling include file nesting *)
  inc_file_level : 0..max_inc_file_nesting;     (* current include file nesting level *)
  page_lineno: 0..99999;			(* line number reset every page *)
  phys_page: 0..99999;				(* count of physical pages *)
  dir_page: 0..99999;				(* count of logical pages *)
  in_page: 0..99999;				(* physical pages within logical page *)
  title_heading: title_type;			(* current title from most recent page directive *)
  saved_title: title_type := '';		(* current title to be printed *)
  dir_type: directive_type;			(* PAGE,HEADER,or INCLUDE directive? *)
  cmdfile, oldcmdfile: text;				(* non-user command file *)

  (* below are reset on a per line basis *)

  bufidx: 0..255;				(* index within buffer of CH *)
  col: 0..255;					(* leftmost logical text column position of CH *)
  firstidx: 0..255;				(* index of first non-white character *)
  firstcol: 0..255;				(* logical column position of same *)
  textidx: 0..255;				(* index of last char of program text or literal *)
  textcol: 0..255;				(* logical column of same *)
  commentidx: 0..255;				(* index of start "(" of last comment on line *)
  commentcol: 0..255;				(* column position of same *)

  (* state vars controlling processing of multi-line comments *)

  splitcomment: boolean;			(* set when eoln appears within comment *)
  commentadjustment: -255..255;			(* number of columns to offset subsequent lines of comment
						   if column position of first is changed *)

$PAGE command declarations
type						(* oprocessing options *)
  outputopts =	(  list,			(* output to .LST file *)
		   edit,			(* replace original *)
		   newop	  );		(* output to .TMP file *)

  caseopts =	(  upper,			(* uppercase program text *)
		   lower,			(* lowercase it *)
		   pass	  );			(* leave it alone *)

  options_type =
      record					(* complete option list *)
	caseopt: caseopts;
	outputopt: outputopts;
	column: 0..255;				(* column in which to align comments *)
	decapflag: boolean;			(* flags decapitalization processing in comments *)
	appendflag: boolean;			(* append output to file *)
	xref_flag: boolean;			(* generate cross reference, if listing *)
	pagelength: 0..255;			(* length of listing page *)
	list_headers: boolean;			(* list $HEADER file contents *)
	list_includes: boolean;			(* list $INCLUDE file contents *)
	echo_cmds: boolean;			(* echo the commands in CMD files *)
	fid_qualifier: file_id;			(* default extension, ppn, etc. *)
	input_defaults: file_id (* default input file name components *)
      end;

var
  line: cmdline;				(* command line to interpret *)
  f_name: file_id;				(* file to process *)
  inc_filename: file_id;			(* include file name *)
  quit_flag, error_flag: boolean; (* returned by command line processor *)
  defaultoptions: options_type;			(* default option set *)
  curoptions: options_type;				(* options_type in effect for processing of a single file *)
  file_queued: boolean;				(* indicates output to 'LPT:' *)
  queue_file: file_id;				(* name of queue file *)
  queue_options: string;			(* special options_type for queue *)
  switch_line: sw_ini_string; (* receives text from SWITCH.INI lines *)

const
    initial_defaults: options_type =
     (  upper, list, 49, false, false, false, 45, false, false,
	false, '', '.PAS'  );
$PAGE command name list
type
  commands = ( listcmd, editcmd, newcmd, uppercmd, lowercmd, passcmd,
	       columncmd, lengthcmd, quitcmd, decapcmd, nodecapcmd,
	       xrefcmd, noxrefcmd, appendcmd, noappendcmd, queueopt,
	       headercmd, noheadcmd, optioncmd, nooptcmd, defcmd,
	       includecmd, noincludecmd, echocmd, noechocmd );

  cmdnames = array[commands] of cmdlist;

var cmd: commands;				(* command to dispatch on *)

const
   cmdnamelist: cmdnames :=
     (	( 'LIST',	4 ),
	( 'EDIT',	4 ),
	( 'NEW',	3 ),
	( 'UPPERCASE',	5 ),
	( 'LOWERCASE',	5 ),
	( 'PASS',	4 ),
	( 'COLUMN',	3 ),
	( 'LENGTH',	3 ),
	( 'QUIT',	4 ),
	( 'DECAP',	1 ),
	( 'NODECAP',	3 ),
	( 'XREF',	1 ),
	( 'NOXREF',	3 ),
	( 'APPEND',	1 ),
	( 'NOAPPEND',   3 ),
	( 'QUEUE',	3 ),
	( 'HEADER',	4 ),
	( 'NOHEADER',	3 ),
	( 'OPTION',	3 ),
	( 'NOOPTION',	3 ),
	( 'DEFAULT',	3 ),
	( 'INCLUDE',	3 ),
	( 'NOINCLUDE',	3 ),
	( 'ECHO',	3 ),
	( 'NOECHO',	3 )  );

external function lookup_cmdnames		(* specific declaration for this scalar type *)
     (	line: cmdline;
	var idx: cmdlineidx;
	list: cmdnames;
	maxcommands: commands;
	var nameidx: commands	): boolean;
$PAGE big letter printing
const
    page_width = 100;
    char_width = 5;
    ch_desc_size = 35;				(* = char_width * char_height *)
    hspace = 3;					(* horizontal space between characters *)
    max_chars = 12;				(* max characters / row *)


type
    prt_chars = ' ' .. '_';			(* ascii columns 2 - 5 *)
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
    hfill: 1 .. page_width;
    len: 0 .. max_chars;
    i: 1 .. max_chars;

begin
    len := length(r);
    if len = 0 then return;
    hfill := (page_width + hspace - (char_width+hspace)*len) div 2;
    ind := 0;
    while ind <> ch_desc_size do begin
	write (output,' ':hfill);
	for i := 1 to len do begin
	    write (output,substr(rep[r[i]],ind+1,char_width));
	    if i <> len then write (output,' ':hspace);
	end;
	writeln (output);
	ind := ind + char_width;
    end;
end;
$PAGE list_header
procedure list_header;

 type
  day_array = array [week_day] of string [3];

 const
  day_name: day_array := ( 'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat' );

 var
  fname: file_id;
  i: 1 .. 4;
  ecode: dir_errors;
  file_desc: dir_attrs;
  title_line: string [105];

 begin
  f_name := filename (input);			(* get actual name of file *)
  dir_attr (ecode, f_name, file_desc);
  fname := substr (f_name, index (f_name, ':') + 1);    (* strip dev and ppn *)
  fname := substr (fname, 1, index (fname, '[') - 1);

  title_line := 'File ' || f_name || '   ' ||
		day_name [day_of_week (extr_date (file_desc.creation))] ||
		' ' || substr (dc_ext (file_desc.creation), 1, 15) ||
		'   MDSI, Company Confidential   FORMAT Version 1.9  ';

  for i := 1 to 4 do begin  writeln (output, title_line); writeln (output)  end;
  for i := 1 to 4 do writeln (output);
  print_row (fname);
  for i := 1 to 4 do writeln (output);
  for i := 1 to 4 do begin  writeln (output, title_line); writeln (output)  end;
end;
$PAGE startline
  procedure startline;				(* init per line information *)
  begin
    bufidx := 0;
    col := 1;
    firstidx := 0;
    firstcol := 0;
    textidx := 0;
    textcol := 0;
    commentidx := 0;
    commentcol := 0;
  end;
$PAGE cvt

type
    page_num = 0 .. 99999;
    cvt_str = string [5];

function cvt(page: page_num): cvt_str;
 begin
  if page = 0 then
    cvt := ''
  else
    cvt := cvt(page div 10) || chr(ord('0') + (page mod 10));
 end;
$PAGE do_header, do_page

procedure do_header(title: title_type);
 begin
  write (output, 'SECTION ', dir_page:4);
  if in_page = 0
    then write (output, ' ':4)
    else write (output, '-', cvt(in_page) || '   ':3);
  writeln (output, ' ', title, ' ':90-8-4-4-1-length(title)-5-4,
	   'PAGE ', phys_page:4);
  writeln (output);
 end;



procedure do_phys_page(title: title_type);
 begin
  page (output);
  phys_page := phys_page + 1;
  if phys_page <> 1 then
    in_page := in_page + 1;
  if curoptions.xref_flag then do_header(title)
 end;



procedure do_dir_page(title: title_type);
 begin
  page(output);
  dir_page := dir_page + 1;
  phys_page := phys_page + 1;
  in_page := 0;
  if curoptions.xref_flag then do_header(title)
 end;
$PAGE record_title
(* These routines keep an alphabetized list of all of the "$page" titles seen,
   along with their physical page number.  When XREF is selected this list is
   dumped at the end of the listing. *)

type 
  title =
      record
	phys_page_num: 0..999999;
	dir_page_num: 0..999999;
	next: ^ title;
	text: title_
      end;

var first_title: ^ title;


(* RECORD TITLE appends a title to the list.  The page number is taking from the
   current page numbers in phys_page and dir_page. *)

procedure record_title ( title_text: string );
 var ntitle, lt, t: ^ title;
 begin
  if title_text = '' then return;
  new (ntitle);
  ntitle^.text := title_text;
  ntitle^.phys_page_num := phys_page;
  ntitle^.dir_page_num := dir_page;
  t := first_title;
  lt := nil;
  while t <> nil do begin
  exit if title_text < t^.text;
    lt := t;
    t := t^.next;
  end;
  ntitle^.next := t;
  if lt = nil
    then first_title := ntitle
    else lt^.next := ntitle;
 end;


$PAGE dump_titles
(* DUMP TITLES is called after the entire file has been listed, and prints
   the alphabetized list of titles, with their page numbers. *)

procedure dump_titles;
 var n, t: ^ title;
 begin
  writeln (output, 'SECTION  ', 'TITLE':10, ' ':60, '  PAGE');
  writeln (output);
  lineno := 3;
  t := first_title;
  while t <> nil do begin
    writeln (output, t^.dir_page_num:6, ' ':3, t^.text, t^.phys_page_num:6);
    lineno := lineno + 1;
    if (lineno mod curoptions.pagelength) = 1 then
    begin
      page(output);
      lineno := 1
    end;
    n := t^.next;
    dispose (t);
    t := n;
  end;
  page (output);
 end;
$PAGE flush
  (* FLUSH massages the last line read then outputs it. It canonicalizes
     leading white space, aligns trailing comments in a designated column,
     and lines up multi-line comments under the first line. *)

  procedure flush;
  var pad: string[255];
      blanks: 0..8;
      newcol: 0..255;
      newlen: 0..255;
      idx: 0..255;
      title_length: 0..255;
      i: 0..255;

  begin
    (* If we have a multi-line comment, align subsequent lines up under the
       the first line of the comment, by forcing FIRSTCOL to the desired
       value; canonicalization then aligns the text properly. Note: this is
       only done to trailing comments that are split across lines, since
       other comments are never moved. *)

    if splitcomment and (firstcol <> 0) then begin
      firstcol := firstcol + commentadjustment;
      if firstcol <= 0 then firstcol := 1;	(* avoid blowing up on a sick looking program *)
    end;
    if curmode <> incomment then splitcomment := false;	(* turn off adjustment when comment done *)

    (* Canonicalize leading white space *)

    if firstcol > 0 then begin			(* only work on non-blank line *)
      pad := '';				(* get minimal tab-blank form *)
      for idx := 1 to ((firstcol - 1) div 8)
	do pad := pad || tab;			(* use as many tabs as possible *)
      pad := pad || substr ('        ', 1, (firstcol - 1) mod 8);
						(* then blanks *)
      newlen := length (pad) + (bufidx - firstidx + 1);
      if newlen <= length (buffer) then begin	(* operate if there is room *)
	buffer := pad || substr (buffer, firstidx, (bufidx - firstidx + 1));
	if textidx > 0 then			(* adjust idx's to reflect change *)
	  textidx := textidx - (firstidx - length (pad) - 1);
	if commentidx > 0 then
	  commentidx := commentidx - (firstidx - length (pad) - 1);
	bufidx := newlen			(* update buffer length *)
      end
    end
    else bufidx := 0;				(* effective null, truncate *)

    (* Align a trailing comment if it is not the only thing on the line. *)

    if commentidx > textidx			(* comment follows last text *)
      then if textidx > 0			(* comment is not alone on the line *)
	then begin
	  newcol := curoptions.column;		(* select column in which to align comment *)
	  while newcol <= (textcol + 1)		(* use standard or first MOD 4 stop after text *)
	    do newcol := newcol + 4;
	  pad := '';				(* get chars to pad between text and comment *)
	  blanks := 0;
	  textcol := textcol + 1;		(* pad chars begin in column after last text *)
	  repeat
	    textcol := textcol + 1;
	    blanks := blanks + 1;
	    if (textcol mod 8) = 1 then begin	(* can use TAB *)
	      pad := pad || tab;
	      blanks := 0;			(* spaces aren't needed *)
	    end
	  until textcol = newcol;
	  pad := pad || substr ('        ', 1, blanks);

	  newlen := textidx + length (pad) + (bufidx - commentidx + 1);
	  if newlen <= length (buffer) then begin   (* if adjusted line not too long, adjust *)
	    buffer := substr (buffer, 1, textidx) || pad ||
			substr (buffer, commentidx, (bufidx - commentidx + 1));
	    bufidx := newlen
	  end;

	  if curmode = incomment then begin	(* comment extends across line boundary *)
	    splitcomment := true;		(* set to line up following lines *)
	    commentadjustment := newcol - commentcol
	  end;
	end (* alignment code *) ;

    (* Output processed line *)

    if curoptions.outputopt = list then begin
      dir_type := no_direct;		(* assume not a directive line until check *)
      if (bufidx >= 5) andif (substr (buffer, 1, 5) = '$PAGE')
	then begin
	  dir_type := page_direct;
	  title_length := bufidx - 5;
	  title_heading := substr(buffer, 6);
	  for i := 1 to length(title_heading) do  (* get rid of tabs *)
	    if title_heading[i] = tab then title_heading[i] := ' ';
	  if verify (title_heading, [' ']) <> 0 then begin  (* make sure there is a title *)
	    saved_title := title_heading;
	    do_dir_page(title_heading);
	    record_title (title_heading);
	    if curfile = main then lineno := 1
	    else inc_lineno[ inc_file_level ] := 1
	    end
	  else begin			(* no title -- simple page eject *)
	    do_phys_page (saved_title);
	    if curfile = main then lineno := lineno + 1
	    else inc_lineno[ inc_file_level ] := inc_lineno[ inc_file_level ] + 1
	    end;
	  page_lineno := 0
	end
	else begin
	  if curfile = main then lineno := lineno + 1
	  else inc_lineno[ inc_file_level ] := inc_lineno[ inc_file_level ] + 1;
	  page_lineno := page_lineno + 1
	  end;
       if inc_file_level < max_inc_file_nesting then begin
	 if (bufidx >= 7) andif (substr(buffer,1,7) = '$HEADER') then
	   dir_type := header_direct
	 else if (bufidx >=8) andif (substr(buffer,1,8) = '$INCLUDE') then
		dir_type := include_direct;
	 inc_filename := substr(buffer, index(buffer,' ',length(buffer)) )
       end;
      if page_lineno > 1 then
	if curoptions.pagelength <> 0 then
	  if (page_lineno mod curoptions.pagelength) = 1 then
	    do_phys_page(title_heading);
      if dir_type <> page_direct then
	if curfile = main then write (lineno:5, tab)
	else write (inc_lineno[ inc_file_level ]:5, '*', tab)
    end;
    if dir_type <> page_direct then writeln (buffer:bufidx)
  end;
$PAGE readch
  (* READCH simply reads the next character from the file and assigns
     its buffer position. *)

  procedure readch;
   begin
     if (inputidx > length(inputline)) orif (inputline[inputidx] in eoln_chars) then
	  ch := cr				(* use CR to signify end of line *)
     else begin
       ch := inputline[inputidx];
       inputidx := inputidx + 1;

       (* note that we are not really in comment unless curmode=incomment
	  and commentlvl>0.  This is because code sets curmode when
	  examining the second character of a possible comment terminator
	  or initiator for reasons documented elsewhere.  However, it
	  sets commentlvl appropriately when it is sure it is in or out. *)

       if curoptions.decapflag and ( (curmode=indirective) or
	(commentlvl>0) (* implying curmode=incomment *) ) then
	  if ch<>'@' then ch:= lowercase(ch)
	  else if (inputidx > length(inputline)) orif
		  (inputline[inputidx] in eoln_chars) then
		    ch := cr			(* implicitly eating the at-sign *)
	  else begin
	    ch := inputline[inputidx];
	    inputidx := inputidx + 1;
	    ch:= uppercase(ch)
	  end;
       if ch<>cr then bufidx := bufidx + 1
     end
   end;
$PAGE nextch
  (* NEXTCH processes the current character, CH, then reads the next 
     character from the file. The processing of a character is done after
     it is read and passed up to the mode setting loop, so that it is
     treated for what it is recognized to be, not for what the previous
     mode was. Calculation of the new logical column position must also
     be done after column values have been recorded for the current char. *)
  procedure nextch;
  begin
    if ch <> cr then begin
      if bufidx <= length (buffer)		(* save char just processed *)
	then buffer[bufidx] := ch;

      (* Record last text position if char is non-white and in program text or
	 literal. Note that blanks in literals are not recorded, but since they
	 must be followed by an ending "'", everything works out. *)

      if curmode in [intext, inliteral] then
	if ord (ch) > ord (' ') then begin
	  textcol := col;
	  textidx := bufidx
	end;

      (* Record first non-white character position if applicable. *)

      if ord (ch) > ord (' ')			(* char is non-white *)
	then if firstcol = 0 then begin		(* and is first non-white *)
	  firstidx := bufidx;
	  firstcol := col
	end;

      (* Update logical column position by advancing COL by the number of
	 columns occupied by this character. *)

      col := col + 1;				(* assume it is non-control char *)
      if ord (ch) < ord (' ')			(* check for control chars *)
	then begin
	  if ch = tab
	    then while (col mod 8) <> 1 do col := col + 1
	    else col := col - 1			(* we had allowed one column for char, undo *)
	end
    end

    else begin					(* CR last char, get next line *)
      flush;
      if curfile = main then begin
	readln(input);
	read(input, inputline)
	end
      else begin  (* we are in an include file *)
	readln(inc_files[ inc_file_level ].inc_file);
	read(inc_files[ inc_file_level ].inc_file, inputline)
	end;
      inputidx := 1;

      startline;
      if ( (curfile = main) andif eof(input) ) then begin	(* cleanup *)
	if list = curoptions.outputopt then page (output);
	goto 100				(* exit mainline while not EOF loop *)
      end;
    end;

    if dir_type in [no_direct, page_direct] then readch (* get the next character *)
  end (*NEXTCH*);

$PAGE read_command_line
procedure read_command_line (var line: cmdline);

var startidx: cmdlineidx;

begin
  loop
    loop
      exit if cmdmode = from_tty do begin
	write (tty, '*');
	break;
	readln (tty);
	read (tty, line);
      end;

      line := '';

      exit if (cmdmode = from_auto_file) and eof (cmdfile) do
	scratch (cmdfile);

      if eof (cmdfile) then begin (* must be from real command file *)
	close (cmdfile);
	cmdfile := oldcmdfile;
	cmdmode := oldcmdmode;
      end
      else begin
	repeat
	  readln (cmdfile, line);
	until (line <> '') or eof (cmdfile);
      end;

      exit if line <> '' do
	if curoptions.echo_cmds then begin
	  writeln (tty, '[', line, ']');  break  end;
    end (* loop *);

    startidx := verify (line, [' ']);
    exit if (startidx = 0) orif (line[startidx] <> '@');

    if cmdmode = from_cmd_file then
      writeln (tty, 'Nested command files not allowed')
    else begin
      line := substr (line, startidx + 1);
      oldcmdmode := cmdmode;
      oldcmdfile := cmdfile;
      cmdmode := from_cmd_file;
      reset (cmdfile, '.CCL ' || line);
      if eof(cmdfile) then begin
	close(cmdfile);
	reset (cmdfile, '.CMD ' || line);
	if eof(cmdfile) then begin
	  close(cmdfile);
	  cmdmode := oldcmdmode;
	  cmdfile := oldcmdfile;
	  writeln (tty, 'Unable to open command file ', line);
	end;
      end;
    end;
  end (* loop *);
end;
$PAGE process_command_line

(* PROCESS COMMAND LINE is called with a command line.  It processes any
   switches in the input line, and returns the input file name, a quit
   flag, and an error flag. *)

procedure process_command_line ( line: cmdline;
				 var f_name: file_id;
				 var quit, error: boolean );

var
    lindex, idx: cmdlineidx;
$PAGE command utilities
procedure skipblanks;
 begin
  while (lindex <= length (line)) andif (ord (line[lindex]) <= ord (' '))
    do lindex := lindex + 1
 end;


function checkpunct ( ch: char ): boolean;
 begin
  skipblanks;
  checkpunct := false;
  if (lindex <= length (line)) andif (line[lindex] = ch)
    then begin
      lindex := lindex + 1;
      checkpunct := true
    end
 end;


type getnumtype = 0..255;

function getnum ( var num: getnumtype ): boolean;
 begin
  num := 0; getnum := false;
  skipblanks;
  while (lindex <= length (line)) andif (line[lindex] in ['0'..'9']) do begin
    getnum := true;
    num := num * 10 + (ord (line[lindex]) - ord ('0'));
    lindex := lindex + 1
  end
 end;


type getnametype = string [6];

function getname ( var name: getnametype ): boolean;
 begin
  name := ''; getname := false;
  skipblanks;
  while (lindex <= length(line)) andif (uppercase(line[lindex]) in ['A'..'Z', '0'..'9']) do begin
    getname := true;
    name := name || uppercase (line[lindex]);
    lindex := lindex + 1;
  end;
 end;
$PAGE process_command_line mainline
 label 150, 200;

 var
  opt_name: sw_ini_name;
  end_quote: char;
  switch_line: sw_ini_string;
  fid_specified: boolean;
  filetemp, fidtemp: file_id;
  defaultname: file_id;

 begin
  error := false;
  quit := false;
  if line = '' then
    goto 200;
  curoptions := defaultoptions;		(* assume the defaults and look for options *)

  lindex := index (line, '/', length (line) + 1); (* get filename *)
  f_name := substr (line, 1, lindex - 1);

  idx := index (f_name, '=');		(* see if there is output info *)
  if idx = 0 then
    fid_specified := false
  else begin
    f_name := substr (line, idx + 1, lindex-idx-1);
    curoptions.fid_qualifier := substr (line, 1, idx - 1);
    fid_specified := true;
  end;

  if lindex <= length (line) then begin	(*  a '/' was found *)
    lindex := lindex + 1;			(* position after the slash *)
    while lindex <= length (line) do begin	(* loop over options *)
      if not lookup_cmdnames (line, lindex, cmdnamelist, maximum (commands), cmd)
	then begin
	  writeln (tty, 'Bad option.');
	  goto 150
	end
	else case cmd of

	  decapcmd:   curoptions.decapflag := true;
	  nodecapcmd: curoptions.decapflag:= false;

	  quitcmd:    goto 200;

	  listcmd:    curoptions.outputopt := list;
	  editcmd:    curoptions.outputopt := edit;
	  newcmd:     curoptions.outputopt := newop;

	  uppercmd:   curoptions.caseopt := upper;
	  lowercmd:   curoptions.caseopt := lower;
	  passcmd:    curoptions.caseopt := pass;

	  appendcmd:  curoptions.appendflag := true;
	  noappendcmd:curoptions.appendflag := false;

	  xrefcmd:    curoptions.xref_flag := true;
	  noxrefcmd:  curoptions.xref_flag := false;

	  headercmd:  curoptions.list_headers := true;
	  noheadcmd:  curoptions.list_headers := false;

	  includecmd: curoptions.list_includes := true;
	  noincludecmd: curoptions.list_includes := false;

	  echocmd: curoptions.echo_cmds := true;
	  noechocmd: curoptions.echo_cmds := false;

	  defcmd:     begin
			if checkpunct (':') orif checkpunct ('=') then;
			if checkpunct ('"') then
			  end_quote := '"'
			else if checkpunct ('''') then
			  end_quote := ''''
			else begin
			  writeln (tty, 'File name defaults expected.');
			  goto 150;
			end;
			idx := index (substr (line, lindex), end_quote);
			if idx = 0 then begin
			  writeln (tty, 'File name defaults not terminated.');
			  goto 150;
			end;
			curoptions.input_defaults := substr (line, lindex, idx-1);
			lindex := lindex + idx;
		      end;

	  queueopt:   begin
			if checkpunct (':') orif checkpunct ('=') then ;
			if checkpunct ('"') then
			  end_quote := '"'
			else if checkpunct ('''') then
			  end_quote := ''''
			else
			  end_quote := ' ';
			if end_quote = ' ' then begin
			  if not getname (opt_name) then begin
			    writeln (tty, 'Queue options expected.');
			    goto 150;
			  end;
			  if sw_ini ('PRINT', opt_name, switch_line) orif
			    sw_ini ('QUEUE', opt_name, switch_line) then
			      queue_options := switch_line
			      else begin
				writeln (tty, 'Print option ', opt_name,
					 ' not in SWITCH.INI');
				goto 150;
			      end;
			end
			else begin
			  idx := index (substr (line, lindex), end_quote);
			  if idx = 0 then begin
			    writeln (tty, 'Quote options not terminated.');
			    goto 150;
			  end;
			  queue_options := substr (line, lindex, idx-1);
			  lindex := lindex + idx;
			end;
		      end;

	  optioncmd:  begin
			if checkpunct (':') orif checkpunct ('=') then ;
			if not getname (opt_name) then begin
			  writeln (tty, 'Option name expected.');
			  goto 150;
			end;
			if sw_ini ('FORMAT', opt_name, switch_line) then begin
			  fidtemp := curoptions.fid_qualifier;
			  process_command_line (switch_line, filetemp, quit, error);
			  if fid_specified then
			    curoptions.fid_qualifier := fidtemp;
			  if error then
			    writeln (tty, '  -- in SWITCH.INI:', opt_name);
			  if error or quit then
			    return;
			end
			else begin
			  writeln (tty, 'FORMAT option ', opt_name, 'not in SWITCH.INI');
			  goto 150;
			end;
		      end;

	  nooptcmd:   begin
			fidtemp := curoptions.fid_qualifier;
			curoptions := initial_defaults;
			if fid_specified then
			  curoptions.fid_qualifier := fidtemp;
			if sw_ini ('PRINT', '', switch_line) orif
			  sw_ini ('QUEUE', '', switch_line)
			    then queue_options := switch_line
			    else queue_options := '';
		      end;

	  lengthcmd:  begin
			if checkpunct (':') then ;
			if checkpunct ('=') then ;
			if getnum ( curoptions.pagelength )
			  then begin
			    if curoptions.pagelength <= 10 then begin
			      writeln (tty, 'Value too small.');
			      goto 150
			    end
			  end
			  else begin		(* no value given *)
			    writeln (tty, 'Page length expected.');
			  end
		      end;

	  columncmd:  begin
			if checkpunct (':') then ;
			if checkpunct ('=') then ;
			if getnum ( curoptions.column )
			  then begin		(* check for reasonable value *)
			    if curoptions.column <= 20 then begin
			      writeln (tty, 'Value too small.');
			      goto 150
			    end
			    else if (curoptions.column mod 8) <> 1 then begin
			      writeln (tty, 'Warning - not a tab stop.');
			      break
			    end
			  end
			  else begin		(* no column value *)
			    writeln (tty, 'Column position expected.');
			    goto 150
			  end
		      end
	end (* case cmd *) ;
      if checkpunct (',') then ;
      if checkpunct ('/') then ;
    end					(* option loop *) 
  end;
  return;

150:
  error := true;
  return;

200:
  quit := true;
  return;
 end;
$PAGE list mainline
begin

  open(tty); rewrite(tty);
  writeln (tty, 'FORMAT, Version 1.9');
  title_heading := '                             ';
  defaultoptions := initial_defaults;
  file_queued := false;
  if sw_ini ('PRINT', '', switch_line) orif
    sw_ini ('QUEUE', '', switch_line)
      then queue_options := switch_line
      else queue_options := '';
  if sw_ini ('FORMAT', '', switch_line) then begin
    process_command_line (switch_line, f_name, quit_flag, error_flag);
    if error_flag then
      writeln (tty, '  -- in SWITCH.INI');
    defaultoptions := curoptions;
  end;
  cmdmode := from_tty;
  if auto_run then begin
    reset (cmdfile, jobnum || 'FMT.TMP');
    if not eof (cmdfile) then
      cmdmode := from_auto_file;
  end;

  loop						(* through files *)
150 (* next command *):

    read_command_line (line);
    process_command_line (line, f_name, quit_flag, error_flag);
  exit if quit_flag;
    if error_flag then
      goto 150;

    if f_name = '' then begin			(* default option setting *)
      defaultoptions := curoptions;
      goto 150
    end;


    (* Have filename and options - go to it *)

    reset(input, curoptions.input_defaults||' '||f_name, [ASCII]);
    if eof (input) then begin			(* non existent file *)
      writeln (tty, 'File ',substr(f_name,1,length(f_name)),
		    ' is null or nonexistent.');
      goto 150
    end;
    read (input, inputline); inputidx := 1;

    if uppercase (curoptions.fid_qualifier) = 'LPT:' then begin (* queue result to printer *)
      curoptions.fid_qualifier := 'DSK:' || jobnum || 'FMT.LST';
      curoptions.appendflag := true;
      if not file_queued then begin
	rewrite (output, curoptions.fid_qualifier);
	queue_file := filename (output);
	close (output);
      end;
      file_queued := true;
    end;

    case curoptions.outputopt of		(* get suffix for output file *)
      list:  curoptions.fid_qualifier := '.lst[] ' || curoptions.fid_qualifier;
      edit:  ;					(* will go in original *)
      newop:   curoptions.fid_qualifier := '.tmp[] ' || curoptions.fid_qualifier;
      others:
    end;

    if curoptions.appendflag then
      rewrite(output, '.PAS ' ||f_name|| ' ' || curoptions.fid_qualifier, [PRESERVE])
    else rewrite(output, '.PAS ' ||f_name|| ' ' || curoptions.fid_qualifier);

    (* set up recording of pages if in xref mode *)

    dir_page := 0;
    phys_page := 0;
    in_page := 0;
    title_heading := '';
    curfile := main;
    if curoptions.xref_flag then begin
      if curoptions.pagelength > 2 then
	curoptions.pagelength := curoptions.pagelength - 2; (* adjust for page header *)
    end;
    (* If listing generate a header page *)

    if curoptions.outputopt = list then begin
      list_header;
      page (output);
      list_header;
      do_phys_page(title_heading);
    end;

    commentlvl:= 0;
    curmode:= intext;
    startline; readch;				(* read first char *)
    lineno := 0;
    inc_file_level := 0;                  (* level zero means in main file *)
    page_lineno := 0;
    splitcomment := false;
    first_title := nil;

    while ( not eof(input) ) orif ( curfile = include )  do begin

      case curmode of

      intext:
	if ch='''' then begin
	  curmode:= inliteral;			(* go into literal mode *)
	  nextch
	end
	else if ch='(' then begin
	  curmode := incomment;			(* assume so that char not recorded as text *)
	  nextch;
	  if ch='*' then begin			(* go into comment mode *)
	    commentidx := bufidx - 1;
	    commentcol := col - 1;
	    commentlvl:= 1;
	    nextch;
	    if ch = '$' then begin
	      nextch;
	      if uppercase (ch) in ['X', 'Y'] then begin
		curmode := intext;
		textidx := bufidx - 3;
	      end;
	    end;
	  end
	  else begin				(* paren really was text *)
	    curmode := intext;
	    textidx := bufidx - 1
	  end
	end
	else if (ch = '$') and (bufidx = 1) then begin
	  repeat				(* make sure directive keyword is in uppercase *)
	    ch:= uppercase(ch);
	    nextch
	  until not (uppercase(ch) in ['A'..'Z']);
	  curmode:= indirective			(* now go into directive mode *)
	end
	else begin				(* uppercase the character *)
	  case curoptions.caseopt of
	    upper: ch := uppercase (ch);
	    lower: ch := lowercase (ch);
	    others:
	  end;
	  nextch				(* and get another for inspection *)
	end;

      inliteral:
	if ch='''' then begin			(* must check for double prime *)
	  nextch;
	  if ch='''' then nextch
	  else curmode:= intext			(* and save this character *)
	end
	else nextch;

      incomment:
	if ch='(' then begin			(* check for nested comment *)
	  nextch;
	  if ch='*' then begin
	    commentlvl:= commentlvl+1;
	    nextch
	  end					(* if not nested, save character *)
	end
	else if ch='*' then begin
	  nextch;				(* check for end of comment *)
	  if ch=')' then begin
	    commentlvl:= commentlvl-1;		(* insure decap works right *)
	    nextch;				(* do this before setting curmode, so that cur char
						   is not seen as text *)
	    if commentlvl<=0 then curmode:= intext;
	  end					(* else, save this character *)
	end
	else nextch;

    indirective:
      begin
	if ch = cr then begin	(* assume directive not in middle *)
	  curmode := intext;
	  nextch;
          if inc_file_level < max_inc_file_nesting  then 
	    if (dir_type = header_direct) and (curoptions.list_headers) then
	      curmode := inheader
	    else if (dir_type = include_direct) and (curoptions.list_includes) then
		   curmode := ininclude
		 else if not (dir_type in [page_direct,no_direct]) then readch;
	    dir_type := no_direct		(* reset the directive type *)
	end
	else nextch
      end;

    inheader, ininclude:
      begin
	inc_file_level := inc_file_level + 1; (* increment inc_file_level *)
	if curmode = inheader 
	  then reset (inc_files[ inc_file_level ].inc_file, '.HDR ' || inc_filename, [ASCII])
	  else reset (inc_files[ inc_file_level ].inc_file, '.INC ' || inc_filename, [ASCII]);
	if eof(inc_files[ inc_file_level ].inc_file) then begin
	  writeln(tty, 'File ',substr(inc_filename,1,length(inc_filename)),
		       ' is null or nonexistent.');
	  inc_file_level := inc_file_level - 1; (* increment inc_file_level *)
	  readch
	  end
	else begin
          inc_files[ inc_file_level ].save_line := inputline; (* save current line *)
          inc_files[ inc_file_level ].save_idx := inputidx;
	  read (inc_files[ inc_file_level ].inc_file, inputline);     (* get the INCLUDE input *)
	  inputidx := 1;
	  curfile := include;
	  inc_lineno[ inc_file_level ] := 0;
	  startline; readch		(* get a new char from the include file *)
	end;
	curmode := intext;
      end

      end (* CASE *);

      if (curfile = include) andif eof(inc_files[ inc_file_level ].inc_file) then begin
	inc_lineno[ inc_file_level ] := 0;
        with inc_files[ inc_file_level ]  do begin
	  close(inc_file);
	  inputline := save_line;
	  inputidx := save_idx;
        end (* with *);
        inc_file_level := inc_file_level -1;
        if inc_file_level = 0
           then curfile := main;  (* current file is back to main file *)
	if not eof( input )  then readch    (* get a CH from the main file *)
	end
    end (* WHILE loop *);

100 (* EOF abort *) :
    if (curoptions.outputopt = list) and (curoptions.xref_flag)
      then dump_titles;				(* print xref of page titles *)
    if output <> ttyoutput then close (output); (* close up this file *)
    close (input);

  end;						(* and go ask for another *)

  if file_queued then begin			(* queue temp list file to printer *)
    rewrite (output, jobnum || 'QUE.TMP');
    writeln (output, 'lpt:=', queue_file, queue_options);
    close (output);
    run ('sys:queue', true);
    writeln (tty, 'QUEUE fails.'); break;
  end;
end.
 G 