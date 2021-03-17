$TITLE SCNLST -- SCANNR Listing Package

module scnlst;

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         S C N L S T                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  SCANNR Lexical Scanner Builder
     
     STARTED:  5 October 1979
     
     PURPOSE:   This  module  contains   the   routines   which   are
        responsible for creating the listing file.
     
     ENTRY POINTS:
     
        TITLE       is  a  pointer  to  a  string node containing the
                    title string for this scanner.  Title is  set  by
                    the parser when it processes the "SCANNER <name>"
                    statement.
     
        LISTFB      is the fio  file  block  for  the  listing  file.
                    Other modules may use this file block to write on
                    the listing file.
     
        LIST_INPUT  is called when the input file has been  read  and
                    parsed.  It  prints  any  error  messages (on the
                    terminal and the listing file),  and  prints  any
                    requested  listings,  such  as the input listing,
                    the action listing, and  the  regular  expression
                    listing.  At  this  point,  we  assume  that file
                    Output has been opened for the listing file.
     
        ECHO_LINE   is an entry point for the error module.  When the
                    error  print  routine  (ErrPrint) determines that
                    there are error  messages  to  be  printed  on  a
                    specified  input line, it calls EchoLine to print
                    that line on the terminal.
     
        PRT_MESSAGE (message)
                    is called to print a specified  message  on  both
                    the listing file and the terminal.  It prints the
                    message  at  the   "current   indentation";   for
                    example,  input  listing  messages  are  indented
                    eight spaces to allow for line numbers.
     
        LIST_RECOGNIZER (pat_num, trans_mat, ch_vec, accept )
                    will print the recognizer  FSA  for  a  specified
                    pattern.  The  transition  matrix, characteristic
                    regular expression vector and  acceptance  vector
                    are parameters of the call.
     
        LIST_SCANNER (trans_mat, ch_vec, accept)
                    will  print  the  composite  scanner  FSA for the
                    complete set of input  patterns.  The  transition
                    matrix,  characteristic  state  list  vector  and
                    acceptance vector are parameters of the call.
     
     ---------------------------------------------------------------- *)
$PAGE declarations

$SYSTEM scannr.typ
$SYSTEM scnlit.typ
$SYSTEM scnfsa.typ

$SYSTEM fio
$SYSTEM scnnam
$SYSTEM scnerr
$SYSTEM scnpat
$SYSTEM scnreu

$SYSTEM dtime.typ
$SYSTEM dtime.inc


public var
    title: str_ptr;
    listfb: file_block;

var cur_line_no: number;
    cur_line: string [line_size];
    listing: boolean;

    dt: dtime_int;
    sub_title: string [line_size];
$PAGE print_row

(*  PRINT ROW will print one banner row of characters on the listing file.  *)

procedure print_row ( row: parm_string );

const
  char_width = 5;
  ch_desc_size = 35;                          (* = char_width * char_height *)
  hspace = 3;                                 (* horizontal space between characters *)
  max_chars = 12;                             (* max characters / row *)


type
  prt_chars = ' ' .. '_';                     (* ascii columns 2 - 5 *)
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
$PAGE print_row - main routine

var
    ind: 1 .. ch_desc_size + 1;
    filler: line_index;
    len: 0 .. max_chars;
    i: 1 .. max_chars;

begin
  len := min (length (row), (listfb.width+hspace) div (char_width+hspace));
  if len = 0 then return;
  filler := (listfb.width + hspace - (char_width+hspace)*len) div 2;
  ind := 1;
  while ind <= ch_desc_size do begin
    fio_tab (listfb, listfb.column + filler);
    for i := 1 to len do begin
      fio_write (listfb, substr (rep[row[i]],ind,char_width));
      if i <> len then fio_space (listfb, hspace);
    end;
    fio_skip (listfb);
    ind := ind + char_width;
  end;
end;
$PAGE make_listing_title

(*  MAKE LISTING TITLE sets up the heading title line that is printed
    repeatedly on each banner page.  *)

procedure make_listing_title ( var line: string [line_size] );

const
    day_name: array [week_day] of packed array [1..3] of char =
     (  'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'  );

var sp: line_index;

begin
  line := 'File ' || filename (input) || '    ' ||
	  'Processed ' || day_name [day_of_week (extr_date (dt))] ||
	  ' ' || substr (dc_ext (dt), 1, 15) ||
	  '    ' || 'SCANNR Version 1';

  if length (line) < listfb.width - 10 then begin
    line := '<<<  ' || line || '  >>>';
    while length (line) < listfb.width - 1 do
      line := '<' || line || '>';
  end
  else begin
    for sp := 1 to (listfb.width - length (line)) div 2 do
      line := ' ' || line;
  end;
end (* make_listing_title *);


$PAGE make_header

(* MAKE HEADER writes a big letter header to the listing file. *)

procedure make_header;

var i: 1 .. 4;
    page: 1 .. 2;
    title_line: string [line_size];

begin
  make_listing_title (title_line);

  for page := 1 to 2 do begin
    for i := 1 to 4 do begin
      fio_line (listfb, title_line);
      fio_skip (listfb);
    end;
    for i := 1 to 4 do fio_skip (listfb);

    print_row (title^);

    for i := 1 to 4 do fio_skip (listfb);
    for i := 1 to 4 do begin
      fio_line (listfb, title_line);
      fio_skip (listfb);
    end;

    fio_page (listfb);
  end;
end;
$PAGE page_title

(*  PAGE TITLE is the routine which is called to print page titles on the
    listing file.  *)

procedure page_title ( var fb: file_block );

var page_num: string [10];

begin
  putstring (page_num, 'Page ', fb.pageno:0);
  fio_write (fb, 'SCANNER ');
  fio_write (fb, title^);
  fio_space (fb, (fb.width - fb.column + 1 - length (page_num) - length (sub_title)) div 2);
  fio_write (fb, sub_title);
  fio_tab (fb, fb.width - length (page_num));
  fio_line (fb, page_num);
  fio_skip (fb);
end (* page_title *);
$PAGE tty_eject

(*  TTY EJECT prints a skips six lines, which should serve in place of a page
    eject on a file which will be printed on a terminal.  *)

procedure tty_eject ( var fb: file_block );

var i: 1 .. 6;

begin
  for i := 1 to 6 do
    fio_skip (fb);
end;
$PAGE list_the_input_file

(*  LIST THE INPUT FILE will reread the input file.  If the Print parameter
    is true, then it will copy the input lines to the listing file.  Any
    error messages will be printed on the terminal, and on the listing file
    if Print is true.  *)

procedure list_the_input_file ( print: boolean );

var num_string: packed array [1..6] of char;

begin
  sub_title := 'Input File Listing';
  listfb.c_column := 9;
  fio_page (listfb);

  reset (input, filename (input));
  cur_line_no := 0;
  while not eof (input) do begin
    readln (input, cur_line);
    cur_line_no := cur_line_no + 1;
    if print then begin
      putstring (num_string, cur_line_no:6);
      fio_write (listfb, num_string);
      fio_space (listfb, 2);
      fio_line (listfb, cur_line);
    end;
    if err_count <> 0 then
      err_print (cur_line_no);
  end (* while not eof (input) *);
end (* list_the_input_file *);
$PAGE list_the_actions

(*  LIST THE ACTIONS will print the action for each defined pattern on the
    listing file.  *)

procedure list_the_actions;

var i: number;
    num_string: string [5];
    act, act1: str_list;

begin
  sub_title := 'Actions Defined';
  listfb.c_column := 5;
  fio_page (listfb);

  for i := 1 to npatterns do begin
    act := pat_action (i);
    putstring (num_string, i:0);
    fio_line (listfb, 'Action for pattern ' || num_string);
    if act = nil then
      fio_line (listfb, '- Ignored');
    act1 := act;
    while act1 <> nil do begin
      if act1 = act
	then fio_write (listfb, '{ ')
	else fio_write (listfb, '  ');
      fio_write (listfb, act1^.str^);
      act1 := act1^.next;
      if act1 = nil
	then fio_line (listfb, ' }')
	else fio_skip (listfb);
    end;
    pat_free (act);
    if i <> npatterns then
      fio_nskip (listfb, 1, 2);
  end (* for i *);
end (* list_the_actions *);
$PAGE list_the_regular_expressions

(*  LIST THE REGULAR EXPRESSIONS will print the regular expression for each
    defined pattern on the listing file.  *)

procedure list_the_regular_expressions;

var i: number;
    num: string [5];

begin
  sub_title := 'Regular Expressions';
  listfb.c_column := 5;
  fio_page (listfb);

  for i := 1 to npatterns do begin
    putstring (num, i:0);
    fio_line (listfb, 'Regular expression for pattern ' || num);
    fio_tab (listfb, 5);
    prt_re (pat_re (i));
    fio_skip (listfb);
    if i <> npatterns then
      fio_nskip (listfb, 1, 2);
  end;
end (* list_the_regular_expressions *);
$PAGE list_input

(*  LIST INPUT produces the listing file for the first phase of the scanner
    builder.  It lists the input file, actions and regular expressions, if
    requested, and calls ErrPrint to print any error messages.  When List-
    Input is called, the standard file Output must be open for the listing
    file.  *)

public procedure list_input;

const list_to_tty = false;

begin
  dt := daytime ();

  (*  Set up the FIO file block for the listing.  *)

  fio_attach (listfb, output);
  if list_to_tty then begin
    listfb.plength := 0;
    listfb.width := 80;
    listfb.new_page := tty_eject;
  end
  else begin
    listfb.plength := 44;
    listfb.width := 106;
    make_header; (* Print the banner pages. *)
    listfb.pageno := 1;
  end;
  listfb.page_header := page_title;

  listing := true;

  if listing or (err_count <> 0) then
    list_the_input_file (listing);

  list_the_actions;

  list_the_regular_expressions;
end (* list_input *);
$PAGE echo_line

(*  ECHO LINE is provided for use by the error routine (ErrPrint) when it
    detects an error.  It will print the current input file line on the
    terminal.  *)

public procedure echo_line;

begin
  writeln (tty, cur_line_no:6, '  ', cur_line);
end;
$PAGE prt_message

(*  PRT MESSAGE will print a message line in the listing file and on the
    terminal.  *)

public procedure prt_message ( message: parm_string );

begin
  writeln (tty, ' ':8, message);
  if listing then begin
    fio_tab (listfb, 9);
    fio_line (listfb, message);
  end;
end (* prt_message *);
$PAGE print_transitions

(*  PRINT TRANSITIONS will print out the transitions from a specified state
    in a specified transition matrix.  *)

procedure print_transitions ( is: number; tr_matrix: transition_matrix );

var is1: number; (* A state number. *)
    nsymbols: number; (* The number of symbols. *)
    a, a1, a2, last_a: number; (* Symbols. *)
    tagged: ^ array [0..*] of boolean;
    state_num: string [5];

const
    no_a = maximum (number);

begin
  fio_line (listfb, 'Transitions:');
  new (tagged, upperbound (tr_matrix^));
  tagged^[0] := true;
  for is1 := 1 to upperbound (tagged^) do
    tagged^[is1] := false;

  nsymbols := max_symbol - min_symbol;
  for a := 0 to nsymbols do begin
    is1 := tr_matrix^[is]^[a];
    if not tagged^[is1] then begin
      fio_tab (listfb, 6);
      tagged^[is1] := true;
      last_a := no_a;
      a1 := a;
      repeat
	a2 := succ (a1);
	while (a2 <= nsymbols) andif (tr_matrix^[is]^[a2] = is1) do
	  a2 := succ (a2);
	if last_a <> no_a then
	  prt_symbol (last_a + min_symbol, ', ');
	if a2 <> succ (a1) then
	  prt_symbol (a1 + min_symbol, '..');
	last_a := pred (a2);
	a1 := a2;
	while (a1 <= nsymbols) andif (tr_matrix^[is]^[a1] <> is1) do
	  a1 := succ (a1);
      until a1 > nsymbols;
      prt_symbol (last_a + min_symbol, ' -> ');
      putstring (state_num, is1:0);
      fio_line (listfb, state_num);
    end (* if *);
  end (* for a *);
end (* print_transitions *);
$PAGE list_recognizer

(*  LIST RECOGNIZER will list the recognizer FSA for a specified pattern.
    The characteristic regular expression and the transition vector for
    each state are printed.  *)

public procedure list_recognizer ( pattern: number;
				   tr_matrix: transition_matrix;
				   ch_vector: re_vector;
				   accept: acc_vector );

var is: number;
    num_string: string [5];

begin
  if pattern = 1 then begin (* First recognizer printed. *)
    sub_title := 'Recognizers for Individual Regular Expressions';
    listfb.c_column := 6;
    fio_page (listfb);
  end
  else (* Leave space after the last recognizer. *)
    fio_nskip (listfb, 4, 4);

  putstring (num_string, pattern:0);
  fio_line (listfb, 'Recognizer for pattern ' || num_string);
  fio_skip (listfb);

  for is := 1 to upperbound (tr_matrix^) do begin
    putstring (num_string, is:0);
    if accept^[is] then
      fio_write (listfb, 'Accept ');
    fio_write (listfb, 'State ' || num_string || ':  ');
    prt_re (ch_vector^[is]);
    fio_skip (listfb);
    print_transitions (is, tr_matrix);
    if is <> upperbound (tr_matrix^) then
      fio_nskip (listfb, 1, 2);
  end;
end (* list_recognizer *);
$PAGE list_scanner

(*  LIST SCANNER will list the composite scanner FSA for the complete set of
    patterns.  The characteristic state vector and the transition vector for
    each state are printed.  *)

public procedure list_scanner ( tr_matrix: transition_matrix;
				ch_vector: st_vector;
				accept: acc_pat_vector );

var is, ip: number;
    num_string: string [5];

begin
  sub_title := 'Composite Scanner FSA';
  listfb.c_column := 6;
  fio_page (listfb);

  for is := 1 to upperbound (tr_matrix^) do begin
    if accept^[is] <> 0 then begin
      putstring (num_string, accept^[is]:0);
      fio_write (listfb, 'Accept ' || num_string || ' in ');
    end;
    putstring (num_string, is:0);
    fio_write (listfb, 'State ' || num_string || ':  < ');
    for ip := 1 to npatterns do begin
      putstring (num_string, ch_vector^[is]^[ip]:0);
      if ip = npatterns
	then fio_line (listfb, num_string || ' >')
	else fio_write (listfb, num_string || ', ');
    end;
    print_transitions (is, tr_matrix);
    if is <> upperbound (tr_matrix^) then
      fio_nskip (listfb, 1, 2);
  end;
end (* list_scanner *).
    