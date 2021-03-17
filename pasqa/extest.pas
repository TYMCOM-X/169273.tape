$ENABLE MDSI_PASCAL
$IF MP_PASCAL
(*$N+,O-,I-,P+,R+,S+,V+ MP/Pascal options *)
$ENDIF

(* EXTEST - a test program for the EXNUM package, a package
   of Nova software extended precision mathematics routines. *)

$IF MDSI_PASCAL program extest;
$IF MP_PASCAL program extest ( input, output );

(* EXNUM.TYP - types needed by EXNUM test program and
   EXNUM PDP-10 stubs. *)

const
  ex_num_str_length = 40;

type
  exnum_str_range = 0..41;
  ex_numb_string = string[ ex_num_str_length ];

$IF MDSI_PASCAL
  ex_numb = minimum(real)..maximum(real) prec 16;  (* Always real on Dec10, to be changed on DG  *)
$ENDIF
$IF MP_PASCAL
  ex_numb = record
    r1: real;
    r2:real;
  end;
$ENDIF

  ex_status = (ex_ok, ex_lst_prec, ex_ill_fmt, ex_rng_excd); (* returned by EXCSN  *)

  (* END INCLUDE FILE EXNUM.TYP -------------------------------------- *)
(* External Procedure Declarations for EXMATH.PAS *)

external var
  ex_if: integer;
  ex_of: integer;
  ex_uf: integer;
  ex_dz: integer;
  ex_de: integer;
  ex_ls: integer;

$IF MDSI_PASCAL
EXTERNAL FUNCTION ex_add ( addend1 : ex_numb; addend2 : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_sub ( minuend : ex_numb;
                       subtrahend : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_sqrd ( ex_numb ): ex_numb;

EXTERNAL FUNCTION ex_mult ( factor1 : ex_numb;
                         factor2 : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_div ( dividend : ex_numb;
                     divisor : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_exp( term : ex_numb; exponent : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_sin ( angle : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_cos( angle : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_tan( angle : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_atan( value : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_asin( value : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_acos( value : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_ln ( term : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_log ( term : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_ip( term : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_fp( term : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_rnd( term : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_lt ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL FUNCTION ex_le ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL FUNCTION ex_gt ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL FUNCTION ex_ge ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL FUNCTION ex_eq ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL FUNCTION ex_ne ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL FUNCTION excsn_cvt_str_no ( number : ex_numb_string; VAR cursor: exnum_str_range; VAR status : ex_status) : ex_numb;

EXTERNAL FUNCTION excns_cvt_no_str ( value : ex_numb ) : ex_numb_string;

EXTERNAL FUNCTION ex_ng( value : ex_numb ) : ex_numb;

EXTERNAL FUNCTION ex_ird( value : ex_numb ) : integer;

EXTERNAL FUNCTION ex_abs( value : ex_numb ) : ex_numb;
$ENDIF


$IF MP_PASCAL
EXTERNAL ASSEMBLY FUNCTION ex_add ( addend1 : ex_numb; addend2 : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_sub ( minuend : ex_numb;
                       subtrahend : ex_numb ) : ex_numb;
EXTERNAL ASSEMBLY FUNCTION ex_sqrd ( x: ex_numb ): ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_mult ( factor1 : ex_numb;
                      factor2 : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_div ( dividend : ex_numb;
                    divisor : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_exp( term : ex_numb; exponent : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_sin ( angle : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_cos( angle : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_tan( angle : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_atan( value : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_asin( value : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_acos( value : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_ln ( term : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_log ( term : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_ip( term : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_fp( term : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_rnd( term : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_lt ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL ASSEMBLY FUNCTION ex_le ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL ASSEMBLY FUNCTION ex_gt ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL ASSEMBLY FUNCTION ex_ge ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL ASSEMBLY FUNCTION ex_eq ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL ASSEMBLY FUNCTION ex_ne ( term1 : ex_numb; term2 : ex_numb ) : boolean;

EXTERNAL ASSEMBLY FUNCTION excsn_cvt_str_no ( number : ex_numb_string; VAR cursor : exnum_str_range; VAR status : ex_status) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION excns_cvt_no_str ( value : ex_numb ) : ex_numb_string;

EXTERNAL ASSEMBLY FUNCTION ex_ng( value : ex_numb ) : ex_numb;

EXTERNAL ASSEMBLY FUNCTION ex_ird( value : ex_numb ) : integer;

EXTERNAL ASSEMBLY FUNCTION ex_abs( value : ex_numb ) : ex_numb;
$ENDIF


(* End External Procedure Declarations for EXMATH.PAS *)

$IF MP_PASCAL INCLUDE syslib.pas
$IF MP_PASCAL INCLUDE sint2st.pas
$IF MP_PASCAL INCLUDE index.pas

$PAGE constant and type declarations

const
  cmd_line_length = 128;
  cmd_line_limit = 130;
$IF MDSI_PASCAL
  tty_in_name = 'TTY:';
  tty_out_name = 'TTY:';
  tab: char = chr( 9 );
$ENDIF
$IF MP_PASCAL
  tty_in_name = '@TTI';
  tty_out_name = '@TTO';
  tab = '<11>';
$ENDIF

type
  token_kind = ( real_number_token, octal_number_token, hex_number_token,
    add_token, subtract_token, mult_token, div_token, eq_token, ne_token,
    gt_token, ge_token, lt_token, le_token, power_token, negate_token,
    abs_token, real_print_token, octal_print_token, hex_print_token,
    pop_token, display_token, fix_token, int_token, frac_token, sin_token,
    cos_token, tan_token, asin_token, acos_token, atan_token, log_token,
    ln_token, round_token, clear_token, exit_token, flags_token,
    dg_print_token, dg_number_token, square_token );

  token_string = string[ 80 ];
  
  token_record = record
    kind: token_kind;
    text: token_string;
  end;

  set_of_char = set of char;

  file_name_string = string[ 80 ];

  output_string = string[ 132 ];

  cmd_string = string[ cmd_line_length ];
  cmd_line_range = 0..cmd_line_limit;

  error_msg = string[ 80 ];

$IF MP_PASCAL selector = 0..9;
$IF MDSI_PASCAL  selector = integer;
  exnum_overlay = packed record
    case tag: selector of
      0:  (ex_number: ex_numb );
$IF MDSI_PASCAL
      1: (int_array: packed array [1..4] of 0..777777b);
      2: (dg_dum1: 0..3;
         dg_sign: 0..1;
        dg_exp: 0..177b;
      dg_dummy: 0..377777777b);
      3: (d10_sign: 0..1;
          d10_exp: 0..377b;
     d10_dummy: 0..777777777b);
      4: (bits: packed array [1..72] of boolean);
$ENDIF
$IF MP_PASCAL   1: (int_array: packed array [1..4] of integer);
  end;
$PAGE forward declarations

procedure new_line; forward;
procedure write_string ( str: output_string ); forward;
function get_current_cursor_position: cmd_line_range; forward;
procedure cancel_cmd_line; forward;
procedure print_entire_stack; forward;
$IF MDSI_PASCAL
$PAGE str

(* STR simulates the MP/Pascal intrinsic function STR, i.e., it
   converts a char to a string. *)

function str ( ch: char ): string[ 1 ];

begin
  str := ch;
end  (* func str *);
$ENDIF
$PAGE fatal_error, internal_error

(* FATAL_ERROR is called when an unrecoverable error occurs (e.g.,
   stack overflow).  *)

procedure fatal_error ( msg: error_msg );

begin
  new_line;
  write_string ( 'Fatal error - ' );
  write_string ( msg );
  new_line;
$IF MDSI_PASCAL stop;
$IF MP_PASCAL ?exit (0, 'Terminating - fatal error in EXTEST' );
end  (* proc fatal_error *);


(* INTERNAL_ERROR should never be called.  It is an internal and
   fatal error if it ever is called.  *)

procedure internal_error ( msg: error_msg );

begin
  new_line;
  write_string ( 'Unexpected internal error - ' );
  write_string ( msg );
  new_line;
$IF MDSI_PASCAL stop;
$IF MP_PASCAL   ?exit ( 0, 'Terminating due to unexpected internal error' );
end  (* proc internal_error *) ;

$PAGE user_error

(* USER_ERROR is called for all user errors except those detected
   in GET_TOKEN.  It prints an error message, cancels the current
   command line, prints the stack and then returns to the caller. *)

procedure user_error ( msg: error_msg );

var
  i: cmd_line_range;

begin
  for i := 1 to get_current_cursor_position - 1 do write_string ( str( ' ' ) );
  write_string ( str( '^' ) );
  new_line;

  write_string ( 'Error - ' );
  write_string ( msg );
  new_line;

  print_entire_stack;

  cancel_cmd_line;

end (* proc user_error *) ;
$PAGE upper

(* Function UPPER uppercases the character it is passed.  *)

function upper ( ch: char ): char;

  begin
    if ( ch >= 'a' ) and ( ch <= 'z' )
      then upper := chr ( ord ( ch ) - (ord ( 'a' ) - ord ( 'A' ) ) )
      else upper := ch;
  end;
$IF MDSI_PASCAL
$PAGE append

(* APPEND duplicates the function of the MP/Pascal append
   function, i.e., it appends one string to another.  *)

procedure append ( var base: string[ * ]; tail: string[ * ] );

begin
  base := base || tail;
end  (* proc append *) ;
$ENDIF
$PAGE read_string

(* READ_STRING reads a string from a text file.  *)

procedure read_string ( var f: text;  var str: 
$IF MDSI_PASCAL string[ * ] );
$IF MP_PASCAL   output_string );

var
  ch: char;

begin

  str := '';
$IF MDSI_PASCAL break ( ttyoutput );
$IF MDSI_PASCAL  if eoln ( f ) then readln ( f );
  while not eoln ( f ) do begin
    read ( f, ch );
    append ( str, upper ( ch ) );
  end;
$IF MP_PASCAL  read ( f, ch );
end  (* proc read_string *) ;
$PAGE source abstraction - declarations and init_source

(* The source abstraction encapsulates all knowledge of the internals
   of the input file from which commands are read.  *)

var
  cmd_file: text;         (* the file from which cmds are read *)
  input_from_tty: boolean;             (* true => prompt written before cmd reads, *)
  output_to_tty: boolean;               (* if either false, input echoed to output file *)


(* INIT_SOURCE prompts the user for the name of the input command
   file and then opens that file.  If the user simply hits return
   when prompted then the terminal is used as the input file.  *)

procedure init_source;

var
  input_file_name: file_name_string;

begin
  write ( ttyoutput, 'Input file or device: ' );

$IF MDSI_PASCAL  read_string ( tty, input_file_name );
$IF MP_PASCAL  read_string ( input, input_file_name );
  input_from_tty := (input_file_name = '') or (input_file_name = tty_in_name);

  if input_file_name = '' then input_file_name := tty_in_name;

$IF MDSI_PASCAL open ( cmd_file, input_file_name );
$IF MP_PASCAL   if input_file_name <> tty_in_name then reset ( cmd_file, input_file_name );

end  (* proc init_source *) ;
$PAGE source abstraction - get_cmd_line

(* GET_CMD_LINE reads a line of text from the input command file. 
   The line read is also written to the output file if the prompt
   flag is false.  Comments are stripped from the cmd line
   after it is echoed to the output file.  *)

function get_cmd_line: cmd_string;

var
  cmd_line: cmd_string;
  comment_start: cmd_line_range;


begin
  repeat
    if input_from_tty then write ( ttyoutput, '*' );

$IF MDSI_PASCAL
    read_string ( cmd_file, cmd_line );
$ENDIF
$IF MP_PASCAL
    if input_from_tty
      then read_string ( input, cmd_line )
      else read_string ( cmd_file, cmd_line );
$ENDIF

    if not (input_from_tty and output_to_tty) then begin            (* echo to output file *)
      write_string ( cmd_line );
      new_line;
    end;

$IF MDSI_PASCAL exit if length( cmd_line ) = 0;
$IF MP_PASCAL    if length ( cmd_line ) = 0 then exitloop;
  until cmd_line[ 1 ] <> ';' ;

  comment_start := index( cmd_line, str( ';' ) );
  if comment_start <> 0
    then cmd_line := cmd_line[1:comment_start - 1];
  get_cmd_line := cmd_line;

end  (* func get_cmd_line *) ;
$PAGE sink abstraction - declarations and init_sink

(* The sink abstraction encapsulates all knowledge of the internals
   of the output file to which the test results are written. *)

var
  output_file: text;


(* INIT_SINK prompts the user for the name of the output file and
   then opens that file.  If the user simply hits return when
   prompted then the terminal is used as the input file.  *)

procedure init_sink;

var
  output_file_name: file_name_string;

begin
  write ( ttyoutput, 'Output file or device: ' );

$IF MDSI_PASCAL  read_string ( tty, output_file_name );
$IF MP_PASCAL  read_string ( input, output_file_name );

  if output_file_name = '' then output_file_name := tty_out_name;
  output_to_tty := output_file_name = tty_out_name;

$IF MDSI_PASCAL
  rewrite ( output_file, output_file_name );
$ENDIF
$IF MP_PASCAL
  if output_file_name <> tty_out_name
    then rewrite ( output_file, output_file_name );
$ENDIF

end  (* proc init_sink *) ;
$PAGE sink abstraction - write_string, write_exnum, write_int, new_line

(* WRITE_STRING writes a string to the output file.  *)

procedure write_string;                 (* forward declared *)
  
begin
$IF MDSI_PASCAL
  write ( output_file, str );
$ENDIF
$IF MP_PASCAL
  if output_to_tty
    then write ( output, str )
    else write ( output_file, str );
$ENDIF
end;


(* WRITE_EXNUM converts an extended number to ascii and writes it
   to the output file.  *)

procedure write_exnum ( ex_number: ex_numb );

begin
  write_string ( excns_cvt_no_str ( ex_number ) );
  write_string ( str( ' ' ) );
end  (* proc write_exnum *) ;


(* WRITE_INT writes an integer to the output file.  *)

procedure write_int ( i: integer );

var
  s: string[ 12 ];
  error: integer;

begin
$IF MDSI_PASCAL  putstring( s, i );
$IF MP_PASCAL  si2st( i, 10 + i2st_signd, s, error );
  write_string ( s );
end;


(* NEW_LINE writes a newline to the output file.  *)

procedure new_line;

begin
$IF MDSI_PASCAL
  writeln ( output_file );
$ENDIF
$IF MP_PASCAL
  if output_to_tty
    then writeln ( output )
    else writeln ( output_file );
$ENDIF
end  (* proc new_line *) ;
$PAGE stack abstraction - declarations, init_stack, stack_element_count

(* The stack abstraction encapsulates all knowledge of the implementation
  of the stack.  *)

const
  stack_size = 20;               (* max number of EX_NUMs which may be on
                                      the stack at any one time *)

type
  exnum_stack = array [1..stack_size] of ex_numb;
  stack_range = 0..stack_size;

var
  stack: exnum_stack;                   (* the stack !!! *)
  top_of_stack: stack_range;               (* index into above of topmost element *)


(* INIT_STACK initializes the stack.  *)

procedure init_stack;

  begin
    top_of_stack := 0;
  end;


(* STACK_ELEMENT_COUNT returns the number of elements currently
   on the stack (or, the index of the topmost element).  *)

function stack_element_count: stack_range;

  begin
    stack_element_count := top_of_stack;
  end  (* func stack_element_count *) ;
$PAGE stack abstraction - read_stack, push_exnum, pop_exnum

(* READ_STACK returns the EX_NUMB indexed by parameter ELEMENT_NUMBER. *)

function read_stack ( element_number: stack_range ): ex_numb;

  begin
    if (element_number > top_of_stack) or (element_number < 1) 
      then internal_error ( 'out of range stack read' )
      else read_stack := stack[ element_number ];
  end;


(* PUSH_EXNUM pushes an extended number onto the stack.  *)

procedure push_exnum ( number: ex_numb );

  begin
    if top_of_stack >= stack_size then begin
      fatal_error ( 'stack overflow' );
    end
    else begin
      top_of_stack := top_of_stack + 1;
      stack[ top_of_stack ] := number;
    end;
  end;


(* POP_EXNUM pops an extended number from the stack.  *)

function pop_exnum: ex_numb;

  begin
    if top_of_stack <= 0 then begin
      internal_error ( 'pop of empty stack ' );
    end
    else begin
      pop_exnum := stack[ top_of_stack ];
      top_of_stack := top_of_stack - 1;
    end;
  end  (* func pop_exnum *) ;
$PAGE cmd line abstraction - declarations

(* The command line abstraction encapsulates all manipulations of
   the command string.  *)

const
  table_size = 27;                  (* number of keywords *)

type
  id_string = string[ 7 ];
  table_range = 1..table_size;
  keyword_table = array [table_range] of id_string;
  keyword_kinds_table = array [table_range] of token_kind;

var
  cursor: cmd_line_range;         (* current position in cmd line *)
  cmd_line: cmd_string;                     (* current cmd line *)
  set_of_hex_digits: set of char;
  real_number_char_set: set of char;
  identifier_char_set: set_of_char;
  keywords: keyword_table;                (* keywords table *)
  keyword_kinds: keyword_kinds_table;     (* corresponding token kinds *)
$PAGE cmd line abstraction - init_cmd_line

(* INIT_CMD_LINE initializes the command line parsing tables
   and reads the first command line.  *)

procedure init_cmd_line;

begin
  cursor := 1;
  cmd_line := get_cmd_line;         (* read 1st cmd line *)

  (* Init some 'constants'.  *)

  set_of_hex_digits := ['0'..'9', 'A'..'F'];
  real_number_char_set := ['0'..'9', '+', '-', 'E', '.'];
  identifier_char_set := ['A'..'Z'];

  (* Init keywords table and table of corresponding token kinds. *)

  keywords[ 1 ] := 'ABS';
  keyword_kinds[ 1 ] := abs_token;
  keywords[ 2 ] := 'ACOS';
  keyword_kinds[ 2 ] := acos_token;
  keywords[ 3 ] := 'ASIN';
  keyword_kinds[ 3 ] := asin_token;
  keywords[ 4 ] := 'ATAN';
  keyword_kinds[ 4 ] := atan_token;
  keyword] := 'CLEAR';
  keyword_kinds[ 5 ] := clear_token;
  keywords[ 6 ] := 'COS';
  keyword_kinds[ 6 ] := cos_token;
  keywords[ 7 ] := 'DISPLAY';
  keyword_kinds[ 7 ] := display_token;
  keywords[ 8 ] := 'EXIT';
  keyword_kinds[ 8 ] := exit_token;
  keywords[ 9 ] := 'FIX';
  keyword_kinds[ 9 ] := fix_token;
  keywords[ 10 ] := 'FRAC';
  keyword_kinds[ 10 ] := frac_token;
  keywords[ 11 ] := 'HEX';
  keyword_kinds[ 11 ] := hex_print_token;
  keywords[ 12 ] := 'INT';
  keyword_kinds[ 12 ] := int_token;
  keywords[ 13 ] := 'LN';
  keyword_kinds[ 13 ] := ln_token;
  keywords[ 14 ] := 'LOG';
  keyword_kinds[ 14 ] := log_token;
  keywords[ 15 ] := 'NEG';
  keyword_kinds[ 15 ] := negate_token;
  keywords[ 16 ] := 'OCT';
  keyword_kinds[ 16 ] := octal_print_token;
  keywords[ 17 ] := 'POP';
  keyword_kinds[ 17 ] := pop_token;
  keywords[ 18 ] := 'QUIT';
  keyword_kinds[ 18 ] := exit_token;
  keywords[ 19 ] := 'ROUND';
  keyword_kinds[ 19 ] := round_token;
  keywords[ 20 ] := 'SIN';
  keyword_kinds[ 20 ] := sin_token;
  keywords[ 21 ] := 'STOP';
  keyword_kinds[ 21 ] := exit_token;
  keywords[ 22 ] := 'TAN';
  keyword_kinds[ 22 ] := tan_token;
  keywords[ 23 ] := 'STACK';
  keyword_kinds[ 23 ] := display_token;
  keywords[ 24 ] := 'FLAGS';
  keyword_kinds[ 24 ] := flags_token;
  keywords[ 25 ] := 'TOS';
  keyword_kinds[ 25 ] := real_print_token;
  keywords[ 26 ] := 'DG';
  keyword_kinds[ 26 ] := dg_print_token;
  keywords[ 27 ] := 'SQR';
  keyword_kinds[ 27 ] := square_token;

end  (* proc init_cmd_line *) ;
$PAGE cmd line abstraction - get_current_cursor_position, cancel_cmd_line

(* GET_CURRENT_CURSOR_POSITION lets the outside world inquire about
   the current position of the cursor.  *)

function get_current_cursor_position;    (* forward declared *)
 
begin
  get_current_cursor_position := cursor;
end;


(* CANCEL_CMD_LINE forces a new line to be read when GET_TOKEN
   is called the next time.  *)

procedure cancel_cmd_line;             (* forward declared *)

begin
  cursor := length ( cmd_line ) + 2;
end;
$PAGE cmd line abstraction - get_token

(* GET_TOKEN returns the next token in the command string,
   acquiring a new command string when necessary.  *)

function get_token: token_record;

var
  ch: char;
  start_index: cmd_line_range;
  i: cmd_line_range;
  dummy: boolean;
  element_found: 0..table_size;
  token: token_record;
$PAGE cmd line abstraction - skip_blanks, next_char in get_token

(* SKIP_BLANKS advances the command string cursor past any string
   of blanks at the current cursor position.  *)

procedure skip_blanks;

  begin
    if cursor <= length ( cmd_line ) then begin
      if cmd_line[cursor] in [ ' ', tab ] then begin
      cursor := cursor + 1;
 skip_blanks;
      end;
    end;
  end;


(* NEXT_CHAR returns the character at the current cusor position and
   increments the cursor.  It is the caller's responsibility to first
   check that the cursor is not beyond the end of the command string. *)

function next_char: char;
  begin
    if cursor <= length ( cmd_line ) then begin
      next_char := cmd_line[cursor];
      cursor := cursor + 1;
    end
    else internal_error ( 'fetch of char beyond end of command line' );
  end  (* func next_char *) ;
$PAGE cmd line abstraction - lex_error in get_token
 
(* LEX_ERROR is called when a lexical error occurs.  It prints a '^'
   at the point of the error, prints the message passed as a parameter,
   cancels the current command line and sets TOKEN.KIND to 
   display_token.  *)

procedure lex_error ( msg: error_msg );

var
  i: cmd_line_range;

begin
  for i:= 1 to cursor - 1 do write_string ( str( ' ' ) );
  write_string ( str( '^' ) );
  new_line;

  write_string ( 'Lexical error - ' );
  write_string ( msg );
  new_line;

  cancel_cmd_line;

  get_token.kind := display_token;
end  (* proc lex_error *);

$PAGE cmd line abstraction - scan_single_char, scan_char_set in get_token

(* SCAN_SINGLE_CHAR returns true and advances the cursor if the char
   passed in is the next character in the command string.  If it is
   not, then false is returned and the cursor is not modified.  *)

function scan_single_char ( ch: char ): boolean;

var
  found: boolean;

begin
  if cursor <= length ( cmd_line ) then begin
    found := cmd_line[ cursor ] = ch;
    if found then cursor := cursor + 1;
    scan_single_char := found;
  end
  else scan_single_char := false;
end;


(* SCAN_CHAR_SET returns true if the character at the current cursor
   position is in the specified character set (parameter CH_SET) and
   advances the cursor past all contiguous characters in the character
   set.  If the current character is not in CH_SET then false is
   returned and the cursor is not modified.  *)

function scan_char_set ( ch_set: set_of_char ): boolean;

var
  dummy: boolean;
  found: boolean;

begin
  if cursor <= length ( cmd_line ) then begin
    found := cmd_line[ cursor ] in ch_set;
    if found then begin
      cursor := cursor + 1;
      dummy := scan_char_set ( ch_set );
    end;
    scan_char_set := found;
  end
  else scan_char_set := false;
end  (* func scan_char_set *) ;
$PAGE cmd line abstraction - get_token - body

begin
  skip_blanks;

  (* Read a new command line if necessary.  *)

  if cursor = length ( cmd_line ) + 2 then begin
    cmd_line := get_cmd_line;
    cursor := 1;
  end;

  (* A carriage return, if not consumed along with the preceeding
     token, is taken as a command to print the topmost stack
     element.  *)

  skip_blanks;
  if cursor = length ( cmd_line ) + 1 then begin
    token.kind := real_print_token;
    cursor := cursor + 1;                (* force read next time around *)
  end
  else begin
    ch := next_char;

    case ch of

      '*':                            (* multiply or power *)
       begin
   if cursor <= length ( cmd_line ) then begin
     if cmd_line[ cursor ] = '*' then begin
          ch := next_char;
              token.kind := power_token;
          end
           else token.kind := mult_token;
          end
       else token.kind := mult_token;
      end;

      '/': token.kind := div_token;     (* divide *)

      '=': token.kind := eq_token;      (* equality test *)

      '>':                               (* '>' or '>=' *)
     begin
   if cursor <= length ( cmd_line ) then begin
     if cmd_line[ cursor ] = '=' then begin
          ch := next_char;
              token.kind := ge_token;
     end
           else token.kind := gt_token;
        end
   else token.kind := gt_token;
        end;

      '<':                              (* '<' or '<=' or '<>' *)
     begin
   if cursor <= length ( cmd_line ) then begin
     if cmd_line[cursor] = '>' then begin
            ch := next_char;
              token.kind := ne_token;
     end
           else if cmd_line[ cursor ] = '=' then begin
             ch := next_char;
              token.kind := le_token;
     end
           else token.kind := lt_token;
        end
   else token.kind := lt_token;
        end;

      '^':                              (* octal or hex number *)
     begin
   start_index := cursor - 1;
  
        if cursor <= length ( cmd_line ) then begin
     ch := next_char;
      if ch = 'O'
             then token.kind := octal_number_token
       else if (ch = 'X') or (ch = 'H')
        then token.kind := hex_number_token
         else if ch = 'G'
        then token.kind := dg_number_token
          else begin
              lex_error ( '"O" or "X" or "H" or "G" expected' );
            return;                   (* <-- error return *)
            end;
          skip_blanks;
        end
   else begin
      lex_error ( '"O" or "X" or "H" or "G" expected' );
            return;                     (* <-- error return *)
          end;

        if not scan_char_set ( set_of_hex_digits ) then begin
           lex_error ( 'digit expected' );
       return;
     end;

        for i := 1 to 3 do begin
        if not scan_single_char ( ',' ) then begin
              lex_error ( 'comma expected' );
       return;
     end;
          if not scan_char_set ( set_of_hex_digits ) then begin
           lex_error ( 'digit expected' );
       return;
     end;
        end;

        token.text := cmd_line[start_index: cursor - start_index];
  end  (* octal or hex number case *) ;

      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-', '.':
       begin                           (* real number *)
       if (cursor > length(cmd_line)) and (ch = '+')
           then token.kind := add_token
        else if (cursor > length(cmd_line)) and (ch = '-')
      then token.kind := subtract_token
   else begin
      start_index := cursor - 1;
            if scan_char_set ( real_number_char_set ) then begin
            token.kind := real_number_token;
              token.text := cmd_line[start_index: cursor - start_index];
          end
           else if ch = '+'
        then token.kind := add_token
        else if ch = '-' 
       then token.kind := subtract_token
           else begin
              token.kind := real_number_token;
              token.text := ch;
           end;
        end;
        end;

      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
      'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z':
        begin                           (* keyword *)
   start_index := cursor - 1;

          dummy := scan_char_set ( identifier_char_set );;
      token.text := cmd_line[start_index: cursor - start_index];

          element_found := 0;
   for i := 1 to table_size do begin
       if token.text = keywords[ i ] then 
             element_found := i;
       end;

        if element_found = 0 then begin
         lex_error ( 'unknown keyword' );
      return;
     end
   else token.kind := keyword_kinds[ element_found ];
        end  (* keyword case *) ;

    end  (* case *);

    (* Some keywords consume any following end of line.  *)

    if token.kind in [octal_print_token, hex_print_token, pop_token,
                      display_token, clear_token, eq_token, ne_token,
               gt_token, ge_token, lt_token, le_token, 
                      fix_token, flags_token, real_print_token,
                     dg_print_token] then begin
      skip_blanks;
      if cursor = length ( cmd_line ) + 1
       then cursor := cursor + 1;
    end;

  end  (* else *) ;

  get_token := token;

end  (* func get_token *) ;
$PAGE cvt_and_push_real

(* CVT_AND_PUSH_REAL converts a real number string to an extended
   number and pushes it onto the stack.  *)

procedure cvt_and_push_real ( token: token_record );

var
  cvt_status: ex_status;
  number: ex_numb;
  token_length: cmd_line_range;
  cursor: exnum_str_range;

begin
  if length ( token.text ) > ex_num_str_length  then begin
    user_error ( 'real number string too long' );
  end
  else begin
    if length ( token.text ) < ex_num_str_length
      then token_length := length ( token.text )
      else token_length := ex_num_str_length;
    cursor := 1;
    number := excsn_cvt_str_no ( token.text[ 1:token_length ], cursor, cvt_status );
    if cvt_status in [ex_ok, ex_lst_prec] then begin
      if cvt_status = ex_lst_prec then
        write_string ( 'Warning - precision loss' );
      push_exnum ( number );
    end
    else if cvt_status = ex_ill_fmt
      then user_error ( 'syntactically illegal number string' )
    else if cvt_status = ex_rng_excd 
      then user_error ( 'number magnitude too large' );
    end;
end  (* proc cvt_and_push_real *) ;
$IF MDSI_PASCAL
$PAGE dg_to_dec10_reformat
(* DG_TO_DEC10_REFORMAT takes a number entered in hex on a DEC-10 using
   a bit pattern appropriate for the DG real format and reformats
   it to the DEC10 double real format. *)

function dg_to_dec10_reformat ( ex_num: ex_numb ): ex_numb options nocheck(value);

var
  dg: exnum_overlay;
  dec10: exnum_overlay;
  offset: 0..73;
  i, j: 0..73;

const
  meaningful: packed array [1..62] of 0..1 :=
    (1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                  0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                  0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1);

begin
  dec10.ex_number := 0;
  if ex_num <> 0 then begin
    dg.ex_number := ex_num;
    with dec10 do begin
      d10_sign := 0;
      d10_exp := (dg.dg_exp - 100b) * 4 + 128;
      offset := 0;
      while not dg.bits[offset + 11] do offset := offset + 1;
      assert( offset <= 3 );
      d10_exp := d10_exp - offset;

      j := 10;
      for i := 1 + offset to 62 do begin
    if meaningful[ i ] = 1 then begin
       if j = 37 then j := j + 1;    (* skip 2nd sign bit *)
         bits[ j ] := dg.bits[ i + 10 ];
       j := j + 1;
 end;
      end;
    end  (* with *) ;

    if dg.dg_sign = 1 then
      dec10.ex_number := -dec10.ex_number;
  end;
  dg_to_dec10_reformat := dec10.ex_number;
end (* proc dg_to_dec10_reformat *) ;
$ENDIF
$PAGE cvt_and_push_binary

(* CVT_AND_PUSH_BINARY converts an octal or hex number string to
   an extended number and pushes it onto the stack.  *)

procedure cvt_and_push_binary ( token: token_record );

var
  cursor: cmd_line_range;
  i: 1..4;
  radix: 8..16;
  ovrlay: exnum_overlay;
  base: 0..128;
  ex_text: token_string;

begin
  if token.kind = octal_number_token
    then radix := 8
    else radix := 16;

  cursor := 3;         (* skip '^x' *)
  ovrlay.tag := 1;

  ex_text := token.text;
  append ( ex_text, ' ' );

  while ex_text[ cursor ] in [ ' ', tab ] do cursor := cursor + 1;

  for i := 1 to 4 do begin
    ovrlay.int_array[ i ] := 0;
    while ex_text[ cursor ] in ['0'..'9', 'A'..'F'] do begin
      if ex_text[cursor] in ['0'..'9']
        then base := ord ( '0' )
      else base := ord ( 'A' ) - 10;
      ovrlay.int_array[ i ] := ord (ex_text[cursor]) - base +
                         radix * ovrlay.int_array[ i ];
      cursor := cursor + 1;
    end;

    if i <> 4 then cursor := cursor + 1;       (* skip comma *)
  end  (* for *) ;

$IF MDSI_PASCAL
  if token.kind = dg_number_token then
    ovrlay.ex_number := dg_to_dec10_reformat(ovrlay.ex_number);
$ENDIF


  ovrlay.tag := 0;
  push_exnum ( ovrlay.ex_number );

end  (* proc cvt_and_push_binary *);

$PAGE real_valued_binary_op

(* REAL_VALUED_BINARY_OP performs a binary operation using the top
   two stack elements, pops those two elements from the stack, and
   pushes the result onto the stack.  *)

procedure real_valued_binary_op ( token: token_record );

var
  result: ex_numb;
  operand1: ex_numb;
  operand2: ex_numb;

begin
  if stack_element_count < 2 then begin
    user_error ( 'insufficient elements on stack' );
  end
  else begin
    operand2 := pop_exnum;
    operand1 := pop_exnum;

    case token.kind of
      add_token:     result := ex_add ( operand1, operand2 );
      subtract_token: result := ex_sub ( operand1, operand2 );
      mult_token:     result := ex_mult ( operand1, operand2 );
      div_token:     result := ex_div ( operand1, operand2 );
      power_token:    result := ex_exp ( operand1, operand2 );
    end;

    push_exnum ( result );
  end  (* else *) ;
end  (* proc real_valued_binary_op *) ;
$PAGE real_valued_unary_op

(* REAL_VALUED_BINARY_OP performs a unary operation using the top
   stack element.  The initial top stack element is popped from the
   stack and replaced by the result.  *)

procedure real_valued_unary_op ( token: token_record );

var
  result: ex_numb;
  operand: ex_numb;

begin
  if stack_element_count < 1 then begin
    user_error ( 'stack empty' );
  end
  else begin
    operand := pop_exnum;

    case token.kind of
      negate_token:   result := ex_ng ( operand );
      abs_token:  result := ex_abs ( operand );
      int_token: result := ex_ip ( operand );
      frac_token: result := ex_fp ( operand );
      sin_token:  result := ex_sin ( operand );
      cos_token: result := ex_cos ( operand );
      tan_token: result := ex_tan ( operand );
      asin_token:        result := ex_asin ( operand );
      acos_token:       result := ex_acos ( operand );
      atan_token:       result := ex_atan ( operand );
      log_token:        result := ex_log ( operand );
      ln_token:          result := ex_ln ( operand );
      round_token:        result := ex_rnd ( operand );
      square_token:      result := ex_sqrd ( operand );
    end;

    push_exnum ( result );
  end (* else *) ;
end  (* proc real_valued_unary_op *) ;
$PAGE integer_valued_unary_op

(* INTEGER_VALUED_UNARY_OP performs a unary operation which yields
   an integer result.  The operand is popped from the stack.  The
   integer is written to the output file.  *)

procedure integer_valued_unary_op ( token: token_record );

begin
  if stack_element_count < 1 then begin
    user_error ( 'stack empty' );
  end
  else begin
    write_int ( ex_ird ( pop_exnum ) );
    new_line;
  end;
end  (* proc integer_valued_unary_op *) ;
$PAGE boolean_valued_binary_op

(* BOOLEAN_VALUED_BINARY_OP performs a binary operation which yields
   a boolean result.  The two operands are popped from the stack,
   the boolean result is written to the output file.  *)

procedure boolean_valued_binary_op ( token: token_record );

var
  result: boolean;
  op1: ex_numb;
  op2: ex_numb;

begin
  if stack_element_count < 2 then begin
    user_error ( 'insufficient elements on stack' );
  end
  else begin
    op2 := pop_exnum;
    op1 := pop_exnum;

    case token.kind of
      eq_token:             result := ex_eq ( op1, op2 );
      ne_token:          result := ex_ne ( op1, op2 );
      gt_token:          result := ex_gt ( op1, op2 );
      ge_token:          result := ex_ge ( op1, op2 );
      lt_token:          result := ex_lt ( op1, op2 );
      le_token:          result := ex_le ( op1, op2 );
    end;

    if result
      then write_string ( 'TRUE' )
      else write_string ( 'FALSE' );
    new_line;
  end  (* else *) ;
end  (* boolean_valued_binary_op *) ;
$PAGE print_entire_stack, cvt_and_print_real

(* PRINT_ENTIRE_STACK does just that.  The contents of the stack is
   unaffected.  *)

procedure print_entire_stack;         (* forward declared *)

var
  i: stack_range;

begin
  write_string ( 'Stack: ' );
  for i := 1 to stack_element_count do 
    write_exnum ( read_stack ( i ) );
  new_line;
end;


(* CVT_AND_PRINT_REAL prints the topmost stack element.  The
   element is not popped from the stack.  *)

procedure cvt_and_print_real;

begin
  if stack_element_count > 0
    then write_exnum ( read_stack ( stack_element_count ) )
    else user_error ( 'print command given with empty stack' );
  new_line;
end  (* proc cvt_and_print_real *) ;
$IF MDSI_PASCAL
$PAGE dec10_to_dg_reformat

(* DEC10_TO_DG_REFORMAT takes a DEC-10 double real and converts
   it to the equivalent DG format double real.  *)

function dec10_to_dg_reformat ( ex_num: ex_numb ): ex_numb options nocheck(values);

var
  negate_flag: boolean;
  d10: exnum_overlay;
  dg: exnum_overlay;
  offset, i, j, k: 0..73;

const
  meaningful: packed array [1..62] of 0..1 :=
       (1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                  0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                  0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1);

begin
  negate_flag := ex_num < 0;
  d10.ex_number := abs( ex_num );
  dg.ex_number := 0;
  if ex_num <> 0 then begin
    with dg do begin
      dg_sign := ord ( negate_flag );
      dg_exp := (((d10.d10_exp + 3) div 4) - 32) + 64;
      k := (4 - d10.d10_exp mod 4) + 1;
      if k = 5 then k := 1;
      j := 1;

      for i := k to 62 do begin
      if meaningful[ i ] = 1 then begin
       bits[ i + 10 ] := d10.bits[ j + 9 ];
          j := j + 1;
   if J + 9 = 37 then j := j + 1;
      end;
      end;

    end  (* with *) ;
  end;
  dec10_to_dg_reformat := dg.ex_number;
end (* func dec10_to_dg_reformat *);
$ENDIF
$PAGE cvt_and_print_binary

(* CVT_AND_PRINT_BINARY prints the topmost stack element in
   either octal or hex.  The element is not popped from the
   stack.  *)

procedure cvt_and_print_binary ( token: token_record );

const
  digit = '0123456789ABCDEF';
  blanks = '      ';

  digits_per_word = 6;

var
  ovrlay: exnum_overlay;
  radix: 8..16;
  i: 1..4;
  word_text: string[ digits_per_word ];
  word_value: integer;
  j: 1..digits_per_word;
  str_length: 1..6;

begin
  if stack_element_count < 1 then begin
    user_error ( 'stack empty' );
  end
  else begin
    write_string( '      ' );
    if token.kind = octal_print_token then begin
      radix := 8;
      str_length := digits_per_word;
    end
    else begin
      radix := 16;
$IF MDSI_PASCAL      str_length := digits_per_word - 1;
$IF MP_PASCAL      str_length := digits_per_word - 2;
    end;

    with ovrlay do begin
      tag := 0;
      ex_number := read_stack ( stack_element_count );
      tag := 1;

$IF MDSI_PASCAL
      if token.kind = dg_print_token then
       ex_number := dec10_to_dg_reformat( ex_number );
$ENDIF

      for i := 1 to 4 do begin
     word_text := blanks[1:str_length];
    word_value := int_array[ i ];
$IF MP_PASCAL    if word_value < 0 then word_value := word_value + (-32767 - 1);
       for j := str_length downto 1 do begin
   word_text[ j ] := digit[ (word_value mod radix) + 1 ];
        word_value := word_value div radix;
$IF MP_PASCAL
    if (j = str_length) and (int_array[ i ] < 0) then begin
         if radix = 8
            then word_value := word_value + 4096
          else word_value := word_value + 2048;
     end;
$ENDIF
        end;
  write_string ( word_text );
   if i <> 4 then write_string ( str( ',' ) );
      end;

      new_line;

    end  (* with *) ;
  end  (* else *) ;
end  (* proc cvt_and_print_binary *) ;

$PAGE pop_and_print_real

(* POP_AND_PRINT_REAL pops and prints the top stack element.  *)

procedure pop_and_print_real;

begin
  if stack_element_count < 1 then begin
    user_error ( 'stack empty' );
  end
  else begin
    write_exnum ( pop_exnum );
    new_line;
  end;
end  (* proc pop_and_print_real *) ;
$PAGE print_flags

(* PRINT_FLAGS prints the overflow flags. *)

procedure print_flags;

begin
  write_string ( 'EX_IF: ' );
  write_int ( ex_if );

  write_string ( '  EX_OF: ' );
  write_int ( ex_of );

  write_string ( '  EX_UF: ' );
  write_int ( ex_uf );

  write_string ( '  EX_DZ: ' );
  write_int ( ex_dz );

  write_string ( '  EX_DE: ' );
  write_int ( ex_de );

  write_string ( '  EX_LS: ' );
  write_int ( ex_ls );

  new_line;
end  (* proc print_flags *) ;
$PAGE do_command

(* DO_COMMAND processes the command corresponding to parameter TOKEN.  *)

procedure do_command ( token: token_record );

var
  i: stack_range;
  ex_number: ex_numb;

begin
  case token.kind of

    real_number_token: cvt_and_push_real ( token );

    dg_number_token,
    octal_number_token,
    hex_number_token:  cvt_and_push_binary ( token );

    add_token,
    subtract_token,
    mult_token,
    div_token,
    power_token:       real_valued_binary_op ( token );

    square_token,
    negate_token,
    abs_token,
    int_token,
    frac_token,
    sin_token,
    cos_token,
    tan_token,
    asin_token,
    acos_token,
    atan_token,
    log_token,
    ln_token,
    round_token: real_valued_unary_op ( token );

    eq_token,
    ne_token,
    gt_token,
    ge_token,
    lt_token,
    le_token:             boolean_valued_binary_op ( token );

    fix_token:           integer_valued_unary_op ( token );

    real_print_token:  cvt_and_print_real;

    dg_print_token,
    octal_print_token,
    hex_print_token:   cvt_and_print_binary ( token );

    pop_token:               pop_and_print_real;

    display_token:       print_entire_stack;

    clear_token:
      for i := 1 to stack_element_count do ex_number := pop_exnum;

    flags_token:         print_flags;

  end  (* case *) ;
end  (* proc do_command *) ;
$PAGE extest mainline

(* EXTEST main routine.  *)

var
  token: token_record;

begin
$IF MDSI_PASCAL
  open ( tty );
  rewrite ( ttyoutput );
$ENDIF

  init_source;
  init_stack;
  init_sink;
  init_cmd_line;

  token := get_token;
  
  while token.kind <> exit_token do begin
    do_command ( token );
    token := get_token;
  end;

end.
   PGÿ