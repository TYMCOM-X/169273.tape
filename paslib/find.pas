$TITLE FIND - Program to select lines from a file

program find;


type
    parm_string = packed array [1..*] of char;

external procedure run ( parm_string; integer );

external function prgm_dir: string [15];

external var auto_run: boolean; (* Set if run with CCL offset 1. *)

$INCLUDE fndpat
$INCLUDE fndrec
$PAGE command line options
type
    option_type = (

	(*  Output file mode  *)

	append_mode,
	catenate_mode,
	create_mode,

	(*  Yes/No switches  *)

	control_switch,
	count_switch,
	identify_switch,
	number_switch,
	print_switch,

	(*  Numeric switches  *)

	record_switch,
	runoffset_switch,

	(*  File default switches  *)

	input_switch,
	output_switch,

	(*  Special action switches  *)

	cmd_alpha,
	cmd_define,
	cmd_exit,
	cmd_find,
	cmd_help,
	cmd_reset,
	cmd_run  );

    output_modes = append_mode .. create_mode;
    yes_no_switches = control_switch .. print_switch;
    numeric_switches = record_switch .. runoffset_switch;
    file_switches = input_switch .. output_switch;
$PAGE command representation
type
    identifier = string [20];

    pat_def = packed record
	pattern: pattern;
	pure_pattern: boolean;
    end;

    definition_list = ^ definition_node;

    definition_node = record
	next: definition_list;
	name: identifier;
	value: pat_def;
    end;

    options_record = record
	output_mode: output_modes;
	yes_no: packed array [yes_no_switches] of boolean;
	numeric: array [numeric_switches] of integer;
	file_defaults: array [file_switches] of file_name;
	definitions: definition_list;
	alpha_pattern, find_pattern: pattern;
    end;

    actions_record = record
	input_name,
	output_name,
	indirect_name,
	run_name: file_name;
	help_action: identifier;
	exit_action: boolean;
	reset_action: boolean;
    end;

    command_record = record
	cmd_options: options_record;
	cmd_actions: actions_record;
    end;

const
    no_actions: actions_record = ( '', '', '', '', '', false, false );
$PAGE data declarations

var standard_defaults,
    default_options: options_record;

const
    indirect_limit = 6; (* Maximum nested indirect files. *)

var cmd_file: text; (* The command input file. *)

    file_stack: array [1..indirect_limit] of text; (* Pending command files. *)

    file_stack_size: 0..indirect_limit;

var current_output: file_name; (* The last output file written. *)

const
    terminal_name = '*TERMINAL*';
$PAGE add_definition
(*  ADD DEFINITION adds a definition at the head of a definition list,
    returning a pointer to the new definition.  *)

function add_definition ( list: definition_list;
			  name: identifier;
			  value: pat_def ): definition_list;

begin
  new (add_definition);
  add_definition^ := ( list, name, value );
end;
$PAGE pop_definitions
(*  POP DEFINITIONS discards definitions from a definition list until a
    particular definition is encountered.  The discarded definition nodes
    are disposed, and their pattern values are freed.  *)

procedure pop_definitions ( from, limit: definition_list );

var def, next_def: definition_list;

begin
  def := from;
  while def <> limit do begin
    next_def := def^.next;
    free_re (def^.value.pattern);
    dispose (def);
    def := next_def;
  end;
end (* pop_definitions *);
$PAGE assign_options
(*  ASSIGN OPTIONS sets one options record to the value of another, recording
    the use of the find and alpha patterns.  *)

procedure assign_options ( var dest: options_record; source: options_record );

begin
  dest := source;
  use_re (dest.find_pattern);
  use_re (dest.alpha_pattern);
end;
$PAGE release_options
(*  RELEASE OPTIONS is called when an options record is about to be assigned to,
    or will no longer be used.  It pops the definition list back to a specified
    definition, and frees the find and alpha patterns.  *)

procedure release_options ( rec: options_record; def_limit: definition_list );

begin
  with rec do begin
    pop_definitions (definitions, def_limit);
    free_re (find_pattern);
    free_re (alpha_pattern);
  end;
end (* release_options *);
$PAGE initialize_the_reader
(*  INITIALIZE THE READER sets up the command line reader variables so that
    the first call to GET A LINE will get the first command line, either from
    the terminal or from an auto-startup file.  *)

procedure initialize_the_reader;

begin
  file_stack_size := 0;
  if auto_run then begin
    reset (cmd_file, '###FND.TMP');
    if (iostatus <> io_ok) orif eof (cmd_file) then
      cmd_file := tty;
  end
  else
    cmd_file := tty;
end;
$PAGE set_standard_defaults
(*  SET STANDARD DEFAULTS sets up the StandardDefaults options record.  This
    contains all the standard option settings, and a large collection of
    predefined patterns.  *)

procedure set_standard_defaults;

const
    char_names: array [chr(0)..chr(32)] of string [3] =
      ( 'NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK', 'BEL', 'BS', 'HT',
	'LF', 'VT', 'FF', 'CR', 'SO', 'SI', 'DLE', 'DC1', 'DC2', 'DC3', 'DC4',
	'NAK', 'SYN', 'ETB', 'CAN', 'EM', 'SUB', 'ESC', 'FS', 'GS', 'RS', 'US',
	'SP' );

var ch: char;
    lc_def, uc_def, n_def, a_def, an_def: pattern;
    def_list: definition_list;

    procedure add_def ( name: identifier; pat: pattern );
    begin
      use_re (pat);
      def_list := add_definition (def_list, name, (pat, true));
    end;

begin
  def_list := nil;
  for ch := chr (0) to chr (32) do
    add_def (char_names [ch], literal (ch));
  add_def ('DEL', literal (chr (127)));
  add_def ('WS', alternate (literal (chr (9)), literal (' ')));
  lc_def := lit_range ('a', 'z');
  uc_def := lit_range ('A', 'Z');
  a_def := alternate (lc_def, uc_def);
  n_def := lit_range ('0', '9');
  an_def := alternate (a_def, n_def);
  add_def ('LC', lc_def);
  add_def ('UC', uc_def);
  add_def ('A', a_def);
  add_def ('N', n_def);
  add_def ('AN', an_def);
  add_def ('PAN', alternate (an_def, literal ('_')));
  add_def ('MAN', alternate (alternate (alternate (an_def, literal ('%')), literal ('$')),
		  literal ('.')));

  assign_options ( standard_defaults, ( catenate_mode,
				      ( false, false, false, false, true ),
				      ( 256, 1 ),
				      ( '', '.FND' ),
				      def_list,
				      an_def,
				      lambda ) );
end (* set_standard_defaults *);
$PAGE do_error
(*  DO ERROR is called with a three-letter error code and a text string.
    It prints an error message, determined by the code, which may contain
    the string.  It also clears the command stack, returning control to the
    terminal.  *)

type
    err_code = packed array [1..3] of char;

procedure do_error ( code: err_code; msg: parm_string );

var err_file: text;
    err_line: string [100];
    text_index: 0 .. 100;

begin

  (*  Find the correct message line in the error message file.  *)

(* The RDLIB routine prgm_dir is not available on the VAX.
   Nor is the error message file *)
$IFNOT vax
  reset (err_file, 'FNDERR.TXT' || prgm_dir ());
  err_line := '   ';
  while (not eof (err_file)) and (substr (err_line, 1, 3) <> code) do
    readln (err_file, err_line);
  close (err_file);
$ENDIF

  (*  Print the error message.  *)

  write (tty, '?FND');
  if substr (err_line, 1, 3) = code then begin
    text_index := search (err_line, ['&']);
    if text_index = 0 then
      writeln (tty, err_line)
    else
      writeln (tty, substr (err_line, 1, text_index - 1), msg,
		    substr (err_line, text_index + 1));
  end
  else
    writeln (tty, code, ' ', msg);

  (*  Exit from any indirect command files.  *)

  if file_stack_size <> 0 then begin
    writeln (tty, '?FNDEIF Error in indirect file ', filename (cmd_file));
    repeat
      close (cmd_file);
      cmd_file := file_stack [file_stack_size];
      file_stack_size := file_stack_size - 1;
    until file_stack_size = 0;
  end;

  if cmd_file <> tty then
    stop; (* <---- Error in auto-startup file -- abort. *)
end (* do_error *);
$PAGE read_a_command
(*  READ A COMMAND will read command lines until it gets a good one.
    It will return the results of its processing in the var parameter
    ParsedCommand.  *)

procedure read_a_command ( var parsed_command: command_record );

const
    line_size = 128;

type
    line_string = string [line_size];

    line_ptr = ^ parm_string;
$PAGE get_a_line
(*  GET A LINE gets a command line from the current command file.  If this is
    the terminal, then it prompts with a star.  End of file on any command file
    other than the primary file pops the command stack.  End of file on the
    terminal is ignored; end of file on an auto-startup file terminates the
    program.

    If a command line ends with a dash, then the dash is discarded, and the
    next line is read and appended to the command, unless we are at the end
    of a file.  The prompt for continuation lines is "#" rather than "*".

    GetALine returns a pointer to a dynamically allocated string in its Line
    parameter.  This should be disposed when it is no longer needed.  *)

procedure get_a_line ( var line: line_ptr );

var cur_line: line_string;
    temp_line: line_ptr;
    continued: boolean; (* Starting to read a continuation line? *)

const
    prompt_char: array [boolean] of char = ( '*', '#' );

begin
  while eof (cmd_file) and (file_stack_size <> 0) do begin
    close (cmd_file);
    cmd_file := file_stack [file_stack_size];
    file_stack_size := file_stack_size - 1;
  end;

  if eof (cmd_file) and (cmd_file <> tty) then
    stop; (* <---- Stop at end of auto-startup file. *)

  new (line, 0);
  continued := false;
  repeat
    if cmd_file = tty then begin
      write (tty, prompt_char [continued]);
      break (tty);
      readln (cmd_file);
    end;
    while not eoln (cmd_file) do begin
      read (cmd_file, cur_line);
      continued := eoln (cmd_file) and (cur_line [length (cur_line)] = '-');
      if continued then
	cur_line := substr (cur_line, 1, length (cur_line) - 1);
      temp_line := line;
      new (line, length (temp_line^) + length (cur_line));
      line^ := temp_line^ || cur_line;
      dispose (temp_line);
    end (* while not eoln *);
    if cmd_file <> tty then
      readln (cmd_file);
  until eof (cmd_file) or not continued;
end (* get_a_line *);
$PAGE parse_command_line
(*  PARSE COMMAND LINE takes a command line in the string parameter CommandLine,
    and parses it into the command record ParsedCommand.  The Success parameter
    is set to indicate whether the command could be successfully parsed.  *)

procedure parse_command_line ( command_line: parm_string;
			       var parsed_command: command_record;
			       var success: boolean );

var cmd_idx: integer;
    cmd_state: ( reading_files, get_switches, line_done );

(*  When a file name or a switch name is successfully parsed, the ErrorContext
    string is set to 'file <name>' or '<name> switch', for use in subsequent
    error messages.  *)

var error_context: string [10 + max (upperbound (file_name), upperbound (identifier))];

label 1 (* command parser failure *);
$PAGE cmd_error
(*  CMD ERROR invokes DoError to print an error message, and then does a
    non-local goto to label 1 in ReadCommandLine.  *)

procedure cmd_error ( code: err_code; text: parm_string );

begin
  do_error (code, text);
  goto 1; (* <---- Back into ReadCommandLine main routine. *)
end;
$PAGE bad_text
(*  BAD TEXT is called when one of a small set of characters is expected, and
    is not found.  It prints one of the messages

	'Illegal character '?' after <context>'

    or

	'Illegal text '???' after <context>'

    where <context> is "file <name>" or "<name> switch".  *)

procedure bad_text;

const
    alphas = ['A'..'Z', '0'..'9'];

var end_text: integer;

begin
  if uppercase (command_line [cmd_idx]) in alphas then begin
    end_text := verify (uppercase (substr (command_line, cmd_idx)), alphas,
			length (command_line) - cmd_idx + 1);
    cmd_error ('ILT', uppercase (substr (command_line, cmd_idx, end_text)) ||
		      ' after ' || error_context);
  end
  else
    cmd_error ('ILC', command_line [cmd_idx] || ' after ' || error_context);
end (* bad_text *);
$PAGE skip_blanks, checkeol, checkpunct
(*  SKIP BLANKS will advance CmdIdx, the command line index pointer, until
    it points to a non-blank character.  *)

procedure skip_blanks;

begin
  while (cmd_idx <= length (command_line)) andif
        (command_line[cmd_idx] = ' ') do
    cmd_idx := cmd_idx + 1;
end;


(*  CHECKEOL will test whether there are any more non-blank characters in
    the command line.  *)

function checkeol (): boolean;

begin
  skip_blanks;
  checkeol := (cmd_idx > length (command_line));
end;


(*  CHECKPUNCT will test whether the next non-blank character in the command
    line is a specified character.  If it is, then the command line index
    will be advanced the punctuation character.  *)

function checkpunct ( punct_char: char ): boolean;

begin
  checkpunct := (not checkeol ()) andif
		(command_line[cmd_idx] = punct_char);
  if checkpunct then
    cmd_idx := cmd_idx + 1;
end;
$PAGE scan_identifier
(*  SCAN IDENTIFIER extracts an alphanumeric identifier from the command
    line.  If there is no such identifier to be found, then it prints a
    specified error message.  *)

function scan_identifier ( code: err_code; text: parm_string ): identifier;

var len: integer;

begin
  if checkeol () orif not (uppercase (command_line [cmd_idx]) in ['A'..'Z']) then
    cmd_error (code, text);
  len := verify (uppercase (substr (command_line, cmd_idx)), ['A'..'Z', '0'..'9'],
		 length (command_line) - cmd_idx + 2) - 1;
  scan_identifier := uppercase (substr (command_line, cmd_idx, len));
  cmd_idx := cmd_idx + len;
end (* scan_identifier *);
$PAGE scan_number
(*  SCAN NUMBER will extract a number from the command line.  If there is
    no number, it will print a specified error message.  *)

function scan_number ( code: err_code; text: parm_string ): integer;

var len: integer;

begin
  if checkeol () orif not (command_line [cmd_idx] in ['0'..'9']) then
    cmd_error (code, text);
  len := verify (substr (command_line, cmd_idx), ['0'..'9'],
		 length (command_line) - cmd_idx + 2) - 1;
  getstring (substr (command_line, cmd_idx, len), scan_number);
  if iostatus <> io_ok then
    cmd_error ('NTL', substr (command_line, cmd_idx, len));
  cmd_idx := cmd_idx + len;
end (* scan_number *);
$PAGE scan_file_name

$IF p10
$INCLUDE find.inc[31024,332214]
$ENDIF

$IF vax
$INCLUDE find.vax[31024,332214]
$ENDIF
$PAGE scan_pattern
(*  SCAN PATTERN will parse a pattern from the command line, returning a
    pattern and a pure pattern, if the pattern was pure.  It will print a
    specified error message if there is a syntax error in the pattern.  *)

procedure scan_pattern ( code: err_code; msg: parm_string;
			 var pat, pure_pat: pattern );

var pure_pattern: pattern;
$PAGE pattern token declarations
type token_type =

     (  nosymbol,

	(*  The following token kinds represent terminal symbols of the pattern
	    grammar.  Getsymbol returns them in InSymbol.  *)

	orsy,		andsy,		notsy,		patternlitsy,
	leftparensy,	rightparensy,	leftanchorsy,	leftnonalphsy,
	rightanchorsy,	rightnonalphsy,	barsy,		ampersandsy,
	literalsy,	dashsy,		primesy,	starsy,
	plussy,		questionsy,	atsy,		numbersy,
	leftbracketsy,	rightbracketsy,	eofsy,

	(*  The following token kinds represent nonterminal symbols of the
	    pattern grammar.  They serve a documentary function.  *)

	accept_nt,	pattern_nt,	pat_term_nt,	pat_fact_nt,
	simp_pat_nt,	post_pat_nt,	reg_expr_nt,	reg_term_nt,
	reg_fact_nt,	element_nt  );


(*  The value of a token is given by a 'sym_value'.  Invalue is a 'sym_
    value', as are the entries in the parse stack.  'Sym_value' is an
    undiscriminated union with variants for the various token kinds.
    The proper variant for an input token may be selected on the basis
    of Insymbol; the variant for a parse stack entry may be selected on
    the basis of the current parser state.  *)

type
    sym_value = record
      case token_type of

	(*  The value of an input literal or any of the various grammar
	    nonterminals is a pattern or regular expression.  *)

	patternlitsy,
	literalsy,
	pattern_nt,
	pat_term_nt,
	pat_fact_nt,
	simp_pat_nt,
	reg_expr_nt,
	reg_term_nt,
	reg_fact_nt,
	element_nt:
	  ( pattern: pattern );

	(*  The value of a numeric input token is its numeric value.  *)

	numbersy:
	  ( number: integer );

    end;
$PAGE parse
(*  PARSE is the primary parsing routine.  Its body is constructed by the
    SLR(1) analyser; the remainder is simply declarations.  *)

const
    stk_max = 40;

type
    stk_node = sym_value;

$INCLUDE lrpars.inc
$PAGE par_error
(*  PAR ERROR is called when an error occurs during parsing.  It is essentially
    a wrapper for CmdError, but has one very important additional function:  it
    pops the parse stack, freeing any patterns which have been created during
    the parsing of this pattern.  *)

procedure par_error ( code: err_code; msg: parm_string );

begin
  while stk_ind <> 0 do begin
    if state[stk_ind] <> 27 then (* State 27 is a number state. *)
      free_re (value[stk_ind].pattern);
    stk_ind := stk_ind - 1;
  end;
  if pure_pattern <> nil then
    free_re (pure_pattern);
  cmd_error (code, msg);
end;
$PAGE have_pure_pattern
(*  HAVE PURE PATTERN is called when the reduction

	<simple pattern> ::= <regular expression>

    is performed.  It records the <regular expression> in the global
    PurePattern variable.  *)

procedure have_pure_pattern ( pat: pattern );

begin
  if pure_pattern <> nil then
    free_re (pure_pattern);
  pure_pattern := pat;
  use_re (pure_pattern);
end;
$PAGE have_impure_pattern
(*  HAVE IMPURE PATTERN is called when a reduction is performed with one of
    the productions involving "<", ">", "||", "&&", or "~".  It frees the
    current PurePattern variabf there is one) and sets it to nil.  *)

procedure have_impure_pattern;

begin
  if pure_pattern <> nil then
    free_re (pure_pattern);
  pure_pattern := nil;
end;
$PAGE getsymbol
(*  GETSYMBOL gets the next pattern token from the command line.  It stores
    the token kind in Insymbol; and if the token is a literal or number, it
    stores the token value in Invalue.  *)

var insymbol: token_type;
    invalue: sym_value;
    defs: definition_list;

procedure getsymbol;

var lit_name: identifier;
    qidx, qstart: integer;

begin
  if checkeol () then
    insymbol := eofsy
  else begin
    case command_line [cmd_idx] of

      'A'..'Z', 'a'..'z':
	begin
	  lit_name := scan_identifier ('', '');
	  defs := parsed_command.cmd_options.definitions;
	  while (defs <> nil) andif (defs^.name <> lit_name) do
	    defs := defs^.next;
	  if defs = nil then
	    par_error ('UPN', lit_name);
	  if defs^.value.pure_pattern
	    then insymbol := literalsy
	    else insymbol := patternlitsy;
	  invalue.pattern := defs^.value.pattern;
	  use_re (invalue.pattern);
	end;

      '0'..'9':
	begin
	  insymbol := numbersy;
	  invalue.number := scan_number ('', '');
	end;

      '"':
	begin
	  qstart := cmd_idx;
	  repeat
	    cmd_idx := cmd_idx + search (substr (command_line, cmd_idx + 1), ['"'],
					 length (command_line) - cmd_idx + 1);
	    if cmd_idx > length (command_line) then
	      par_error ('UTS', substr (command_line, qstart, min (cmd_idx - qstart, 20)));
	    cmd_idx := cmd_idx + 1;
	  until (cmd_idx > length (command_line)) orif (command_line [cmd_idx] <> '"');
	  insymbol := literalsy;
	  qidx := cmd_idx - 2;
	  invalue.pattern := lambda;
	  while qidx <> qstart do begin
	    invalue.pattern := catenate (literal (command_line[qidx]), invalue.pattern);
	    if command_line[qidx] = '"' then
	      qidx := qidx - 1;
	    qidx := qidx - 1;
	  end;
	  use_re (invalue.pattern);
	end;

      '.':
	begin
	  insymbol := literalsy;
	  invalue.pattern := anychar;
	  use_re (invalue.pattern);
	  cmd_idx := cmd_idx + 1;
	end;

      '|':
	begin
	  if (cmd_idx < length (command_line)) andif
	     (command_line[cmd_idx + 1] = '|') then begin
	    insymbol := orsy;
	    cmd_idx := cmd_idx + 1;
	  end
	  else
	    insymbol := barsy;
	end;

      '&':
	begin
	  if (cmd_idx < length (command_line)) andif
	     (command_line[cmd_idx + 1] = '&') then begin
	    insymbol := andsy;
	    cmd_idx := cmd_idx + 1;
	  end
	  else
	    insymbol := ampersandsy;
	end;

      '~':
	insymbol := notsy;

      '<':
	begin
	  if (cmd_idx < length (command_line)) andif
	     (command_line[cmd_idx + 1] = '<') then begin
	    insymbol := leftnonalphsy;
	    cmd_idx := cmd_idx + 1;
	  end
	  else
	    insymbol := leftanchorsy;
	end;

      '>':
	begin
	  if (cmd_idx < length (command_line)) andif
	     (command_line[cmd_idx + 1] = '>') then begin
	    insymbol := rightnonalphsy;
	    cmd_idx := cmd_idx + 1;
	  end
	  else
	    insymbol := rightanchorsy;
	end;

      '(':
	insymbol := leftparensy;

      ')':
	insymbol := rightparensy;

      '-':
	insymbol := dashsy;

      '''':
	insymbol := primesy;

      '*':
	insymbol := starsy;

      '+':
	insymbol := plussy;

      '?':
	insymbol := questionsy;

      '@':
	insymbol := atsy;

      '[':
	insymbol := leftbracketsy;

      ']':
	insymbol := rightbracketsy;

      '/', ',':
	insymbol := eofsy;

      others:
	par_error ('BCP', command_line[cmd_idx]);

    end (* case command_line [cmd_idx] *);
  end (* if not checkeol *);

  if insymbol in [orsy, andsy, notsy, leftanchorsy, rightanchorsy,
		  leftnonalphsy, rightnonalphsy,
		  leftparensy, rightparensy, barsy, ampersandsy,
		  dashsy, primesy, starsy, plussy, questionsy,
		  atsy, leftbracketsy, rightbracketsy] then
    cmd_idx := cmd_idx + 1;
end (* getsymbol *);
$PAGE parse_error
(*  PARSE ERROR is invoked by Parse if it detects a syntax error during parsing
    of the pattern.  *)

procedure parse_error;
begin
  par_error (code, msg);
end;
$PAGE parse - main routine
var itemp: integer;
    lit_ch1, lit_ch2: char;

$INCLUDE find.par
$PAGE scan_pattern - main routine
var parsed_pattern: sym_value;

begin
  pure_pattern := nil;
  if parse (parsed_pattern) then;
  pat := parsed_pattern.pattern;
  pure_pat := pure_pattern;
end;
$PAGE lookup_option
(*  LOOKUP OPTION takes an identifier string which has been read from the
    command line.  If the identifier represents a valid switch, then the
    OptionCode parameter will be set to the indicated option value, and the
    Negated parameter will be true if the switch is negatable and prefixed
    with NO.  Otherwise, an unknown switch error will be reported.  *)

procedure lookup_option ( switch: identifier;
			  var option_code: option_type;
			  var negated: boolean );
$PAGE command names
const
    max_name_len = 10;

type
    option_name_record = record
	name: packed array [1..max_name_len] of char;
	min_abbrev: 1 .. max_name_len;
    end;

const
    option_names: array [option_type] of option_name_record =

     (  ( 'APPEND    ', 3 ),
	( 'CATENATE  ', 3 ),
	( 'CREATE    ', 3 ),

	( 'CONTROL   ', 3 ),
	( 'COUNT     ', 3 ),
	( 'IDENTIFY  ', 2 ),
	( 'NUMBER    ', 3 ),
	( 'PRINT     ', 2 ),

	( 'RECORD    ', 3 ),
	( 'RUNOFFSET ', 6 ),

	( 'INPUT     ', 2 ),
	( 'OUTPUT    ', 3 ),

	( 'ALPHA     ', 2 ),
	( 'DEFINE    ', 3 ),
	( 'EXIT      ', 1 ),
	( 'FIND      ', 1 ),
	( 'HELP      ', 1 ),
	( 'RESET     ', 5 ),
	( 'RUN       ', 3 )  );
$PAGE lookup_option - main routine
var switch_text: identifier;
    code: option_type;
    option_found: boolean;

begin
  negated := (length (switch) > 2) andif (substr (switch, 1, 2) = 'NO');
  if negated
    then switch_text := substr (switch, 3)
    else switch_text := switch;

  option_found := false;
  for code := minimum (option_type) to maximum (option_type) do begin
    with option_names [code] do begin
      exit if (length (switch_text) >= min_abbrev) andif
	      (substr (name, 1, length (switch_text)) = switch_text) do begin
	option_found := true;
	option_code := code;
      end;
    end;
  end;

  if option_found and negated then begin
    if not (option_code in [minimum (yes_no_switches) .. maximum (yes_no_switches)]) then
      option_found := false;
  end;

  if not option_found then
    cmd_error ('UKS', switch);
end (* lookup_option *);
$PAGE more_command
(*  MORE COMMAND is used a test function during the parsing of the command
    line.  If the command line is regarded as being composed of a file-name
    part, with the form [[<file>]=][<file>], and a switch part, with the
    form [/<switch>{(/|,)<switch}], then MoreCommand determines whether
    the command index is still in the file-name part of the command, by
    seeing whether a slash or the end of the line has been encountere yet.
    This function examines and updates a global state variable, CmdState,
    whose value may be ReadingFiles, GetSwitches (slash seen), or LineDone
    (end of line seen).  *)

function more_command (): boolean;

begin
  if cmd_state = reading_files then begin
    if checkeol then
      cmd_state := line_done
    else if checkpunct ('/') then
      cmd_state := get_switches;
  end;
  more_command := (cmd_state = reading_files);
end (* more_command *);
$PAGE switch_value_required
(*  SWITCH VALUE REQUIRED will make sure that the next character is a colon,
    printing an error message if it isn't.  *)

procedure switch_value_required ( switch: identifier );

begin
  if not checkpunct (':') then
    cmd_error ('SVR', switch);
end;
$PAGE process_switch
(*  PROCESS SWITCH will process a single option switch from the command line.  *)

procedure process_switch;

var switch_name: identifier;
    option_code: option_type;
    negative: boolean;
    def_name: identifier;
    pat, pure_pat: pattern;

begin
  if checkeol () then
    cmd_error ('SEE', '');

  switch_name := scan_identifier ('SNE', '');
  error_context := switch_name || ' switch';
  lookup_option (switch_name, option_code, negative);

  with parsed_command, cmd_options, cmd_actions do begin
    case option_code of

      minimum (output_modes) .. maximum (output_modes):
	output_mode := option_code;

      minimum (yes_no_switches) .. maximum (yes_no_switches):
	yes_no[option_code] := not negative;

      record_switch:
	begin
	  switch_value_required (switch_name);
	  numeric [record_switch] := scan_number ('NSR', 'RECORD');
	end;

      runoffset_switch:
	begin
	  if checkpunct (':')
	    then numeric [runoffset_switch] := scan_number ('NSR', 'RUNOFFSET')
	    else numeric [runoffset_switch] := 1;
	end;

      minimum (file_switches) .. maximum (file_switches):
	begin
	  switch_value_required (switch_name);
	  file_defaults[option_code] := scan_file_name (switch_name, false);
	end;

      cmd_alpha:
	begin
	  switch_value_required (switch_name);
	  scan_pattern ('EAP', '', pat, pure_pat);
	  free_re (pat);
	  if pure_pat = nil then
	    cmd_error ('RRA', '');
	  free_re (alpha_pattern);
	  alpha_pattern := pure_pat;
	end;

      cmd_define:
	begin
	  switch_value_required (switch_name);
	  def_name := scan_identifier ('DNR', '');
	  if not checkpunct (':') then
	    cmd_error ('DPR', def_name);
	  scan_pattern ('EDP', def_name, pat, pure_pat);
	  if pure_pat = nil then
	    definitions := add_definition (definitions, def_name, (pat, false))
	  else begin
	    free_re (pat);
	    definitions := add_definition (definitions, def_name, (pure_pat, true));
	  end;
	end;

      cmd_exit:
	exit_action := true;

      cmd_find:
	begin
	  switch_value_required (switch_name);
	  scan_pattern ('EFP', '', pat, pure_pat);
	  free_re (find_pattern);
	  find_pattern := pat;
	  if pure_pat <> nil then
	    free_re (pure_pat);
	end;

      cmd_help:
	begin
	  if checkpunct (':')
	    then help_action := scan_identifier ('HSR', '')
	    else help_action := 'HELP';
	end;

      cmd_reset:
	begin
	  release_options (cmd_options, default_options.definitions);
	  assign_options (cmd_options, standard_defaults);
	  reset_action := true;
	end;

      cmd_run:
	begin
	  switch_value_required (switch_name);
	  run_name := scan_file_name ('Run', true);
	end;


    end (* case option_code *);
  end (* with parsed_command *);
end (* process_switch *);
$PAGE parse_file_part
(*  PARSE FILE PART parses the file part of a command line.  This is the
    part of the command line that has the form:

	[[<output file>] = ] <input file>				*)

procedure parse_file_part;

begin
  with parsed_command, cmd_actions do begin

    if more_command () then begin
      if checkpunct ('=') then begin
	output_name := '';
	input_name := scan_file_name ('Source', true);
      end

      else begin
	output_name := terminal_name;
	input_name := scan_file_name ('Source', true);
	if more_command () then begin
	  if checkpunct ('=') then begin
	    output_name := input_name;
	    input_name := scan_file_name ('Source', true);
	  end
	  else
	    bad_text;
	end;
      end;

    end;

  end (* with *);
end (* parse_file_part *);
$PAGE parse_switch_part
(*  PARSE SWITCH PART parses the switch part of a command line.  This is the
    part of the command line that has the form:

	[ / <switch> {(/|,) <switch>}]					*)

procedure parse_switch_part;

begin
  if more_command () then
    bad_text

  else if cmd_state = get_switches then begin
    repeat
      process_switch;
    until not (checkpunct ('/') orif checkpunct (','));
    if not checkeol () then
      bad_text;
  end;
end (* parse_switch_part *);
$PAGE parse_command_line - main routine
begin
  cmd_idx := 1;
  cmd_state := reading_files;

  if checkpunct ('@') then begin (* Indirect file. *)
    parsed_command.cmd_actions.indirect_name := scan_file_name ('Indirect', true);
    if more_command () then
      bad_text;
  end

  else begin
    parse_file_part;
    parse_switch_part;
  end;

  success := true;
  return; (* <----  Return with a successful parse. *)

1 (* command parse error *):

  success := false;
end (* parse_command_line *);
$PAGE read_a_command - main routine
var cmd_line: line_ptr;
    success: boolean;

begin
  loop
    assign_options (parsed_command.cmd_options, default_options);
    parsed_command.cmd_actions := no_actions;
    get_a_line (cmd_line);
    parse_command_line (cmd_line^, parsed_command, success);
    dispose (cmd_line);

    if success then return; (* <---- Return with parsed command. *)

    with parsed_command do begin (* Restore default command. *)
      if cmd_actions.reset_action
	then release_options (cmd_options, standard_defaults.definitions)
	else release_options (cmd_options, default_options.definitions);
    end;
  end (* loop *);
end (* read_a_command *);
$PAGE process_the_command

(*  PROCESS THE COMMAND performs the processing associated with a parsed
    command line in the record Command.  *)

procedure process_the_command ( command: command_record );

label 1 (* error abort *);
$PAGE process_error
(*  PROCESS ERROR invokes DoError to print an error message, and then does a
    non-local goto to label 1 in ReadCommandLine.  *)

procedure process_error ( code: err_code; text: parm_string );

begin
  do_error (code, text);
  goto 1; (* <---- Back into ProcessTheCommand main routine. *)
end;
$PAGE do_help
(*  DO HELP will print a specified help message.  Help messages are read from
    the help file, which consists of text sections, delimited by header lines
    with the format "[name]".  The last line in the file is "[]".  A help
    code causes printing of the section whose name it matches.  Any help code
    which does not match any of the section names will cause the first section
    to be printed.  *)

procedure do_help ( help_code: identifier );

var help_file: text;
    help_line: string [80];
    l: integer;

begin

(* The RDLIB routine prgm_dir is not available on the VAX.
   Nor is the help file.
 *)
$IFNOT vax
  reset (help_file, 'FIND.HLP' || prgm_dir ());
  if iostatus <> io_ok then
    process_error ('HIN', '');
$ENDIF

  l := min (length (help_code), 3);
  if substr (help_code, 1, l) = substr ('ALL', 1, l) then begin
    while not eof (help_file) do begin
      readln (help_file, help_line);
      if (help_line = '') orif (help_line[1] = '[') then
	writeln (tty)
      else
	writeln (tty, help_line);
    end;
  end

  else begin
    repeat
      while help_file^ <> '[' do
	readln (help_file);
      readln (help_file, help_line);
      l := min (length (help_line) - 2, length (help_code));
    until substr (help_line, 2, l) = substr (help_code, 1, l);
    if eof (help_file) then begin
      reset (help_file, filename (help_file));
      readln (help_file);
    end;
    while help_file^ <> '[' do begin
      readln (help_file, help_line);
      writeln (tty, help_line);
    end;
  end;
end (* do_help *);
$PAGE do_indirect
(*  DO INDIRECT will save the current command file and default options on
    the command stack, and will open a new indirect command file with a new
    set of default options.  *)

procedure do_indirect ( ind_name: file_name );

var indirect_file: text;

begin
  if file_stack_size = indirect_limit then
    process_error ('IFN', ind_name);

  (*  Open the indirect file.  *)

  reset (indirect_file, '.CMD ' || ind_name);
  if iostatus <> io_ok then begin
    reset (indirect_file, '.CCL ' || ind_name);
    if iostatus <> io_ok then
      process_error ('UOI', ind_name);
  end;

  (*  Push the old file on the file stack.  *)

  file_stack_size := file_stack_size + 1;
  file_stack [file_stack_size] := cmd_file;
  cmd_file := indirect_file;
end (* do_indirect *);
$PAGE process_file
(*  PROCESS FILE reads a specified input file, processing it according to
    the directions in a specified options record.  *)

procedure process_file ( input_name, output_name: file_name; cmd_options: options_record );
$PAGE open_input

(*  OPEN INPUT opens the input file.  The input name is taken from the specified
    name and the current input file name defaults.  The Control switch determines
    whether the file is opened in normal or Ascii mode.  *)

procedure open_input;

const
    is_ascii: array [boolean] of set of io_options = ( [], [ascii] );

begin
  with cmd_options do begin
    reset ( input,
	    file_defaults[input_switch] || ' ' || input_name,
	    is_ascii[yes_no[control_switch]] );
    if iostatus <> io_ok then
      process_error ('UOS', input_name);
  end;
end (* open_input *);
$PAGE open_output

(*  OPEN OUTPUT opens the output file.  If output is not to the terminal, then
    the output name is taken from the specified name, the current output file
    name defaults, and the input file (except that the directory of the input
    file is ignored.

    The opening of the file is controlled by the FileMode switch.  If the mode
    is Append or Catenate, then the file is opened in Preserve mode.  However,
    if the mode is catenate, and the name of the opened file does not match the
    name of the current output file, then the file is emptied.  *)

procedure open_output;

const
    preserve_it: array [boolean] of set of io_options = ( [], [preserve] );

begin
  with cmd_options do begin
    if output_name = terminal_name then begin
      output := ttyoutput;
      current_output := terminal_name;
    end

    else begin
      rewrite ( output,
		filename (input) || '[]' || file_defaults[output_switch] ||
		' ' || output_name,
		preserve_it[output_mode <> create_mode] );
      if iostatus <> io_ok then
	process_error ('UOO', output_name);
      if (output_mode = catenate_mode) and
	 (current_output <> terminal_name) and
	 (filename (output) <> current_output) then
	empty (output);
      current_output := filename (output);
    end;
  end (* with cmd_options *);
end (* open_output *);
$PAGE process_file - main routine

type
    out_codes = ( noprint_nonumber, noprint_number, print_nonumber, print_number );

const
    out_code_selector: array [boolean, boolean] of out_codes =
      ( (noprint_nonumber, noprint_number), (print_nonumber, print_number) );

var input_line: ^ string [*];
    out_code: out_codes;
    line_count, match_count: integer;

begin
  with cmd_options do begin
    open_input;
    open_output;
    new (input_line, numeric[record_switch]);
    set_recognizer (find_pattern);

    if yes_no[identify_switch] then
      writeln (output, '******  ', filename (input));
    out_code := out_code_selector [yes_no[print_switch], yes_no[number_switch]];
    line_count := 0;
    match_count := 0;

    while not eof (input) do begin
      line_count := line_count + 1;
      read (input, input_line^);
      if not eoln (input) then
	writeln (tty, '[Line', line_count, ' truncated]');
      readln (input);
      if match (input_line^) then begin
	match_count := match_count + 1;
	case out_code of
	  print_nonumber:  writeln (output, input_line^);
	  noprint_number:  writeln (output, line_count:6);
	  print_number:    writeln (output, line_count:6, '  ', input_line^);
	  noprint_nonumber:  (* no action *);
	end;
      end;
    end (* while not eof *);

    if yes_no[count_switch] then
      writeln (output, '******  ', match_count:0);

    if input <> tty then
      close (input);
    if output <> ttyoutput then
      close (output);
    dispose (input_line);
  end (* with *);
end (* process_file *);
$PAGE process_the_command - main routine
begin
  with command, cmd_actions do begin

    if (help_action <> '') then
      do_help (help_action)

    else if run_name <> '' then begin
      run (run_name, cmd_options.numeric[runoffset_switch]);
      process_error ('URP', run_name);
    end

    else if exit_action then
      stop

    else if indirect_name <> '' then
      do_indirect (indirect_name)

    else if input_name <> '' then
      process_file (input_name, output_name, cmd_options)

    else begin
      if reset_action
	then release_options (default_options, standard_defaults.definitions)
	else release_options (default_options, default_options.definitions);
      assign_options (default_options, cmd_options);
    end;
  end;

1 (* Error abort *):

end (* process_the_command *);
$PAGE find - main program
var command: command_record;

begin
  rewrite (tty);
  writeln (tty, 'FIND Version 1');
  writeln (tty);
  open (tty);

  initialize_the_reader;
  initre;
  set_standard_defaults;
  clr_recognizer;
  current_output := '';
  assign_options (default_options, standard_defaults);
  loop
    read_a_command (command);
    process_the_command (command);
    release_options (command.cmd_options, default_options.definitions);
  end;
end (* find *).
i ~¢