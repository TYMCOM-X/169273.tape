$TITLE CMDUTL - Command Line Processing Utilities

module cmdutl;

type
    cmd_line = packed array [1..*] of char;
    cmd_lookup_record = record
	text: packed array [1..10] of char;
	abbrev: 1 .. 10;
	code: integer;
    end;
$PAGE cmd_skip_blanks
$HEADER cmdski
$PAGE
public procedure cmd_skip_blanks ( line: cmd_line; var cursor: integer );

begin
  assert (cursor > 0);
  if cursor <= length (line) then
    cursor := verify (substr (line, cursor), [' '], length (line) - cursor + 2)
		+ cursor - 1;
end (* cmd_skip_blanks *);
$PAGE cmd_eol
$HEADER cmdeol
$PAGE
public function cmd_eol ( line: cmd_line; var cursor: integer ): boolean;

begin
  cmd_skip_blanks (line, cursor);
  cmd_eol := (cursor > length (line));
end (* cmd_eol *);
$PAGE cmd_check_punct
$HEADER cmdche
$PAGE
public function cmd_check_punct ( line: cmd_line; var cursor: integer;
				  punct: char ): boolean;

begin
  cmd_check_punct := (not cmd_eol (line, cursor)) andif (line [cursor] = punct);
  if cmd_check_punct then
    cursor := cursor + 1;
end (* cmd_check_punct *);
$PAGE cmd_token
$HEADER cmdtok
$PAGE
public function cmd_token ( line: cmd_line; var cursor: integer;
			    token_chars: set of char;
			    var token: string [*] ): boolean;

var len: integer;

begin
  if cmd_eol (line, cursor) then
    cmd_token := false (* end of line, no token *)
  else begin
    len := verify (substr (line, cursor), token_chars, length (line) - cursor + 2) - 1;
    if len = 0 then
      cmd_token := false (* no token to be found *)
    else begin
      cmd_token := true;
      token := substr (line, cursor, len);
      cursor := cursor + len;
    end;
  end;
end (* cmd_token *);
$PAGE cmd_number
$HEADER cmdnum
$PAGE
public function cmd_number ( line: cmd_line; var cursor: integer;
			     neg_ok: boolean; var number: integer ): boolean;

var c, len: integer;
    signed: 0 .. 1;

begin
  cmd_number := false; (* assume until number found *)
  if not cmd_eol (line, cursor) then begin
    signed := ord ((line [cursor] = '-') and neg_ok);
    c := cursor + signed;
    len := verify (substr (line, c), ['0'..'9'], length (line) - c + 2) - 1;
    if len <> 0 then begin
      getstring (substr (line, cursor, len + signed), number);
      if iostatus = io_ok then begin
	cmd_number := true;
	cursor := c + len;
      end;
    end;
  end;
end (* cmd_number *);
$PAGE cmd_lookup
$HEADER cmdloo
$PAGE
public function cmd_lookup ( line: cmd_line; var cursor: integer;
			     token_chars: set of char;
			     list: array [1..*] of cmd_lookup_record;
			     var match: integer ): boolean;

var name: packed array [1..10] of char;
    i, l, l1: integer;

begin
  cmd_lookup := false; (* assume until lookup succeeds *)
  if not cmd_eol (line, cursor) then begin
    if uppercase (line [cursor]) in token_chars then begin (* get an alphanumeric name *)
      l := verify (uppercase (substr (line, cursor)), token_chars, length (line) - cursor + 2) - 1;
      l1 := min (l, 10);
      name := uppercase (substr (line, cursor, l1));
      for i := 1 to upperbound (list) do begin
	with list [i] do begin
	  exit if (l1 >= abbrev) and (substr (text, 1, l1) = name) do begin
	    match := code;
	    cursor := cursor + l;
	    cmd_lookup := true;
	  end;
	end;
      end (* for i *);
    end

    else (* line [cursor] not in token_chars *) begin
      l1 := length (line) - cursor + 1;
      for i := 1 to upperbound (list) do begin
	with list [i] do begin
	  exit if (text [1] = line [cursor]) andif
		  (l1 >= abbrev) andif
		  (substr (text, 1, abbrev) = substr (line, cursor, abbrev)) do begin
	    match := code;
	    cursor := cursor + abbrev;
	    cmd_lookup := true;
	  end;
	end;
      end (* for i *);
    end;
  end (* if not cmd_eol *);
end (* cmd_lookup *);
$PAGE cmd_string
$HEADER cmdstr
$PAGE
public function cmd_string ( line: cmd_line; var cursor: integer;
			     quote: char; var rstring: string [*] ): boolean;

var len: integer;

begin
  assert (cursor > 0);
  if cursor > length (line) then
    cmd_string := false
  else begin
    len := search (substr (line, cursor), [quote]);
    if len = 0 then
      cmd_string := false (* no closing quote *)
    else begin
      cmd_string := true;
      rstring := substr (line, cursor, len - 1);
      cursor := cursor + len;
    end;
  end;
end (* cmd_string *);
$PAGE cmd_dqstring
$HEADER cmddqs
$PAGE
public function cmd_dqstring ( line: cmd_line; var cursor: integer;
			       quote: char; var rstring: string [*] ): boolean;

var c1, len: integer;

begin
  assert (cursor > 0);
  rstring := '';
  if cursor > length (line) then
    cmd_dqstring := false
  else begin
    c1 := cursor;
    loop
      len := search (substr (line, c1), [quote]);
    exit if len = 0
      do
	cmd_dqstring := false; (* no terminating quote *)
      rstring := rstring || substr (line, c1, len - 1);
      c1 := c1 + len;
    exit if (c1 > length (line)) orif (line [c1] <> quote)
      do
	cmd_dqstring := true; (* terminating quote found *)
      rstring := rstring || quote;
      c1 := c1 + 1;
    end;
    if cmd_dqstring then
      cursor := c1;
  end;
end (* cmd_dqstring *);
$PAGE cmd_file_name
$HEADER cmdfil
$PAGE
public function cmd_file_name ( line: cmd_line; var cursor: integer;
				name_required: boolean; var name: string [*] ): boolean;

CONST
   MAXTOKEN    =    4;
   DFASTATE1   =    1;  (* CODE FOR INITIAL STATE OF DFA *)
   MAXDFASTATE =   57;  (* CODE FOR MAX STATE OF DFA *)
   MINTERMINAL =  -16;  (* MIN TERMINAL CODE *)
   EODATA      =   -1;  (* CODE FOR END-OF-DATA *)

TYPE
   STATERANGE  = 1..MAXDFASTATE;
   EXSTATERANGE= 0..MAXDFASTATE;
   LEXTOKEN    = ( ERROR_TOKEN, DEFAULT_TOKEN, FILE_TOKEN );

CONST
   DELTA: PACKED ARRAY[STATERANGE,MINTERMINAL..EODATA] OF EXSTATERANGE = (
      0,21,0,0,20,14,0,4,0,19,19,19,3,3,3,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,52,0,0,30,18,0,0,15,19,19,19,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,
      10,10,10,0,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,53,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,13,0,0,0,0,0,0,0,0,0,0,7,0,0,0,0,0,7,0,0,0,0,0,0,0,
      0,0,8,0,0,0,0,0,0,53,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,27,0,0,0,0,
      0,10,10,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,40,0,0,0,0,0,
      0,0,0,0,0,0,0,57,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,21,0,0,20,0,0,0,0,
      14,14,14,14,14,14,0,0,21,0,0,20,14,0,4,0,19,19,19,19,19,19,0,0,0,
      0,0,44,0,0,0,0,0,0,0,0,0,0,0,0,56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,52,
      0,0,30,0,0,0,0,18,18,18,18,18,18,0,0,52,0,0,30,18,0,0,0,19,19,19,
      19,19,19,0,0,0,0,7,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,39,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0,0,
      0,0,23,0,0,0,0,0,23,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31,0,0,36,50,0,0,0,29,
      29,29,29,29,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31,0,0,36,51,0,
      0,0,29,29,29,29,29,29,0,0,0,0,23,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,55,0,0,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,
      0,0,0,0,0,0,0,0,0,33,0,0,0,0,0,33,0,0,0,0,0,0,0,0,0,34,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,33,0,0,0,0,0,0,0,0,0,34,0,0,0,0,26,
      0,0,0,0,0,0,0,0,0,0,37,0,0,0,0,0,37,0,0,0,0,0,0,0,0,0,38,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,11,0,0,0,0,0,37,0,0,0,0,0,0,0,0,0,38,0,0,0,0,
      28,0,0,0,0,0,0,0,0,0,0,41,0,0,0,0,0,41,0,0,0,0,0,0,0,0,0,42,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,25,0,0,0,0,0,41,0,0,0,0,0,0,0,0,0,42,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,35,0,0,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,46,0,0,26,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,48,0,0,0,31,0,0,36,0,0,0,0,50,50,50,50,50,50,
      0,0,31,0,0,36,0,0,0,0,51,51,51,51,51,51,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,43,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,47,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      45,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,54,0,0);

   (* FINAL[X] = 0 IF STATE X IS NOT A FINAL STATE
                 1 IF STATE X RECOGNIZES <*****END OF DATA****>
                 2 IF STATE X RECOGNIZES <FILE SPEC>
                 3 IF STATE X RECOGNIZES <FILE DEFAULTS>
                 4 IF STATE X RECOGNIZES <BAD SPEC>
                                                                 *)
   FINAL: PACKED ARRAY[EXSTATERANGE] OF 0..MAXTOKEN = (
      0,3,1,2,4,0,0,0,0,0,0,0,3,3,3,3,2,2,2,2,4,4,0,0,0,0,3,3,2,2,4,4,0,
      0,0,0,4,0,0,0,4,0,0,0,4,0,0,0,0,0,3,2,4,4,0,0,4,4);

   TOKEN_KIND: PACKED ARRAY [0..MAXTOKEN] OF LEXTOKEN = (
      ERROR_TOKEN, DEFAULT_TOKEN, FILE_TOKEN, DEFAULT_TOKEN, ERROR_TOKEN );
$PAGE
PROCEDURE SCAN(BEGIN_INDEX: integer; var END_INDEX: integer;
               VAR LEXEME: LEXTOKEN);

   VAR
      CURRSTATE, CURRFINAL: EXSTATERANGE;
      OLDINDEX:  integer;

  (* GETCHAR maps characters from the input line into the terminal classes
     expected by SCAN and used to index the transition matrix.  *)

  function getchar: minterminal..eodata;

    begin
      if end_index >= length (line) then begin	(* past end of string *)
	getchar := eodata;
      end
      else begin
        end_index := end_index + 1;
	case uppercase ( line [ end_index ] ) of
	  'A'..'Z':	getchar := -2;
	  '0'..'7':	getchar := -3;
	  '8','9':	getchar := -4;
	  '#':		getchar := -5;
	  '?':		getchar := -6;
	  '*':		getchar := -7;
	  ':':		getchar := -8;
	  '(':		getchar := -9;
	  ')':		getchar := -10;
	  '.':		getchar := -11;
	  '[':		getchar := -12;
	  ',':		getchar := -13;
	  ']':		getchar := -14;
	  '<':		getchar := -15;
	  '>':		getchar := -16;
	  others:	getchar := -1		(* end of data *)
	end (* case *) ;
      end  (* else *) ;
    end  (* getchar *);
$PAGE
   BEGIN (* SCAN *)
      CURRSTATE := DFASTATE1;  (* START IN INITIAL STATE *)
      CURRFINAL := 0;
      OLDINDEX  := 0;  (* width of lexeme AS OF LAST FINAL STATE *)
      end_index := begin_index - 1;

      WHILE CURRSTATE <> 0 DO
         BEGIN
            IF FINAL[CURRSTATE] <> 0 THEN
               BEGIN
                  CURRFINAL := CURRSTATE;
                  OLDINDEX := END_INDEX - BEGIN_INDEX + 1
               END;
            CURRSTATE := DELTA[CURRSTATE, getchar ()];
         END;
      END_INDEX := BEGIN_INDEX + OLDINDEX;	(* on exit, END_INDEX is index of 1st *)
						(* char PAST file name *)

      lexeme := token_kind [final [currfinal]];

   END; (* SCAN *)
$PAGE
var end_index: integer;
    lexeme: lextoken;

begin
  cmd_skip_blanks (line, cursor);
  scan (cursor, end_index, lexeme);

  cmd_file_name := (lexeme = file_token) or
		   ( (not name_required) and (lexeme = default_token) );

  if cmd_file_name then begin
    name := substr (line, cursor, end_index - cursor);
    cursor := end_index;
  end;
end (* cmd_file_name *);
$PAGE cmd_query
$HEADER cmdque
$PAGE
public function cmd_query ( question, elaboration: packed array [1..*] of char ): boolean;

var response: string [10];

begin
  write (tty, question);
  loop
    break (ttyoutput);
    readln (tty);
    read (tty, response);
    response := uppercase (response);

  exit if (response = 'YES') or (response = 'YE') or (response = 'Y') do
    cmd_query := true;

  exit if (response = 'NO') or (response = 'N') do
    cmd_query := false;

    if (response = '') or (elaboration = '')
      then write (tty, question)
      else write (tty, elaboration);
  end;
end (* cmd_query *);
$PAGE cmd_getline
$HEADER cmdget
$PAGE
public procedure cmd_getline ( prompt: packed array [1..*] of char;
			    var line: string [*]; var cursor: integer );

begin
  write (tty, prompt);
  break (ttyoutput);
  readln (tty);
  read (tty, line);
  cursor := 1;
end (* cmd_getline *);
$PAGE cmd_display_table
$HEADER cmddis
$PAGE
public procedure cmd_display_table ( list: array [1..*] of cmd_lookup_record;
				     indent, line_width: integer );

var posn: integer;
    i: integer;
    name: string [11];

begin
  posn := line_width;
  for i := 1 to upperbound (list) do begin
    name := substr (list[i].text, 1, search (list[i].text, [' '], 11) - 1);
    if i <> upperbound (list) then
      name := name || ',';
    posn := posn + length (name) + 1;
    if posn <= line_width then
      write (tty, ' ', name)
    else begin
      if i <> 1 then
	writeln (tty);
      write (tty, '': indent, name);
      posn := indent + length (name);
    end;
  end;
  writeln (tty);
end (* cmd_display_table *).
   