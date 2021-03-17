$TITLE CMDUTL.PAS, last modified 5/11/84, zw
MODULE cmdutl;
(*TYM-Pascal Command Line Processing Utilities*)

TYPE
cmd_line = PACKED ARRAY [1..*] OF CHAR;
cmd_lookup_record = RECORD
  TEXT: PACKED ARRAY [1..10] OF CHAR;
  abbrev: 1 .. 10;
  code: INTEGER;
END;
$PAGE cmd_skip_blanks
$HEADER cmdski
$PAGE
PUBLIC
PROCEDURE cmd_skip_blanks ( line: cmd_line; VAR CURSOR: INTEGER );
BEGIN
  assert (CURSOR > 0);
  IF CURSOR <= LENGTH (line) THEN CURSOR := VERIFY (SUBSTR (line, CURSOR)
    , [' '], LENGTH (line) - CURSOR + 2) + CURSOR - 1;
END (* cmd_skip_blanks *);
$PAGE cmd_eol
$HEADER cmdeol
$PAGE
PUBLIC
FUNCTION cmd_eol ( line: cmd_line; VAR CURSOR: INTEGER ): BOOLEAN;
BEGIN
  cmd_skip_blanks (line, CURSOR);
  cmd_eol := (CURSOR > LENGTH (line));
END (* cmd_eol *);
$PAGE cmd_check_punct
$HEADER cmdche
$PAGE
PUBLIC
FUNCTION cmd_check_punct ( line: cmd_line; VAR CURSOR: INTEGER; punct: CHAR )
  : BOOLEAN;
BEGIN
  cmd_check_punct := (NOT cmd_eol (line, CURSOR)) ANDIF
    (line [CURSOR] = punct);
  IF cmd_check_punct THEN CURSOR := CURSOR + 1;
END (* cmd_check_punct *);
$PAGE cmd_token
$HEADER cmdtok
$PAGE
PUBLIC
FUNCTION cmd_token ( line: cmd_line; VAR CURSOR: INTEGER;
  token_chars: SET OF CHAR; VAR token: STRING [*] ): BOOLEAN;
VAR
len: INTEGER;
BEGIN
  IF cmd_eol (line, CURSOR) THEN cmd_token := FALSE (* end of line, no token *)
  ELSE BEGIN
    len := VERIFY (SUBSTR (line, CURSOR), token_chars, LENGTH (line)
      - CURSOR + 2) - 1;
    IF len = 0 THEN cmd_token := FALSE (* no token to be found *)
    ELSE BEGIN
      cmd_token := TRUE;
      token := SUBSTR (line, CURSOR, len);
      CURSOR := CURSOR + len;
    END;
  END;
END (* cmd_token *);
$PAGE cmd_number
$HEADER cmdnum
$PAGE
PUBLIC
FUNCTION cmd_number ( line: cmd_line; VAR CURSOR: INTEGER; neg_ok: BOOLEAN;
  VAR number: INTEGER ): BOOLEAN;
VAR
c, len: INTEGER;
signed: 0 .. 1;
BEGIN
  cmd_number := FALSE; (* assume until number found *)
  IF NOT cmd_eol (line, CURSOR) THEN BEGIN
    signed := ORD ((line [CURSOR] = '-') AND neg_ok);
    c := CURSOR + signed;
    len := VERIFY (SUBSTR (line, c), ['0'..'9'], LENGTH (line) - c + 2) - 1;
    IF len <> 0 THEN BEGIN
      GETSTRING (SUBSTR (line, CURSOR, len + signed), number);
      IF iostatus = IO_OK THEN BEGIN
	cmd_number := TRUE;
	CURSOR := c + len;
      END;
    END;
  END;
END (* cmd_number *);
$PAGE cmd_lookup
$HEADER cmdloo
$PAGE
PUBLIC
FUNCTION cmd_lookup ( line: cmd_line; VAR CURSOR: INTEGER;
  token_chars: SET OF CHAR; list: ARRAY [1..*] OF cmd_lookup_record;
  VAR match: INTEGER ): BOOLEAN;
VAR
name: PACKED ARRAY [1..10] OF CHAR;
i, l, l1: INTEGER;
BEGIN
  cmd_lookup := FALSE; (* assume until lookup succeeds *)
  IF NOT cmd_eol (line, CURSOR) THEN BEGIN
    IF UPPERCASE (line [CURSOR]) IN token_chars THEN BEGIN (* get an alphanumeric name *)
      l := VERIFY (UPPERCASE (SUBSTR (line, CURSOR))
	, token_chars, LENGTH (line) - CURSOR + 2) - 1;
      l1 := MIN (l, 10);
      name := UPPERCASE (SUBSTR (line, CURSOR, l1));
      FOR i := 1 TO UPPERBOUND (list) DO BEGIN
	WITH list [i] DO BEGIN
	  EXIT IF (l1 >= abbrev) AND (SUBSTR (TEXT, 1, l1) = name) DO BEGIN
	    match := code;
	    CURSOR := CURSOR + l;
	    cmd_lookup := TRUE;
	  END;
	END;
      END (* for i *);
    END
    ELSE (* line [cursor] not in token_chars *)BEGIN
      l1 := LENGTH (line) - CURSOR + 1;
      FOR i := 1 TO UPPERBOUND (list) DO BEGIN
	WITH list [i] DO BEGIN
	  EXIT IF (TEXT [1] = line [CURSOR]) ANDIF (l1 >= abbrev) ANDIF
	    (SUBSTR (TEXT, 1, abbrev) = SUBSTR (line, CURSOR, abbrev))
	    DO BEGIN
	    match := code;
	    CURSOR := CURSOR + abbrev;
	    cmd_lookup := TRUE;
	  END;
	END;
      END (* for i *);
    END;
  END (* if not cmd_eol *);
END (* cmd_lookup *);
$PAGE cmd_string
$HEADER cmdstr
$PAGE
PUBLIC
FUNCTION cmd_string ( line: cmd_line; VAR CURSOR: INTEGER; quote: CHAR;
  VAR rstring: STRING [*] ): BOOLEAN;
VAR
len: INTEGER;
BEGIN
  assert (CURSOR > 0);
  IF CURSOR > LENGTH (line) THEN cmd_string := FALSE
  ELSE BEGIN
    len := SEARCH (SUBSTR (line, CURSOR), [quote]);
    IF len = 0 THEN cmd_string := FALSE (* no closing quote *)
    ELSE BEGIN
      cmd_string := TRUE;
      rstring := SUBSTR (line, CURSOR, len - 1);
      CURSOR := CURSOR + len;
    END;
  END;
END (* cmd_string *);
$PAGE cmd_dqstring
$HEADER cmddqs
$PAGE
PUBLIC
FUNCTION cmd_dqstring ( line: cmd_line; VAR CURSOR: INTEGER; quote: CHAR;
  VAR rstring: STRING [*] ): BOOLEAN;
VAR
c1, len: INTEGER;
BEGIN
  assert (CURSOR > 0);
  rstring := '';
  IF CURSOR > LENGTH (line) THEN cmd_dqstring := FALSE
  ELSE BEGIN
    c1 := CURSOR;
    LOOP
      len := SEARCH (SUBSTR (line, c1), [quote]);
      EXIT IF len = 0 DO cmd_dqstring := FALSE; (* no terminating quote *)
      rstring := rstring || SUBSTR (line, c1, len - 1);
      c1 := c1 + len;
      EXIT IF (c1 > LENGTH (line)) ORIF (line [c1] <> quote)
	DO cmd_dqstring := TRUE; (* terminating quote found *)
      rstring := rstring || quote;
      c1 := c1 + 1;
    END;
    IF cmd_dqstring THEN CURSOR := c1;
  END;
END (* cmd_dqstring *);
$PAGE cmd_file_name
$HEADER cmdfil
$PAGE
PUBLIC
FUNCTION cmd_file_name ( line: cmd_line; VAR CURSOR: INTEGER;
  name_required: BOOLEAN; VAR name: STRING [*] ): BOOLEAN;
CONST
maxtoken = 4;
dfastate1 = 1; (* CODE FOR INITIAL STATE OF DFA *)
maxdfastate = 57; (* CODE FOR MAX STATE OF DFA *)
minterminal = -16; (* MIN TERMINAL CODE *)
eodata = -1; (* CODE FOR END-OF-DATA *)
TYPE
staterange = 1..maxdfastate;
exstaterange= 0..maxdfastate;
lextoken = ( error_token, default_token, file_token );
CONST
delta: PACKED ARRAY[staterange,minterminal..eodata] OF exstaterange = ( 0,21,0
  ,0,20,14,0,4,0,19,19,19,3,3,3,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,52,0,0,30
  ,18,0,0,15,19,19,19,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0, 10,10,10,0,12,0,0,0,0,0
  ,0,0,0,0,0,0,0,0,0,0,0,53,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,13,0,0,0,0,0,0,0,
  0,0,0,7,0,0,0,0,0,7,0,0,0,0,0,0,0, 0,0,8,0,0,0,0,0,0,53,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,27,0,0,0,0, 0,10,10,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0
  ,0,40,0,0,0,0,0, 0,0,0,0,0,0,0,57,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,21,0,0,20,0,
  0,0,0, 14,14,14,14,14,14,0,0,21,0,0,20,14,0,4,0,19,19,19,19,19,19,0,0,0, 0,0
  ,44,0,0,0,0,0,0,0,0,0,0,0,0,56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,52, 0,0,30,0,0,
  0,0,18,18,18,18,18,18,0,0,52,0,0,30,18,0,0,0,19,19,19, 19,19,19,0,0,0,0,7,0,
  0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,39,0,0,16,0,0,0,0,0,0,0,0,0
  ,0,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0,0, 0,0,23,0,0,0,0,0,23,0,0,0,0,0,0,0,0,0,
  24,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  31,0,0,36,50,0,0,0,29, 29,29,29,29,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31
  ,0,0,36,51,0, 0,0,29,29,29,29,29,29,0,0,0,0,23,0,0,0,0,0,0,0,0,0,24,0,0,0,0,
  0,0, 0,0,0,0,0,0,0,0,0,55,0,0,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0, 0,0,
  0,0,0,0,0,0,0,33,0,0,0,0,0,33,0,0,0,0,0,0,0,0,0,34,0,0,0,0,0,0, 0,0,0,0,0,0,
  0,0,0,32,0,0,0,0,0,33,0,0,0,0,0,0,0,0,0,34,0,0,0,0,26, 0,0,0,0,0,0,0,0,0,0,
  37,0,0,0,0,0,37,0,0,0,0,0,0,0,0,0,38,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,11,0,0,0
  ,0,0,37,0,0,0,0,0,0,0,0,0,38,0,0,0,0, 28,0,0,0,0,0,0,0,0,0,0,41,0,0,0,0,0,41
  ,0,0,0,0,0,0,0,0,0,42,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,25,0,0,0,0,0,41,0,0,0,0
  ,0,0,0,0,0,42,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,35,0,0,9,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,46,0,0,26,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ,0, 0,0,0,0,0,0,0,0,0,0,0,48,0,0,0,31,0,0,36,0,0,0,0,50,50,50,50,50,50, 0,0,
  31,0,0,36,0,0,0,0,51,51,51,51,51,51,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,43,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 49,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,47,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 45,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ,0,54,0,0);
(* FINAL[X] = 0 IF STATE X IS NOT A FINAL STATE
		 1 IF STATE X RECOGNIZES <*****END OF DATA****>
		 2 IF STATE X RECOGNIZES <FILE SPEC>
		 3 IF STATE X RECOGNIZES <FILE DEFAULTS>
		 4 IF STATE X RECOGNIZES <BAD SPEC>
								 *)
final: PACKED ARRAY[exstaterange] OF 0..maxtoken = ( 0,3,1,2,4,0,0,0,0,0,0,0,3
  ,3,3,3,2,2,2,2,4,4,0,0,0,0,3,3,2,2,4,4,0, 0,0,0,4,0,0,0,4,0,0,0,4,0,0,0,0,0,
  3,2,4,4,0,0,4,4);
token_kind: PACKED ARRAY [0..maxtoken] OF lextoken = ( error_token,
  default_token, file_token, default_token, error_token );
$PAGE

  PROCEDURE scan(begin_index: INTEGER; VAR end_index: INTEGER;
    VAR lexeme: lextoken);
  VAR
  currstate, currfinal: exstaterange;
  oldindex: INTEGER;
  (* GETCHAR maps characters from the input line into the terminal classes
       expected by SCAN and used to index the transition matrix.  *)

    FUNCTION getchar: minterminal..eodata;
    BEGIN
      IF end_index >= LENGTH (line) THEN BEGIN (* past end of string *)
	getchar := eodata;
      END
      ELSE BEGIN
	end_index := end_index + 1;
	CASE UPPERCASE ( line [ end_index ] ) OF
	  'A'..'Z': getchar := -2;
	  '0'..'7': getchar := -3;
	  '8','9': getchar := -4;
	  '#': getchar := -5;
	  '?': getchar := -6;
	  '*': getchar := -7;
	  ':': getchar := -8;
	  '(': getchar := -9;
	  ')': getchar := -10;
	  '.': getchar := -11;
	  '[': getchar := -12;
	  ',': getchar := -13;
	  ']': getchar := -14;
	  '<': getchar := -15;
	  '>': getchar := -16;
	  OTHERS: getchar := -1 (* end of data *)
	END (* case *);
      END (* else *);
    END (* getchar *);
$PAGE
  BEGIN (* SCAN *)
    currstate := dfastate1; (* START IN INITIAL STATE *)
    currfinal := 0;
    oldindex := 0; (* width of lexeme AS OF LAST FINAL STATE *)
    end_index := begin_index - 1;
    WHILE currstate <> 0 DO BEGIN
      IF final[currstate] <> 0 THEN BEGIN
	currfinal := currstate;
	oldindex := end_index - begin_index + 1
      END;
      currstate := delta[currstate, getchar ()];
    END;
    end_index := begin_index + oldindex; (* on exit, END_INDEX is index of 1st *)
    (* char PAST file name *)
    lexeme := token_kind [final [currfinal]];
  END; (* SCAN *)
$PAGE
VAR
end_index: INTEGER;
lexeme: lextoken;
BEGIN
  cmd_skip_blanks (line, CURSOR);
  scan (CURSOR, end_index, lexeme);
  cmd_file_name := (lexeme = file_token) OR ( (NOT name_required) AND
    (lexeme = default_token) );
  IF cmd_file_name THEN BEGIN
    name := SUBSTR (line, CURSOR, end_index - CURSOR);
    CURSOR := end_index;
  END;
END (* cmd_file_name *);
$PAGE cmd_query
$HEADER cmdque
$PAGE
PUBLIC
FUNCTION cmd_query ( question, elaboration: PACKED ARRAY [1..*] OF CHAR )
  : BOOLEAN;
VAR
response: STRING [10];
BEGIN
  WRITE (TTY, question);
  LOOP
    BREAK (TTYOUTPUT);
    READLN (TTY);
    READ (TTY, response);
    response := UPPERCASE (response);
    EXIT IF (response = 'YES') OR (response = 'YE') OR (response = 'Y')
      DO cmd_query := TRUE;
    EXIT IF (response = 'NO') OR (response = 'N') DO cmd_query := FALSE;
    IF (response = '') OR (elaboration = '') THEN WRITE (TTY, question)
    ELSE WRITE (TTY, elaboration);
  END;
END (* cmd_query *);
$PAGE cmd_getline
$HEADER cmdget
$PAGE
PUBLIC
PROCEDURE cmd_getline ( prompt: PACKED ARRAY [1..*] OF CHAR;
  VAR line: STRING [*]; VAR CURSOR: INTEGER );
BEGIN
  WRITE (TTY, prompt);
  BREAK (TTYOUTPUT);
  READLN (TTY);
  READ (TTY, line);
  CURSOR := 1;
END (* cmd_getline *);
$PAGE cmd_display_table
$HEADER cmddis
$PAGE
PUBLIC
PROCEDURE cmd_display_table ( list: ARRAY [1..*] OF cmd_lookup_record;
  indent, line_width: INTEGER );
VAR
posn: INTEGER;
i: INTEGER;
name: STRING [11];
BEGIN
  posn := line_width;
  FOR i := 1 TO UPPERBOUND (list) DO BEGIN
    name := SUBSTR (list[i].TEXT, 1, SEARCH (list[i].TEXT, [' '], 11) - 1);
    IF i <> UPPERBOUND (list) THEN name := name || ',';
    posn := posn + LENGTH (name) + 1;
    IF posn <= line_width THEN WRITE (TTY, ' ', name)
    ELSE BEGIN
      IF i <> 1 THEN WRITELN (TTY);
      WRITE (TTY, '': indent, name);
      posn := indent + LENGTH (name);
    END;
  END;
  WRITELN (TTY);
END (* cmd_display_table *).
