$LENGTH 44
$TITLE INDCOM User command processor for INDENT
module INDCOM;

(*$Y20
$HEADER INDCOM.HDR
*)
$PAGE GLOBAL declarations for program
(*$X10
$OPTIONS NOSOURCE
*)

$INCLUDE INDENT.TYP

(*$X10
$OPTIONS SOURCE
*)

public var
  LINE: TEXTLINE;			(* processed input line *)
  THIS_TOKEN,				(* current token type *)
  NEXT_TOKEN: TOKENTYPE;		(* lookahead--may be comment *)

  CURSTART,
  CUREND,				(* start and finish or current tok *)

  TOK_IND,				(* offset of curtok from start of line *)
  ORIG_IND,				(* original indent of line *)
  OUT_IND,				(* final indent from FORMAT *)
  CUR_IND,				(* current minimum from FORMAT *)
  LIN_INDENT,				(* out_ind for this line *)
  TOK_LEN,				(* length of text of current token *)
  OUT_MAX,				(* maximum output column *)
  COM_COL,				(* column to which to align comments *)
  COM_IND,				(* column for stand-alones to start *)
  EOL_CT,				(* num of end-of-lines after this *)
  QUANTUM: LINE_IDX;			(* current indent quantum *)

  OPTIONS: OPTIONSET;			(* current options in effect *)

  TOK_BEFORES,
  TOK_AFTERS: TOKSET;			(* set of before/after spacing -- token *)
  
  SYM_BEFORES,
  SYM_AFTERS: SYMSET;			(* set of same -- but for symbols *)

  WORD_CASE,				(* case selection for reserved words *)
  TEXT_CASE: CASE_TYPE;			(* and for normal text *)
  ALIGN_COM,				(* true if comments to be aligned *)
  SPREAD: boolean;			(* true if SPREAD option in effect *)

  INFILE,
  OUTFILE: text;			(* output file id *)

static var				(* the default settings for params *)
  DEF_COM_COL: LINE_IDX := 41;		(* comment column setting *)
  DEF_QUAN: LINE_IDX := 2;		(* quantum of indentation *)
  DEF_OPTIONS: OPTIONSET := [];		(* default options -- none *)
  DEF_S_BEFORES: SYMSET := [];		(* space before symbols *)
  DEF_S_AFTERS: SYMSET := [];		(* space after symbols *)
  DEF_T_BEFORES: TOKSET := [];		(* space before reserved words *)
  DEF_T_AFTERS: TOKSET := [];		(* space after reserved words *)
  DEF_W_CASE: CASE_TYPE := NO_CHANGE;   (* reserved word case *)
  DEF_T_CASE: CASE_TYPE := NO_CHANGE;	(* case for program text *)
  DEF_AL_COM: boolean := false;		(* column alignment of comments *)
  DEF_SPREAD: boolean := false;		(* spread option *)
  DEF_OUT_MAX: LINE_IDX := 100;		(* maximum output column *)

type					(* a few extras for command process *)
  FNSTR = string[30];			(* for file descriptors *)
  COM_LINE = string[255];		(* for command lines *)
  FILE_IDX = 0..3;			(* indication of active files *)

  OPT_TOK = (				(* scalar for user option selection *)
    NOT_GOOD, SPR_TOK, NOSPR_TOK, IND_TOK, COL_TOK, WID_TOK, BEF_TOK,
    AFT_TOK, WORD_TOK, TEXT_TOK, UP_TOK, DOWN_TOK, MIXED_TOK, NONE_TOK,
    QUAN_TOK
    );

  OPT_TEXT = packed array [1..10] of char;

  OPT_TABL = array [OPT_TOK] of OPT_TEXT; (* the lookup table *)

  OPT_SET = set of OPT_TOK;		(* for valid options *)

  SYM_SUBRANGE = '!'..'_';
  SYM_TAB_TYPE = array[SYM_SUBRANGE] of SYMBOLS;

const
  ALL_OPT: OPT_SET := [SPR_TOK..QUAN_TOK]; (* all interesting tokens *)
  NEEDPARENS: OPT_SET := 		(* set of options taking parens *)
    [ IND_TOK..TEXT_TOK, QUAN_TOK];

var
  OPT_TABLE: OPT_TABL = (		(* text table for lookup routine *)
    '', 'SPREAD', 'NOSPREAD', 'INDENT', 'COLUMN', 'WIDTH', 'BEFORE',
    'AFTER', 'WORD', 'TEXT', 'UP', 'DOWN', 'MIXED', 'NONE', 'QUANTUM' 
    );


static var				(* file administration vars *)
  WHICH: FILE_IDX := 0;			(* zero level of indirect to start *)
  INFNAME,				(* name of input file *)
  OUTFNAME,				(* name of output file *)
  PRI_FNAME,				(* names of primary, secondary,... *)
  SEC_FNAME,				(* and tertiary command files *)
  TER_FNAME: FNSTR;
  PRI_FILE,				(* and the file variables *)
  SEC_FILE,
  TER_FILE: text;

  USRLINE: COM_LINE;
  LPTR: LINE_IDX;
  ERROR: boolean;

external function WORD_LOOKUP (COM_LINE): TOKENTYPE;
external function SYM_LOOKUP (COM_LINE): SYMBOLS;

external const				(* from reader *)

  SYM_TABLE: SYM_TAB_TYPE;
$PAGE READLINE read a command line from where get_line wants it
function READLINE (var F: text): COM_LINE;

(* READLINE merely commonizes the functions of reading input lines from
   either command files or from TTY:.  Since the file identifier is
   passed to us, we don't care where it comes from.  Functions of
   READLINE also include removal of all blanks from input line, and
   conversion of vertical bars to exclamation points so the lookup
   routines in DO_RES_SYM will find them. *)

var
  TEMP: packed array[1..LINLEN] of char;
  TIDX: LINE_IDX;

  begin
  TIDX := 0;
  READLINE := '';
  if eoln (F) then readln(F);		(* may not be if RESET and not OPEN *)

  while not eoln(F) and not eof(F) do
    begin
    if F^ <> ' ' then
      begin				(* fill up the packed array *)
      TIDX := TIDX + 1;
      TEMP [TIDX] := uppercase(F^)
      end;
    get (F)
    end;

  READLINE := TEMP[1: TIDX];

  (* Now eliminate vertical bars if any *)

    loop
      TIDX := index (READLINE, '|');
    exit if TIDX = 0;
      READLINE[TIDX] := '!'
    end
  end (* function READLINE *);
$PAGE GET_LINE get a command line
function GET_LINE: COM_LINE;

(* GET_LINE returns the string form of the next command line to be
   executed. It hides the details of secondary and tertiary command
   files internally. Upon reading a null line from the primary source
   (either 'TTY:' or '###IND.TMP'), GET_LINE executes a STOP. *)

var
  DONE: boolean;			(* used to kick out of loop *)

  begin
  DONE := false;
  GET_LINE := '';

  while not DONE do
    case WHICH of			(* case on 'active' indirect level *)

      0: begin				(* nothing open yet *)
	WHICH := 1;
	PRI_FNAME := '###IND.TMP';	(* try to open temp file first *)
	reset (PRI_FILE, PRI_FNAME);
	if eof (PRI_FILE) then		(* no temp file to be had *)
	  begin				(* so get 'TTY:' *)
	  PRI_FNAME := 'TTY:';
	  open (PRI_FILE, PRI_FNAME);   (* which hopefully will never fail *)
	  writeln(tty, 'INDENT 1.2 ', compdate);
	  break(tty)
	  end
      end (* initial call *);

      1: begin
	if PRI_FNAME = 'TTY:' then	(* must prompt for input *)
	  begin
	  write (tty, '*');
	  break (tty)
	  end;
	GET_LINE := READLINE (PRI_FILE); (* get the input line *)
	if eof (PRI_FILE) or (GET_LINE = '') then 
	  begin
	  if PRI_FNAME <> 'TTY:' then	(* we're done. delete IND.TMP file *)
	    rewrite(PRI_FILE);
	  stop				(* and halt the program *)
	  end;

	if GET_LINE[1] = '@' then	(* kick up to indirect level two *)
	  begin
	  SEC_FNAME := substr (GET_LINE, 2, length(GET_LINE) - 1);
	  open (SEC_FILE, '.CMD ' || SEC_FNAME);  (* try to open *)
	  if eof (SEC_FILE) then	(* can't find file *)
	    writeln(tty, 'Can''t open secondary file ', SEC_FNAME)
	  else WHICH := 2		(* got it -- hit loop again for line *)
	  end
	else DONE := true		(* otherwise we got a command line *)
      end (* level one *);

      2: begin
	GET_LINE := READLINE (SEC_FILE);

	if eof (SEC_FILE) then
	  WHICH := 1			(* back to level one if eof *)
	else if GET_LINE[1] = '@' then
	  begin				(* tertiary level of indirect *)
	  TER_FNAME := substr (GET_LINE, 2, length (GET_LINE) - 1);
	  open (TER_FILE, '.CMD '||TER_FNAME);
	  if eof (TER_FILE) then
	    writeln( tty, 'Can''t open tertiary file ', TER_FNAME)
	  else WHICH := 3
	  end
	else DONE := (GET_LINE <> '')	(* don't bother returning null line *)
      end (* secondary read *);

      3: begin
	GET_LINE := READLINE (TER_FILE);
	if eof (TER_FILE) then WHICH := 2
	else if GET_LINE[1] = '@' then
	  writeln (tty, 'Level of indirection exceeded.')
	else DONE := (GET_LINE <> '')
      end (* tertiary read *)
    end (* case and while *)
  end (* function GET_LINE *);
$PAGE SETDEFAULT to set default pars
procedure SETDEFAULT;

(* SETDEFAULT places the contents of the appropriate default parameter
   cells into the cells used by the rest of the INDENT program. Thus a
   single command may override the default settings, with those defaults
   being restored after the execution of the command. *)

  begin
  COM_COL := DEF_COM_COL;
  QUANTUM := DEF_QUAN;
  OPTIONS := DEF_OPTIONS;
  SYM_BEFORES := DEF_S_BEFORES;
  SYM_AFTERS := DEF_S_AFTERS;
  TOK_BEFORES := DEF_T_BEFORES;
  TOK_AFTERS := DEF_T_AFTERS;
  WORD_CASE := DEF_W_CASE;
  TEXT_CASE := DEF_T_CASE;
  ALIGN_COM := DEF_AL_COM;
  SPREAD := DEF_SPREAD;
  OUT_MAX := DEF_OUT_MAX
  end (* procedure SETDEFAULT *);
$PAGE NEWDEFAULT to change default pars
  procedure NEWDEFAULT;

  (* NEWDEFAULT is called to move the program parameters in the normal
     locations into the default save locations. It is called when a
     list of options has been processed, and these options comprise
     the new default settings. The option processor (below) places
     all new values into the normal locations. *)

    begin
    DEF_COM_COL := COM_COL;
    DEF_QUAN := QUANTUM;
    DEF_OPTIONS := OPTIONS;
    DEF_S_BEFORES := SYM_BEFORES;
    DEF_S_AFTERS := SYM_AFTERS;
    DEF_T_BEFORES := TOK_BEFORES;
    DEF_T_AFTERS := TOK_AFTERS;
    DEF_W_CASE := WORD_CASE;
    DEF_T_CASE := TEXT_CASE;
    DEF_AL_COM := ALIGN_COM;
    DEF_SPREAD := SPREAD;
    DEF_OUT_MAX := OUT_MAX
    end (* procedure NEWDEFAULT *);
$PAGE GETCMDHEADER header and error routine
public procedure GETCMD;

(* GETCMD is the boss of this module, and directs the parsing of user
   input lines.  Parsing routines within which errors may occur are
   defined within the scope of GETCMD so that an out-of-scope GOTO
   may be performed by the error routine to restart things. Following
   are the error routine and the other parsing routines. *)

label 1;

  procedure ERROR (MSG: COM_LINE);

  begin
  writeln(tty, MSG, '.');
  goto 1
  end (* procedure ERROR *);
$PAGE DO_INT simple routine to return integer value of input text
  function DO_INT: LINE_IDX;

    begin
    DO_INT := 0;
    while (length(USRLINE) >= LPTR) andif
      (USRLINE[LPTR] in ['0'..'9']) do
      begin
      DO_INT := (DO_INT * 10) + ord(USRLINE[LPTR]) - ord('0');
      LPTR := LPTR + 1
      end
    end (* function DO_INT *);
$PAGE DO_OPT_LOOK to lookup user selected options
  function DO_OPT_LOOK: OPT_TOK;

  (* DO_OPT_LOOK munches alphabetic characters in USRLINE starting at LPTR
     to identify a selected option. The search will terminate when either
     the alphabetic characters have been exhausted (found a non-alpha) or
     if the characters already scanned cannot form one of the OPT_TOK
     keywords. If a non-alpha (or end-of-line) was encountered, DO_OPT_LOOK
     will return the token found, or NOT_GOOD if the characters scanned
     do not form a non-ambiguous substring of one of the tokens. *)

  var
    OPT_IDX: OPT_TOK;			(* for walking the table *)
    OPT_VALID: OPT_SET;			(* set of tokens still valid *)
    OPT_V_CARD: LINE_IDX;			(* cardinality of above set *)
    TAB_IDX: LINE_IDX;			(* index of characters in table *)

    begin
    OPT_VALID := ALL_OPT;
    OPT_V_CARD := ord (maximum(OPT_TOK));  (* actually one less than size *)
    TAB_IDX := 1;				(* start at the first char *)

      repeat
	for OPT_IDX := SPR_TOK to QUAN_TOK do (* walk the token list *)
	  if OPT_IDX in OPT_VALID then 	(* don't bother if already excluded *)
	    if OPT_TABLE[OPT_IDX,TAB_IDX] <> USRLINE[LPTR] then
	      begin			(* no longer possible to match this *)
	      OPT_VALID := OPT_VALID - [OPT_IDX]; (* remove from set *)
	      OPT_V_CARD := OPT_V_CARD - 1 (* and decrem. running cardinality *)
	      end
	    else DO_OPT_LOOK := OPT_IDX;	(* use funct. val. to keep track *)

	LPTR := LPTR + 1;			(* onto next source character *)
	TAB_IDX := TAB_IDX + 1		(* and next table character *)

      until ((LPTR > length(USRLINE)) orif not (USRLINE[LPTR] in ['A'..'Z']))
	or  (OPT_V_CARD = 0);	(* until no more, or nothing possible *)

    if OPT_V_CARD <> 1 then		(* either ambiguous or nonsense *)
      DO_OPT_LOOK := NOT_GOOD

    (* If the cardinality of the valid set is exactly one, then DO_OPT_LOOK
       will have been set in the above FOR loop to the correct token. *)

    end (* function DO_OPT_LOOK *);
$PAGE DO_RES_SYM process a list of BEFORE/AFTER options
  procedure DO_RES_SYM ( var TOKS: TOKSET;
			 var SYMS: SYMSET );

  (* DO_RES_SYM processes the list of quoted special symbols and
     (quoted) reserved words in a BEFORE/AFTER list. It calls the
     lookup routines in READER to identify them. The two parameters
     are either the BEFORE or AFTER spacing sets (we don't care
     which), and the routine adds each identified element to the
     appropriate set. If the option list is null, the sets are
     cleared. *)

  var
    TMPTOK: TOKENTYPE;			(* returned by WORD_LOOKUP *)
    TMPSYM: SYMBOLS;			(* and SYM_LOOKUP or SYM_TABLE *)
    GOT_ONE: boolean;			(* true if not null list *)
    TMPSTR: string[20];			(* temp for specialsym/ resword *)
    DELIM: char;			(* delim for quotes, either ' or " *)

    begin
    GOT_ONE := false;

    while (LPTR <= length(USRLINE)) do
      begin
      TMPSTR := '';
      if USRLINE[LPTR] in ['''','"'] then
	begin				(* quoted string, strip off quotes *)
	DELIM := USRLINE[LPTR];
	LPTR := LPTR + 1;		(* skip first one *)
	while (LPTR <= length(USRLINE)) andif
	  (USRLINE[LPTR] <> DELIM) do	(* tack on chars while within quotes *)
	  begin
	  TMPSTR := TMPSTR || USRLINE[LPTR];
	  LPTR := LPTR + 1
	  end;

	if LPTR > length(USRLINE) then	(* no close quote found *)
	  ERROR ('No close quote found in BEFORE/AFTER item');
	LPTR := LPTR + 1		(* else skip past close *)
	end

      else				(* non-quoted, just munch chars *)
	while (LPTR <= length(USRLINE)) andif
	  not (USRLINE[LPTR] in [',',')']) do
	  begin				(* take chars while not delimiter *)
	  TMPSTR := TMPSTR || USRLINE[LPTR];
	  LPTR := LPTR + 1
	  end;

      if TMPSTR <> '' then		(* we have something *)
	begin
	GOT_ONE := true;		(* if not already *)
	if TMPSTR[1] in ['A'..'Z'] then
	  begin				(* alphabetic, lookup as resword *)
	  TMPSYM := NO_GOOD;
	  TMPTOK := WORD_LOOKUP (TMPSTR)
	  end
	else begin			(* must be special symbol *)
	  TMPTOK := ETC;
	  if length(TMPSTR) = 1 then	(* lookup in single char array *)
	    TMPSYM := SYM_TABLE[ TMPSTR[1] ]
	  else TMPSYM := SYM_LOOKUP (TMPSTR)
	  end;

	if (TMPSYM in [NO_GOOD,OPEN_COM_OP,CLOSE_COM_OP])
	and(TMPTOK = ETC) then		(* all failed, comments no good *)
	  ERROR ('Unrecognized reserved word or symbol -- "'||TMPSTR||'"');

	if TMPSYM = NO_GOOD then
	  TOKS := TOKS + [TMPTOK]	(* otherwise is one or the other *)
	else SYMS := SYMS + [TMPSYM]
	end (* got something *);

      if LPTR > length (USRLINE) then
	ERROR ('Incomplete BEFORE/AFTER list');

      exit if USRLINE [LPTR] <> ',';	(* else we're done *)

      LPTR := LPTR + 1
      end (* while *);

    if not GOT_ONE then
      begin				(* null list, reset sets *)
      SYMS := [];
      TOKS := []
      end
    end (* procedure DO_RES_SYM *);

$PAGE DO_IND_OPT to process INDENT(1,2,3...) option list
  procedure DO_IND_OPT;

  (* DO_IND_OPT processes numbers  in a list following the keyword INDENT,
     and, if acceptable, places them into the set OPTIONS. *)

  var
    TMP: LINE_IDX;

    begin
    OPTIONS := [];			(* in case we find nothing *)
    while (LPTR <= length(USRLINE)) andif
      (USRLINE[LPTR] <> ')' ) do
      begin
      TMP := DO_INT;			(* try to get a number *)
      if (TMP > 0) and (TMP <= MAX_OPT) then
	OPTIONS := OPTIONS + [TMP]
      else ERROR ('Invalid INDENT() option number');
      if (LPTR <= length(USRLINE)) andif
 	(USRLINE[LPTR] = ',') then
	LPTR := LPTR + 1
      end
    end (* procedure DO_IND_OPT *);
$PAGE DO_OPTIONS to decode and process option list
  procedure DO_OPTIONS;

  (* DO_OPTIONS processes a list of options separated by commas. It is
     assumed that the slash has been removed before the option list. *)

  var
    CASE_OPT,				(* temp for case conversion keyword *)
    WHAT: OPT_TOK;			(* the current option token *)
    CASE_TMP: CASE_TYPE;		(* temp for case conversion scalar *)

    begin
      loop
	WHAT := DO_OPT_LOOK;		(* get the option keyword *)
	if WHAT in NEEDPARENS then	(* most keywords require parens *)
	  begin
	  if (LPTR > length(USRLINE))	(* at end of line, no paren *)
	  orif (USRLINE[LPTR] <> '(') then  (* orif no paren *)
	    ERROR ('Option requires open paren')
	  else LPTR := LPTR + 1		(* otherwise tick past it *)
	  end;

	case WHAT of

	  SPR_TOK, NOSPR_TOK:		(* one of the SPREAD tokens *)
	    SPREAD := (WHAT = SPR_TOK); (* set SPREAD accordingly *)

	  IND_TOK:			(* 'INDENT' option keyword *)
	    DO_IND_OPT;			(* easy, call slave routine *)

	  COL_TOK:
	    begin			(* comment alignment column *)
	    COM_COL := DO_INT;		(* get integer within parens *)
	    if COM_COL > LINLEN then	(* an absurd specification *)
	      ERROR ('Invalid column specification');
	    ALIGN_COM := (COM_COL <> 0); (* zero implies no comment alignment *)
	    COM_COL := COM_COL - 1
	    end;

	  WID_TOK:			(* maximum output width specif. *)
	    begin
	    OUT_MAX := DO_INT;		(* get the integer *)
	    if (OUT_MAX <= 0) or	(* check for 'range' *)
	    (OUT_MAX > LINLEN) then
	      ERROR ('Invalid width specification')
	    end;

	  BEF_TOK:			(* list of 'BEFORE' spacings *)
	    DO_RES_SYM (TOK_BEFORES, SYM_BEFORES);

	  AFT_TOK:			(* and of 'AFTER' spacings *)
	    DO_RES_SYM (TOK_AFTERS, SYM_AFTERS);

	  QUAN_TOK:			(* QUANTUM specification *)
	    begin
	    QUANTUM := DO_INT;		(* get the number *)
	    if (QUANTUM <= 0) or
	       (QUANTUM > 20) then	(* absurd quantum spec. *)
	      ERROR ('Invalid quantum specification')
	    end;

	  TEXT_TOK, WORD_TOK:		(* case conversion specs *)
	    begin
	    CASE_OPT := DO_OPT_LOOK;	(* get case keyword *)
	    if CASE_OPT = NONE_TOK then (* now cascade down possibilities *)
	      CASE_TMP := NO_CHANGE
	    else if CASE_OPT = UP_TOK then
	      CASE_TMP := CAP
	    else if CASE_OPT = DOWN_TOK then
	      CASE_TMP := DECAP
	    else if CASE_OPT = MIXED_TOK then
	 CASE_TMP := MIXED
	    else ERROR ('Invalid case conversion option');

	    if WHAT = WORD_TOK then	(* anything OK for reserved words *)
	      WORD_CASE := CASE_TMP
	    else if CASE_TMP = MIXED then (* but mixed not OK for text *)
	      ERROR ('''MIXED'' not allowed for ''TEXT''')
	    else TEXT_CASE := CASE_TMP
	    end; 

	  OTHERS:			(* 'NOT_GOOD' *)
	    ERROR ('Unrecognizable option keyword')

	end (* case *);

	if WHAT in NEEDPARENS then	(* now get the closeparen *)
	  if (LPTR > length(USRLINE)) orif
	     (USRLINE[LPTR] <> ')') then
	  ERROR ('Need close paren for option')
	  else LPTR := LPTR + 1;	(* tick past it if found *)

      exit if LPTR >= length(USRLINE);	(* we're done, successful *)

      exit if (USRLINE[LPTR] <> ',') do (* something there, not a comma *)
	if length(USRLINE) <> LPTR then
	  ERROR ('Extraneous characters follow option list');

	LPTR := LPTR + 1;		(* else tick past comma *)

      exit if LPTR > length(USRLINE)	(* OK if terminal comma (null option)*)
      end (* loop *)
    end (* procedure DO_OPTIONS *);
$PAGE GETCMD extract info from command line

(* GETCMD tries to make a useful interpretation of an input line. It
   calls GET_LINE for a line to parse, and goes to work. It will not
   return until we are ready to process (i.e. input and output files
   are open). *)

var
  EQ_IDX,				(* for index of equalsign if found *)
  SL_IDX: LINE_IDX;			(* and for slash if found *)

  begin;
  1:					(* come here after error *)
  SETDEFAULT;				(* restore default settings *)
  USRLINE := GET_LINE;			(* get an input line *)
  EQ_IDX := index (USRLINE, '=');	(* find interesting characters *)
  SL_IDX := index (USRLINE, '/');

  if SL_IDX <> 0 then
    begin				(* there's an option list there *)
    LPTR := SL_IDX + 1;			(* set cursor for DO_OPTIONS et.al. *)
    DO_OPTIONS;				(* if it returns, all went well *)
    if SL_IDX = 1 then
      begin				(* first on line, new default info *)
      NEWDEFAULT;			(* put them in default cells *)
      goto 1				(* and back to the top *)
      end
    end (* option processing *);

  if (EQ_IDX <> 0) and			(* equals sign -- output filespec *)
    ((SL_IDX = 0) or (SL_IDX > EQ_IDX)) (* if not within option list!! *)
  then begin
    OUTFNAME := substr (USRLINE, 1, EQ_IDX - 1);

    (* we have an explicit output file now. There must be in input spec
       also, terminated either by end of line, or by option slash. *)

    if SL_IDX = 0 then
      INFNAME := substr (USRLINE, EQ_IDX + 1)
    else INFNAME := substr(USRLINE, EQ_IDX + 1, SL_IDX - EQ_IDX - 1)
    end

  else begin				(* no output filespec *)
    if SL_IDX = 0 then
      INFNAME := USRLINE		(* no options also -- inputspec only *)
    else INFNAME := substr (USRLINE, 1, SL_IDX - 1);
    OUTFNAME := INFNAME || '.NEW[,]';	(* enforce .NEW on output file *)
    end;

  (* now let's open the files. If OK, return to caller, else just call
     ERROR, which will kick us back to the top. *)

  open (INFILE, '.PAS '||INFNAME);
  if eof(INFILE) then ERROR ('Can''t open input file');
  rewrite (OUTFILE, '.NEW '||OUTFNAME);
  if not eof(OUTFILE) then ERROR ('Can''t rewrite output file')

  (* if we got here, all is ready for the coroutines to rip. *)

  end (* procedure GETCMD and module INDCOM *).
) 6j