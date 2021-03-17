$LENGTH 44
$TITLE TALK10 file transfer program
program TALK10;
(*
+-------------------------------------------------------------------+
|                                                                   |
|                            T A L K - 1 0                          |
|			     _ _ _ _   _ _			    |
|								    |
+-------------------------------------------------------------------+



MDSI, COMPANY CONFIDENTIAL

STARTED JULY 26, 2978

TALK10 -- THE CYPHERNET SIDE OF THE TALK10 COMMUNICATIONS PROGRAM TO
TRANSFER FILES BETWEEN CYPHERNET AND A RSX-11MV3.

OVERVIEW -- Communication between the machines is limited to ASCII
  files only to the -10, and both ASCII and binary files from the
  -10 to the -11. The user initiates communication on the -11 by
  running TALK10, logging onto the -10, and running this program.
  When a 'SEND' or 'GET' command is ready to be processed, the -10
  responds with 'Ready.'. At this point, the user should type (RS)
  (control-uparrow) to get back to the -11 prompt, and issue the /I
  (initiate) command to start the transfer. The -11 is capable of
  detecting and reporting some errors, and when the -10 program is
  re-entered (by the /T command to the -11), a carriage return will
  prompt the -10 to give an error report, if any.

The -10 will not dismiss the -11 for input indefinitely, but will
  execute a wait loop, testing for terminal input. After two minutes
  of waiting, the -10 will send a NUL (control-@) every three seconds,
  immediately before every SLEEP monitor call, in hopes of reviving
  the -11 (i.e. perhaps the -11's dismissal of the -10 for input 
  was not fully satisfied). After 5 minutes, the -10 writes an error
  log file onto the user's account and stops. This 3 second wait loop
  is executed only when the -10 is expecting input from the -11 pro-
  gram, not from the user. Normal <readln>s are used to read user
  commands.

RESPONSIBLE: JERRY ROSEN, PASCAL GROUP.

CHANGES: NONE.
*)
$INCLUDE TALK10.TYP
$PAGE GLOBAL vars and such

label
  2;					(* non-local for proc. ERROR *)

var
  BSOURCE: BINFILE;			(* binary file id for SEND10 *)
  TEXTFILE: text;			(* ascii file for get and send *)

  BINBLOCK: FORMATBLOCK;		(* block for binary SEND *)

  TOPQ: FNPTR := nil;			(* top and bottom ptrs of *)
  BOTTOMQ: FNPTR := nil;		(* filename list *)

  SCRAPBLOCK:  array[1..256]of 
    EIGHTBIT;				(* for saving previous line *)
  NEXTIN, SCRSIZ: INTEGER;		(* start, size of prev. line *)
$PAGE GOODTIME gets TOD in string format

function GOODTIME: FNAME;

(* GOODTIME translates the ridiculous milliseconds past midnite given
   by the predefined function TIME into a more reasonable string of
   characters. Leading zeroes are supplied except for hours. *)

var
  SEC, MIN, HR: INTEGER;

  begin
  SEC := TIME div 1000;			(* scrap the milliseconds *)
  MIN := SEC div 60;
  SEC := SEC mod 60;			(* secs now correct *)
  HR := MIN div 60;			(* hr now correct *)
  MIN := MIN mod 60;			(* and min now correct *)

  if HR < 10 then			(* only one character *)
    GOODTIME := chr(HR + ord('0'))
  else GOODTIME := chr(HR div 10 + ord('0')) ||
		   chr(HR mod 10 + ord('0'));

  GOODTIME := GOODTIME || ':' ||
	      chr(MIN div 10 + ord('0')) ||
	      chr(MIN mod 10 + ord('0')) || ':' ||
	      chr(SEC div 10 + ord('0')) ||
	      chr(SEC mod 10 + ord('0'))
  end (* function GOODTIME *);
$PAGE ERROR to bomb out, writing error file.

var
  ERRFILE: text;			(* another global file id *)

public procedure ERROR (MSG: FNAME);

  begin
  rewrite(ERRFILE,'ERR11.T10');
  writeln(ERRFILE, MSG);
  writeln(ERRFILE);
  writeln(ERRFILE, date, ' at ', GOODTIME,' GMT.');
  writeln(ERRFILE);
  if TOPQ <> nil then
  with TOPQ^ do				(* give indication of which transfer *)
    writeln(ERRFILE,'Transfer ',SRCFILE,' to ',DSTFILE,' being processed.');
  close(ERRFILE);
  writeln(tty);
  writeln(tty,'$$full;key;bro on;nlf off');	(* to reset any previous damage *)
  goto 2
  end (* procedure ERROR *) ;
$PAGE PUTWCR to write a string followed by <cr> only

procedure PUTWCR (FN: FNAME);

(* PUTWCR is used to put the file directive to the -11. It is assumed
   that the caller has placed the directive character (GETDIR or
   SENDDIR) as the first character of the string. *)

  begin
  break(tty);
  write(tty,FN);
  write(tty, CR );
  break(tty)
  end;
$PAGE GETNEXTFILE little routine to move down list

procedure GETNEXTFILE (var HEAD, TAIL: FNPTR);

(* GETNEXTFILE removes the top entry in the list of transfer requests
   held by HEAD and TAIL. If there is only one entry in the list, both
   HEAD and TAIL are set to nil. GETNEXTFILE must never be called with
   HEAD nil (the definitive test for an empty list). *)

var
  WALK: FNPTR;

  begin
  WALK := HEAD;
  if HEAD = TAIL then
    begin
    HEAD := nil;
    TAIL := nil
    end
  else HEAD := HEAD^.NEXT;
  dispose(WALK)
  end (* procedure GETNEXTFILE *);
$PAGE GETLIT routine to read a literal text line from tty:

procedure GETLIT (var LIN: LITLINE;
		  var LEN: EIGHTBIT);

(* GETLIT uses the MACRO routine GETCH to get the next line from
   tty: in literal form, using PASSCC and PASSCR to insure that
   all characters are received. The Cyphernet Command Language
   manual states specifically that ALL carriage returns have line
   feeds automatically appended to them. Thus, GETLIT confidently
   can do a GETCH after a <cr>, knowing that the <lf> is there. *)

  var
    JUNK: char;				(* temp for passing to GETCH *)
    I: INTEGER;				(* control variable *)

  begin
  for I := 1 to 30 do
    LIN[I] := ' ';			(* clear it out *)

  LEN := 0;
  repeat
    GETCH(JUNK);
    LEN := LEN + 1;
    LIN[LEN] := JUNK;
    if JUNK = CR then
      GETCH(JUNK)			(* chuck the <lf> after <cr> *)
  until eoln(tty)
  end;
$PAGE GETABK reads in text file block, makes into -11 format

procedure GETABK(var AB: FORMATBLOCK;	(* block of chars to go to 11 *)
		 var SZ: INTEGER;	(* character count in block *)
		    BIN: boolean);	(* true if binary source file *)

(* GETABK reads in 512 characters of a text file, and translates them
   into the -11 record format (two length bytes, that many characters,
   a NUL fill byte if necessary to make next length word-aligned). *)

var
  I: INTEGER;				(* local loop index *)

  begin
  SZ := 0;				(* haven't copied anything yet *)
    loop				(* with several exits *)
      for I := 0 to SCRSIZ - 1 do
	begin				(* copy over old stuff *)
	exit if SZ >= 512; 		(* if we can *)
	SZ := SZ + 1;
	NEXTIN := NEXTIN + 1;
	AB[SZ].BYTE := SCRAPBLOCK[NEXTIN];
	SCRSIZ := SCRSIZ - 1
	end;

      if (SZ mod 2) = 1 then		(* if SZ odd *)
	begin
	SZ := SZ + 1;			(* add extra fill byte *)
	AB[SZ].BYTE := 0
	end;

    if BIN then
      exit if eof(BSOURCE)		(* if emptied last time *)
    else exit if eof(TEXTFILE);
    exit if SZ >= 512;

      if BIN then
	begin
	get(BSOURCE);			(* gets '1' record *)
	exit if eof(BSOURCE)
	  do SCRSIZ := 0;		(* if it's there *)
	get(BSOURCE);			(* 0 after 1 *)
	get(BSOURCE);			(* lo-order size byte *)
	SCRSIZ := BSOURCE^.BYTE - 2;	(* accounts for 1-0 pair *)
	SCRAPBLOCK[1] := SCRSIZ - 2;	(* and checksum at end *)

	for I := 2 to SCRSIZ do		(* read good bytes *)
	  begin
	  get(BSOURCE);
	  SCRAPBLOCK[I] := BSOURCE^.BYTE
	  end;
	NEXTIN := 0;
	get(BSOURCE);			(* toss checksum *)
	get(BSOURCE)			(* and padding after *)
	end
      else begin			(* text file *)
	for I := 1 to 256 do		(* get a new line from file *)
	  begin
	  get(TEXTFILE);
	  if TEXTFILE^ = CR then
	    get(TEXTFILE);		(* chuck out CR's and LF's *)
	  exit if eoln(TEXTFILE) or
	    eof(TEXTFILE) do SCRSIZ := I - 1;
	  SCRAPBLOCK[I] := ord(TEXTFILE^)
	  end;				(* should never terminate normally *)

	AB[SZ+1].BYTE := SCRSIZ mod 256;(* length of next record *)
	SZ := SZ + 2;			(* low order first, of course *)
	AB[SZ].BYTE := SCRSIZ div 256;
	NEXTIN := 0			(* since we have a new line *)
	end (* text *)
    end (* loop *)
  end (* procedure GETABK *);
$PAGE SEND10 sends a list of files to the -11.

function SEND10 (var HEAD, TAIL: FNPTR)	(* list of transfers *)
  : TALKERR;				(* returns non-fatal error code *)

(* SEND10 processes the list of file transfers pointed to by HEAD and
   TAIL. Certain error conditions are considered non-fatal, such as
   source file not found, and refusal of directive by -11; an appro-
   priate function value is returned. Fatal errors, however, such as
   time-out in GETLIT, or missing ACK for zero-size block (end of file),
   are not diagnosed to the terminal (since the -11 may not be in
   talk mode) and cause program termination. *)

  var
    BLSIZ1,				(* two chars for sending size *)
    BLSIZ2,
    LINLEN,				(* line length from GETLIT *)
    CSUM: EIGHTBIT;			(* checksum from BCSUM or ACSUM *)

    LINE11: LITLINE; 			(* literal text from GETLIT *)


    I, BSIZE: INTEGER;			(* loop index, blocksize *)
    TMPSIZE, LCSIZE: INTEGER;		(* count-down for size, temp *)
    DIR: char;				(* for putting directive character *)


  begin					(* SEND10 *)
  SEND10 := TOK;			(* start out with no errors *)

    loop				(* for each file on list *)
      GETLIT(LINE11,LINLEN);		(* get ack from 'previous' EOF *)
    exit if LINE11[1] <> ACK
      do ERROR('No EOF ACK.');		(* fatal error *)
    exit if HEAD = nil;			(* no more files *)

      with HEAD^ do			(* open source file *)
	if BINARY then
	  begin
	  DIR := GETBIN;
	  open (BSOURCE, '.OBJ '||SRCFILE);
	  if eof(BSOURCE) then
	    SEND10 := TNO10FILE
	  end
	else begin
	  DIR := GETASC;
	  open (TEXTFILE, '.TXT '||SRCFILE);
	  if eof(TEXTFILE) then
	    SEND10 := TNO10FILE
	  end (* if and with *);

    exit if SEND10 <> TOK;		(* non-fatal error -- file not found *)

      PUTWCR(DIR||HEAD^.DSTFILE);	(* put directive *)
      GETLIT(LINE11,LINLEN);		(* get ack/nak *)
      exit if LINE11[1] <> ACK
	do SEND10 := T11REFUSED;	(* non-fatal -- directive refused *)

      NEXTIN := 0; SCRSIZ := 0;		(* init'ze format conv'n vars *)
	loop				(* for blocks in each file *)
	  BSIZE := 512;
	  GETABK(BINBLOCK,BSIZE,HEAD^.BINARY); (* get block from file *)

	  BCSUM(BINBLOCK,BSIZE,CSUM);	(* get checksum for bytes *)

	exit if BSIZE = 0;		(* EOF reached, no more to send *)
	  BLSIZ1 := BSIZE mod 256;	(* low order of size (send first) *)
	  BLSIZ2 := BSIZE div 256;	(* and send high order last *)

	  repeat			(* for sending a block *)
	    break(tty);			(* to be safe *)
	    PUT8BITSTOTTY(BLSIZ1);	(* write size and csum *)
	    PUT8BITSTOTTY(BLSIZ2);
	    PUT8BITSTOTTY(CSUM);
	    GETLIT(LINE11,LINLEN);	(* wait for ack of size/csum *)
	    if LINE11[1] <> ACK then
	      ERROR('Can''t send size and cs.'); (* something's really wrong *)

	    TMPSIZE := BSIZE;
	      loop
		LCSIZE := min(128,TMPSIZE);
		for I := (BSIZE - TMPSIZE + 1) to (BSIZE - TMPSIZE + LCSIZE) do
		  PUT8BITSTOTTY(BINBLOCK[I].BYTE);
		TMPSIZE := TMPSIZE - LCSIZE;
	      exit if TMPSIZE = 0;
		GETLIT(LINE11,LINLEN)	(* and wait for prompt between *)
	      end;
	    break(tty);			(* to be safe again *)
	    GETLIT(LINE11,LINLEN);	(* wait for ack/nak *)
	  until LINE11[1] = ACK
	end (* loop to put file *);

    exit if SEND10 <> TOK;		(* non-fatal error *)
      BLSIZ1 := 0;			(* put zero size for EOF *)
      PUT8BITSTOTTY(BLSIZ1);
      PUT8BITSTOTTY(BLSIZ1);
      PUT8BITSTOTTY(BLSIZ1);

      if HEAD^.BINARY then
	close(BSOURCE)
      else close(TEXTFILE);		(* close currently open file *)
      GETNEXTFILE(HEAD, TAIL)		(* pop item from file queue *)
    end;

  break(tty);
  write(tty, EOT, CR );	(* send EOT *)
  close(BSOURCE); close(TEXTFILE)		(* clean up files *)
  end (* function SEND10 *);
$PAGE GET10 receives a list of files from the -11

function GET10 (var HEAD, TAIL: FNPTR)	(* head and tail of list of xfers *)
  : TALKERR;				(* returns non-fatal error codes *)

(* GET10 performs transfers of ASCII files from the -11 to the -10.
   It sends the -11 a send-<filename> directive for every transfer on
   the list. If the -11 responds to the directive with <nak><cr>, the
   directive has been refused. Otherwise the -11 responds with the
   size-checksum bytes for the first block. Since the size is sent as
   two ASCII characters in the range 100-137b, each giving the low or
   high-order 5 bits of the size, there is no danger that the refusal
   will not be recognized. When the -11 sends the end-of-file zero
   length bytes, and the -10 acknowledges, the -11 sends <ack><cr> to
   indicate its readiness for the next directive. *)

  var
    TEMPCH: char;			(* to pass to GETCH *)
    PUTBYTE,				(* for sending acks and nacks *)
    LINLEN,				(* line length from GETLIT *)
    CSUM11,				(* csum given by -11 *)
    CSUM10: EIGHTBIT;			(* csum calculated by ACSUM *)

    WALK: FNPTR;			(* to dispose of FNRECs *)
    LINE11: LITLINE;			(* literal text from GETLIT *)

    I, BSIZE: INTEGER;			(* loop index, blocksize *)

  begin
  GET10 := TOK;

    loop
      GETLIT(LINE11,LINLEN);		(* get ack from previous eof ack *)
    exit if LINE11[1] <> ACK
      do ERROR('No EOF ack-ack.');
    exit if HEAD = nil;			(* end of transfer list *)

      with HEAD^ do
	begin				(* open destination file *)
	rewrite(TEXTFILE,'.XFR '||DSTFILE);
	if not eof(TEXTFILE) then
	  GET10 := TNO10FILE		(* can't open output file?? *)
	end;

    exit if GET10 <> TOK;

      PUTWCR(SENDDIR||HEAD^.SRCFILE);	(* put directive *)

	loop				(* for each block in file *)
	  GETLIT(LINE11,LINLEN);	(* get response to last ack *)
	exit if LINE11[1] = NAK
	  do GET10 := T11REFUSED;	(* only first try if at all *)

	  BSIZE := (ord(LINE11[1]) - 100B) +
		   (ord(LINE11[2]) - 100b) * 32;

	  CSUM11 := (ord(LINE11[3]) - 100B) +
		    (ord(LINE11[4]) - 100B) * 32; (* csum like size *)
	  write(tty, ACK );    	(* ack for size, csum *)
	  break(tty);               	(* force it out *)

	exit if BSIZE = 0;		(* eof marker *)
	  for I := 1 to BSIZE do	(* get the block *)
	    begin
	    GETCH(TEMPCH);
	    BINBLOCK[I].BYTE := ord(TEMPCH);
	    if TEMPCH = CR then GETCH(TEMPCH) (* chuck LF after CR *)
	    end;
	  GETCH(TEMPCH);		(* get rid of <cr> at end *)
	  GETCH(TEMPCH);		(* and <lf> from Cyphernet *)

	  BCSUM(BINBLOCK,BSIZE,CSUM10); (* get our csum *)
	  if CSUM10 = CSUM11 then
	    begin			(* write out block *)
	    for I := 1 to BSIZE do
	      begin
	      TEXTFILE^ := chr(BINBLOCK[I].BYTE);
	      put(TEXTFILE)
	      end;
	    TEMPCH := ACK
	    end
	  else TEMPCH := NAK;		(* csum error -- send nak *)
	  write(tty,TEMPCH);	(* -11 listening for seven bits *)
	  break(tty)
	end (* block loop *);

    exit if GET10 <> TOK;		(* to stop if directive refused *)

      close(TEXTFILE);
      GETNEXTFILE(HEAD,TAIL)		(* bump up file list *)
    end (* file list loop *);

  break(tty);
  write(tty, EOT, CR );	(* send EOT *)
  close(TEXTFILE)
  end (* function GET10 *);
$PAGE COMUTIL vars and little level 2's for Command

function COMMAND: boolean;

var
  LINE: COMLINE;			(* line being parsed *)
  CURRCH: char;				(* for currently parsed char *)
  CURIDX: INTEGER;			(* parsing cursor *)
  CURCOM: COMTYPE;			(* type of command parsed *)
  NEWFN: FNPTR;				(* for making new transfer rec's *)
  PERR: boolean;			(* parsing error *)
  XFRERR: TALKERR;			(* transfer error *)

  procedure GETLIN;			(* to get non-null line from tty: *)
  var
    CH: CHAR;

    begin
    LINE := '';
    while LINE = '' do
      begin				(* prompt and get non-null line *)
      write(tty,'T10:');
      break(tty);
      readln(tty);
      while not eoln(tty) do		(* stuff in the characters *)
	begin
	read(tty,CH);
	LINE := LINE || uppercase(CH)
	end
      end;
    CURIDX := 1
    end (* procedure GETLIN *);


  procedure EATBLANKS;			(* to advance parsing cursor *)
    begin
    while (CURIDX <= length(LINE)) andif
	  (LINE[CURIDX] = ' ') do
      CURIDX := CURIDX + 1
    end (* procedure EATBLANKS *);
$PAGE GETCOM parse directive, within Command
  function GETCOM: boolean;		(* directive parser *)

  (* GETCOM parses the first token on the line, comparing it against
     the table of commands CTAB. A command will be parsed correctly
     if it matches the table entry for its length, and if it is not
     ambiguous (only one command in the table matches it). For exam-
     ple, 'L' in QED would be ambiguous, since it could be either
     'LOAD' or 'LIST'. *)

  var
    VALID: COMSET;			(* commands still possible *)
    CTABIDX: INTEGER;			(* index into chars in table *)
    CURRCH: char;			(* current char being looked at *)
    COMIDX: COMTYPE;			(* index into command table *)
    VALIDCT: INTEGER;			(* cardinality of VALID *)

    begin
    VALID := ALLCOMS;			(* all possible initially *)
    VALIDCT := ord(HIGHCMD) - ord(LOWCMD) + 1;
    CURCOM := ERRCOM;
    CTABIDX := 1;
    EATBLANKS;

    while (CTABIDX <= CTOKLEN) and
	  (CURIDX <= length(LINE) ) do
      begin
      CURRCH := LINE[CURIDX];
      exit if CURRCH = ' ';		(* end of token *)
      for COMIDX := LOWCMD to HIGHCMD do
	if COMIDX in VALID then		(* command still possible *)
	  if CURRCH <> CTAB[COMIDX,CTABIDX] then
	    begin			(* eliminate command from set *)
	    VALIDCT := VALIDCT - 1;	(* keep track of cardinality *)
	    VALID := VALID - [COMIDX]
	    end
	  else CURCOM := COMIDX;	(* when only one valid, the one *)
      exit if VALIDCT = 0;		(* exhausted possibles, bad command *)
      CTABIDX := CTABIDX + 1;		(* next chars in table *)
      CURIDX := CURIDX + 1		(* next char in line *)
      end (* while *);

    GETCOM := VALIDCT = 1;		(* only good if exactly one found *)
    if VALIDCT = 0 then
      writeln(tty,'Invalid command.');
    if VALIDCT > 1 then
      writeln(tty, 'Ambiguous command.')
    end (* function GETCOM *);

$PAGE GETFN parse filename, within Command
  function GETFN(var FN: FNAME): boolean;

  (* GETFN attempts to parse a file descriptor. First, the cursor
     is advanced past any leading blanksthere is no next
     character, or if the next character is a file-name delimiter
     (blank, equals, comma, or slash), GETFN returns false. Other-
     wise, the characters in the line up to but not including the
     next delimiter (or end-of-line) is taken to be a filename, and
     GETFN returns true, with the filename as FN. Commas within
     pairs of '[' and ']' are considered part of the filename, and
   not delimiters. *)

  var
    TAKECOMMA: boolean;			(* true if within brackets *)
    CH: char;

  label
    3;					(* to avoid duplication of code *)

    begin
    FN := '';
    GETFN := false;			(* no delimiter found yet *)
    TAKECOMMA := false;			(* not within brackets *)
    EATBLANKS;

    while (CURIDX <= length(LINE))
      and not GETFN do			(* while not eoln and no delim yet *)
      begin
      CH := LINE[CURIDX];		(* current parsing character *)
	case CH of

	  '=',' ','/': GETFN := true;	(* delimiter found *)

	  ',':if not TAKECOMMA then
	    GETFN := true		(* like delimter *)
	  else goto 3;			(* like any other character *)

	  '[':begin			(* set TAKECOMMA, like any other *)
	    TAKECOMMA := true;
	    goto 3			(* always non-delimiter *)
	  end;

	  ']':begin
	  TAKECOMMA := false;
	  goto 3
	  end;

	  others:begin
	  3: FN := FN || CH;		(* tack on char *)
	  CURIDX := CURIDX + 1		(* and do next one *)
	  end
	end (* case *)
      end (* while *);

    GETFN := (FN <> '')			(* if we found something *)
    end (* function GETFN *);
$PAGE COMMAND code for COMMAND 

  begin					(* procedure command *)
  repeat
    GETLIN
  until GETCOM;				(* until a correctly parsed line *)

  PERR := false;
  COMMAND := true;			(* false only for exit *)
  if (CURCOM = SENDCOM) or
     (CURCOM = GTCOMM) then		(* get a file transfer list *)
    repeat
      EATBLANKS;
    exit if CURIDX > length(LINE);	(* no more on line *)
      if TOPQ <> nil then
	CURIDX := CURIDX + 1;		(* tick past comma we know is there *)
      new(NEWFN);			(* file transfer record *)
      if TOPQ = nil then
	TOPQ := NEWFN
      else BOTTOMQ^.NEXT := NEWFN;	(* chain on from above *)
      BOTTOMQ := NEWFN;
      BOTTOMQ^.NEXT := nil;
      BOTTOMQ^.BINARY := false;
    exit if not GETFN(BOTTOMQ^.DSTFILE)	(* out if no filename there *)
    orif (CURIDX > length(LINE))	(* orif only one filename *)
      do PERR := true;			(* both are error condx *)

      CURRCH := LINE[CURIDX];
    exit if CURRCH <> '='		(* must follow dest file *)
      do PERR := true;
      CURIDX := CURIDX + 1;		(* tick past '=' *)
    exit if not GETFN(BOTTOMQ^.SRCFILE)	(* get source file *)
      do PERR := true;

      if CURIDX < length(LINE) then	(* something after filename *)
	if (LINE[CURIDX] = '/') and	(* could be '/B' *)
	(CURCOM = SENDCOM) then		(* only valid on send *)
	  if LINE[CURIDX+1] = 'B' then
	    begin			(* got binary switch, legal *)
	    BOTTOMQ^.BINARY := true;
	    CURIDX := CURIDX + 2	(* tick past /b *)
	    end;
      EATBLANKS;
    until (CURIDX > length(LINE)) orif
      (LINE[CURIDX] <> ',');

  if PERR or (CURIDX <= length(LINE)) then
    begin				(* parsing error, don't do command *)
    writeln(tty, 'Unparsable file specs.');
    while TOPQ <> nil do
      GETNEXTFILE(TOPQ,BOTTOMQ)		(* get rid of descriptors *)
    end
  else 					(* execute command *)
    case CURCOM of

      QUITCOM, EXITCOM: COMMAND := false; (* leave program at main *)

      HELPCOM: begin
	writeln(tty);
	writeln(tty,'Help for TALK10. Commands to the -10 are HELP, SEND,');
	writeln(tty,'GET, QUIT or EXIT. SEND and GET take a list of file');
	writeln(tty,'transfer descriptors of the form:');
	writeln(tty,'		dstfile=srcfile[/b] [,filetransferlist]');
	writeln(tty);
	writeln(tty,'The list will be processed until empty or until one of');
	writeln(tty,'the transfers is invalid. When returning control to the');
	writeln(tty,'-11 by hitting ^^ after the ''Ready'' prompt, issue the ');
	writeln(tty,'''/I'' command to start the transfer. When returning to');
	writeln(tty,'the -10 via ''/t'' command, a carriage return will print');
	writeln(tty,'error messages from the previous transfer, if any.');
	writeln(tty,'For further info, please see Jerry Rosen. ');
	writeln(tty)
      end;

      SENDCOM, GTCOMM: 
      if TOPQ = nil then
	writeln(tty,'Need filename list.')
      else begin
	writeln(tty,'$$half;crt;dec;key;bro off;cr 0,0;ff on,0,0;htab on,8,0');
	writeln(tty,'$$len 0;lf 0;slew 0;vtab on,1,0;wait off;nlf');
	PASSCC; PASSCR;
	writeln(tty,'Ready -- hit control-uparrow and give ''/I'' to -11');
	break(tty);
	if CURCOM = GTCOMM then
	  XFRERR := GET10(TOPQ,BOTTOMQ)
	else XFRERR := SEND10(TOPQ,BOTTOMQ);
	break(tty);
	readln(tty);			(* <cr> to get error report *)
	writeln(tty,'$$full;key;bro on;nlf off');
	FLTRCC;
	case XFRERR of
	  TOK: writeln(tty,'No error detected.');
	  T11REFUSED: writeln(tty,'11 refused directive.');
	  TNO10FILE: writeln(tty,'10 file not openable.')
	end (* case errormessage *);

	while TOPQ <> nil do
	with TOPQ^ do
	  begin				(* print uncompleted xfers *)
	  if BINARY then
	    write(tty,'Binary ');
	  writeln(tty,'Transfer ',SRCFILE,' to ',DSTFILE,' not done.');
	  GETNEXTFILE(TOPQ,BOTTOMQ)
	  end
      end (* case GTCOMM, SENDCOM *)
    end (* case stmt *)
  end (* function COMMAND *);
$PAGE MAIN program for TALK-10

  begin					(* mainline *)
  open(tty); rewrite(ttyoutput);
  writeln(tty,'TALK10 V1.0 ',compdate,' on Cyphernet.');

  while COMMAND do;			(* while command returns true *)

  writeln(tty, 'Exit TALK10 on Cyphernet.');
  2: end.
   ; @s