$WIDTH=100
$LENGTH=55
$TITLE pasutl.pas, last modified 9/15/83, zw

MODULE pasutl;
(*PASCAL utilities*)
$PAGE directory
(*dbg -- global debug flag, TRUE signifies debugging*)
(*err -- global error flag, TRUE signifies error*)
(*tkn -- global token variable, contains current token string*)
(*errmsg -- write out message to error file and terminal*)
(*error -- signal error with message*)
(*fatal -- fatal error with message*)
(*chkerr -- check condition, signal error if true*)
(*chkftl -- check condition, fatal error if true*)
(*errfil -- open error log file*)
(*logstr -- write out text string to log file*)
(*loglin -- write out text line to log file*)
(*logfil -- open output log file*)
(*wrstr -- write out text string*)
(*wrlin -- write out line of text*)
(*wrchk -- check if string can be written*)
(*rdlin -- try to read in line of text*)
(*getcmd -- try to get next command line*)
(*cmdfil -- open input command file*)
(*scan -- scan a token*)
(*numstr -- try to convert a number to a string*)
(*strnum -- try to convert a string to a number*)
(*abbrev -- see if one string is an abbreviation of another*)
(*lkpwrd -- lookup word in list, may be abbreviated, retrieve index*)
(*query -- ask question, accept response from list*)
(*askyn -- ask question, accept yes/no response*)
(*lptoff -- restore output after lpton*)
(*lpton -- connect output to printer*)
(*ttyoff -- restore input and output after ttyon*)
(*ttyon -- force input and output from terminal*)
(*popio -- try to pop input and output files from stack*)
(*pushio -- try to push input and output files onto stack*)
(*clsfil -- close file*)
(*opnfil -- open file*)
(*closio -- close input and output file, terminal not closed*)
(*openio -- try to open input and output files*)
(*d -- debug message*)
(*b -- begin debug message*)
(*e -- end debug message*)
(*debug -- set/reset debug flag*)
(*dbgfil -- open debug log file*)
(*optnum -- set numeric option value*)
(*getopt -- get options from command line*)
(*getarg -- try to retrieve nth argument from command line*)
(*start -- try to set up PASCAL program*)
(*copy -- copy INPUT to OUTPUT*)
(*help -- display help file to terminal*)
(*crypt -- encrypt INPUT to OUTPUT*)
$PAGE constants, types and variables
$INCLUDE pasutl.typ

VAR
    cmdlin: cmdlintyp;
    errfile, dbgfile, logfile, cmdfile: txtfil;
    filstk: ARRAY[1 .. filstksiz] OF RECORD
      ifil, ofil: txtfil
    END;
    filptr: 0 .. filstksiz;
    cmdstk: ARRAY[1 .. cmdstksiz] OF txtfil;
    cmdptr: 0 .. cmdstksiz;
    wrpos: 0 .. cmdlinlen;
    ttyison, lptison, lptwason: bin;
    initialdbg: bin;
    dbgindent: int;
    coldstart: bin := TRUE;
    scnbuf: cmdlintyp := '';
    scnch: CHAR := ' ';
    scncsr: -1 .. cmdlinlen := -1;
$PAGE public declarations

PUBLIC VAR
    dbg: bin := FALSE;

PUBLIC VAR
    err: bin := FALSE;

PUBLIC VAR
    tkn: STRING[tknsiz] := '';

PUBLIC PROCEDURE errmsg(msg: str);

FORWARD;

PUBLIC PROCEDURE error(msg: str);

FORWARD;

PUBLIC PROCEDURE fatal(msg: str);

FORWARD;

PUBLIC PROCEDURE chkerr(cond: bin; msg: str);

FORWARD;

PUBLIC PROCEDURE chkftl(cond: bin; msg: str);

FORWARD;

PUBLIC PROCEDURE errfil(fil: str);

FORWARD;

PUBLIC PROCEDURE logstr(txtstr: str);

FORWARD;

PUBLIC PROCEDURE loglin(txtlin: str);

FORWARD;

PUBLIC PROCEDURE logfil(fil: str);

FORWARD;

PUBLIC PROCEDURE wrstr(txtstr: str);

FORWARD;
$PAGE

PUBLIC PROCEDURE wrlin(txtlin: str);

FORWARD;

PUBLIC FUNCTION wrchk(len: int): bin;

FORWARD;

PUBLIC FUNCTION rdlin(VAR lin: str): bin;

FORWARD;

PUBLIC FUNCTION getcmd(prompt: str; VAR cmdlin: str): bin;

FORWARD;

PUBLIC PROCEDURE cmdfil(fil: str);

FORWARD;

PUBLIC PROCEDURE scan(tknstr: STRING[tknsiz]);

FORWARD;

PUBLIC FUNCTION numstr(num: int; VAR strval: str): bin;

FORWARD;

PUBLIC FUNCTION strnum(strval: str; VAR num: int): bin;

FORWARD;

PUBLIC FUNCTION abbrev(s1, s2: str): bin;

FORWARD;

PUBLIC FUNCTION lkpwrd(word: str; list: wrdlst): int;

FORWARD;

PUBLIC FUNCTION query(question: str; list: wrdlst): int;

FORWARD;

PUBLIC FUNCTION askyn(question: str): bin;

FORWARD;
$PAGE

PUBLIC PROCEDURE lptoff;

FORWARD;

PUBLIC PROCEDURE lpton;

FORWARD;

PUBLIC PROCEDURE ttyoff;

FORWARD;

PUBLIC PROCEDURE ttyon;

FORWARD;

PUBLIC FUNCTION popio: bin;

FORWARD;

PUBLIC FUNCTION pushio: bin;

FORWARD;

PUBLIC PROCEDURE clsfil(VAR fil: txtfil);

FORWARD;

PUBLIC FUNCTION opnfil(n, d: str; VAR f: txtfil; m: str): bin;

FORWARD;

PUBLIC PROCEDURE closio;

FORWARD;

PUBLIC FUNCTION openio(ifil, ofil: str): bin;

FORWARD;

PUBLIC PROCEDURE d(msg: str);

FORWARD;

PUBLIC PROCEDURE b(msg: str);

FORWARD;
$PAGE

PUBLIC PROCEDURE e(msg: str);

FORWARD;

PUBLIC PROCEDURE debug;

FORWARD;

PUBLIC PROCEDURE dbgfil(fil: str);

FORWARD;

PUBLIC PROCEDURE optnum(VAR opt: int; defval, argnum: int);

FORWARD;

PUBLIC PROCEDURE getopt (keys: wrdlst; VAR flags: ARRAY[1 .. *] OF int);

FORWARD;

PUBLIC FUNCTION getarg(n: int; VAR argstr: str): bin;

FORWARD;

PUBLIC FUNCTION start(prog, cmd: str): bin;

FORWARD;

PUBLIC PROCEDURE copy;

FORWARD;

PUBLIC PROCEDURE help(fil: str);

FORWARD;

PUBLIC PROCEDURE crypt(key: str);

FORWARD;
$PAGE errmsg, error, fatal, chkerr, chkftl, errfil

PROCEDURE errmsg(msg: str);

BEGIN
  IF errfile <> NILF THEN
    WRITELN(errfile, msg);
  IF errfile <> TTYOUTPUT THEN
    WRITELN(TTYOUTPUT, msg);
  BREAK(TTYOUTPUT)
END;

PROCEDURE error(msg: str);

BEGIN
  errmsg('error: ' || msg);
  err := TRUE
END;

PROCEDURE fatal(msg: str);

BEGIN
  errmsg('fatal: ' || msg);
  STOP
END;

PROCEDURE chkerr(cond: bin; msg: str);

BEGIN
  err := FALSE;
  IF cond THEN
    error(msg)
END;

PROCEDURE chkftl(cond: bin; msg: str);

BEGIN
  err := FALSE;
  IF cond THEN
    fatal(msg)
END;

PROCEDURE errfil(fil: str);

BEGIN
  chkftl(NOT opnfil(fil, 'ERROR.LOG', errfile, 'LOG'), 'error log file')
END;
$PAGE logstr, loglin, logfil

PROCEDURE logstr(txtstr: str);

BEGIN
  IF (logfile <> NILF) THEN
    WRITE(logfile, txtstr);
  IF logfile = TTYOUTPUT THEN
    BREAK(TTYOUTPUT)
END;

PROCEDURE loglin(txtlin: str);

BEGIN
  IF (logfile <> NILF) THEN
    WRITELN(logfile, txtlin);
  IF logfile = TTYOUTPUT THEN
    BREAK(TTYOUTPUT)
END;

PROCEDURE logfil(fil: str);


  PROCEDURE opnlog(fil: str);

  BEGIN
    chkerr(NOT opnfil(fil, '', logfile, 'LOG'), 'log file')
  END;

BEGIN
  clsfil(logfile);
  IF INDEX(fil, '.') > 0 THEN
    opnlog(fil)
  ELSE
    opnlog(fil || logext);
END;
$PAGE wrstr, wrlin, wrchk

PROCEDURE wrstr(txtstr: str);

BEGIN
  IF OUTPUT <> NILF THEN
    WRITE(OUTPUT, txtstr);
  IF OUTPUT = TTYOUTPUT THEN
    BREAK(TTYOUTPUT);
  wrpos := wrpos + LENGTH(txtstr);
  IF logfile <> TTYOUTPUT THEN
    logstr(txtstr)
END;

PROCEDURE wrlin(txtlin: str);

BEGIN
  IF OUTPUT <> NILF THEN
    WRITELN(OUTPUT, txtlin);
  IF OUTPUT = TTYOUTPUT THEN
    BREAK(TTYOUTPUT);
  wrpos := 0;
  IF logfile <> TTYOUTPUT THEN
    loglin(txtlin)
END;

FUNCTION wrchk(len: int): bin;

BEGIN
  wrchk := (wrpos + len) <= cmdlinlen
END;
$PAGE rdlin, getcmd

FUNCTION rdlin(VAR lin: str): bin;

BEGIN
  IF (INPUT = NILF) OR (lin = eofflag) THEN
    lin := eofflag
  ELSE IF INPUT = TTY THEN BEGIN
    IF cmdfile = NILF THEN BEGIN
      READLN(TTY);
      READ(TTY, lin)
    END
    ELSE BEGIN
      INPUT := cmdfile;
      WHILE NOT rdlin(lin) DO BEGIN
	clsfil(INPUT);
	IF cmdptr > 0 THEN BEGIN
	  cmdfile := cmdstk[cmdptr];
	  cmdptr := PRED(cmdptr)
	END
	ELSE
	  cmdfile := NILF
      END;
      INPUT := TTY
    END;
    loglin(lin)
  END
  ELSE IF EOF(INPUT) THEN
    lin := eofflag
  ELSE
    READLN(INPUT, lin);
  rdlin := lin <> eofflag
END;

FUNCTION getcmd(prompt: str; VAR cmdlin: str): bin;

BEGIN
  ttyon;
  IF cmdfile = NILF THEN BEGIN
    wrlin('');
    wrstr(prompt)
  END
  ELSE BEGIN
    loglin('');
    logstr(prompt)
  END;
  getcmd := rdlin(cmdlin);
  ttyoff
END;
$PAGE cmdfil

PROCEDURE cmdfil(fil: str);


  PROCEDURE opncmd(fil: str);

  BEGIN
    chkerr(NOT opnfil(fil, '', cmdfile, 'INPUT'), 'command file');
  END;

BEGIN
  chkerr(cmdptr = cmdstksiz, 'command file stack overflow');
  IF NOT err THEN BEGIN
    cmdptr := SUCC(cmdptr);
    cmdstk[cmdptr] := cmdfile;
    IF INDEX(fil, '.') > 0 THEN
      opncmd(fil)
    ELSE
      opncmd(fil || cmdext);
    IF err THEN BEGIN
      cmdfile := cmdstk[cmdptr];
      cmdptr := PRED(cmdptr)
    END
  END
END;
$PAGE scan

PROCEDURE scan(tknstr: STRING[tknsiz]);

CONST
    delims = [' ', CHR(9)];
    namchrs = ['A' .. 'Z', '0' .. '9', '_'];


  PROCEDURE getbuf;

  BEGIN
    IF scncsr = -1 THEN
      scnbuf := '.EOF'
    ELSE
      scnbuf := '';
    IF rdlin(scnbuf) THEN BEGIN
      scnbuf := UPPERCASE(scnbuf);
      IF scnbuf = '' THEN
	scncsr := 0
      ELSE
	scncsr := 1
    END
    ELSE
      scncsr := -1
  END;


  PROCEDURE getch;

  BEGIN
    IF scncsr > 0 THEN BEGIN
      scnch := scnbuf[scncsr];
      IF scncsr = LENGTH(scnbuf) THEN
	scncsr := 0
      ELSE
	scncsr := scncsr + 1
    END
    ELSE BEGIN
      scnch := ' ';
      getbuf
    END
  END;
$PAGE

BEGIN
  IF (tknstr = '') OR (tkn = tknstr) THEN BEGIN
    WHILE NOT (scncsr < 0) AND (scnch IN delims) DO
      getch;
    IF scncsr < 0 THEN
      tkn := ''
    ELSE BEGIN
      tkn := scnch;
      IF NOT (scnch IN namchrs) THEN
	getch
      ELSE BEGIN
	getch;
	WHILE NOT (scncsr < 0) AND (scnch IN namchrs) DO BEGIN
	  IF LENGTH(tkn) < UPPERBOUND(tkn) THEN
	    tkn := tkn || scnch;
	  getch
	END
      END
    END
  END
  ELSE
    error('scanning "' || tknstr || '" and found "' || tkn || '"')
END;
$PAGE numstr, strnum

FUNCTION numstr(num: int; VAR strval: str): bin;

BEGIN
  PUTSTRING(strval, num);
  numstr := IOSTATUS = IO_OK
END;

FUNCTION strnum(strval: str; VAR num: int): bin;

BEGIN
  GETSTRING(strval, num);
  strnum := IOSTATUS = IO_OK
END;
$PAGE abbrev, lkpwrd

FUNCTION abbrev(s1, s2: str): bin;

VAR
    i, p1, l1: int;

BEGIN
  p1 := VERIFY(s1, [' '], 1);
  l1 := SEARCH(SUBSTR(s1 || ' ', p1), [' ']) - 1;
  IF l1 <= LENGTH(s2) THEN BEGIN
    abbrev := UPPERCASE(SUBSTR(s1, p1, l1)) = UPPERCASE(SUBSTR(s2, 1, l1))
  END
  ELSE
    abbrev := FALSE
END;

FUNCTION lkpwrd(word: str; list: wrdlst): int;

VAR
    i: int;

BEGIN
  lkpwrd := 0;
  FOR i := 1 TO UPPERBOUND(list) DO
EXIT IF abbrev(word, list[i]) DO
    lkpwrd := i
END;
$PAGE query, askyn

FUNCTION query(question: str; list: wrdlst): int;

VAR
    index, i: int;
    cmdlin: cmdlintyp;

BEGIN
  REPEAT
    cmdlin := '';
  EXIT IF NOT getcmd(question, cmdlin);
    index := lkpwrd(cmdlin, list);
    IF index = 0 THEN BEGIN
      chkftl(cmdfile <> NILF, 'query error in command file');
      wrlin('Respond with one of:');
      FOR i := 1 TO UPPERBOUND(list) DO
	wrlin(list[i])
    END
  UNTIL index > 0;
  query := index
END;

FUNCTION askyn(question: str): bin;

BEGIN
  CASE query(question, ('YES', 'NO')) OF
    0:
      askyn := FALSE;
    1:
      askyn := TRUE;
    2:
      askyn := FALSE
  END
END;
$PAGE lptoff, lpton

PROCEDURE lptoff;

BEGIN
  IF lptison THEN BEGIN
    WRITE(TTYOUTPUT, CHR(27), CHR(91), CHR(52), CHR(105));
    BREAK(TTYOUTPUT);
    lptison := FALSE
  END
END;

PROCEDURE lpton;

BEGIN
  IF NOT lptison THEN BEGIN
    chkerr(OUTPUT <> TTYOUTPUT, 'output is not terminal for printer');
    IF NOT err THEN BEGIN
      WRITE(TTYOUTPUT, CHR(27), CHR(91), CHR(53), CHR(105));
      BREAK(TTYOUTPUT);
      lptison := TRUE
    END
  END
END;
$PAGE ttyoff, ttyon

PROCEDURE ttyoff;

BEGIN
  IF ttyison THEN BEGIN
    chkftl(NOT popio, 'can not pop I/O to turn off terminal');
    ttyison := FALSE;
    IF lptwason THEN BEGIN
      lpton;
      lptwason := FALSE
    END
  END;
END;

PROCEDURE ttyon;

BEGIN
  IF NOT ttyison THEN BEGIN
    lptwason := lptison;
    lptoff;
    chkftl(NOT pushio, 'can not push I/O to turn on terminal');
    INPUT := TTY;
    OUTPUT := TTYOUTPUT;
    ttyison := TRUE
  END
END;
$PAGE popio, pushio

FUNCTION popio: bin;

BEGIN
  chkerr(filptr = 0, 'file stack underflow');
  IF err THEN
    popio := FALSE
  ELSE BEGIN
    INPUT := filstk[filptr].ifil;
    OUTPUT := filstk[filptr].ofil;
    filptr := PRED(filptr);
    popio := TRUE
  END
END;

FUNCTION pushio: bin;

BEGIN
  chkerr(filptr = filstksiz, 'file stack overflow');
  IF err THEN
    pushio := FALSE
  ELSE BEGIN
    filptr := SUCC(filptr);
    filstk[filptr].ifil := INPUT;
    filstk[filptr].ofil := OUTPUT;
    pushio := TRUE
  END
END;
$PAGE clsfil

PROCEDURE clsfil(VAR fil: txtfil);

BEGIN
  IF fil = INPUT THEN
    scncsr := -1;
  IF fil = OUTPUT THEN
    wrpos := 0;
  IF fil = TTYOUTPUT THEN
    lptoff;
  IF (fil <> TTYOUTPUT) AND (fil <> TTY) THEN
    CLOSE(fil);
  fil := NILF
END;
$PAGE opnfil

FUNCTION opnfil(n, d: str; VAR f: txtfil; m: str): bin;

VAR
    nam: filnam;

BEGIN
  chkerr(LENGTH(n) > UPPERBOUND(nam), 'file name too long: "' || n || '"');
  IF NOT err THEN BEGIN
    IF n = '' THEN
      nam := UPPERCASE(d)
    ELSE
      nam := UPPERCASE(n);
    IF nam = '' THEN
      nam := ttynam;
    CASE lkpwrd(m, ('INPUT', 'OUTPUT', 'LOG')) OF
      0:
	fatal('open mode: ' || m);
      1: BEGIN
	IF nam = ttynam THEN
	  f := TTY
	ELSE IF nam = lptnam THEN
	  error('LPT: is not an input device')
	ELSE BEGIN
	  RESET(f, nam);
	  chkerr(IOSTATUS <> IO_OK, 'input file: ' || nam)
	END
      END;
      2: BEGIN
	IF nam = ttynam THEN
	  f := TTYOUTPUT
	ELSE IF nam = lptnam THEN BEGIN
	  f := TTYOUTPUT;
	  lpton
	END
	ELSE BEGIN
	  REWRITE(f, nam);
	  chkerr(IOSTATUS <> IO_OK, 'output file: ' || nam)
	END
      END;
$PAGE
      3: BEGIN
	IF nam = ttynam THEN
	  f := TTYOUTPUT
	ELSE IF nam = lptnam THEN
	  error('can not log to LPT:')
	ELSE BEGIN
	  IF n = '' THEN BEGIN
	    RESET(f, nam);
	    IF IOSTATUS = IO_OK THEN BEGIN
	      CLOSE(f);
	      REWRITE(f, nam, [PRESERVE])
	    END
	    ELSE
	      f := TTYOUTPUT
	  END
	  ELSE BEGIN
	    REWRITE(f, nam, [PRESERVE]);
	    chkerr(IOSTATUS <> IO_OK, 'log file: ' || nam)
	  END
	END
      END
    END
  END;
  IF f = INPUT THEN BEGIN
    scncsr := 0;
    scnch := ' '
  END;
  IF f = OUTPUT THEN
    wrpos := 0;
  IF err THEN
    f := NILF;
  opnfil := NOT err
END;
$PAGE closio, openio

PROCEDURE closio;

BEGIN
  clsfil(INPUT);
  clsfil(OUTPUT);
  chkftl(NOT popio, 'can not pop I/O after closing I/O')
END;

FUNCTION openio(ifil, ofil: str): bin;

BEGIN
  chkftl(NOT pushio, 'can not push I/O to open I/O');
  IF NOT err THEN
    err := NOT opnfil(ifil, '', INPUT, 'INPUT');
  IF NOT err THEN
    err := NOT opnfil(ofil, '', OUTPUT, 'OUTPUT');
  openio := NOT err;
  IF err THEN
    chkftl(NOT popio, 'can not pop I/O to open I/O')
END;
$PAGE d, b, e, debug, dbgfil

PROCEDURE d(msg: str);

BEGIN
  IF dbg THEN
    WRITELN(dbgfile, ' ': dbgindent * 2, msg)
END;

PROCEDURE b(msg: str);

BEGIN
  d(msg);
  dbgindent := dbgindent + 1
END;

PROCEDURE e(msg: str);

BEGIN
  IF dbgindent > 0 THEN
    dbgindent := dbgindent - 1
  ELSE
    dbgindent := 0;
  d(msg)
END;

PROCEDURE debug;

VAR
    newdbg: bin;

BEGIN
  newdbg := askyn('Debug?');
  IF newdbg THEN
    d('Debug flag is ON.')
  ELSE
    d('Debug flag is OFF.');
  dbg := newdbg;
  IF dbg THEN
    dbgindent := 0;
END;

PROCEDURE dbgfil(fil: str);

BEGIN
  chkftl(NOT opnfil(fil, 'DEBUG.LOG', dbgfile, 'LOG'), 'debug log file')
END;
$PAGE optnum

PROCEDURE optnum(VAR opt: int; defval, argnum: int);

VAR
    argwrd: wrdtyp;
    valpos: int;

BEGIN
  IF getarg(argnum, argwrd) THEN BEGIN
    valpos := SEARCH(argwrd || optflag, [optflag]) + 1;
    IF valpos > LENGTH(argwrd) THEN
      opt := defval
    ELSE IF NOT strnum(SUBSTR(argwrd, valpos), opt) THEN BEGIN
      error('argument value not numeric: "' || argwrd || '"');
      opt := defval
    END
  END
  ELSE
    opt := defval
END;
$PAGE getopt

PROCEDURE getopt(keys: wrdlst; VAR flags: ARRAY[1 .. *] OF int);

VAR
    wrdidx, argidx, argwrdlen: int;
    argwrd: wrdtyp;

BEGIN
  FOR wrdidx := 1 TO UPPERBOUND(flags) DO
    flags[wrdidx] := 0;
  argidx := 1;
  WHILE getarg(argidx, argwrd) DO BEGIN
    argwrdlen := SEARCH(argwrd || optflag, [optflag]) - 1;
    IF argwrdlen > 0 THEN BEGIN
      wrdidx := lkpwrd(SUBSTR(argwrd, 1, argwrdlen), keys);
      IF wrdidx = 0 THEN
	error('unknown argument key: "' || argwrd || '"')
      ELSE IF wrdidx <= UPPERBOUND(flags) THEN
	flags[wrdidx] := argidx
    END;
    argidx := argidx + 1
  END
END;
$PAGE getarg

FUNCTION getarg(n: int; VAR argstr: str): bin;

VAR
    argnum, argpos, newargpos, arglen: int;

BEGIN
  argnum := 0;
  argpos := 0;
  REPEAT
    newargpos := argpos + INDEX(SUBSTR(cmdlin, argpos + 1), argflag, 0);
  EXIT IF newargpos <= argpos DO
      argnum := 0;
    argpos := newargpos;
    argnum := argnum + 1
  UNTIL argnum >= n;
  IF (n > 0) ANDIF (argnum = n) THEN BEGIN
    arglen := INDEX(SUBSTR(cmdlin, argpos + 1), argflag, 0) - 1;
    IF arglen < 0 THEN
      arglen := LENGTH(cmdlin) - argpos;
    IF arglen > UPPERBOUND(argstr) THEN
      arglen := UPPERBOUND(argstr);
    argstr := SUBSTR(cmdlin, argpos + 1, arglen);
    getarg := TRUE
  END
  ELSE BEGIN
    getarg := FALSE;
    argstr := ''
  END
END;
$PAGE start

FUNCTION start(prog, cmd: str): bin;

VAR
    done, ok: bin;
    ipos, opos, ilen, olen: int;


  PROCEDURE setup;

  BEGIN
    CLOSE;
    OPEN(TTY);
    REWRITE(TTYOUTPUT);
    INPUT := TTY;
    OUTPUT := TTYOUTPUT;
    cmdfile := NILF;
    logfile := NILF;
    errfile := NILF;
    dbgfile := NILF;
    cmdptr := 0;
    filptr := 0;
    ttyison := FALSE;
    lptison := FALSE;
    lptwason := FALSE;
    ttyon;
    errfil('');
    dbgfil('');
    coldstart := FALSE;
    done := FALSE;
   ialdbg := dbg;
    wrlin(prog)
  END;


  PROCEDURE dpyinfo;

  BEGIN
    wrlin('Program: ' || prog);
    wrstr('<output file>' || filflag || '<input file>');
    wrstr(argflag || 'arg1' || optflag || '##');
    wrlin(argflag || 'arg1' || optflag || '##...');
    wrlin(cmdflag || '<cmd file>');
    wrlin(logflag || '<log file>');
    wrlin('/HELP');
    wrlin('/EXIT or carriage return')
  END;
$PAGE


  PROCEDURE scanio;

  BEGIN
    opos := 1;
    olen := INDEX(cmdlin, filflag, 0) - opos;
    IF olen < 0 THEN
      ipos := 1
    ELSE
      ipos := olen + 2;
    ilen := INDEX(cmdlin, argflag, LENGTH(cmdlin) + 1) - ipos;
    IF olen < 0 THEN
      olen := 0;
    IF ilen < 0 THEN
      ilen := 0;
  END;
$PAGE

BEGIN
  IF coldstart THEN
    setup
  ELSE BEGIN
    WHILE filptr > 0 DO
      closio;
    lptoff;
    ttyon;
    done := TRUE;
  END;
  IF ok AND initialdbg AND NOT done THEN
    debug;
  start := ok AND NOT done;
  IF done THEN
    CLOSE
END;
$PAGE copy

PROCEDURE copy;

VAR
    lin: lstlintyp;

BEGIN
  lin := '';
  WHILE rdlin(lin) DO BEGIN
    IF EOPAGE THEN
      PAGE;
    wrlin(lin)
  END
END;
$PAGE help

PROCEDURE help(fil: str);

VAR
    lin: cmdlintyp;
    count: int;
    endfil: bin;

BEGIN
  chkerr(NOT openio(fil || '.HLP', ''), 'help file');
  IF NOT err THEN BEGIN
    wrlin('');
    count := 0;
    lin := '';
    REPEAT
      endfil := NOT rdlin(lin);
      IF endfil ORIF (UPPERCASE(lin) = '*END*') THEN
	endfil := TRUE
      ELSE IF UPPERCASE(lin) = '*PAUSE*' THEN BEGIN
	count := 0;
	endfil := NOT askyn('Continue? ')
      END
      ELSE BEGIN
	IF count > 20 THEN BEGIN
	  count := 0;
	  endfil := NOT askyn('Continue? ')
	END;
	IF NOT endfil THEN BEGIN
	  wrlin(lin);
	  count := SUCC(count)
	END
      END
    UNTIL endfil;
    closio
  END
END;
$PAGE crypt

PROCEDURE crypt(key: str);

VAR
    x: ARRAY[CHAR, CHAR] OF CHAR;
    lin: lstlintyp;
    lincsr, keycsr: int;


  PROCEDURE load_x;

  VAR
      c, c1, c2: CHAR;
      minpchar, maxpchar: CHAR;
      pchar: SET OF CHAR;

  BEGIN (*load translation matrix*)
    pchar := [' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
      'M' , 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a'
	, 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'f', 'k', 'l', 'm', 'n', 'o',
	  'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2',
	    '3', '4', '5', '6', '7', '8', '9', '!', '@', '#', '$', '%', '^', '&'
	      , '*', '(', ')', '_', '-', '+', '=', '{', '[', '}', ']', '|', '\',
		':', ';', '"' , '''', '<', ',', '>', '.', '?', '/', '~', '`'];
    FOR minpchar := MINIMUM(CHAR) TO MAXIMUM(CHAR) DO
  EXIT IF minpchar IN pchar;
    FOR maxpchar := MAXIMUM(CHAR) DOWNTO MINIMUM(CHAR) DO
  EXIT IF maxpchar IN pchar;
    FOR c1 := MINIMUM(CHAR) TO MAXIMUM(CHAR) DO
      FOR c2 := MINIMUM(CHAR) TO MAXIMUM(CHAR) DO
	x[c1, c2] := c1;
    c := minpchar;
    REPEAT
      c := SUCC(c)
    UNTIL c IN pchar;
    FOR c2 := minpchar TO maxpchar DO
      IF c2 IN pchar THEN
	FOR c1 := maxpchar DOWNTO minpchar DO
	  IF c1 IN pchar THEN BEGIN
	    x[c1, c2] := c;
	    IF c1 <> minpchar THEN
	      REPEAT
		IF c = maxpchar THEN
		  c := minpchar
		ELSE
		  c := SUCC(c)
	      UNTIL c IN pchar
	  END
  END;
$PAGE

BEGIN
  IF key = '' THEN
    error('null crypt key')
  ELSE BEGIN
    load_x;
    keycsr := 1;
    lin := '';
    WHILE rdlin(lin) DO BEGIN
      FOR lincsr := 1 TO LENGTH(lin) DO BEGIN
	lin[lincsr] := x[lin[lincsr], key[keycsr]];
	keycsr := keycsr + 1;
	IF keycsr > LENGTH(key) THEN
	  keycsr := 1
      END;
      wrlin(lin)
    END
  END
END.
 : ×