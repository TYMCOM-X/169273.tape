$WIDTH=100
$LENGTH=55
$TITLE PASUTL.PAS, last modified 11/1/83, zw

MODULE pasutl OPTIONS SPECIAL(COERCIONS, WORD);
(*TYM-Pascal Utility Library*)

$SYSTEM PASLIB.INC

CONST
version = '1.0'; (*current version of utility library*)
eof_flag = '.EOF'; (*signals end-of-file for INPUT*)
file_flag = '='; (*seperates file names in command line*)
info_flag = '?'; (*signals information display*)
tty_nam = 'TTY:'; (*name of terminal*)
lpt_nam = 'LPT:'; (*name of printer*)

PUBLIC VAR
cmdfil: STRING := ''; (*null signifies no command file*)
cmdlin: STRING := ''; (*global command line*)
err: BOOLEAN := FALSE; (*error flag, TRUE implies error*)
dfinam: STRING := tty_nam; (*default input file name*)
dfonam: STRING := tty_nam; (*default output file name*)
dfiext: STRING := 'TXT'; (*default input file extension*)
dfoext: STRING := 'TXT'; (*default output file extension*)

VAR
saved_input: TEXT := NILF; (*saved INPUT during command file access*)
saved_output: TEXT := NILF; (*saved OUTPUT during command file access*)
lpt_is_on: BOOLEAN := FALSE; (*flag set TRUE if OUTPUT connected to printer*)

PUBLIC PROCEDURE opntty;
BEGIN (*open terminal for input and output*)
  OPEN(TTY);
  REWRITE(TTYOUTPUT)
END;

PUBLIC PROCEDURE ttystr(s: STRING);
BEGIN (*write string to terminal*)
  WRITE(TTYOUTPUT, s);
  BREAK(TTYOUTPUT)
END;

PUBLIC PROCEDURE ttylin(l: STRING);
BEGIN (*write line of text to terminal*)
  WRITELN(TTYOUTPUT, l);
  BREAK(TTYOUTPUT)
END;

PUBLIC PROCEDURE wrstr(s: STRING);
BEGIN (*write string to OUTPUT*)
  IF OUTPUT = TTYOUTPUT THEN ttystr(s) ELSE WRITE(OUTPUT, s)
END;

PUBLIC PROCEDURE wrlin(l: STRING);
BEGIN (*write line of text to OUTPUT*)
  IF OUTPUT = TTYOUTPUT THEN ttylin(l) ELSE WRITELN(OUTPUT, l)
END;

PUBLIC PROCEDURE wrchrs(c: ARRAY[1 .. *] OF INTEGER);
VAR i: INTEGER;
BEGIN (*write characters to OUTPUT*)
  FOR i := 1 TO UPPERBOUND(c) DO wrstr(CHR(c[i]))
END;

PUBLIC FUNCTION rdlin(VAR l: STRING): BOOLEAN;
BEGIN (*read line from INPUT, return FALSE if end-of-file*)
  IF l = eof_flag THEN l := eof_flag
  ELSE IF INPUT = NILF THEN l := eof_flag
  ELSE IF INPUT = TTY THEN BEGIN
    READLN(TTY);
    READ(TTY, l)
  END
  ELSE IF EOF(INPUT) THEN l := eof_flag
  ELSE READLN(l);
  rdlin := (l <> eof_flag)
END;

PUBLIC PROCEDURE error(m: STRING);
BEGIN (*declare error*)
  opntty;
  ttylin('error: ' || m);
  err := TRUE
END;

PUBLIC PROCEDURE fatal(m: STRING);
BEGIN (*declare error and stop*)
  error(m);
  STOP
END;

PUBLIC PROCEDURE assume(c: BOOLEAN; m: STRING);
BEGIN (*if condition not true, declare fatal error with message*)
  IF NOT c THEN fatal(m)
END;

PUBLIC PROCEDURE opncmd;
BEGIN (*open command channel*)
  saved_input := INPUT;
  saved_output := OUTPUT;
  IF cmdfil <> '' THEN BEGIN (*use pending commands*)
    RESET(INPUT, cmdfil);
    IF EOF(INPUT) THEN BEGIN (*command file empty*)
      SCRATCH(INPUT);
      cmdfil := ''
    END
    ELSE REWRITE(OUTPUT, cmdfil)
  END;
  IF cmdfil = '' THEN BEGIN (*no command file, use terminal*)
    INPUT := TTY;
    OUTPUT := TTYOUTPUT
  END
END;

PUBLIC PROCEDURE clscmd;
VAR l: STRING;
BEGIN (*close command channel*)
  IF cmdfil <> '' THEN BEGIN (*save pending commands*)
    IF rdlin(l) THEN BEGIN (*command file not empty*)
      wrlin(l);
      WHILE rdlin(l) DO wrlin(l);
    END
    ELSE cmdfil := '';
    CLOSE(INPUT);
    IF cmdfil = '' THEN SCRATCH(OUTPUT)
    ELSE CLOSE(OUTPUT)
  END;
  OUTPUT := saved_output;
  INPUT := saved_input
END;

PUBLIC PROCEDURE dpystr(s: STRING);
BEGIN (*display string if no commands pending*)
  IF cmdfil = '' THEN ttystr(s)
END;

PUBLIC PROCEDURE dpylin(l: STRING);
BEGIN (*display line of text if no commands pending*)
  IF cmdfil = '' THEN ttylin(l)
END;

PUBLIC PROCEDURE dpytab(t: ARRAY[1 .. *] OF STRING);
VAR i: INTEGER;
BEGIN (*display table of words*)
  FOR i := 1 TO UPPERBOUND(t) DO dpylin(t[i])
END;

PUBLIC FUNCTION abbrev(s1, s2: STRING): BOOLEAN;
VAR i, p1, l1: INTEGER;
BEGIN (*return TRUE if s1 is an abbreviation of s2*)
  p1 := VERIFY(s1, [' '], 1);
  l1 := SEARCH(SUBSTR(s1 || ' ', p1), [' ']) - 1;
  IF l1 <= LENGTH(s2) THEN BEGIN (*compare substrings*)
    abbrev := UPPERCASE(SUBSTR(s1, p1, l1)) = UPPERCASE(SUBSTR(s2, 1, l1))
  END
  ELSE abbrev := FALSE
END;

PUBLIC FUNCTION lkpwrd(w: STRING; t: ARRAY[1 .. *] OF STRING): INTEGER;
VAR i: INTEGER;
BEGIN (*lookup word in table, return index of word in table or zero*)
  lkpwrd := 0;
  FOR i := 1 TO UPPERBOUND(t) DO EXIT IF abbrev(w, t[i]) DO lkpwrd := i
END;

PUBLIC PROCEDURE ask(q: STRING; VAR r: STRING);
BEGIN (*ask a question, get response*)
  opncmd;
  dpylin('');
  dpystr(q);
  dpystr(' ');
  r := '';
  IF NOT rdlin(r) THEN r := '';
  clscmd
END;

PUBLIC FUNCTION asktab(q: STRING; t: ARRAY[1 .. *] OF STRING): INTEGER;
VAR i: INTEGER; r: STRING;
BEGIN (*ask a question, return index of response word in table*)
  REPEAT
    ask(q, r);
    i := lkpwrd(r, t);
    IF i > 0 THEN asktab := i
    ELSE BEGIN
      assume(cmdfil = '', 'invalid response in command file: ' || cmdfil);
      dpylin('Please respond with one of the following words:');
      dpytab(t)
    END
  UNTIL i > 0
END;

PUBLIC FUNCTION askyn(q: STRING): BOOLEAN;
BEGIN (*ask a question, return TRUE if 'YES', FALSE if 'NO'*)
  askyn := (asktab(q, ('YES', 'NO')) = 1);
END;

PUBLIC PROCEDURE lpton;
BEGIN (*connect OUTPUT to printer, VT102 to LA50*)
  wrchrs((27, 91, 53, 105)); lpt_is_on := TRUE
END;

PUBLIC PROCEDURE lptoff;
BEGIN (*disconnect OUTPUT from printer, VT102 to LA50*)
  wrchrs((27, 91, 52, 105)); lpt_is_on := TRUE
END;

PUBLIC PROCEDURE dffil(VAR nam: STRING; str, dfnam, dfext: STRING);
BEGIN (*default file name and extension*)
  IF str = '' THEN nam := dfnam ELSE nam := str;
  IF UPPERCASE(nam) = tty_nam THEN nam := tty_nam
  ELSE IF UPPERCASE(nam) = lpt_nam THEN nam := lpt_nam
  ELSE IF INDEX(nam, '.') = 0 THEN nam := nam || '.' || dfext;
  nam := UPPERCASE(nam)
END;

PUBLIC PROCEDURE clsfil(VAR f: TEXT);
BEGIN (*close a file, keep terminal open*)
  IF (f = OUTPUT ) AND lpt_is_on THEN lptoff;
  IF (f <> NILF) AND (f <> TTY) AND (f <> TTYOUTPUT) THEN CLOSE(f);
  f := NILF
END;

PUBLIC PROCEDURE openio(i, o: STRING);
VAR ifil, ofil: STRING;
BEGIN (*open standard INPUT and OUTPUT files*)
  clsfil(INPUT); clsfil(OUTPUT); ifil := i; ofil := o; err := FALSE;
  dffil(ifil, i, dfinam, dfiext);
  dffil(ofil, o, dfonam, dfoext);
  IF ifil = tty_nam THEN INPUT := TTY
  ELSE IF ifil = lpt_nam THEN error('LPT: is not an input device')
  ELSE BEGIN (*try to open input file*)
    RESET(INPUT, ifil);
    IF IOSTATUS <> IO_OK THEN BEGIN (*input file error*)
      INPUT := NILF;
      error('bad input file: ' || ifil)
    END
  END;
  IF NOT err THEN BEGIN (*input file is open*)
    IF ofil = tty_nam THEN OUTPUT := TTYOUTPUT
    ELSE IF ofil = lpt_nam THEN BEGIN OUTPUT := TTYOUTPUT; lpton END
    ELSE BEGIN (*try to open output file*)
      REWRITE(OUTPUT, ofil);
      IF IOSTATUS <> IO_OK THEN BEGIN (*output file error*)
	OUTPUT := NILF;
	clsfil(INPUT);
        error('bad output file: ' || ofil)
      END
    END
  END
END;

PUBLIC FUNCTION rdyio(p: STRING): BOOLEAN;
VAR done, io_open: BOOLEAN; opos, olen, ipos, ilen: INTEGER;
BEGIN (*try to open I/O from files specified in command line*)
  done := FALSE; io_open := FALSE;
  REPEAT
    cmdlin := ''; ask(p, cmdlin);
    IF cmdlin = '' THEN done := TRUE
    ELSE IF abbrev(cmdlin, info_flag) THEN BEGIN
      dpylin('Please respond with I/O file specifications:');
      dpylin('<output file> ' || file_flag || ' <input file>')
    END
    ELSE BEGIN (*parse file names from command line*)
      opos := 1; olen := INDEX(cmdlin, file_flag, 0) - opos;
      IF olen < 0 THEN ipos := 1
      ELSE ipos := olen + 2;
      ilen := INDEX(cmdlin || ' ', ' ') - ipos;
      IF olen < 0 THEN olen := 0;
      IF ilen < 0 THEN ilen := 0;
      openio(SUBSTR(cmdlin, ipos, ilen), SUBSTR(cmdlin, opos, olen));
      io_open := NOT err;
      IF (ipos + ilen + 1) >= LENGTH(cmdlin) THEN cmdlin := ''
      ELSE cmdlin := SUBSTR(cmdlin, ipos + ilen)
    END
  UNTIL done OR io_open;
  rdyio := io_open AND NOT done
END;

PUBLIC PROCEDURE start(b: STRING);
BEGIN (*start a program with banner*)
  opntty; opncmd; clscmd; dpylin(b);
  INPUT := TTY; OUTPUT := TTYOUTPUT
END;

PUBLIC PROCEDURE wrglob(f: STRING; VAR a, b: BOOLEAN);
(*save global variables on heap, save heap in file*)
VAR h: heap_area;
  PROCEDURE hpput(a, b: INTEGER; VAR h: heap_area);
  (*copy data from address a to b into new heap area*)
  VAR i: INTEGER; d: maximum_area;
  BEGIN
    NEW(h, b - a); d := PTR(a); FOR i := 1 TO b - a DO h^[i] := d^[i]
  END;
BEGIN
  CLOSE; hpput(ORD(ADDRESS(a)), ORD(ADDRESS(b)), h);
  assume(hpwrite(f, h), 'unable to save heap to ' || f);
  DISPOSE(h)
END;

PUBLIC PROCEDURE rdglob(f: STRING; d: BOOLEAN; VAR a, b: BOOLEAN);
(*restore heap from file, maybe delete file, load global variables from heap*)
VAR h: heap_area;
  PROCEDURE hpget(a, b: INTEGER; h: heap_area);
  (*copy data from heap area into address a to b, dispose heap area*)
  VAR i: INTEGER; d: maximum_area;
  BEGIN
    assume((h <> NIL), 'lost root pointer to heap');
    d := PTR(a); FOR i := 1 TO b - a DO d^[i] := h^[i]; DISPOSE(h)
  END;
BEGIN
  CLOSE; h := hpread(f, d);
  assume((h <> NIL), 'lost root pointer to heap read from ' || f);
  hpget(ORD(ADDRESS(a)), ORD(ADDRESS(b)), h);
END.
    