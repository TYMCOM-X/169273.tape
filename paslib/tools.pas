$WIDTH=100
$LENGTH=55
$TITLE tools.pas, last modified 3/16/83, zw

MODULE tools;
  (*a demonstration of usefull programming tools*)

$PAGE tools declarations

$INCLUDE TOOLS.TYP

(*command line buffer*)
CONST cmdlen = 80; (*length of command buffer*)
VAR cmdline: PACKED ARRAY [1 .. cmdlen] OF CHAR; (*command buffer*)
VAR cmdcsr: 0 .. cmdlen + 1; (*cursor into command buffer*)

(*input and output buffers*)
CONST buflen = 80; (*buffer length*)
VAR ibuf: PACKED ARRAY [1 .. buflen] OF CHAR; (*input buffer*)
VAR ibufcsr: 0 .. buflen + 1 := 0; (*pts to next char to get*)
VAR ibufcnt: 0 .. buflen; (*count of characters in input buffer*)
VAR obuf: PACKED ARRAY [1 .. buflen] OF CHAR; (*output buffer*)
VAR obufcsr: 0 .. buflen + 1 := 0; (*pts to last char put*)

$PAGE openio, closeio

FUNCTION openio(inputname, outputname: FILE_NAME): BOOLEAN;
  (*try to open the standard INPUT and OUTPUT files*)
  BEGIN
    BEGIN (*try to open input file*)
      IF UPPERCASE(inputname) = 'TTY:' THEN (*input from terminal*)
        INPUT := TTY (*TTY was previously opened*)
      ELSE (*input from file*)
        RESET(INPUT, inputname, [RETRY])
      END;
    IF IOSTATUS <> IO_OK THEN BEGIN (*input file error*)
      WRITELN(TTY, '% Bad input file: ' || inputname);
      openio := FALSE (*flag failure*)
      END
    ELSE BEGIN (*input file established*)
      BEGIN (*try to open output file*)
        IF UPPERCASE(outputname) = 'TTY:' THEN (*output to terminal*)
          OUTPUT := TTYOUTPUT (*previously opened*)
        ELSE (*output to file*)
          REWRITE(OUTPUT, outputname, [RETRY])
        END;
      IF IOSTATUS <> IO_OK THEN BEGIN (*output file error*)
        IF INPUT <> TTY THEN CLOSE(INPUT); (*input file was already opened*)
        WRITELN(TTY, '% Bad output file: ' || outputname);
        openio := FALSE (*flag failure*)
        END
      ELSE (*output file established*)
        openio := TRUE (*flag success*)
      END
    END;

PROCEDURE closeio;
  (*close the standard INPUT and OUTPUT files*)
  BEGIN
    IF (INPUT <> NILF) ANDIF (INPUT <> TTY) THEN (*don't close tty:*)
      CLOSE(INPUT); (*close input file*)
    IF (OUTPUT <> NILF) ANDIF (OUTPUT <> TTYOUTPUT) THEN (*don't close tty:*)
      CLOSE(OUTPUT) (*close output file*)
    END;

$PAGE scanready, scannull, scanchar

FUNCTION scanready: BOOLEAN;
  (*skip blanks and tabs, TRUE if not at the end of the command line*)
  BEGIN
    scanready := (cmdcsr >= 1) ANDIF (cmdcsr <= cmdlen);
    IF scanready THEN BEGIN (*cursor not at end of line*)
      scanready := FALSE; (*assume failure*)
      FOR cmdcsr := cmdcsr TO cmdlen DO (*skip blanks and tabs*)
        EXIT IF NOT (cmdline[cmdcsr] IN [blank, tab]) DO scanready := TRUE
      (*if TRUE then cursor at non-blank/tab char in line*)
      (*if FALSE then cursor is at end of line*)
      END
    (*if TRUE then cursor at non-blank/tab char in line*)
    (*if FALSE then cursor not defined*)
    END;

FUNCTION scannull: BOOLEAN;
  (*see if command line is empty, skip blanks and tabs*)
  BEGIN
    scannull := NOT scanready
    (*if FALSE, cursor at non-blank/tab character in line*)
    (*if TRUE, cursor not defined*)
    END;

FUNCTION scanchar(c: CHAR): BOOLEAN;
  (*try to scan the specified character from the command line*)
  VAR savecsr: INTEGER; (*a place to save the cursor*)
  BEGIN
    savecsr := cmdcsr; (*save the cursor in case of failure*)
    scanchar := scanready ANDIF (cmdline[cmdcsr] IN [c]);
    (*if TRUE then cursor at char c in line*)
    (*if FALSE, know nothing about cursor*)
    IF NOT scanchar THEN (*failure*)
      cmdcsr := savecsr (*restore cursor to original state*)
    ELSE (*success*)
      cmdcsr := cmdcsr + 1 (*advance cursor to next char in line*)
    END;

$PAGE scanname

FUNCTION scanname(VAR filename: FILE_NAME): BOOLEAN;
  (*try to scan a file name from the command line*)
  CONST alpha = ['A' .. 'Z', 'a' .. 'z'];
  CONST numeric = ['0' .. '9'];
  CONST nameset = alpha + numeric + [':', '.', '(', ')', '[', ']'];
  VAR savecsr: INTEGER; (*a place to save the cursor*)
  VAR namecsr, namelen: INTEGER; (*defines name string*)
  BEGIN (*does not handle case where last char in line is name char*)
    savecsr := cmdcsr; (*save the cursor in case of failure*)
    scanname := scanready;
    IF scanname THEN BEGIN (*cursor at non-blank/tab char in line*)
      namecsr := cmdcsr; (*assume start of name string*)
      FOR cmdcsr := cmdcsr TO cmdlen DO (*skip over valid name chars*)
        EXIT IF NOT (cmdline[cmdcsr] IN nameset);
      (*cursor at char past last name char OR at end of line*)
      namelen := cmdcsr - namecsr; (*calculate length of name string*)
      scanname := (namelen > 0) ANDIF (namelen <= UPPERBOUND(filename))
      (*if TRUE then a non-null name string exists*)
      (*if FALSE then name string null or too long*)
      END;
    IF NOT scanname THEN (*failure*)
      cmdcsr := savecsr (*restore cursor to original state*)
    ELSE (*success, return name string*)
      filename := UPPERCASE(SUBSTR(cmdline, namecsr, namelen))
    END;

$PAGE setup, getio

PUBLIC PROCEDURE setup(progname: STRING[*]);
  (*set up TTY, TTYOUTPUT, INPUT and OUTPUT, identify program*)
  BEGIN
    OPEN(TTY); (*open terminal for input*)
    INPUT := TTY; (*initial standard input is tty:*)
    REWRITE(TTYOUTPUT); (*open terminal for output*)
    OUTPUT := TTYOUTPUT; (*initial standard output is tty:*)
    WRITELN(TTY, progname); (*identify program*)
    WRITELN(TTY)
    END;

PUBLIC FUNCTION getio: BOOLEAN;
  (*get the standard INPUT and OUTPUT files*)
  VAR inputname, outputname: FILE_NAME;
  BEGIN
    closeio; (*close previously opened standard INPUT and OUTPUT*)
    REPEAT
      LOOP (*get a command line*)
        FOR cmdcsr := 1 TO cmdlen DO cmdline[cmdcsr] := blank; (*empty line*)
        WRITE(TTY, '*'); (*prompt for command line input*)
        BREAK(TTY); (*flush prompt to terminal*)
        READLN(TTY); (*wait for user input*)
        READ(TTY, cmdline); (*get line user typed*)
        cmdcsr := 1; (*point cursor to first char in line*)
        EXIT IF NOT scanchar('?'); (*"?" means help*)
        WRITELN(TTY, '<output file> = <input file>, <arguments>')
        END;
      IF scannull THEN (*null command -- getio fails*)
        getio := FALSE
      ELSE BEGIN (*not null command, get file specifications*)
        getio := TRUE;
        IF NOT scanname(outputname) THEN (*default is terminal*)
          outputname := 'TTY:';
        IF scanchar('=') THEN BEGIN (*name is actually for output file*)
          IF NOT scanname(inputname) THEN (*default is terminal*)
            inputname := 'TTY:'
          END
        ELSE BEGIN (*only one file name given*)
          inputname := outputname; (*name is actually for input file*)
          outputname := 'TTY:' (*default is terminal*)
          END;
        IF scanchar(',') THEN (*accept ','*)
        END
      UNTIL (NOT getio) ORIF openio(inputname, outputname)
    END;

$PAGE getarg

PUBLIC FUNCTION getarg(n: INTEGER;
  VAR arg: PACKED ARRAY [1 .. *] OF CHAR; maxsize: INTEGER): INTEGER;
  (*get the nth argument from the command line, return length of arg*)
  CONST delchrs = [blank, tab, newline, newpage]; (*delimiter characters*)
  VAR c: CHAR; (*a character*)
  VAR wc: INTEGER; (*word counter*)
  VAR inword: BOOLEAN; (*TRUE if in a word, arguments are words*)
  VAR savecsr: INTEGER; (*a place to save the command line cursor*)
  FUNCTION getc: BOOLEAN;
    (*get the next char from the command line*)
    BEGIN
      getc := (cmdcsr > 0) ANDIF (cmdcsr <= cmdlen); (*still in line?*)
      IF getc THEN BEGIN c := cmdline[cmdcsr]; cmdcsr := cmdcsr + 1 END
      END;
  BEGIN
writeln(tty, '"', substr(cmdline, cmdcsr, 20), '"');
    savecsr := cmdcsr;
    inword := FALSE; wc := 0;
    WHILE (wc < n) ANDIF getc DO BEGIN (*skip prior arguments*)
      IF c IN delchrs THEN inword := FALSE
      ELSE IF NOT inword THEN BEGIN inword := TRUE; wc := wc + 1 END
      END;
    IF (n > 0) ANDIF (wc = n) THEN BEGIN (*found nth argument*)
      getarg := 1;
      WHILE NOT (c IN delchrs) DO BEGIN (*get argument chars*)
        IF getarg < maxsize THEN BEGIN (*accept up to maxsize-1 chars*)
          arg[getarg] := c; getarg := getarg + 1
          END;
        EXIT IF NOT getc
        END
      END
    ELSE getarg := 1;
    arg[getarg] := eos; getarg := getarg - 1; (*end of argument string*)
    cmdcsr := savecsr (*reset command line cursor*)
    END;

$PAGE getb, getc, getl

PUBLIC FUNCTION getb: BOOLEAN;
  (*try to get buffer from input device, FALSE if end of file*)
  BEGIN
    IF lasteopage THEN BEGIN
      ibufcnt := 1; ibuf[1] := newpage; lasteopage := FALSE
      END
    ELSE IF lasteoln THEN
      ibufcnt := 1; ibuf[1] := newline; laseoln := FALSE;
      END
    ELSE BEGIN
      IF ibufempty THEN GET(INPUT);
      READ(INPUT, ibuf);
      lasteoln := EOLN(INPUT);
      lasteopage := EOPAGE(INPUT);
      IF lasteoln THEN ibuf := CURSOR(INPUT)
      ELSE ibufcnt := buflen
    END;

PUBLIC FUNCTION getc(VAR c: CHAR): BOOLEAN;
  (*try to get next character from input, FALSE if end of file*)
  BEGIN
    getc := (ibufcsr > 0) ANDIF (ibufcsr <= ibufcnt);
    getc := NOT getc ANDIF getb;
    IF getc THEN BEGIN
      c := ibuf[ibufcsr]; ibufcsr := ibufcsr + 1
      END
    ELSE ibufcsr := 0
    END;

PUBLIC FUNCTION getl(VAR l: PACKED ARRAY [1 .. *] OF CHAR;
  len: INTEGER): INTEGER;
  (*get a line of characters from the standard input*)
  VAR c: CHAR;
  BEGIN
    getl := 1; c := eos;
    WHILE (getl <= (len - 2)) ANDIF getc(c) ANDIF (c <> newline) DO BEGIN
      l[getl] := c; getl := getl + 1;
      END;
    IF c <> eos THEN l[getl] := newline ELSE getl := getl - 1;
    l[getl + 1] := eos;
    END;

$PAGE putb, putc, putl

PUBLIC PROCEDURE putb;
  (*send buffer of characters to output device*)
  BEGIN
    IF obufcsr > 0 THEN WRITE(SUBSTR(obuf, 1, obufcsr));
    obufcsr := 0;
    BREAK
    END;

PUBLIC PROCEDURE putc(c: CHAR);
  (*put character to output*)
  BEGIN
    IF c = newline THEN BEGIN putb; WRITELN; BREAK END
    ELSE BEGIN
      IF obufcsr >= buflen THEN putb;
      obufcsr := obufcsr + 1; obuf[obufcsr] := c
      END
    END;

PUBLIC PROCEDURE putl(l: PACKED ARRAY [1 .. *] OF CHAR);
  (*put line to standard output*)
  VAR i: INTEGER; (*an index*)
  BEGIN
    WHILE l[i] <> eos DO BEGIN
      putc(l[i]); i := i + 1
      END;
    putc(newline)
    END;

$PAGE putdec

PUBLIC PROCEDURE putdec(n, w: INTEGER);
  (*output number n in field of minimum width w*)
  BEGIN
    WRITE(n: w)
    END;

PUBLIC FUNCTION strnum(s: STRING[*]): INTEGER;
  (*convert a string of digits to an integer*)
  BEGIN
    GETSTRING(s, strnum)
    EXCEPTION
      IO_ERROR: strnum := 0
    END.

   