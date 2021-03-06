PROGRAM print_la50 OPTIONS SPECIAL(COERCIONS, WORD);

CONST
eof_flag = '.EOF';
file_flag = '=';
info_flag = '?';
tty_nam = 'TTY:';
lpt_nam = 'LPT:';

VAR
cmdfil: STRING := '';
cmdlin: STRING := '';
err: BOOLEAN := FALSE;
dfinam: STRING := tty_nam;
dfonam: STRING := tty_nam;
dfiext: STRING := 'TXT';
dfoext: STRING := 'TXT';

VAR
saved_input: TEXT := NILF;
saved_output: TEXT := NILF;
lpt_is_on: BOOLEAN := FALSE;

PROCEDURE opntty;
BEGIN
  OPEN(TTY);
  REWRITE(TTYOUTPUT)
END;

PROCEDURE ttystr(s: STRING);
BEGIN
  WRITE(TTYOUTPUT, s);
  BREAK(TTYOUTPUT)
END;

PROCEDURE ttylin(l: STRING);
BEGIN
  WRITELN(TTYOUTPUT, l);
  BREAK(TTYOUTPUT)
END;

PROCEDURE wrstr(s: STRING);
BEGIN
  IF OUTPUT = TTYOUTPUT THEN ttystr(s) ELSE WRITE(OUTPUT, s)
END;

PROCEDURE wrlin(l: STRING);
BEGIN
  IF OUTPUT = TTYOUTPUT THEN ttylin(l) ELSE WRITELN(OUTPUT, l)
END;

PROCEDURE wrchrs(c: ARRAY[1 .. *] OF INTEGER);
VAR i: INTEGER;
BEGIN
  FOR i := 1 TO UPPERBOUND(c) DO wrstr(CHR(c[i]))
END;

FUNCTION rdlin(VAR l: STRING): BOOLEAN;
BEGIN
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

PROCEDURE error(m: STRING);
BEGIN
  opntty;
  ttylin('error: ' || m);
  err := TRUE
END;

PROCEDURE fatal(m: STRING);
BEGIN
  error(m);
  STOP
END;

PROCEDURE assume(c: BOOLEAN; m: STRING);
BEGIN
  IF NOT c THEN fatal(m)
END;

PROCEDURE opncmd;
BEGIN
  saved_input := INPUT;
  saved_output := OUTPUT;
  IF cmdfil <> '' THEN BEGIN
    RESET(INPUT, cmdfil);
    IF EOF(INPUT) THEN BEGIN
      SCRATCH(INPUT);
      cmdfil := ''
    END
    ELSE REWRITE(OUTPUT, cmdfil)
  END;
  IF cmdfil = '' THEN BEGIN
    INPUT := TTY;
    OUTPUT := TTYOUTPUT
  END
END;

PROCEDURE clscmd;
VAR l: STRING;
BEGIN
  IF cmdfil <> '' THEN BEGIN
    IF rdlin(l) THEN BEGIN
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

PROCEDURE dpystr(s: STRING);
BEGIN
  IF cmdfil = '' THEN ttystr(s)
END;

PROCEDURE dpylin(l: STRING);
BEGIN
  IF cmdfil = '' THEN ttylin(l)
END;

PROCEDURE dpytab(t: ARRAY[1 .. *] OF STRING);
VAR i: INTEGER;
BEGIN
  FOR i := 1 TO UPPERBOUND(t) DO dpylin(t[i])
END;

FUNCTION abbrev(s1, s2: STRING): BOOLEAN;
VAR i, p1, l1: INTEGER;
BEGIN
  p1 := VERIFY(s1, [' '], 1);
  l1 := SEARCH(SUBSTR(s1 || ' ', p1), [' ']) - 1;
  IF l1 <= LENGTH(s2) THEN BEGIN
    abbrev := UPPERCASE(SUBSTR(s1, p1, l1)) = UPPERCASE(SUBSTR(s2, 1, l1))
  END
  ELSE abbrev := FALSE
END;

FUNCTION lkpwrd(w: STRING; t: ARRAY[1 .. *] OF STRING): INTEGER;
VAR i: INTEGER;
BEGIN
  lkpwrd := 0;
  FOR i := 1 TO UPPERBOUND(t) DO EXIT IF abbrev(w, t[i]) DO lkpwrd := i
END;

PROCEDURE ask(q: STRING; VAR r: STRING);
BEGIN
  opncmd;
  dpylin('');
  dpystr(q);
  dpystr(' ');
  r := '';
  IF NOT rdlin(r) THEN r := '';
  clscmd
END;

FUNCTION asktab(q: STRING; t: ARRAY[1 .. *] OF STRING): INTEGER;
VAR i: INTEGER; r: STRING;
BEGIN
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

FUNCTION askyn(q: STRING): BOOLEAN;
BEGIN
  askyn := (asktab(q, ('YES', 'NO')) = 1);
END;

PROCEDURE lpton;
BEGIN
  wrchrs((27, 91, 53, 105)); lpt_is_on := TRUE
END;

PROCEDURE lptoff;
BEGIN
  wrchrs((27, 91, 52, 105)); lpt_is_on := FALSE
END;

PROCEDURE clsfil(VAR f: TEXT);
BEGIN
  IF (f = OUTPUT ) AND lpt_is_on THEN lptoff;
  IF (f <> NILF) AND (f <> TTY) AND (f <> TTYOUTPUT) THEN CLOSE(f);
  f := NILF
END;

PROCEDURE clsio;
BEGIN
  clsfil(INPUT); clsfil(OUTPUT)
END;

PROCEDURE opnio(i, o: STRING);
VAR ifil, ofil: STRING;
PROCEDURE dffil(VAR nam: STRING; str, dfnam, dfext: STRING);
BEGIN
  IF str = '' THEN nam := dfnam ELSE nam := str;
  IF UPPERCASE(nam) = tty_nam THEN nam := tty_nam
  ELSE IF UPPERCASE(nam) = lpt_nam THEN nam := lpt_nam
  ELSE IF INDEX(nam, '.') = 0 THEN nam := nam || '.' || dfext;
  nam := UPPERCASE(nam)
END;
BEGIN
  clsio;
  ifil := i; ofil := o; err := FALSE;
  dffil(ifil, i, dfinam, dfiext);
  dffil(ofil, o, dfonam, dfoext);
  IF ifil = tty_nam THEN INPUT := TTY
  ELSE IF ifil = lpt_nam THEN error('LPT: is not an input device')
  ELSE BEGIN
    RESET(INPUT, ifil);
    IF IOSTATUS <> IO_OK THEN BEGIN
      INPUT := NILF;
      error('bad input file: ' || ifil)
    END
  END;
  IF NOT err THEN BEGIN
    IF ofil = tty_nam THEN OUTPUT := TTYOUTPUT
    ELSE IF ofil = lpt_nam THEN BEGIN OUTPUT := TTYOUTPUT; lpton END
    ELSE BEGIN
      REWRITE(OUTPUT, ofil);
      IF IOSTATUS <> IO_OK THEN BEGIN
	OUTPUT := NILF;
	clsfil(INPUT);
        error('bad output file: ' || ofil)
      END
    END
  END
END;

PROCEDURE start(b: STRING);
BEGIN
  opntty; opncmd; clscmd; dpylin(b);
  INPUT := TTY; OUTPUT := TTYOUTPUT
END;

PROCEDURE print;
VAR line: STRING[132]; name: STRING;
BEGIN
  REPEAT
    ask('Enter name of file to print: ', name);
    err := FALSE; line := '';
    opnio(name, 'LPT:');
    IF NOT err THEN WHILE rdlin(line) DO wrlin(line);
    clsio
  UNTIL name = ''
END;

PROCEDURE la50;
VAR
horizontal_pitch: INTEGER;
vertical_pitch: INTEGER;
density: INTEGER;
page_length: INTEGER;
BEGIN
  horizontal_pitch := 1;
  vertical_pitch := 1;
  density := 1;
  page_length := 66;
  IF NOT askyn('Standard settings?') THEN BEGIN
    CASE asktab('Horizontal:', ('NORMAL','40','48','66','80','96','132')) OF
      1: horizontal_pitch := horizontal_pitch;
      2: horizontal_pitch := 5;
      3: horizontal_pitch := 6;
      4: horizontal_pitch := 8;
      5: horizontal_pitch := 1;
      6: horizontal_pitch := 2;
      7: horizontal_pitch := 4;
    END;
    CASE asktab('Vertical:', ('NORMAL', '22','33','44','66','88','132')) OF
      1: vertical_pitch := vertical_pitch;
      2: vertical_pitch := 4;
      3: vertical_pitch := 5;
      4: vertical_pitch := 6;
      5: vertical_pitch := 1;
      6: vertical_pitch := 2;
      7: vertical_pitch := 3;
    END;
    CASE asktab('Density:', ('NORMAL', 'LOW', 'HIGH')) OF
      1: density := density;
      2: density := 1;
      3: density := 2;
    END;
  END;
  CASE vertical_pitch OF
    1: page_length := 66;
    2: page_length := 88;
    3: page_length := 132;
    4: page_length := 22;
    5: page_length := 33;
    6: page_length := 44;
  END;
  lpton;
  wrchrs((0, 27, 91, horizontal_pitch + ORD('0'), 119));
  wrchrs((0, 27, 91, vertical_pitch + ORD('0'), 122));
  wrchrs((0, 27, 91, page_length, 116));
  wrchrs((0, 27, 91, density + ORD('0'), 34, 122));
  lptoff;
END;

BEGIN
  start('TYM-Pascal LA50 Printer Controller');
  la50;
  print;
END.
    