$WIDTH=100
$LENGTH=55
$TITLE star.pas, last modified 6/8/83, zw
MODULE star;
(*star command processor -- sets up standard INPUT and OUTPUT*)

(*
The STAR command processor solicits file names for standard INPUT
and OUTPUT.  In addition, there are provisions for accessing user
documentation and command arguments.  STAR is intended for filter
applications where some data from some input source is operated on
and then sent to some output sink.  Operations may vary according 
to the command arguments given along with the input and output file
names.  The STAR module defines three public functions, "STAR", "ARG"
and "READLIN".

EXTERNAL FUNCTION star(prognam: STRING[*]): BOOLEAN;

Function STAR takes a string parameter, the program name.  This is used in
the initial identification banner.  The first six characters are combined
with the extension ".HLP" to specify the help file which is displayed if the
users response is /HELP.  TTY is opened.  The function returns TRUE if file
names have been parsed and INPUT and OUTPUT file opened.  It returns FALSE
if the program is to exit.  STAR will accept indirect command files. The
default extension for a command file is ".CMD".  Up to three files may be
nested.  Setting up another command file as the last item in a command file
does not count as nesting.


EXTERNAL FUNCTION arg(n: INTEGER; VAR val: STRING[*]): BOOLEAN;

Function arg gets the nth argument.  It returns TRUE if there is an nth
argument, FALSE otherwise.  An arguments is the text which follows a "/".

EXTERNAL FUNCTION readlin(VAR lin: STRING[*]): BOOLEAN;

Function "READLIN" reads a line of text from the standard INPUT.  It
returns FALSE if end of file.  Note that it correctly handles TTY and
allows for an end-of-file flag ".EOF".
*)

$PAGE sample user program

(*
the following is an example of a simple application of STAR

PROGRAM copy;
EXTERNAL FUNCTION star(prognam: STRING[*]): BOOLEAN;
EXTERNAL FUNCTION readlin(VAR lin: STRING[*]): BOOLEAN;
VAR lin: STRING[132];
BEGIN
  WHILE star('COPY') DO WHILE readlin(lin) DO WRITELN(lin)
  END.

In general, a user program will have the "WHILE STAR(...) DO ..." construct
as the primary statement in its main block.  Note how READLIN handles TTY
input.

In general a main block will look like:

BEGIN
  set_up_prog;
  WHILE star('PROG') DO BEGIN
    read_args_to_set_parameters;
    WHILE read_data(data) DO BEGIN
      process_data(data);
      write_data(data)
      END
    END;
  clean_up_prog
  END.

Please note that STAR is responsible for opening and closing all of the
standard files TTY, TTYOUTPUT, INPUT and OUTPUT.  The user should never
adjust these variables directly.
*)

$PAGE barf, closefil, match, switch

PROCEDURE barf(msg: STRING[*]);
BEGIN (*display message and STOP*)
  REWRITE(TTYOUTPUT); WRITELN(TTYOUTPUT, msg); BREAK(TTYOUTPUT); STOP
  END;

PROCEDURE closefil(VAR fil: TEXT);
BEGIN (*close a file if it is not TTY, set to NILF in any case*)
  IF (fil <> TTY) ANDIF (fil <> TTYOUTPUT) THEN CLOSE(fil);
  fil := NILF
  END;

FUNCTION match(s1, s2: STRING[*]): BOOLEAN;
VAR i: INTEGER;
BEGIN (*return TRUE if s1 is equal to or an abbreviation of s2*)
  match := LENGTH(s1) <= LENGTH(s2);
  IF match THEN FOR i := 1 TO LENGTH(s2) DO BEGIN
    EXIT IF i > LENGTH(s1);
    EXIT IF s1[i] <> s2[i] DO match := FALSE
    END
  END;
    
FUNCTION switch(lin, key: STRING[*]): BOOLEAN;
BEGIN (*return TRUE if switch in line matches key*)
  switch := (lin <> '') ANDIF (lin[1] = '/');
  IF switch THEN switch := match(UPPERCASE(SUBSTR(lin, 2)), UPPERCASE(key))
  END;

$PAGE stksiz, stk, stkptr, pushfil, popfil, nulstk

CONST stksiz = 5; (*maximum number of nested command files + 2*)
VAR stk: ARRAY [1 .. stksiz] OF TEXT; (*the file stack*)
VAR stkptr: 0 .. stksiz := 0; (*the file stack pointer*)

PROCEDURE pushfil(fil: TEXT);
BEGIN (*push file onto file stack*)
  IF stkptr < stksiz THEN BEGIN
    stkptr := stkptr + 1; stk[stkptr] := fil
    END
  ELSE barf('? File stack overflow.')
  END;

PROCEDURE popfil(VAR fil: TEXT);
BEGIN (*pop a file from the file stack*)
  IF stkptr > 0 THEN BEGIN
    fil := stk[stkptr]; stkptr := stkptr - 1
    END
  ELSE barf('? File stack underflow.')
  END;

FUNCTION nulstk: BOOLEAN;
BEGIN (*return TRUE if file stack is empty*)
  nulstk := stkptr < 1
  END;

$PAGE openio, getio

FUNCTION openio(onam, inam: STRING[*]): BOOLEAN;
BEGIN (*try to open OUTPUT and INPUT files*)
  openio := TRUE;
  pushfil(OUTPUT);
  IF (onam = '') ORIF (UPPERCASE(onam) = 'TTY:') THEN OUTPUT := TTYOUTPUT
  ELSE BEGIN
    REWRITE(OUTPUT, onam, [RETRY]); openio := IOSTATUS = IO_OK
    END;
  IF NOT openio THEN BEGIN
    popfil(OUTPUT); WRITELN('% Bad output file: ', onam)
    END
  ELSE BEGIN
    pushfil(INPUT);
    IF (inam = '') ORIF (UPPERCASE(inam) = 'TTY:') THEN INPUT := TTY
    ELSE BEGIN
      RESET(INPUT, inam, [RETRY]); openio := IOSTATUS = IO_OK
      END;
    IF NOT openio THEN BEGIN
      closefil(OUTPUT);
      popfil(INPUT); popfil(OUTPUT); WRITELN('% Bad input file: ', inam)
      END
    END
  END;

FUNCTION getio(lin: STRING[*]): BOOLEAN;
VAR opos, olen, ipos, ilen: INTEGER;
BEGIN (*get INPUT and OUTPUT files using names given in command line*)
  opos := 1;
  olen := INDEX(lin, '=', 0) - opos;
  IF olen < 0 THEN ipos := 1 ELSE ipos := olen + 2;
  ilen := INDEX(lin, '/', LENGTH(lin) + 1) - ipos;
  IF olen < 0 THEN olen := 0;
  IF ilen < 0 THEN ilen := 0;
  getio := openio(SUBSTR(lin, opos, olen), SUBSTR(lin, ipos, ilen));
  END;

$PAGE readlin, getcmd, cmdfil, setupflag, setup

PUBLIC FUNCTION readlin(VAR lin: STRING[*]): BOOLEAN;
BEGIN (*try to read a line of text from INPUT, return FALSE if EOF*)
  readlin := NOT EOF; IF NOT readlin THEN RETURN;
  IF INPUT = TTY THEN BEGIN
    BREAK; READLN; READ(lin); readlin := NOT switch(lin, 'EOF')
    END
  ELSE READLN(lin)
  END;

PROCEDURE getcmd(prompt: STRING[*]; VAR command: STRING[*]);
BEGIN (*get a command from INPUT, prompt if TTY*)
  IF INPUT = TTY THEN WRITE(TTYOUTPUT, prompt);
  IF NOT readlin(command) THEN command := '';
  command := SUBSTR(command, VERIFY(command, [' ', CHR(9)], 1))
  END;

PROCEDURE cmdfil(filnam: STRING[*]);
BEGIN (*set up a command file*)
  pushfil(INPUT);
  IF (filnam = '') ORIF (UPPERCASE(filnam) = 'TTY:') THEN INPUT := TTY
  ELSE BEGIN
    RESET(INPUT, filnam, [RETRY]);
    IF IOSTATUS <> IO_OK THEN RESET(INPUT, filnam || '.CMD', [RETRY]);
    IF IOSTATUS <> IO_OK THEN BEGIN
      popfil(INPUT); WRITELN('% Bad indirect file: ', filnam)
      END
    END
  END;

VAR setupflag: BOOLEAN := TRUE; (*controls setup operation first time only*)

PROCEDURE setup(prognam: STRING[*]);
BEGIN (*open TTY, set up INPUT and OUTPUT*)
  IF setupflag THEN BEGIN
    OPEN(TTY); REWRITE(TTYOUTPUT); INPUT := TTY; OUTPUT := TTYOUTPUT;
    WRITELN(prognam); WRITELN; setupflag := FALSE
    END
  ELSE IF NOT nulstk THEN BEGIN
    closefil(INPUT); closefil(OUTPUT); popfil(INPUT); popfil(OUTPUT)
    END;
  END;

$PAGE dpyinfo, dpyhelp

PROCEDURE dpyinfo(prognam: STRING[*]);
BEGIN (*display information describing star command processor*)
  WRITELN('');
  WRITELN('Program: ', prognam);
  WRITELN('');
  WRITELN('Specify output file, input file and arguments by:');
  WRITELN('  *outfil.ext = infil.ext /arg1 /arg2 ...');
  WRITELN('or invoke a command file by:');
  WRITELN('  *@cmdfil.ext');
  WRITELN('or display this programs help file by:');
  WRITELN('  */help');
  WRITELN('or exit this program by entering just a carriage return or:');
  WRITELN('  */exit');
  WRITELN('');
  WRITELN('Specify TTY end of file with "/EOF".');
  WRITELN('');
  WRITELN('Note that default output and input files are the terminal.');
  WRITELN('For example,');
  WRITELN('  *filnam.ext');
  WRITELN('specifies input = "filnam.ext", output = "TTY:" and');
  WRITELN('  *=');
  WRITELN('specifies both input and output to be "TTY:".');
  WRITELN('')
  END;

PROCEDURE dpyhelp(prognam: STRING[*]);
VAR fil: TEXT; lin: STRING[79];
BEGIN (*display help file for program*)
  RESET(fil, prognam || '.HLP', [RETRY]);
  IF IOSTATUS <> IO_OK THEN WRITELN('% Can not access help file.')
  ELSE BEGIN
    WRITELN;
    WHILE NOT EOF(FIL) DO BEGIN READLN(fil, lin); WRITELN(lin) END;
    WRITELN;
    CLOSE(fil)
    END
  END;

$PAGE cmdlin, star, arg

VAR cmdlin: STRING[80] := ''; (*the command line*)

PUBLIC FUNCTION star(prognam: STRING[*]): BOOLEAN;
BEGIN (*star command processor, returns FALSE if program is to exit*)
  setup(prognam);
  REPEAT
    star := FALSE;
    WHILE EOF AND NOT nulstk DO popfil(INPUT);
    EXIT IF EOF AND nulstk DO cmdlin := '/EXIT';
    getcmd('*', cmdlin); IF cmdlin = '' THEN cmdlin := '/EXIT';
    WHILE EOF AND NOT nulstk DO popfil(INPUT);
    EXIT IF switch(cmdlin, 'EXIT') DO cmdlin := '/EXIT';
    IF cmdlin[1] = '?' THEN dpyinfo(prognam)
    ELSE IF cmdlin[1] = '@' THEN cmdfil(SUBSTR(cmdlin, 2))
    ELSE IF switch(cmdlin, 'HELP') THEN dpyhelp(prognam)
    ELSE star := getio(cmdlin);
    UNTIL star
  END;

PUBLIC FUNCTION arg(n: INTEGER; VAR val: STRING[*]): BOOLEAN;
VAR argnum, argpos, newargpos, arglen: INTEGER;
BEGIN (*try to get the nth argument from the command line*)
  argnum := 0; argpos := 0;
  REPEAT
    newargpos := argpos + INDEX(SUBSTR(cmdlin, argpos + 1), '/', 0);
    EXIT IF newargpos <= argpos DO argnum := 0;
    argnum := argnum + 1; argpos := newargpos
    UNTIL argnum >= n;
  arg := (n > 0) ANDIF (argnum = n);
  IF arg THEN BEGIN
    arglen := INDEX(SUBSTR(cmdlin, argpos + 1), '/', 0) - 1;
    IF arglen < 0 THEN arglen := LENGTH(cmdlin) - argpos;
    IF arglen > UPPERBOUND(val) THEN arglen := UPPERBOUND(val);
    val := SUBSTR(cmdlin, argpos + 1, arglen)
    END
  END.
   