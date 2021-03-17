$WIDTH=100
$LENGTH=55
$TITLE editor.pas, last modified 6/10/83, zw
PROGRAM editor;
  (*a simple text editor*)

(*
This is a simple text editor.  Following the pattern of a filter,
input is placed in a buffer where it may be altered by commands
issued during terminal interaction.  The buffer is then copied to
the output after any/all modifications have been made.  Only the
most primitive INSERT, DELETE, PRINT and FIND functions have been
implemented.

This program is a good example of the implementation and use of
a linked list structure.

The buffer structure is a linked list of lines.  The current line number
is kept for the line pointed to by the current pointer.  To enable
insertions, the pointer to the previous line is also kept.
*)

CONST
  linsiz = 132;

TYPE
  linrcd = RECORD lin: STRING[linsiz]; next: ^linrcd END;

VAR
  sav_input: TEXT;
  line: STRING[linsiz];
  curnum: INTEGER := 0;
  buffer, curptr, prevptr: ^linrcd := NIL;
  cmdlin: STRING[80];
  c: CHAR;
  done: BOOLEAN;

EXTERNAL FUNCTION star(prognam: STRING[*]): BOOLEAN;
EXTERNAL FUNCTION readlin(VAR lin: STRING[*]): BOOLEAN;

$PAGE insert, addlin, delete

PROCEDURE insert;
BEGIN (*insert line at curptr, advance to next line*)
  NEW(curptr); curptr^.lin := line;
  IF prevptr = NIL THEN BEGIN (*at line zero or one*)
    curptr^.next := buffer; curnum := 1; buffer := curptr
    END
  ELSE BEGIN (*at line past one*)
    curptr^.next := prevptr^.next; prevptr^.next := curptr
    END;
  prevptr := curptr; curptr := prevptr^.next; curnum := curnum + 1
  END;

PROCEDURE addlin;
BEGIN (*insert line after curptr, advance to it*)
  NEW(curptr); curptr^.lin := line;
  IF prevptr = NIL THEN BEGIN (*at line zero or one*)
    IF buffer = NIL THEN BEGIN (*at line zero*)
      buffer := curptr; curptr^.next := NIL; curnum := 1
      END
    ELSE BEGIN (*at line one*)
      curptr^.next := buffer^.next; buffer^.next := curptr;
      prevptr := buffer; curnum := 2
      END
    END
  ELSE BEGIN (*at line past one*)
    curptr^.next := prevptr^.next^.next; prevptr^.next^.next := curptr;
    prevptr := prevptr^.next; curnum := curnum + 1
    END
  END;

PROCEDURE delete;
BEGIN (*delete line at curptr*)
  IF prevptr = NIL THEN BEGIN (*at line zero or one*)
    IF buffer <> NIL THEN BEGIN (*at line one*)
      buffer := curptr^.next; DISPOSE(curptr); curptr := buffer
      END
    END
  ELSE BEGIN (*at line past one*)
    prevptr^.next := curptr^.next; DISPOSE(curptr); curptr := prevptr^.next
    END;
  IF curptr = NIL THEN BEGIN prevptr := NIL; curnum := 0; line := '' END
  ELSE line := curptr^.lin
  END;

$PAGE validlin, gototop, gotonext, gotolin

FUNCTION validlin: BOOLEAN;
BEGIN (*return true if current line is valid*)
  validlin := (curptr <> NIL) ANDIF (curnum > 0)
  END;

PROCEDURE gototop;
BEGIN (*goto the top of the buffer*)
  curptr := buffer; prevptr := NIL; curnum := 1;
  IF validlin THEN line := curptr^.lin ELSE curnum := 0
  END;

PROCEDURE gotonext;
BEGIN (*goto the next line*)
  IF NOT validlin THEN gototop
  ELSE BEGIN
    prevptr := curptr; curptr := prevptr^.next; curnum := curnum + 1;
    IF validlin THEN line := curptr^.lin ELSE line := ''
    END
  END;

PROCEDURE gotolin(n: INTEGER);
BEGIN (*try to goto nth line*)
  gototop;
  IF (n > curnum) ANDIF (curnum > 0) THEN BEGIN
    WHILE (curptr^.next <> NIL) ANDIF (curnum < n) DO gotonext
    END;
  END;

$PAGE ngetcmd, linnum, printlin, beep

PROCEDURE getcmd(VAR c: CHAR);
CONST cmdset = ['I', 'D', 'P', 'F', 'G', 'H', '?', 'E', 'Q'];
BEGIN (*get a command*)
  REPEAT
    WRITE(TTY, '|'); IF NOT readlin(cmdlin) THEN cmdlin := 'Q';
    cmdlin := UPPERCASE(SUBSTR(cmdlin, SEARCH(cmdlin, cmdset, 1)));
    IF cmdlin = '' THEN cmdlin := 'G';
    UNTIL (cmdlin <> '') ANDIF (cmdlin[1] IN cmdset);
  c := cmdlin[1]; cmdlin := SUBSTR(cmdlin, 2)
  END;

PROCEDURE linnum(num: INTEGER);
BEGIN (*formatted print of line number*)
  WRITE(TTY, num: 4, ' ')
  END;

PROCEDURE printlin(num: INTEGER);
BEGIN (*print current line*)
  linnum(num); WRITELN(TTY, line)
  END;

PROCEDURE beep;
BEGIN (*beep the terminal to signify an error*)
  WRITE(TTY, CHR(7))
  END;

$PAGE doinsert, dodelete, doprint

PROCEDURE doinsert;
VAR n: INTEGER; lin: STRING[linsiz];
BEGIN (*insert lines*)
  IF cmdlin = '' THEN BEGIN IF curnum < 1 THEN n := 1 ELSE n := curnum END
  ELSE GETSTRING(cmdlin, n);
  gotolin(n);
  IF n > curnum THEN BEGIN (*insert after last line*)
    IF n > curnum + 1 THEN beep;
    LOOP (*insert after current line*)
      linnum(curnum + 1); EXIT IF NOT readlin(line); addlin
      END
    END
  ELSE BEGIN
    IF n <> curnum THEN beep;
    LOOP (*insert at current line*)
      linnum(curnum); EXIT IF NOT readlin(line); insert
      END
    END
  END;

PROCEDURE dodelete;
VAR n, i: INTEGER;
BEGIN (*delete lines*)
  IF cmdlin = '' THEN n := 1 ELSE GETSTRING(cmdlin, n);
  n := curnum + n - 1;
  IF n < curnum THEN beep
  ELSE FOR i := curnum TO n DO BEGIN
    EXIT IF NOT validlin DO beep;
    printlin(i); delete
    END
  END;

PROCEDURE doprint;
VAR n, i: INTEGER;
BEGIN (*print lines*)
  IF cmdlin = '' THEN n := 1 ELSE GETSTRING(cmdlin, n);
  n := curnum + n - 1;
  IF (n < curnum) OR NOT validlin THEN beep
  ELSE BEGIN
    printlin(curnum);
    FOR i := curnum + 1 TO n DO BEGIN
      gotonext; EXIT IF NOT validlin DO beep;
      printlin(i)
      END
    END
  END;

$PAGE found, dofind

FUNCTION found(s1, s2: STRING[*]): BOOLEAN;
VAR i: INTEGER;
BEGIN (*return TRUE if s1 is a substring of s2*)
  found := FALSE;
  FOR i := 1 TO LENGTH(s2) DO BEGIN
    EXIT IF LENGTH(SUBSTR(s2, i)) < LENGTH(s1);
    EXIT IF UPPERCASE(SUBSTR(s2, i, LENGTH(s1))) = UPPERCASE(s1)
      DO found := TRUE
    END
  END;

PROCEDURE dofind;
VAR sav: INTEGER;
BEGIN (*find a string*)
  sav := curnum;
  WHILE validlin ANDIF NOT found(cmdlin, line) DO gotonext;
  IF NOT validlin THEN BEGIN gotolin(sav); beep END;
  IF validlin THEN printlin(curnum) ELSE beep
  END;
        
$PAGE dogoto, dohelp

PROCEDURE dogoto;
VAR n: INTEGER;
BEGIN (*goto line*)
  IF cmdlin = '' THEN n := curnum + 1 ELSE GETSTRING(cmdlin, n);
  gotolin(n);
  IF curnum <> n THEN beep;
  IF validlin THEN printlin(curnum)
  END;

PROCEDURE dohelp;
BEGIN (*display  helpful information*)
  WRITELN(TTY, 'Commands are:');
  WRITELN(TTY, 'In  insert line n, default n = current line');
  WRITELN(TTY, '    Use ".END" to stop inserting lines.');
  WRITELN(TTY, 'Dn  print and delete n lines, default n = 1');
  WRITELN(TTY, 'Pn  print and goto n lines, default n = 1');
  WRITELN(TTY, 'Fs  find and print line containing a string');
  WRITELN(TTY, 'Gn  goto and print line n, default n = next line');
  WRITELN(TTY, 'H   help, display this command list');
  WRITELN(TTY, 'E   exit, save buffer');
  WRITELN(TTY, 'Q   quit, do not save buffer');
  WRITELN(TTY, 'Just a carriage return is a goto and print next line.');
  WRITELN(TTY, 'A beep signifies an invalid line number/range.')
  END;

$PAGE delbuff, stbuff, ldbuff, main

PROCEDURE delbuff;
BEGIN (*delete buffer*)
  gototop; WHILE validlin DO delete
  END;

PROCEDURE stbuff;
BEGIN (*store buffer to OUTPUT*)
  gototop; WHILE validlin DO BEGIN WRITELN(line); gotonext END
  END;

PROCEDURE ldbuff;
VAR lin: STRING[linsiz];
BEGIN (*load buffer from INPUT*)
  WHILE readlin(line) DO addlin; gototop
  END;

BEGIN
  WHILE star('EDITOR') DO BEGIN
    delbuff; ldbuff; done := FALSE;
    sav_input := INPUT; INPUT := TTY;
    REPEAT
      getcmd(c);
      CASE c OF
        '?': WRITELN(TTY, 'Commands are I D P F G H E and Q.');
        'I': doinsert;
        'D': dodelete;
        'P': doprint;
        'F': dofind;
        'G': dogoto;
        'H': dohelp;
        'E': BEGIN stbuff; done := TRUE END;
        'Q': done := TRUE
        END
      UNTIL done;
    INPUT := sav_input
    END
  END.
