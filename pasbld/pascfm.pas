$TITLE PASCFM.PAS, last modified 5/11/84, zw
MODULE pascfm OPTIONS special(word);
(*TYM-Pascal compiler command file manager*)
$HEADER pascfm
$PAGE declarations
$SYSTEM pascal
$SYSTEM pasfil
$SYSTEM pascfm.typ
EXTERNAL VAR
cmd_name_stack: cmd_stack;
cmd_list: pending_command;
last_cmd: pending_command;

VAR
old_list: pending_command;
old_last: pending_command;
$PAGE init_cmd_stack
(*  InitCmdStack pushes a new entry into the pending command file stack,
    preparatory to copying the command file into the pending command list.  *)

PROCEDURE init_cmd_stack ( name: FILE_NAME );
VAR
new_name: cmd_stack;
BEGIN
  NEW (new_name);
  new_name^.cmd_file_name := name;
  new_name^.next := cmd_name_stack;
  cmd_name_stack := new_name;
  old_list := cmd_list;
  old_last := last_cmd;
  cmd_list := NIL;
END (* init_cmd_stack *);
$PAGE add_cmd_line
(*  AddCmdLine will append a text line to the pending command list.  *)

PROCEDURE add_cmd_line ( line: line_string; overflow: BOOLEAN );
VAR
new_line: pending_command;
BEGIN
  IF line = '' THEN RETURN; (* <---- No blank lines from command files. *)
  NEW (new_line, LENGTH (line));
  WITH new_line^ DO BEGIN
    next := NIL;
    EOF := FALSE;
    too_long := overflow;
    TEXT := line;
  END;
  IF cmd_list = NIL THEN cmd_list := new_line
  ELSE last_cmd^.next := new_line;
  last_cmd := new_line;
END (* add_cmd_line *);
$PAGE fin_cmd_stack
(*  FinCmdStack appends an eof record to the pending command list.  *)

PROCEDURE fin_cmd_stack;
VAR
new_line: pending_command;
BEGIN
  NEW (new_line, 0);
  WITH new_line^ DO BEGIN
    next := old_list;
    EOF := TRUE;
    too_long := FALSE;
  END;
  IF cmd_list = NIL THEN cmd_list := new_line
  ELSE last_cmd^.next := new_line;
  IF old_last = NIL THEN last_cmd := new_line
  ELSE last_cmd := old_last;
END (* fin_cmd_stack *);
$PAGE rd_cmd_file
(*  RdCmdFile copies lines from a command file into the pending command list.  *)
PUBLIC
PROCEDURE rd_cmd_file ( VAR f: TEXT );
VAR
line: line_string;
overflow: BOOLEAN;
BEGIN
  init_cmd_stack (FILENAME (f));
  WHILE NOT EOF (f) DO BEGIN
    line := '';
    overflow := FALSE;
    WHILE NOT EOLN (f) DO BEGIN
      IF LENGTH (line) > UPPERBOUND (line) - 2 THEN overflow := TRUE
      ELSE line := line || f^;
      GET (f);
    END;
    READLN (f);
    add_cmd_line (line, overflow);
  END (* while not eof (f) *);
  fin_cmd_stack;
END (* rd_cmd_file *);
$PAGE rd_tmpcor
(*  RdTmpcor takes a pointer to a character array which has been read from a
    tmpcor command file, and copies lines from it into the pending command
    list.  *)
PUBLIC
PROCEDURE rd_tmpcor ( name: parm_string; buf: tmp_buffer; len: INTEGER );
CONST
nul = CHR (0);
cr = CHR (13);
lf = CHR (10);
VAR
ind, dest, eol: INTEGER;
cr_lf: PACKED ARRAY [1..2] OF CHAR;
BEGIN
  init_cmd_stack (name);
  cr_lf := cr || lf;
  (*  Squeeze NULs out of the buffer.  *)
  dest := 0;
  FOR ind := 1 TO len DO IF buf^[ind] <> nul THEN BEGIN
    dest := dest + 1;
    buf^[dest] := buf^[ind];
  END;
  ind := 1;
  WHILE ind <= dest DO BEGIN
    eol := INDEX (SUBSTR (buf^, ind, dest-ind+1), cr_lf);
    add_cmd_line (SUBSTR (buf^, ind, eol-1), (eol > UPPERBOUND(line) - 2));
    ind := ind + eol + 1;
  END;
  fin_cmd_stack;
END (* rd_tmpcor *);
$PAGE cmd_save
(*  CMD SAVE will write the current contents of the pending command list and the
    command file name stack to a specified file.  Command list entries are
    written as lines with the format:
	XY<len> <text>
    where X is " " normally, or "*" if the line is too long; Y is " " normally,
    or "*" if this is an eof entry; <len> is the number of characters in the
    command, terminated with a space; and <text> is the text of the line.
    The command list entries are followed by a empty line, some lines with
    the stacked command file names, and another empty line.  *)
PUBLIC
PROCEDURE cmd_save ( VAR f: FILE OF * );
VAR
tlst: pending_command;
tstk: cmd_stack;
len: INTEGER;
BEGIN
  tlst := cmd_list;
  WHILE tlst <> NIL DO BEGIN
    len := LENGTH (tlst^.TEXT);
    WRITE (f, len, tlst^: SIZE (tlst^, len));
    tlst := tlst^.next;
  END;
  len := 0;
  WRITE (f, len);
  tstk := cmd_name_stack;
  WHILE tstk <> NIL DO BEGIN
    len := 1;
    WRITE (f, len, tstk^.cmd_file_name);
    tstk := tstk^.next;
  END;
  len := 0;
  WRITE (f, len);
END (* cmd_save *);
$PAGE cmd_clear
(*  CMD CLEAR will dispose of the pending command list and the command file
    name stack.  *)
PUBLIC
PROCEDURE cmd_clear;
VAR
tlst: pending_command;
tstk: cmd_stack;
BEGIN
  WHILE cmd_list <> NIL DO BEGIN
    tlst := cmd_list;
    cmd_list := cmd_list^.next;
    DISPOSE (tlst);
  END;
  WHILE cmd_name_stack <> NIL DO BEGIN
    tstk := cmd_name_stack;
    cmd_name_stack := cmd_name_stack^.next;
    DISPOSE (tstk);
  END;
END (* cmd_clear *);
$PAGE cmd_restore
(*  CMD RESTORE will restore the pending command list and command file name
    stack from a specified file.  *)
PUBLIC
PROCEDURE cmd_restore ( VAR f: FILE OF * );
VAR
tlst: pending_command;
tstk, stk_last: cmd_stack;
len: INTEGER;
BEGIN
  cmd_clear; (* Get rid of whatever is there now. *)
  LOOP
    READ (f, len);
    EXIT IF len = 0;
    NEW (tlst, len);
    READ (f, tlst^: SIZE (tlst^, len));
    tlst^.next := NIL;
    IF cmd_list = NIL THEN cmd_list := tlst
    ELSE last_cmd^.next := tlst;
    last_cmd := tlst;
  END;
  LOOP
    READ (f, len);
    EXIT IF len = 0;
    NEW (tstk);
    WITH tstk^ DO BEGIN
      READ (f, cmd_file_name);
      next := NIL;
    END;
    IF cmd_name_stack = NIL THEN cmd_name_stack := tstk
    ELSE stk_last^.next := tstk;
    stk_last := tstk;
  END;
END (* cmd_restore *).
