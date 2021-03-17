$WIDTH=100
$LENGTH=55
$TITLE PASCFM.PAS, last modified 1/13/84, zw
MODULE pascfm OPTIONS SPECIAL(WORD);
(*TYM-Pascal compiler -- command file manager*)

$HEADER PASCFM.HDR

$PAGE system modules

$SYSTEM PASCAL.INC
$SYSTEM PASFIL.INC

$INCLUDE PASCFM.TYP

$PAGE definitions

EXTERNAL VAR
cmd_name_stack: cmd_stack;
cmd_list: pending_command;
last_cmd: pending_command;

var
    old_list: pending_command;
    old_last: pending_command;
$PAGE init_cmd_stack
(*  InitCmdStack pushes a new entry into the pending command file stack,
    preparatory to copying the command file into the pending command list.  *)

procedure init_cmd_stack ( name: file_name );

var new_name: cmd_stack;

begin
  new (new_name);
  new_name^.cmd_file_name := name;
  new_name^.next := cmd_name_stack;
  cmd_name_stack := new_name;
  old_list := cmd_list;
  old_last := last_cmd;
  cmd_list := nil;
end (* init_cmd_stack *);
$PAGE add_cmd_line
(*  AddCmdLine will append a text line to the pending command list.  *)

procedure add_cmd_line ( line: line_string; overflow: boolean );

var new_line: pending_command;

begin
  if line = '' then
    return; (* <---- No blank lines from command files. *)

  new (new_line, length (line));
  with new_line^ do begin
    next := nil;
    eof := false;
    too_long := overflow;
    text := line;
  end;
  if cmd_list = nil
    then cmd_list := new_line
    else last_cmd^.next := new_line;
  last_cmd := new_line;
end (* add_cmd_line *);
$PAGE fin_cmd_stack
(*  FinCmdStack appends an eof record to the pending command list.  *)

procedure fin_cmd_stack;

var new_line: pending_command;

begin
  new (new_line, 0);
  with new_line^ do begin
    next := old_list;
    eof := true;
    too_long := false;
  end;
  if cmd_list = nil
    then cmd_list := new_line
    else last_cmd^.next := new_line;
  if old_last = nil
    then last_cmd := new_line
    else last_cmd := old_last;
end (* fin_cmd_stack *);
$PAGE rd_cmd_file
(*  RdCmdFile copies lines from a command file into the pending command list.  *)

public procedure rd_cmd_file ( var f: text );

var
    line: line_string;
    overflow: boolean;

begin
  init_cmd_stack (filename (f));

  while not eof (f) do begin
    line := '';
    overflow := false;
    while not eoln (f) do begin
      if length (line) > upperbound (line) - 2
        then overflow := true
        else line := line || f^;
      get (f);
    end;
    readln (f);

    add_cmd_line (line, overflow);
  end (* while not eof (f) *);

  fin_cmd_stack;
end (* rd_cmd_file *);
$PAGE rd_tmpcor
(*  RdTmpcor takes a pointer to a character array which has been read from a
    tmpcor command file, and copies lines from it into the pending command
    list.  *)

public procedure rd_tmpcor ( name: parm_string; buf: tmp_buffer; len: integer );

const
    nul = chr (0);
    cr = chr (13);
    lf = chr (10);

var ind, dest, eol: integer;
    cr_lf: packed array [1..2] of char;

begin
  init_cmd_stack (name);
  cr_lf := cr || lf;

  (*  Squeeze NULs out of the buffer.  *)

  dest := 0;
  for ind := 1 to len do
    if buf^[ind] <> nul then begin
      dest := dest + 1;
      buf^[dest] := buf^[ind];
    end;

  ind := 1;
  while ind <= dest do begin
    eol := index (substr (buf^, ind, dest-ind+1), cr_lf);
    add_cmd_line (substr (buf^, ind, eol-1), (eol > upperbound(line) - 2));
    ind := ind + eol + 1;
  end;

  fin_cmd_stack;
end (* rd_tmpcor *);
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

public procedure cmd_save ( var f: file of * );

var tlst: pending_command;
    tstk: cmd_stack;
    len: integer;

begin
  tlst := cmd_list;
  while tlst <> nil do begin
    len := length (tlst^.text);
    write (f, len, tlst^: size (tlst^, len));
    tlst := tlst^.next;
  end;
  len := 0;
  write (f, len);

  tstk := cmd_name_stack;
  while tstk <> nil do begin
    len := 1;
    write (f, len, tstk^.cmd_file_name);
    tstk := tstk^.next;
  end;
  len := 0;
  write (f, len);
end (* cmd_save *);
$PAGE cmd_clear
(*  CMD CLEAR will dispose of the pending command list and the command file
    name stack.  *)

public procedure cmd_clear;

var tlst: pending_command;
    tstk: cmd_stack;

begin
  while cmd_list <> nil do begin
    tlst := cmd_list;
    cmd_list := cmd_list^.next;
    dispose (tlst);
  end;

  while cmd_name_stack <> nil do begin
    tstk := cmd_name_stack;
    cmd_name_stack := cmd_name_stack^.next;
    dispose (tstk);
  end;
end (* cmd_clear *);
$PAGE cmd_restore
(*  CMD RESTORE will restore the pending command list and command file name
    stack from a specified file.  *)

public procedure cmd_restore ( var f: file of * );

var tlst: pending_command;
    tstk, stk_last: cmd_stack;
    len: integer;

begin
  cmd_clear; (* Get rid of whatever is there now. *)

  loop
    read (f, len);
  exit if len = 0;
    new (tlst, len);
    read (f, tlst^: size (tlst^, len));
    tlst^.next := nil;
    if cmd_list = nil
      then cmd_list := tlst
      else last_cmd^.next := tlst;
    last_cmd := tlst;
  end;

  loop
    read (f, len);
  exit if len = 0;
    new (tstk);
    with tstk^ do begin
      read (f, cmd_file_name);
      next := nil;
    end;
    if cmd_name_stack = nil
      then cmd_name_stack := tstk
      else stk_last^.next := tstk;
    stk_last := tstk;
  end;
end (* cmd_restore *).
