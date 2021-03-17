$TITLE BLDHLP.PAS, last modified 4/29/84, zw
PROGRAM build_help_file;
$SYSTEM CMDUTL.INC
$SYSTEM VERSIO.INC

TYPE
index_record = RECORD
  next: ^index_record;
  content: cmd_lookup_record
END;

VAR
help_file: FILE OF *;
INDEX: ^ index_record;
index_cursor, index_entries: INTEGER;

PROCEDURE write_line(line: STRING[80]);
TYPE
str = PACKED ARRAY[1 .. *] OF CHAR;
VAR
len: INTEGER;
TEXT: PACKED ARRAY[1 .. 80] OF CHAR;
BEGIN
  len := LENGTH(line);
  TEXT := line;
  WRITE(help_file, len, TEXT: SIZE (str, len))
END;

PROCEDURE open_files;
VAR
name: FILE_NAME;
BEGIN
  OPEN(TTY);
  REWRITE(TTY);
  WRITELN(TTYOUTPUT, 'TYM-Pascal Help File Utility, Version ', version());
  LOOP
    WRITE(TTYOUTPUT, 'Enter source file: ');
    BREAK(TTYOUTPUT);
    READLN(TTY);
    READ(TTY, name);
    IF name = '' THEN STOP;
    RESET(INPUT, name);
    EXIT IF iostatus = IO_OK;
    WRITELN(TTYOUTPUT, '?Can not open ', name)
  END;
  LOOP
    WRITE(TTYOUTPUT, 'Enter help file: ');
    BREAK(TTYOUTPUT);
    READLN(TTY);
    READ(TTY, name);
    IF name = '' THEN STOP;
    REWRITE(help_file, name, [SEEKOK]);
    EXIT IF iostatus = IO_OK;
    WRITELN(TTYOUTPUT, '?Can not open ', name)
  END
END;

PROCEDURE copy_text;
VAR
text_line: STRING[80];
last: ^index_record;
CONST
end_marker: INTEGER = -1;
BEGIN
  NEW(INDEX);
  last := INDEX;
  last^ := (NIL, ('*', 1, CURSOR(help_file)));
  index_entries := 1;
  WHILE NOT EOF(INPUT) DO BEGIN
    READLN(INPUT, text_line);
    IF (text_line = '') ORIF (text_line[1] <> '\') THEN write_line(text_line)
    ELSE BEGIN
      WRITE(help_file, end_marker);
      NEW(last^.next);
      last := last^.next;
      last^ := (NIL, (SUBSTR(text_line, 2), LENGTH(text_line)
	-1, CURSOR(help_file)));
      index_entries := index_entries + 1
    END
  END;
  WRITE(help_file, end_marker)
END;

PROCEDURE make_index;
VAR
index_table: ^ARRAY[1 .. *] OF cmd_lookup_record;
i: INTEGER;
BEGIN
  NEW(index_table, index_entries);
  FOR i := 1 TO index_entries DO BEGIN
    index_table^[i] := INDEX^.content;
    INDEX := INDEX^.next
  END;
  index_cursor := CURSOR(help_file);
  WRITE(help_file, index_table^: SIZE(index_table^, index_entries));
  WRITERN(help_file, 1, index_cursor, index_entries)
END;

BEGIN
  open_files;
  index_cursor := 0;
  index_entries := 0;
  WRITE(help_file, index_cursor, index_entries);
  copy_text;
  make_index
END.
