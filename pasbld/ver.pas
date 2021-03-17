$TITLE VER.PAS, last modified 4/29/84, zw
PROGRAM ver OPTIONS special(word);
(*TYM-Pascal Set Version Utility Program for DEC PDP-10 Version Word.*)
$SYSTEM VERSIO.INC

TYPE
version_record = PACKED RECORD
  code: 0 .. 7b;
  major: 0 .. 777b;
  minor: 0 .. 77b;
  edit: 0 .. 777777b;
END;

VAR
verstr: STRING[40];
verrcd: version_record;
relname: FILE_NAME;

FUNCTION null_ver(ver: version_record): BOOLEAN;
BEGIN
  WITH ver DO null_ver := (code = 0) AND (major = 0) AND (minor = 0) AND
    (edit = 0)
END;

PROCEDURE write_ver(relname: FILE_NAME; version: version_record);
VAR
relfile: FILE OF *;
BEGIN
  REWRITE(relfile, relname);
  IF iostatus <> IO_OK THEN WRITELN(TTYOUTPUT, '?Unable to open ', relname)
  ELSE BEGIN
    WRITE(relfile, 000006000001b); (* NAME record *)
    WRITE(relfile, 0);
    WRITE(relfile, 024036746164b); (* name = PASVER *)
    WRITE(relfile, 000001000002b); (* CODE record *)
    WRITE(relfile, 0);
    WRITE(relfile, 137b); (* addr = absolute 137B *)
    WRITE(relfile, version); (* data = version word *)
    WRITE(relfile, 000005000002b); (* END record *)
    WRITE(relfile, 200000000000b);
    WRITE(relfile, 0); (* highseg break = relocatable 0 *)
    WRITE(relfile, 140b); (* lowseg break = absolute 140B *)
    CLOSE(relfile);
    WRITELN(TTYOUTPUT, relname || ' created')
  END
END;

PROCEDURE read_ver(relname: FILE_NAME; VAR version: version_record);
VAR
relfile: FILE OF *;
BEGIN
  RESET(relfile, relname, [SEEKOK]);
  IF iostatus = IO_OK THEN READRN(relfile, 7, version)
  ELSE WRITELN(TTYOUTPUT, 'New file.');
  CLOSE(relfile)
END;

PROCEDURE display_ver(version: version_record);

  FUNCTION width(x: INTEGER): INTEGER;
  VAR
  xx: INTEGER;
  BEGIN
    xx := x;
    width := 0;
    REPEAT
      width := width + 1;
      xx := xx DIV 8;
    UNTIL xx = 0;
  END;
BEGIN
  WITH version DO BEGIN
    WRITE(TTYOUTPUT, 'Current version is ');
    IF major <> 0 THEN WRITE(TTYOUTPUT, major:width(major):o);
    IF minor <> 0 THEN BEGIN
      IF minor <= 26 THEN WRITE(TTYOUTPUT, CHR(ORD('A') - 1 + minor))
      ELSE IF minor <= 52 THEN WRITE(TTYOUTPUT, 'A', CHR(ORD('A')-1+minor-26))
      ELSE WRITE(TTYOUTPUT, 'B', CHR(ORD('A') - 1 + minor - 52))
    END;
    IF edit <> 0 THEN WRITE(TTYOUTPUT, '(', edit:width(edit):o, ')');
    IF code <> 0 THEN WRITE(TTYOUTPUT, '-', code:1:o);
    WRITELN(TTYOUTPUT)
  END;
END;

FUNCTION parse_code(VAR verstr: STRING[*]; VAR code: INTEGER): BOOLEAN;
BEGIN
  parse_code := TRUE;
  IF verstr [1] = '-' THEN BEGIN
    IF NOT (verstr[2] IN ['0' .. '7']) THEN parse_code := FALSE
    ELSE BEGIN
      code := ORD(verstr[2]) - ORD('0');
      verstr := SUBSTR(verstr, 3)
    END
  END
  ELSE code := 0
END;

FUNCTION parse_edit(VAR verstr: STRING[*]; VAR edit: INTEGER): BOOLEAN;
VAR
x: INTEGER;
BEGIN
  parse_edit := TRUE;
  IF verstr [1] = '(' THEN BEGIN
    verstr := SUBSTR(verstr, 2);
    x := VERIFY(verstr, ['0' .. '7']);
    IF x = 0 THEN parse_edit := FALSE
    ELSE BEGIN
      GETSTRING(verstr, edit: x - 1: o);
      IF edit > 777777b THEN parse_edit := FALSE
      ELSE BEGIN
	IF verstr[x] <> ')' THEN parse_edit := FALSE
	ELSE verstr := SUBSTR(verstr, x + 1)
      END
    END
  END
  ELSE edit := 0
END;

FUNCTION parse_major(VAR verstr: STRING[*]; VAR major: INTEGER): BOOLEAN;
VAR
x: INTEGER;
BEGIN
  parse_major := TRUE;
  x := VERIFY(verstr, ['0' .. '7']);
  IF x = 1 THEN major := 0
  ELSE GETSTRING(verstr, major: x - 1: o);
  IF major > 777b THEN parse_major := FALSE
  ELSE verstr := SUBSTR(verstr, x)
END;

FUNCTION parse_minor(VAR verstr: STRING[*]; VAR minor: INTEGER): BOOLEAN;
VAR
x: INTEGER;
BEGIN
  parse_minor := TRUE;
  x := VERIFY(verstr, ['A'..'Z']);
  CASE x OF
    1: minor := 0;
    2: minor := ORD(verstr [1]) - ORD('A') + 1;
    3: IF SUBSTR(verstr, 1, 2) > 'BK' THEN parse_minor := FALSE
    ELSE minor := (ORD(verstr [1]) - ORD('A') + 1) * 26 + (ORD(verstr [2])
      - ORD('A') + 1);
    OTHERS: parse_minor := FALSE
  END;
  IF parse_minor THEN verstr := SUBSTR(verstr, x)
END;

FUNCTION parse_ver(VAR verstr: STRING[*]; VAR ver: version_record): BOOLEAN;
VAR
major, minor, edit, code: INTEGER;
BEGIN
  parse_ver := TRUE;
  verstr := UPPERCASE(verstr) || ' ';
  IF (verstr = ' ') AND NOT null_ver(ver) THEN ver.edit := ver.edit + 1
  ELSE BEGIN
    parse_ver := parse_major(verstr, major) ANDIF parse_minor(verstr, minor)
      ANDIF parse_edit(verstr, edit) ANDIF parse_code(verstr, code) ANDIF
      (verstr = ' ');
    IF parse_ver THEN BEGIN
      ver.major := major;
      ver.minor := minor;
      ver.edit := edit;
      ver.code := code
    END
  END
END;

BEGIN
  OPEN(TTY);
  REWRITE(TTYOUTPUT);
  WRITELN(TTYOUTPUT, 'TYM-Pascal Version Manipulator, Version ', version());
  WRITELN(TTYOUTPUT);
  WRITELN(TTYOUTPUT, 'This program operates on a REL file which establishes');
  WRITELN(TTYOUTPUT, 'the standard DEC .JBVER version word for a program.');
  WRITELN(TTYOUTPUT);
  WRITE(TTYOUTPUT, 'Enter REL file name: ');
  BREAK(TTYOUTPUT);
  READLN(TTY);
  READ(TTY, relname);
  verrcd := (0, 0, 0, 0);
  read_ver(relname, verrcd);
  LOOP
    WRITELN(TTYOUTPUT);
    IF NOT null_ver(verrcd) THEN display_ver(verrcd);
    WRITE(TTYOUTPUT, 'Enter new version: ');
    BREAK(TTYOUTPUT);
    READLN(TTY);
    READ(TTY, verstr);
    EXIT IF parse_ver(verstr, verrcd) DO write_ver(relname, verrcd);
    WRITELN(TTYOUTPUT, 'Version syntax is:');
    WRITELN(TTYOUTPUT, '  [<major>] [<minor>] ["("<edit>")"] ["-"<code>]');
    WRITELN(TTYOUTPUT, '  0 <= major <= 777b');
    WRITELN(TTYOUTPUT, '  "A" <= minor <= "BK"');
    WRITELN(TTYOUTPUT, '  0 <= edit <= 777777b');
    WRITELN(TTYOUTPUT, '  0 <= code <= 7');
    WRITELN(TTYOUTPUT, 'Type a blank line to just increment the edit.')
  END;
  IF NOT null_ver(verrcd) THEN display_ver(verrcd)
END.
