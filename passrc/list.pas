$WIDTH=100
$LENGTH=55
$TITLE LIST.PAS, last modified 11/2/83, zw

PROGRAM list;
(*list a file*)

EXTERNAL PROCEDURE start(STRING);
EXTERNAL FUNCTION rdyio(STRING): BOOLEAN;
EXTERNAL FUNCTION rdlin(VAR STRING): BOOLEAN;
EXTERNAL PROCEDURE wrlin(STRING);
EXTERNAL PROCEDURE getopt(ARRAY[1..*] OF STRING; VAR ARRAY{1..*] OF INTEGER);
EXTERNAL PROCEDURE optnum(VAR INTEGER; INTEGER; INTEGER);

CONST bufsiz = 80;

VAR
optpos: ARRAY[1..14] OF INTEGER;
optkey: ARRAY[1..14] OF wrdtyp := ('ODD', 'EVEN', 'LEGAL', 'DOUBLE',
  'BREAK', 'HEADER', 'WRAP', 'LOGICAL', 'GO', 'START', 'COUNT', 'MARGIN',
  'WIDTH', 'CLEAN');
opts: SET OF (listodd, listeven, listlegal, listdouble, listbreak,
  listheader, listwrap, listlogical, listgo, listclean);
line_buffer: ARRAY [1 .. bufsiz] OF lstlintyp; (*line buffer*)
line: STRING[lstlinlen * 2]; (*current line text*)
last_line: INTEGER; (*last line of a page, number of lines per page*)
line_counter: INTEGER; (*buffer index of current line*)
block_begin: INTEGER; (*buffer index of first line of block*)
block_end: INTEGER; (*buffer index of last line of block*)
spaces: lstlintyp; (*a bunch of spaces for padding*)
i: INTEGER; (*index for spaces*)
not_sub_page: BOOLEAN; (*TRUE if not a sub-page*)
wrapped: BOOLEAN; (*TRUE if line wrapped around*)
listed_page_count: INTEGER; (*count of pages actually listed*)
logical_page_count: INTEGER; (*count of logical pages*)
physical_page_count: INTEGER; (*count of physical pages*)
sub_page_count: INTEGER; (*count of logicl sub-pages*)
start_line: INTEGER; (*starting logical page number*)
line_count: INTEGER; (*count of pages to actually list*)
left_margin: INTEGER; (*number of spaces listed before line*)
line_width: INTEGER; (*width of line text listed*)

PROCEDURE rtrim(VAR s: STRING);
VAR i: INTEGER;
BEGIN (*trim all spaces and tabs from right side of string*)
  IF s <> '' THEN BEGIN (*non-null string*)
    FOR i := LENGTH(s) DOWNTO 1 DO EXIT IF NOT (s[i] IN [' ', CHR(9)]);
    s := SUBSTR(s, 1, i)
  END
END;

PROCEDURE outlin(s: STRING; i: INTEGER);
BEGIN (*output line s, line number i*)
    IF listlegal IN opts THEN BEGIN (*legal format*)
    (*initial margin, line number and boarder*)
      WRITE(' ': mar, i: 2, ' || ');
      IF s = '' THEN (*blank lines marked with "///"*)
	WRITELN(SUBSTR('///' || spaces, 1, wid), ' |')
      ELSE (*non-blank line*)
	WRITELN(SUBSTR(s || spaces, 1, wid), ' |');
	(*double-space*)
      WRITELN(' ': mar + 2, ' || ', SUBSTR(spaces, 1, wid), ' |')
    END
    ELSE BEGIN (*standard format*)
      WRITELN(' ': mar, s);
      IF listdouble IN opts THEN (*double-space option*)
	WRITELN
    END
  END;
$PAGE


  PROCEDURE newpage;

  VAR
      i: INTEGER; (*index to line buffer*)
      writeflag: BOOLEAN; (*TRUE if page actually to be listed*)

  BEGIN (*list contents of line buffer, start new page*)
  (*count physical page*)
    physical_page_count := physical_page_count + 1;
    (*count logical page*)
    IF not_sub_page THEN BEGIN (*not sub-page, reset sub-page count*)
      logical_page_count := logical_page_count + 1;
      sub_page_count := 0
    END
    ELSE (*is sub-page, keep same logical page count*)
      sub_page_count := sub_page_count + 1;
      (*ok to list if not even or odd*)
    writeflag := NOT ((listeven IN opts) OR (listodd IN opts));
    (*ok to list if even and even physical page*)
    writeflag := writeflag OR ((listeven IN opts) AND ((physical_page_count MOD 2) = 0));
    (*ok to list if odd and odd physical page*)
    writeflag := writeflag OR ((listodd IN opts) AND ((physical_page_count MOD 2) = 1));
    (*not ok to list if logical page before start*)
    writeflag := writeflag AND (logical_page_count >= start);
    (*not ok to list if already listed enough pages*)
    writeflag := writeflag AND ((count < 1) OR (listed_page_count < count));
$PAGE
    IF writeflag THEN BEGIN (*actually list page*)
    (*heading of three lines*)
    IF NOT (listclean IN opts) THEN BEGIN
      WRITELN;
      IF listheader IN opts THEN (*header option*)
	WRITELN(FILENAME(INPUT))
      ELSE (*no header*)
	WRITELN;
      WRITELN
      END;
      (*list all lines in buffer*)
      FOR i := 1 TO line_counter DO
	outlin(line_buffer[i], i);
	(*finish up page with blank lines*)
	IF NOT (listclean IN opts) THEN BEGIN
      FOR i := line_counter + 1 TO last_line DO
	outlin('', i);
	(*footing of three lines with logical page number*)
      WRITELN;
      WRITE(' ': mar + (wid DIV 2), '(', logical_page_count: 0);
      IF sub_page_count > 0 THEN (*include sub-page number, if any*)
	WRITE('-', sub_page_count: 0);
      WRITELN(')');
      (*count page actually listed*)
      listed_page_count := listed_page_count + 1;
      (*send a formfeed to keep things aligned*)
      PAGE
END
    END;
    (*reset line counter to beginning of buffer*)
    line_counter := 0
  END;
$PAGE

BEGIN
  BEGIN (*set up initial values of flags and counters*)
    not_sub_page := TRUE;
    listed_page_count := 0;
    physical_page_count := 0;
    logical_page_count := 0;
    sub_page_count := 0;
    line_counter := 0;
    lin := '';
  END;
  BEGIN (*load up spaces for padding*)
    spaces := '';
    FOR i := 1 TO UPPERBOUND(spaces) DO
      spaces := spaces || ' ';
    FOR i := 1 TO UPPERBOUND(line_buffer) DO line_buffer[i] := spaces
  END;
  BEGIN (*set up parameters with default or specified values*)
    IF s <= 0 THEN (*default start*)
      start := 1
    ELSE (*use specified start value*)
      start := s;
    IF c <= 0 THEN (*default count*)
      count := 0
    ELSE (*use specified count value*)
      count := c;
    IF w <= 0 THEN BEGIN (*default width*)
      IF listlegal IN opts THEN (*60 char lines for legal*)
	wid := 60
      ELSE (*70 char lines for normal*)
	wid := 70
    END
    ELSE (*use specified width value*)
      wid := w;
    IF wid > lstlinlen THEN (*width out-of-bounds*)
      wid := lstlinlen;
    IF m <= 0 THEN (*default margin: 10 spaces*)
      mar := 10
    ELSE (*use specified margin value*)
      mar := m;
    IF (mar + wid) > lstlinlen THEN (*margin out-of-bounds, set to zero*)
      mar := 0
  END;
  IF listeven IN opts THEN (*left margin becomes right margin if even*)
    mar := 0;
IF listclean IN opts THEN last_line := 66 ELSE last_line := 60;
  IF (listlegal IN opts) OR (listdouble IN opts) THEN (*30 double-spaced lines*)
    last_line := last_line DIV 2;
  IF (OUTPUT = TTYOUTPUT) AND NOT (listgo IN opts) THEN (*pause for setup*)
    REPEAT
    UNTIL askyn('Ready to list? ');
$PAGE
  WHILE rdlin(lin) DO BEGIN (*process a line of input*)
    rtrim(lin); (*trim trailing spaces and tabs*)
    line_counter := line_counter + 1; (*line_counter now points to free buffer line*)
    wrapped := FALSE; (*assume not wrapped around*)
    WHILE (LENGTH(lin) > wid) AND (listwrap IN opts) DO BEGIN (*wrap*)
      line_buffer[line_counter] := SUBSTR(lin, 1, wid); (*save part of line*)
      IF line_counter < bufsiz THEN (*advance to next line in buffer*)
	line_counter := line_counter + 1;
      lin := SUBSTR(lin, wid + 1); (*advance to next part of line*)
      wrapped := TRUE (*flag line as wrapped*)
    END;
    IF wrapped AND (listwrap IN opts) THEN (*last part of wrapped line*)
    (*note that wrapped part of line is placed at right margin*)
      line_buffer[line_counter] := SUBSTR(spaces, 1, wid - LENGTH(lin)) || lin
    ELSE (*normal placement of line in buffer*)
      line_buffer[line_counter] := SUBSTR(lin || ' ', 1, MIN(LENGTH(lin), wid));
    IF EOPAGE THEN BEGIN (*a formfeed is a logical page break*)
      not_sub_page := TRUE; (*signal logical page break*)
      newpage (*flush buffer*)
    END
    ELSE IF line_counter >= (last_line + 1) THEN BEGIN (*normal page break*)
    (*at this point there is one more line in the buffer than can be listed*)
    (*logical page if not checking for sub-pages*)
      not_sub_page := NOT (listlogical IN opts);
      block_end := line_counter; (*mark end of block*)
      FOR line_counter := block_end DOWNTO 1 DO (*search up for blank line*)
    EXIT IF line_buffer[line_counter] = '';
      IF (line_counter <= (last_linenumber DIV 2)) AND NOT ((listbreak IN opts) OR (listclean IN opts)) THEN
      (*break at last line*)
	block_begin := block_end - 1
      ELSE (*break at nearest blank line*)
	block_begin := line_counter;
      line_counter := block_begin - 1; (*reset line count to end of list page*)
      newpage; (*flush buffer, save block*)
      LOOP (*skip initial blank lines of block*)
      EXIT IF block_begin > block_end;
      EXIT IF line_buffer[block_begin] <> '';
	block_begin := block_begin + 1
      END;
      line_counter := 1; (*patch compiler bug*)
      FOR line_counter := 1 TO block_end - block_begin + 1 DO (*shift block up*)
	line_buffer[line_counter] := line_buffer[line_counter + block_begin - 1];
	(*point line counter to last line of shifted block*)
      line_counter := line_counter - 1
    END
  END;
  IF line_counter > 0 THEN (*flush partial page in buffer*)
    newpage;
  IF physical_page_count < 1 THEN (*always print at least one page*)
    newpage
  ELSE IF (listeven IN opts) AND ((physical_page_count MOD 2) = 1) THEN (*even up listing*)
    newpage
END.


BEGIN
  start('LIST');
  WHILE rdyio('*) DO BEGIN
    getopt(optkey, optpos);
    opts := []; start_line := -1; line_count := -1;
    left_margin := -1; line_width := -1;
    IF opts[1] > 0 THEN opts := opts + [listodd];
    IF opts[2] > 0 THEN opts := opts + [listeven];
    IF opts[3] > 0 THEN opts := opts + [listlegal];
    IF opts[4] > 0 THEN opts := opts + [listdouble];
    IF opts[5] > 0 THEN opts := opts + [listbreak];
    IF opts[6] > 0 THEN opts := opts + [listheader];
    IF opts[7] > 0 THEN opts := opts + [listwrap];
    IF opts[8] > 0 THEN opts := opts + [listlogical];
    IF opts[9] > 0 THEN opts := opts + [listgo];
    IF opts[10] > 0 THEN optnum(start_line, -1, opts[10]);
    IF opts[11] > 0 THEN optnum(line_count, -1, opts[11]);
    IF opts[12] > 0 THEN optnum(left_margin, -1, opts[12]);
    IF opts[13] > 0 THEN optnum(line_width, -1, opts[13]);
    IF opts[14] > 0 THEN opts := opts + [listclean];
    do_list
  END
END.
  