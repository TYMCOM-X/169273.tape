$WIDTH=100
$LENGTH=55
$TITLE lstutl.pas, last modified 9/15/83, zw

MODULE lstutl;
(*listing utility*)
$SYSTEM PASUTL
$PAGE constants, types and variables
$INCLUDE LSTUTL.TYP
$PAGE list

PUBLIC PROCEDURE list(f: listflags; s, c, m, w: int);

CONST
    bufsiz = 80;

VAR
    linbuf: ARRAY [1 .. bufsiz] OF lstlintyp; (*line buffer*)
    lin: STRING[lstlinlen * 2]; (*current line text*)
    laslin: int; (*last line of a page, number of lines per page*)
    lincnt: int; (*buffer index of current line*)
    blkbgn: int; (*buffer index of first line of block*)
    blkend: int; (*buffer index of last line of block*)
    spc: lstlintyp; (*a bunch of spaces for padding*)
    i: int; (*index for spc*)
    notsubpage: bin; (*TRUE if not a sub-page*)
    wrapped: bin; (*TRUE if line wrapped around*)
    lstpag: int; (*count of pages actually listed*)
    lgcpag: int; (*count of logical pages*)
    phypag: int; (*count of physical pages*)
    subpag: int; (*count of logicl sub-pages*)
    start: int; (*starting logical page number*)
    count: int; (*count of pages to actually list*)
    mar: int; (*number of spaces listed before line*)
    wid: int; (*width of line text listed*)


  PROCEDURE rtrim(VAR s: str);

  VAR
      i: int;

  BEGIN (*trim all spaces and tabs from right side of string*)
    IF s <> '' THEN BEGIN
      FOR i := LENGTH(s) DOWNTO 1 DO
    EXIT IF NOT (s[i] IN [' ', CHR(9)]);
      s := SUBSTR(s, 1, i)
    END
  END;
$PAGE


  PROCEDURE outlin(s: STRING[*]; i: int);

  BEGIN (*output line s, line number i*)
    IF listlegal IN f THEN BEGIN (*legal format*)
    (*initial margin, line number and boarder*)
      WRITE(' ': mar, i: 2, ' || ');
      IF s = '' THEN (*blank lines marked with "///"*)
	WRITELN(SUBSTR('///' || spc, 1, wid), ' |')
      ELSE (*non-blank line*)
	WRITELN(SUBSTR(s || spc, 1, wid), ' |');
	(*double-space*)
      WRITELN(' ': mar + 2, ' || ', SUBSTR(spc, 1, wid), ' |')
    END
    ELSE BEGIN (*standard format*)
      WRITELN(' ': mar, s);
      IF listdouble IN f THEN (*double-space option*)
	WRITELN
    END
  END;
$PAGE


  PROCEDURE newpage;

  VAR
      i: int; (*index to line buffer*)
      writeflag: bin; (*TRUE if page actually to be listed*)

  BEGIN (*list contents of line buffer, start new page*)
  (*count physical page*)
    phypag := phypag + 1;
    (*count logical page*)
    IF notsubpage THEN BEGIN (*not sub-page, reset sub-page count*)
      lgcpag := lgcpag + 1;
      subpag := 0
    END
    ELSE (*is sub-page, keep same logical page count*)
      subpag := subpag + 1;
      (*ok to list if not even or odd*)
    writeflag := NOT ((listeven IN f) OR (listodd IN f));
    (*ok to list if even and even physical page*)
    writeflag := writeflag OR ((listeven IN f) AND ((phypag MOD 2) = 0));
    (*ok to list if odd and odd physical page*)
    writeflag := writeflag OR ((listodd IN f) AND ((phypag MOD 2) = 1));
    (*not ok to list if logical page before start*)
    writeflag := writeflag AND (lgcpag >= start);
    (*not ok to list if already listed enough pages*)
    writeflag := writeflag AND ((count < 1) OR (lstpag < count));
$PAGE
    IF writeflag THEN BEGIN (*actually list page*)
    (*heading of three lines*)
    IF NOT (listclean IN f) THEN BEGIN
      WRITELN;
      IF listheader IN f THEN (*header option*)
	WRITELN(FILENAME(INPUT))
      ELSE (*no header*)
	WRITELN;
      WRITELN
      END;
      (*list all lines in buffer*)
      FOR i := 1 TO lincnt DO
	outlin(linbuf[i], i);
	(*finish up page with blank lines*)
	IF NOT (listclean IN f) THEN BEGIN
      FOR i := lincnt + 1 TO laslin DO
	outlin('', i);
	(*footing of three lines with logical page number*)
      WRITELN;
      WRITE(' ': mar + (wid DIV 2), '(', lgcpag: 0);
      IF subpag > 0 THEN (*include sub-page number, if any*)
	WRITE('-', subpag: 0);
      WRITELN(')');
      (*count page actually listed*)
      lstpag := lstpag + 1;
      (*send a formfeed to keep things aligned*)
      PAGE
END
    END;
    (*reset line counter to beginning of buffer*)
    lincnt := 0
  END;
$PAGE

BEGIN
  BEGIN (*set up initial values of flags and counters*)
    notsubpage := TRUE;
    lstpag := 0;
    phypag := 0;
    lgcpag := 0;
    subpag := 0;
    lincnt := 0;
    lin := '';
  END;
  BEGIN (*load up spaces for padding*)
    spc := '';
    FOR i := 1 TO UPPERBOUND(spc) DO
      spc := spc || ' ';
    FOR i := 1 TO UPPERBOUND(linbuf) DO linbuf[i] := spc
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
      IF listlegal IN f THEN (*60 char lines for legal*)
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
  IF listeven IN f THEN (*left margin becomes right margin if even*)
    mar := 0;
IF listclean IN f THEN laslin := 66 ELSE laslin := 60;
  IF (listlegal IN f) OR (listdouble IN f) THEN (*30 double-spaced lines*)
    laslin := laslin DIV 2;
  IF (OUTPUT = TTYOUTPUT) AND NOT (listgo IN f) THEN (*pause for setup*)
    REPEAT
    UNTIL askyn('Ready to list? ');
$PAGE
  WHILE rdlin(lin) DO BEGIN (*process a line of input*)
    rtrim(lin); (*trim trailing spaces and tabs*)
    lincnt := lincnt + 1; (*lincnt now points to free buffer line*)
    wrapped := FALSE; (*assume not wrapped around*)
    WHILE (LENGTH(lin) > wid) AND (listwrap IN f) DO BEGIN (*wrap*)
      linbuf[lincnt] := SUBSTR(lin, 1, wid); (*save part of line*)
      IF lincnt < bufsiz THEN (*advance to next line in buffer*)
	lincnt := lincnt + 1;
      lin := SUBSTR(lin, wid + 1); (*advance to next part of line*)
      wrapped := TRUE (*flag line as wrapped*)
    END;
    IF wrapped AND (listwrap IN f) THEN (*last part of wrapped line*)
    (*note that wrapped part of line is placed at right margin*)
      linbuf[lincnt] := SUBSTR(spc, 1, wid - LENGTH(lin)) || lin
    ELSE (*normal placement of line in buffer*)
      linbuf[lincnt] := SUBSTR(lin || ' ', 1, MIN(LENGTH(lin), wid));
    IF EOPAGE THEN BEGIN (*a formfeed is a logical page break*)
      notsubpage := TRUE; (*signal logical page break*)
      newpage (*flush buffer*)
    END
    ELSE IF lincnt >= (laslin + 1) THEN BEGIN (*normal page break*)
    (*at this point there is one more line in the buffer than can be listed*)
    (*logical page if not checking for sub-pages*)
      notsubpage := NOT (listlogical IN f);
      blkend := lincnt; (*mark end of block*)
      FOR lincnt := blkend DOWNTO 1 DO (*search up for blank line*)
    EXIT IF linbuf[lincnt] = '';
      IF (lincnt <= (laslin DIV 2)) AND NOT ((listbreak IN f) OR (listclean IN f)) THEN
      (*break at last line*)
	blkbgn := blkend - 1
      ELSE (*break at nearest blank line*)
	blkbgn := lincnt;
      lincnt := blkbgn - 1; (*reset line count to end of list page*)
      newpage; (*flush buffer, save block*)
      LOOP (*skip initial blank lines of block*)
      EXIT IF blkbgn > blkend;
      EXIT IF linbuf[blkbgn] <> '';
	blkbgn := blkbgn + 1
      END;
      lincnt := 1; (*patch compiler bug*)
      FOR lincnt := 1 TO blkend - blkbgn + 1 DO (*shift block up*)
	linbuf[lincnt] := linbuf[lincnt + blkbgn - 1];
	(*point line counter to last line of shifted block*)
      lincnt := lincnt - 1
    END
  END;
  IF lincnt > 0 THEN (*flush partial page in buffer*)
    newpage;
  IF phypag < 1 THEN (*always print at least one page*)
    newpage
  ELSE IF (listeven IN f) AND ((phypag MOD 2) = 1) THEN (*even up listing*)
    newpage
END.
    