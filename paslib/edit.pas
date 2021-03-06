PROGRAM edit;

VAR
line1: INTEGER; {first line number}
line2: INTEGER; {second line number}
nlines: INTEGER; {# of line numbers specified}
curln: INTEGER; {current line -- value of dot}
lastln: INTEGER; {last line -- value of $}
pat: STRING; {pattern}

#include "match.p"
#include "makepat.p"

{prevln -- get line before n}
FUNCTION prevln
(n: INTEGER): INTEGER;
BEGIN
  IF (n <= 0) THEN prevln := lastln ELSE prevln := n - 1
END;

{nextln -- get line after n}
FUNCTION nextln
(n: INTEGER): INTEGER;
BEGIN
  IF (n >= lastln) THEN nextln := 0 ELSE nextln := n + 1
END;

{patscan -- find next occurrence of pattern after line n}
FUNCTION patscan
(way: character; VAR n: INTEGER): stcode;
VAR done: BOOLEAN; line: STRING;
BEGIN
  n := curln; patscan := err; done := FALSE;
  REPEAT
    IF (way = scan) THEN n := nextln(n) ELSE n := prevln(n);
    gettxt(n, line);
    IF (match(line, pat)) THEN BEGIN
      patscan := ok; done := TRUE
    END
  UNTIL (n = curln) OR (done)
END;

{optpat -- get optional pattern from lin[i], increment i}
FUNCTION optpat
(VAR lin: STRING; VAR i: INTEGER): stcode;
BEGIN
  IF (lin[i] = endstr) THEN i := 0
  ELSE IF (lin[i + 1] = endstr) THEN i := 0
  ELSE IF (lin[i + 1] = lin[i]) THEN i := i + 1 {repeated delimiter}
  ELSE i := makepat(lin, i + 1, lin[i], pat);
  IF (pat[1] = endstr) THEN i := 0;
  IF (i = 0) THEN BEGIN
    pat[1] := endstr; optpat := err
  END
  ELSE optpat := ok
END;

{skipbl -- skip blanks and tabs at s[i]...}
PROCEDURE skipbl(VARs: STRING; VAR i: INTEGER);
BEGIN
  WHILE (s[i] = blank) OR (s[i] = tab) DO i := i + 1
END;

{getnum -- get single line number component}
FUNCTION getnum
(VAR lin: STRING; VAR i, num: INTEGER; VAR status: stcode): stcode;
BEGIN
  status := ok; skipbl(lin, i);
  IF (isdigit(lin[i])) THEN BEGIN
    num := ctoi(lin, i); i := i - 1 {will advance at end}
  END
  ELSE IF (lin[i] = curline) THEN num := curln
  ELSE IF (lin[i] = lastline) THEN num := lastln
  ELSE IF (lin[i] = scan) OR (lin[i] = backscan) THEN BEGIN
    IF (optpat(lin, i) = err THEN status := err
    ELSE status := patscan(lin[i], num)
  END
  ELSE status := enddata;
  IF (status = ok) THEN i := i + 1;
  getnum := status
END;

{getone -- get one line number expression}
FUNCTION getone
(VAR lin: STRING; VAR i, num: INTEGER; VAR status: stcode): stcode;
VAR istart, mul, pnum: INTEGET=R;
BEGIN
  istart := i; num := 0;
  IF (getnum(lin, i, num, status) = ok) THEN REPEAT
    skipbl(lin, i);
    IF (lin[i] <> plus) AND (lin[i] <> minus) THEN status := enddata
    ELSE BEGIN
      IF (lin[i] = plus) THEN mul := +1 ELSE mul := -1;
      i := i + 1;
      IF (getnum(lin, i, pnum, status) = ok) THEN num := num + mul * pnum;
      IF (status = enddata) THEN status := err
    END
  UNTIL (status <> ok);
  IF (num < 0) OR (num > lastln) THEN status := err;
  IF (status <> err) THEN BEGIN
    IF (i <= istart) THEN status := enddata ELSE status := ok
  END;
  getone := status
END;

{getlist -- get list of line nums at lin[i], increment i}
FUNCTION getlist
(VAR lin: STRING; VAR i: INTEGER; VAR status: stcode): stcode;
VAR num: INTEGER; done: BOOLEAN;
BEGIN
  line2 := 0; nlines := 0;
  done := (getone(lin, i, num, status) <> ok);
  WHILE (NOT done) DO BEGIN
    line1 := line2; line2 := num; nlines := nlines + 1;
    IF (lin[i] = semicol) THEN curln := num;
    IF (lin[i] = comma) or (lin[i] = semicol) THEN BEGIN
      i := i + 1;
      done := (getone(lin, i, num, status) <> ok)
    END
  END;
  nlines := MIN(nlines, 2);
  IF (nlines = 0) THEN line2 := curln;
  IF (nlines <= 1) THEN line1 := line2;
  IF (status <> err) THEN status := ok;
  getlist := status
END;

{doprint

{dowrite

{getfn

{subst

{getrhs

{move

{ckp

{lndelete

{default



{docmd

{clrbuf

{doglob

{ckglob

{doread

{setbuf

{edit -- main routine for text editor}
PROCEDURE edit;
VAR cursave, i: INTEGER; status: stcode; more: BOOLEAN;
BEGIN
  setbuf; pat[1] := endstr; savefile[1] := endstr;
  IF (getarg(1, savefile, maxstr)) THEN BEGIN
    IF (doread(0, savefile) = err) THEN message('?')
  END;
  more := getline(lin, stdin, maxstr);
  WHILE (more) DO BEGIN
    i := 1; cursave := curlin;
    IF (getlist(lin, i, status) = ok) THEN BEGIN
      IF (ckglob(lin, i, status) = ok)
      THEN status := doglob(lin, i, cursave, status)
      ELSE IF (status <> err) THEN status := docmd(lin, i, false, status)
    END;
    IF (status = err) THEN BEGIN
      message('?'); curln := MIN(cursave, lastln)
    END
    ELSE IF (status = enddata) THEN more := FALSE;
    IF (more) THEN more := getline(lin, stdin, maxstr)
  END;
  clrbuf
END;

BEGIN
  edit
END.
  