$TITLE PRTRW.PAS last modified 5/9/84, zw
PROGRAM prtrw;
(*TYM-Pascal Pretty Printer Reserved Words*)

CONST
line_size = 78;

TYPE
int_val = 0 .. 200;

VAR
name: STRING [20];
len: 0 .. 20;
ind: int_val;
i: 0 .. 21;
word: ARRAY [1..200] OF STRING [20];
nwords: int_val := 0;
size_ind: ARRAY [1..21] OF 1..200 :=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,
  1);
out_count: 0 .. line_size := 0;
separator: ARRAY[BOOLEAN] OF STRING [2] :=(', ', ');');

PROCEDURE get_name;
(*GET_NAME sets the string variable NAME to the first 20 characters of the
    next line of the input file, if there are that many.*)
VAR
ind: 0 .. 20;
buf: PACKED ARRAY [1..20] OF CHAR;
BEGIN
  ind := 0;
  WHILE NOT EOLN(INPUT)AND(ind <> 20)DO BEGIN
    ind := ind + 1;
    buf [ind] := INPUT^;
    GET(INPUT);
  END;
  READLN(INPUT);
  name := SUBSTR(buf,1,ind);
END;

PROCEDURE new_line;
(*NEW_LINE starts a new output line.*)
BEGIN
  WRITELN;
  out_count := 0;
END;

PROCEDURE put_string(s: STRING);
(*PUT_STRING writes a string to the output, starting a new line if there
    isn't enough for the string on the current line.*)
BEGIN
  IF out_count + LENGTH(s)> line_size THEN BEGIN
    new_line;
    WRITE('  ');
    out_count := 2;
  END;
  WRITE(s);
  out_count := out_count + LENGTH(s);
END;

FUNCTION intstring(i: int_val): STRING;
(*INTSTRING returns the digit string representing a given integer.*)
VAR
i1: int_val;
BEGIN
  i1 := i;
  intstring := '';
  REPEAT
    intstring := CHR((i1 MOD 10)+ORD('0'))|| intstring;
    i1 := i1 DIV 10;
  UNTIL i1 = 0;
END;

BEGIN
  REWRITE(TTYOUTPUT);
  WRITELN(TTYOUTPUT, '[building PRTRW.TYP]');
  BREAK;
  RESET(INPUT,'PRTRW.TAB');
  (*Read the list of names.*)
  WHILE NOT EOF DO BEGIN
    get_name;
    len := LENGTH(name);
    FOR ind := nwords DOWNTO size_ind [len+1] DO word [ind+1] := word [ind];
    FOR i := len + 1 TO 21 DO size_ind [i] := size_ind [i] + 1;
    word [size_ind[len+1]-1] := LOWERCASE(name);
    nwords := nwords + 1;
  END;
  (*Generate the reserved word tables.*)
  REWRITE(OUTPUT,'PRTRW.TYP');
  WRITELN('CONST');
  WRITELN('maxrw = ',intstring(nwords+20),';');
  WRITELN('max_rw_len = 20;');
  WRITELN();
  WRITELN('TYPE');
  WRITELN('token_kind =');
  put_string('(');
  put_string('lparensy, ');
  put_string('rparensy, ');
  put_string('lbracketsy, ');
  put_string('rbracketsy, ');
  put_string('commentsy, ');
  put_string('dollarsy, ');
  put_string('colonsy, ');
  put_string('semicolonsy, ');
  put_string('commasy, ');
  put_string('endfsy, ');
  FOR i := 1 TO 20 DO BEGIN
    FOR ind := size_ind [i] TO size_ind [i+1] - 1 DO put_string(word[ind]||
      'sy, ')
  END;
  put_string('etc);');
  WRITELN();
  WRITELN('VAR');
  WRITELN('nrw: array [1..21] of 1..',intstring(nwords+21),' :=');
  put_string('(');
  FOR i := 1 TO 21 DO put_string(intstring(size_ind[i]+i-1)||separator[i=21]);
  new_line;
  WRITELN('rw: ARRAY[1..',intstring(nwords+20),'] OF STRING[20] :=');
  put_string('(');
  FOR i := 1 TO 20 DO BEGIN
    FOR ind := size_ind [i] TO size_ind [i+1] - 1 DO put_string(''''||
      UPPERCASE(word[ind])||''', ');
    put_string(''''''||separator[i=20]);
  END;
  new_line;
  WRITELN('rwsym: ARRAY[1..',intstring(nwords+20),'] OF token_kind :=');
  put_string('(');
  FOR i := 1 TO 20 DO BEGIN
    FOR ind := size_ind [i] TO size_ind [i+1] - 1 DO put_string(word[ind]||
      'sy, ');
    put_string('etc'||separator[i=20]);
  END;
END.
    