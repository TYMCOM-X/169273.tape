program prtrw;

(*  This program reads a list of the reserved words used by PRETTY, and produces
    the PRTRW.INC file containing the declarations for the reserved word table
    for the PRTINP module.  The list of reserved words is read from PRTRW.PAS--
    that is, from this source file.  The reserved word list is:

**********  Reserved Word Table  **********
BEGIN
OF
LOOP
REPEAT
WHILE
END
UNTIL
DO
EXIT
THEN
ELSE
IF
PROCEDURE
FUNCTION
CONST
TYPE
VAR
RECORD
LABEL
EXTERNAL
PUBLIC
STATIC
FORWARD
EXTERN
FORTRAN
OR
AND
ORIF
ANDIF
CASE
INITPROCED
PASCAL
**********  End Reserved Word Table  **********)


const
    line_size = 80;


type
    int_val = 0 .. 100;
var
    name: string [10];
    len: 0 .. 10;
    ind: int_val;
    i: 0 .. 11;
    word: array [1..100] of string [10];
    nwords: int_val := 0;
    size_ind: array [1..11] of 1..100 :=
	( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 );
    out_count: 0 .. line_size := 0;


  (*  GET_NAME sets the string variable NAME to the first 10 characters of the
      next line of the input file, if there are that many.  *)

  procedure get_name;
  var ind: 0 .. 10;
      buf: packed array [1..10] of char;
  begin
    ind := 0;
    while not eoln(input) and (ind <> 10) do begin
      ind := ind + 1;
      buf [ind] := input^;
      get (input);
    end;
    readln (input);
    name := substr (buf,1,ind);
  end (* get_name *);


  (*  NEW_LINE starts a new output line.  *)

  procedure new_line;
  begin
    writeln;
    out_count := 0;
  end (* new_line *);


  (*  PUT_STRING writes a string to the output, starting a new line if there
      isn't enough for the string on the current line.  *)

  procedure put_string ( s: string );
  begin
    if out_count + length(s) > line_size then begin
      new_line;
      write ('        ');
      out_count := 8;
    end;
    write (s);
    out_count := out_count + length(s);
  end (* put_string *);


  (*  INTSTRING returns the digit string representing a given integer.  *)

  function intstring ( i: int_val ): string;
  var i1: int_val;
  begin
    i1 := i;
    intstring := '';
    repeat
      intstring := chr((i1 mod 10)+ord('0')) || intstring;
      i1 := i1 div 10;
    until i1 = 0;
  end (* intstring *);


var separator: array [boolean] of string [3] := ( ', ', ' );' );


begin
  reset (input,'PRTRW.PAS');

  (*  Find the start of the key word list.  *)

  repeat get_name until name = '**********';

  (*  Read the list of names.  *)

  loop
    get_name;
  exit if name = '**********'; (* Check for the end of the list. *)
    len := length(name);
    for ind := nwords downto size_ind [len+1] do
      word [ind+1] := word [ind];
    for i := len + 1 to 11 do
      size_ind [i] := size_ind [i] + 1;
    word [size_ind[len+1]-1] := lowercase(name);
    nwords := nwords + 1;
  end (* loop *);

  (*  Generate the reserved word tables.  *)

  rewrite (output,'PRTRW.INC');
  writeln ('const');
  writeln ('    maxrw = ',intstring(nwords+10),';');
  writeln ('    max_rw_len = 10;');
  writeln ();
  writeln ('var');
  writeln ('    nrw: array [1..11] of 1..',intstring(nwords+11),' :=');
  put_string ('      ( ');
  for i := 1 to 11 do
    put_string (intstring(size_ind[i]+i-1)||separator[i=11]);
  new_line;
  writeln ('    rw: array [1..',intstring(nwords+10),'] of string [10] :=');
  put_string ('      ( ');
  for i := 1 to 10 do begin
    for ind := size_ind [i] to size_ind [i+1] - 1 do
      put_string (''''||uppercase(word[ind])||''', ');
    put_string (''''''||separator[i=10]);
  end;
  new_line;
  writeln ('    rwsym: array [1..',intstring(nwords+10),'] of token_kind :=');
  put_string ('      ( ');
  for i := 1 to 10 do begin
    for ind := size_ind [i] to size_ind [i+1] - 1 do
      if length(word[ind]) <= 8
	then put_string (word[ind]||'sy, ')
	else put_string (substr(word[ind],1,8)||'sy, ');
    put_string ('etc'||separator[i=10]);
  end;

end (* prtrw *).
