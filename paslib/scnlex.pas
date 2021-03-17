$TITLE SCNLEX -- SCANNR Lexical Scanner
$LENGTH 43

module scnlex;

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         S C N L E X                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  SCANNR Lexical Scanner Builder
     
     STARTED:  24 July 1978
     
     PURPOSE:  This module reads the input file and parse  the  input
        lines into tokens.
     
     ENTRY POINTS:
     
        lexinit     is called to initialize the lexical scanner.
     
        getsymbol   is  called by the parser module whenever it needs
                    a new token.  Getsymbol returns  the  next  token
                    type  in  Insymbol.  If  the  token  is a string,
                    name, or action then Getsymbol returns a  pointer
                    to  a string node containing its text in Invalue.
                    If the token is a number, then Getsymbol  returns
                    its value in Invalue.
     
        insymbol    is  the  type  of  the  last  token  found  by  a
                    Getsymbol call.
     
        invalue     may contain semantic  information  for  the  last
                    token found by a Getsymbol call.
     
     ---------------------------------------------------------------- *)
$PAGE declarations


$INCLUDE scannr.typ
$INCLUDE scnlit.typ
$INCLUDE scntok.typ

$INCLUDE scnerr


public var
    insymbol: token_type;
    invalue : sym_value;

var
    line_no: number; (* The current line number. *)
    line: string [line_size]; (* The current input line. *)
    column, (* Current position in line. *)
    token_col: line_index; (* Start of the current token. *)
    token_line: number; (* Line number of the current token. *)
$PAGE lexinit

(*  LEXINIT initializes the lexical scanner.  *)


public procedure lexinit;

begin
  line_no := 0;
  line := '';
  column := 1;
end (* lexinit *);
$PAGE get_next_line

(*  GET NEXT LINE will read the next line from the input file.  This routine
    should never be called if there isn't a next line.  *)


procedure get_next_line;

begin
  assert (not eof (input));

  (*  Read the next line.  *)

  read (input, line);
  line_no := line_no + 1;
  column := 1;

  (*  Check for input line truncation.  *)

  if not eoln (input) then
    err_loc (line_no, 0, 'Input line too long');

  readln (input);
end (* get_next_line *);
$PAGE error

(*  ERROR will cal ErrLoc to print a specified error message at the current
    token line and column position.  *)

procedure error ( msg: packed array [1..*] of char );

begin
  err_loc (token_line, token_col, msg);
end;
$PAGE getsymbol

(*  GetSymbol finds the next token in the input text.  It returns the token
    type in 'insymbol', and if the token is a literal string, name, number,
    or action string, it returns the token text in 'invalue.lit_val'.  *)


public procedure getsymbol;

var
    sym_str: string [10];
    numval: number;
    t_col,
    v_col,
    t_width,
    q_count: line_index;
    char_val: ord (minimum (char)) .. ord (maximum (char));
    act_list: str_list;

begin
  insymbol := nosymbol;

  while insymbol = nosymbol do begin
    if column > length (line) then begin
      if eof (input)
	then insymbol := eofsy
	else get_next_line;
    end

    else begin
      token_col := column; (* Record the current column number. *)
      token_line := line_no; (* Record the current line number. *)
      case line[column] of

	' ': (* Skip spaces *)
	  column := column + verify (substr (line, column + 1), [' '],
				     length (line) - column + 1);

	'A'..'Z', 'a'..'z', '_': (* Name *)
	  begin
	    column := column + verify (substr (line, column + 1),
				       ['A'..'Z', 'a'..'z', '0'..'9', '_'],
				       length (line) - column + 1);
	    sym_str := uppercase (substr (line, token_col, column - token_col));
	    if sym_str = 'SCANNER' then
	      insymbol := scannersy
	    else if sym_str = 'ALPHABET' then
	      insymbol := alphabetsy
	    else if sym_str = 'ASCII' then
	      insymbol := asciisy
	    else if sym_str = 'NUMERIC' then
	      insymbol := numericsy
	    else if sym_str = 'IGNORE' then
	      insymbol := ignoresy
	    else if sym_str = 'SYMBOLS' then
	      insymbol := symbolssy
	    else if sym_str = 'END' then
	      insymbol := endsy
	    else if sym_str = 'IS' then
	      insymbol := issy
	    else begin
	      insymbol := namesy;
	      new (invalue.lit_val, column - token_col);
	      invalue.lit_val^ := uppercase (substr (line, token_col, column - token_col) );
	    end;
	  end;

	'0'..'9': (* Number *)
	  begin
	    insymbol := numbersy;
	    column := column + verify (substr (line, column + 1),
				       ['0'..'9'], length (line) - column + 1);
	    getstring (substr (line, token_col, column - token_col), numval);
	    if iostatus = io_novf then begin
	      error ('Number too large');
	      invalue.num_val := maximum (invalue.num_val);
	    end
	    else
	      invalue.num_val := numval;
	  end;

	'{': (* Action *)
	  begin
	    insymbol := actionsy;
	    column := column + 1;
	    invalue.action_list := nil;
	    act_list := nil;
	    loop
	      t_col := column + verify (substr (line, column), [' '],
					length (line) - column + 2) - 1;
	      column := t_col + search (substr (line, t_col), ['}'],
					length (line) - t_col + 2) - 1;
	      if column <> t_col then begin (* Save an action line. *)
		if act_list = nil then begin (* The first line of the action. *)
		  new (act_list);
		  invalue.action_list := act_list;
		end
		else begin (* Subsequent action lines. *)
		  new (act_list^.next);
		  act_list := act_list^.next;
		end;
		new (act_list^.str, column - t_col); (* Create the string node. *)
		act_list^.str^ := substr (line, t_col, column - t_col);
		act_list^.next := nil;
	      end;

	    exit if column <= length (line); (* Closing "}" found. *)

	    exit if eof (input) do
	      error ('Unterminated action');

	      get_next_line;
	    end;
	    if invalue.action_list = nil then begin (* Null action. *)
	      new (invalue.action_list); (* Fake one null line. *)
	      with invalue.action_list^ do begin
		new (str, 0);
		next := nil;
	      end;
	    end;
	  end;

	'"': (* String *)
	  begin
	    q_count := 0; (* The number of interior quotes. *)
	    column := column + 1;

	    loop
	      column := column + search (substr (line, column), ['"'],
					 length (line) - column + 2) - 1;

	    exit if column > length (line) do
	      error ('Unterminated string');

	      column := column + 1;

	    exit if (column > length (line)) orif
		    (line[column] <> '"') do

	      insymbol := stringsy;
	      q_count := q_count + 1; (* Inner quote found. *)
	      column := column + 1;
	    end;

	    if insymbol = stringsy then begin
	      new (invalue.lit_val, column - token_col - q_count - 2);
	      t_col := token_col + 1;
	      v_col := 1;
	      loop
		t_width := search (substr (line, t_col), ['"']) - 1;
		substr (invalue.lit_val^, v_col, t_width) :=
		  substr (line, t_col, t_width);
		t_col := t_col + t_width + 1;
	      exit if t_col = column;
		invalue.lit_val^[v_col+t_width] := '"';
		v_col := v_col + t_width + 1;
		t_col := t_col + 1;
	      end;
	    end;
	  end;

	'/': (* Ascii character code *)
	  begin
	    column := column + verify (substr (line, column + 1),
				       ['0'..'9'], length (line) - column + 1);
	    if column = token_col + 1 then
	      error ('Invalid ascii character specification')
	    else begin
	      insymbol := stringsy;
	      new (invalue.lit_val, 1);
	      getstring (substr (line, token_col + 1, column - token_col - 1), char_val);
	      if iostatus = io_novf then begin
		error ('Character value too large');
		invalue.lit_val^[1] := maximum (char);
	      end
	      else
		invalue.lit_val^[1] := chr (char_val);
	    end;
	  end;

	',': (* Comma *)
	  insymbol := commasy;

	';': (* Semicolon *)
	  insymbol := semicolonsy;

	'=': (* Equal *)
	  insymbol := equalsy;

	'|': (* Or *)
	  insymbol := orsy;

	'&': (* And *)
	  insymbol := andsy;

	'-': (* Dash *)
	  insymbol := dashsy;

	'.': (* Ellipsis? *)
	  begin
	    column := column + 1;
	    if line[column] = '.'
	      then insymbol := ellipsissy
	      else error ('Invalid character');
	  end;

	'(':
	  begin
	    column := column + 1;
	    if (column <= length (line)) andif
	       (line[column] = '*') then begin (* Comment *)
	      column := column + 1;
	      loop
		column := column + index (substr (line, column), '*)',
					  length (line) - column + 1);
	      exit if column <= length (line) do column := column + 1;
	      exit if eof (input) do error ('Unterminated comment');
		get_next_line;
	      end;
	    end
	    else (* Left Parenthesis *)
	      insymbol := lparensy;
	  end;

	')': (* Right Parenthesis *)
	  insymbol := rparensy;

	'^': (* Lambda *)
	  insymbol := lambdasy;

	'?': (* Question Mark *)
	  insymbol := questionsy;

	'*': (* Star *)
	  insymbol := starsy;

	'@': (* Times *)
	  insymbol := timessy;

	'+': (* Plus *)
	  insymbol := plussy;

	'''': (* Not *)
	  insymbol := notsy;

	others:
	  begin
	    error ('Invalid character');
	    column := column + 1;
	  end;

      end (* case line[column] *);
    end (* if not eof (input) *);
  end (* while insymbol = nosymbol *);

  if not (insymbol in [namesy, numbersy, stringsy, lparensy, eofsy,
		       scannersy, alphabetsy, asciisy, numericsy,
		       symbolssy, endsy, issy]) then
    column := column + 1; (* Step past token. *)

  invalue.line_no := token_line;
  invalue.column_no := token_col;

end (* getsymbol *).
 