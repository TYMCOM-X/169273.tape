$WIDTH=100
$LENGTH=55
$TITLE DEBLEX.PAS, last modified 11/28/83, zw
MODULE deble$ OPTIONS NOCHECK, SPECIAL(WORD);
(*debugger lexical analyzer*)

(*note: must expand scalar_cst*)

$HEADER DEBLEX.HDR

$INCLUDE debug.typ
$INCLUDE debug.inc
$INCLUDE debio.inc
$PAGE lex$scan - constant tables
public procedure lex$scan (var lex_scan_rec: lex_scan_rec_type;
			  var status:	    status_code);

  const
    tab = chr(11b);
    cr  = chr(15b);

    rw_table: array [1..9] of record
				word_text: packed array [1..6] of char;
				tkind: token_kind
			      end := (
	('AND   ', andop  ),
	('BEGIN ', beginsy),
	('ELSE  ', elsesy ),
	('END   ', endsy  ),
	('IF    ', ifsy   ),
	('IN    ', inop   ),
	('NOT   ', notsy  ),
	('OR    ', orop   ),
	('THEN  ', thensy ) );

    prel_class: packed array [' '..'~' (* tilde *)] of token_kind :=  (
        otherssy,   otherssy,   stringconst,intconst,   otherssy,   otherssy,   
	otherssy,   stringconst,lparent,    rparent,    asterisk,   plus,       
	comma,      minus,      period,     slash,      intconst,   intconst, 
	intconst,   intconst,   intconst,   intconst,   intconst,   intconst,   
	intconst,   intconst,   colon,      semicolon,  ltop,       eqop,        
	gtop,       otherssy,   atsign,     ident,      ident,      ident,      
	ident,      ident,      ident,      ident,      ident,      ident,      
	ident,      ident,      ident,      ident,      ident,      ident,      
	ident,      ident,      ident,      ident,      ident,      ident,      
	ident,      ident,      ident,      ident,      ident,      lbrack,    
	otherssy,   rbrack,     arrow,      otherssy,   otherssy,   ident,   
	ident,      ident,      ident,      ident,      ident,      ident,      
	ident,      ident,      ident,      ident,      ident,      ident,      
	ident,      ident,      ident,      ident,      ident,      ident,      
	ident,      ident,      ident,      ident,      ident,      ident,      
	ident,      otherssy,   otherssy,   otherssy,   otherssy  );

    digits: array [radix_type] of set of char :=  (* chars to scan, not just those legal *)
              (['0'..'9'], ['0'..'9'], ['0'..'9', 'A'..'F']);
$PAGE lex$scan - locals
  type
    state_type = (decimal_integer, final_hex_int,
		  int_with_e, int_with_dot, int_ellipsis,
		  real_or_hex, real_with_dot, final_real,
		  e_sign, real_with_e, cant_continue); (* for number-scanning automaton *)
  var 
    charcnt:	0..maximum (cursor_range);
    i:		lowerbound (rw_table)..upperbound (rw_table);
    lit_delim,			(* remembers expected terminating string delimiter *)
    cur_ch:	char;	(* holds char from last call to next_char *)
    radix:	radix_type;
    state,
    next_state:	state_type;
    accepted:	(int_token, real_token);
    start,
    cursor_at_accepted:	cursor_range; (* to record positions of start and end of tokens *)

  const
    accept_int: set of state_type :=
	[decimal_integer, final_hex_int, int_with_e, int_ellipsis, real_or_hex];
    accept_real: set of state_type :=
	[int_with_dot, real_or_hex, real_with_dot, final_real];
    accepting_states: set of state_type := accept_int + accept_real;
$PAGE next_char, uc_next_char - in lex$scan
(* NEXT CHAR places the next character of the command string into
   CUR_CH.  If at the end of the string, a carriage return is
   placed in CUR_CH.  *)

procedure next_char;

  begin
    with lex_scan_rec do begin
      if cursor <= length (cmd_line) then
	cur_ch := cmd_line [cursor]
      else
	cur_ch := cr;
      cursor := cursor + 1 (* advance to next avail char (if there is one) *)
    end
  end;



(* UC NEXT CHAR is a convenient wrapper. *)

procedure uc_next_char;

  begin
    next_char;
    cur_ch := uppercase (cur_ch)
  end;
$PAGE lex$scan - body
begin
  with lex_scan_rec do begin
    if cursor = cursor_at_next_token then begin
      cursor := cursor_after_token;
      return	(* <-- I've already scanned the next token! *)
    end;
  
    cursor_at_next_token := cursor; (* so I can detect later request for same token *)
    repeat
      uc_next_char
    until (cur_ch <> ' ') and (cur_ch <> tab); (* obtain first char of new token *)
    if (cur_ch < lowerbound (prel_class)) orif (cur_ch > upperbound (prel_class)) then
      tkind := otherssy
    else	(* printing character *)
      tkind := prel_class [cur_ch];	(* preliminary classification of token *)


    (* look at cases where preliminary classification on basis of
       first character isn't adequate *)

    case cur_ch of

      cr:
	tkind := eofsy;


      '.': begin
	next_char; (* lookahead *)
	if cur_ch = '.' then
	  tkind := elipsis
	else
	  cursor := cursor - 1
      end;


      ':': begin
	next_char; (* lookahead *)
	if cur_ch = '=' then
	  tkind := becomes
	else
	  cursor := cursor - 1
      end;


      '<': begin
	next_char; (* lookahead *)
	if cur_ch = '>' then
	  tkind := neop
	else if cur_ch = '=' then
	  tkind := leop
	else
	  cursor := cursor - 1
      end;


      '>': begin
	next_char; (* lookahead *)
	if cur_ch = '=' then
	  tkind := geop
	else
	  cursor := cursor - 1
      end;


      'A'..'Z': begin
	start := cursor - 1; (* position of cur_ch *)
	repeat
	  uc_next_char
	until not (cur_ch in ['A'..'Z', '0'..'9', '_']);
	cursor := cursor - 1; (* wipe out 1 char lookahead *)
	charcnt := cursor - start; (* last char of id is at cursor - 1 *)
	if charcnt > upperbound (id_string) then begin	(* id was truncated *)
	  charcnt := upperbound (id_string);
	  writ$str ('Warning - identifier truncated to ');
	  writ$int (upperbound (id_string), decimal_radix);
	  writ$nl (' characters.')
	end;
	next_token.id_text := uppercase (cmd_line [start : charcnt]);

	(* lookup id in reserved word table *)

	for i := lowerbound (rw_table) to upperbound (rw_table) do
	  exit if rw_table[i].word_text = next_token.id_text do
	    tkind := rw_table[i].tkind;
	next_token.tkind := tkind (* we've changed next_token *)
      end;


      '''', '"': begin
	lit_delim := cur_ch;   (* save delimiter *)
	charcnt := 0;
	loop
	  loop
	    next_char;
	   exit if (cur_ch = lit_delim) or (cur_ch = cr);
	    charcnt := charcnt + 1;
	    next_token.cons_node.str_val [charcnt] := cur_ch
	  end;
	  if cur_ch <> cr then
	    next_char
	  else
	    status := no_str_delim; (* ERROR - delimiter missing *)
	 exit if cur_ch <> lit_delim;
	  charcnt := charcnt+1;
	  next_token.cons_node.str_val [charcnt] := lit_delim
	end;
	cursor := cursor - 1; (* wipe out 1 char lookahead *)
	next_token.cons_node.str_len := charcnt;
	next_token.cons_node.str_varying_ref := false;
	next_token.cons_node.kind := string_cst;
	next_token.tkind := tkind (* we've changed next_token *)
      end;


      '#': begin
	uc_next_char; (* should be radix code *)
	if not (cur_ch in ['O', 'D', 'H']) then (* no "B" until compiler FULLY supports it *)
	  status := bad_radix (* ERROR - bad radix code *)
	else begin
	  start := cursor; (* position of first digit *)
	  if      cur_ch = 'O' then radix := octal_radix
	  else if cur_ch = 'D' then radix := decimal_radix
	  else if cur_ch = 'H' then radix := hex_radix;
	  repeat
	    uc_next_char
	  until not (cur_ch in digits [radix]);
	  cursor := cursor - 1; (* wipe out 1 char lookahead *)
	  charcnt := cursor - start; (* last digit is at cursor - 1 *)
	  if charcnt = 0 then
	    status := bad_digits (* ERROR - no digits *)
	  else with next_token do begin
	    id_text := cmd_line [start : charcnt];
	    case radix of
	      octal_radix:   getstring (id_text, cons_node.scalar_val[1]: charcnt: o);
	      decimal_radix: getstring (id_text, cons_node.scalar_val[1]: charcnt);
	      hex_radix:     getstring (id_text, cons_node.scalar_val[1]: charcnt: h)
	    end;
	    case iostatus of
	      io_novf: status := too_many_bits; (* ERROR - too big *)
	      io_dgit: status := bad_digits; (* ERROR - improper digits for radix *)
	      others:  status_in_given_radix := success (* presumably io_ok *)
	    end;
	    status_in_addr_radix := status_in_given_radix;
	    address_value := cons_node.scalar_val [1];
	    cons_node.kind := scalar_cst
	  end;
	  next_token.tkind := tkind (* we've changed next_token *)
	end
      end;


      '0'..'9': begin
	start := cursor - 1; (* position of cur_ch *)
	next_state := decimal_integer; (* start state of number-scanning automaton *)

	repeat (* until cant_continue *)
	  uc_next_char; (* advance cur_ch - lookahead *)
	  state := next_state;
	  next_state := cant_continue;
	  if state in accepting_states then
	    cursor_at_accepted := cursor - 1; (* keep track of end of accepted token ... *)
	  if state in accept_real then (* ... and which kind it is *)
	    accepted := real_token
	  else if state in accept_int then
	    accepted := int_token;

	  case state of

	    decimal_integer:
	      if cur_ch = 'E' then
		next_state := int_with_e
	      else if cur_ch in ['A'..'D', 'F'] then
		next_state := final_hex_int
	      else if cur_ch = '.' then
		next_state := int_with_dot
	      else if cur_ch in ['0'..'9'] then
		next_state := decimal_integer;

	    final_hex_int:
	      if cur_ch in ['0'..'9', 'A'..'F'] then
		next_state := final_hex_int;

	    int_with_e:
	      if cur_ch in ['A'..'F'] then
		next_state := final_hex_int
	      else if cur_ch in ['0'..'9'] then
		next_state := real_or_hex
	      else if cur_ch in ['+', '-'] then
		next_state := e_sign;

	    int_with_dot:
	      if cur_ch = '.' then
		next_state := int_ellipsis
	      else if cur_ch = 'E' then
		next_state := real_with_e
	      else if cur_ch in ['0'..'9'] then	
		next_state := real_with_dot;

	    int_ellipsis:
	      cursor_at_accepted := cursor_at_accepted - 2;  (* backup *)

	    real_or_hex: begin
	      if address_radix = hex_radix then (* "arbitrarily" resolve ambiguity *)
		accepted := int_token
	      else
		accepted := real_token;
	      if cur_ch in ['A'..'F'] then
		next_state := final_hex_int
	      else if cur_ch in ['0'..'9'] then
		next_state := real_or_hex
	    end;

	    real_with_dot:
	      if cur_ch = 'E' then
		next_state := real_with_e
	      else if cur_ch in ['0'..'9'] then
		next_state := real_with_dot;

	    real_with_e:
	      if cur_ch in ['+', '-'] then
		next_state := e_sign
	      else if cur_ch in ['0'..'9'] then
		next_state := final_real;

	    e_sign:
	      if cur_ch in ['0'..'9'] then
		next_state := final_real;

	    final_real:
	      if cur_ch in ['0'..'9'] then
		next_state := final_real
	  end
	until state = cant_continue;

	cursor := cursor_at_accepted; (* wipe out 1 or more chars lookahead *)
	charcnt := cursor_at_accepted - start;
	with next_token do begin
	  id_text := cmd_line [start : charcnt];
	  if accepted = real_token then begin
	    lex_scan_rec.tkind := realconst;
	    cons_node.kind := real_cst;
	    getstring (id_text, cons_node.real_val[1]);
	    cons_node.real_prec := search (id_text, ['E', 'e'], charcnt + 1) - 1;
	    if index (id_text, '.') > 0 then
	      cons_node.real_prec := cons_node.real_prec - 1
	  end
	  else begin
	    cons_node.kind := scalar_cst;
	    getstring (id_text, cons_node.scalar_val[1]: charcnt);
	    case iostatus of
	      io_novf: status_in_given_radix := too_many_bits;
	      io_dgit: status_in_given_radix := bad_digits;
	      others:  status_in_given_radix := success (* presumably io_ok *)
	    end;
	    if address_radix = octal_radix then
	      getstring (id_text, address_value: charcnt: o)
	    else if address_radix = hex_radix then
	      getstring (id_text, address_value: charcnt: h)
	    else
	      a$$ert (false);
	    case iostatus of
	      io_novf: status_in_addr_radix := too_many_bits;
	      io_dgit: status_in_addr_radix := addr_radix_expected;
	      others:  status_in_addr_radix := success (* presumably io_ok *)
	    end
	  end
	end (* with next_token *);
	next_token.tkind := tkind (* we've changed next_token *)
      end
    end (* case cur_ch *);
  
    cursor_after_token := cursor
  end
end (* lex$scan *);
$PAGE lex$keyword
(* LEX$KEYWORD is a convenience for scanning command names, and display command
   keywords.  Its function is similar to LEX$SCAN's processing of identifiers,
   with the following exceptions:
	- it is more forgiving about what precedes the keyword.  It will scan
	  over control characters in addition to the blanks and tabs that
	  LEX$SCAN would skip over.
	- only letters are accepted as part of the token, whereas LEX$SCAN would
	  eat up numbers and underscores as well.  This means that a command
	  such as ".cle2" can be accepted as if it had been properly entered as
	  ".cle 2".
	- LEX$SCAN makes special recognition of "reserved words".  This routine
	  does not, so this routine doesn't forbid "begin", for ex., being a
	  command name (which isn't to say that that would be desirable).
	- LEX_SCAN_REC isn't changed, except for the CURSOR field.   *)

public procedure lex$keyword (var lex_scan_rec: lex_scan_rec_type;
			      var keyword:      id_string);

  var
    prev_cursor: cursor_range;

  begin
    keyword := '';
    with lex_scan_rec do begin
      while (cursor <= length (cmd_line)) andif (cmd_line [cursor] <= ' ') do
	cursor := cursor + 1;
      if (cursor > length (cmd_line)) orif not (uppercase (cmd_line [cursor]) in ['A'..'Z']) then
	return; (* <-- return *)

      prev_cursor := cursor; (* remember where keyword starts *)
      repeat
	cursor := cursor + 1
      until (cursor > length (cmd_line)) orif
		not (uppercase (cmd_line [cursor]) in ['A'..'Z']);

      keyword := uppercase (cmd_line [prev_cursor: cursor - prev_cursor])
    end
  end (* lex$keyword *).
 