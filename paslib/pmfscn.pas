$TITLE pmfscn -- primary file scanner with macro expansion
$LENGTH 43

module scan_module;

$INCLUDE pmf.typ
$PAGE pmfcmd.inc file
$INCLUDE pmfcmd.inc
$PAGE pmfinp.inc file
$INCLUDE pmfinp.inc
$PAGE pmfput.inc file
$INCLUDE pmfput.inc
$PAGE pmfdef.inc file
$INCLUDE pmfdef.inc
$PAGE pmfexp.inc file
$INCLUDE pmfexp.inc
$PAGE pmferr.inc file
$INCLUDE pmferr.inc
$PAGE module description
(*  The scanner module is responsible for parsing the stream of input
    characters which are produced by the input (pmfinp) module and for
    deciding what they "mean" and what to do with them.  At the scanner
    level, the input is made up, not of characters, but of tokens.  A
    token may be:  an alphanumeric symbol; a single special character
    or special character pair; a quoted string; or one line of a comment,
    from the start of the comment or the start of the line to the end of
    the comment or the end of the line.  Tokens are characterized by a
    their kind (what sort of token they are) and their source text.  *)

(*  The scanner acts as a dispatcher.  Most tokens are just sent to the
    output (pmfput) module.  Break flags are simply discarded.  Text in
    string quotes is sent to the output module, regardless of what it is.
    Macro calls are passed to a processing routine for expansion.  *)


const
  token_size = 256;

var
  token_text: string [token_size];

  token_kind:
    ( normal, symbolic, lquote, rquote, lparen, rparen, comment,
    arg_separator, break_flag, eval_flag, spacing, finish );


public procedure scanner; forward;
$PAGE get_token
(*  GET_TOKEN gets input characters with GET_CHAR, and assembles
    them into a token.  The text and kind of the token are returned
    in the global variables 'token_text' and 'token_kind'.  *)


var cmt_level: string_index;                      (* 'Cmt_level' records the current comment
						     nesting level.  It is initialized on entry
						     to SCANNER.  It tells GET_TOKEN whether
						     to get a new token or to keep scanning an
						     old comment each time it is called.  Note
						     that 'cmt_level' will be zero whenever
						     SCANNER calls CALL_MACRO, as well as when
						     CALL_MACRO or SCANNER returns, so no
						     problems can arise from the recursive calls
						     to SCANNER during processing of evaluated
						     arguments.  *)


procedure get_token;

  var
    c: char;
    text_buffer: packed array [1..token_size] of char;
    count: 0 .. token_size;

  label
  (* start *) 100;
$PAGE get_token:  scan_comment
  procedure scan_comment;

  begin
    if cmt_level = 0 then                           (* New comment. *)
      begin
	text_buffer [1:2] := '(*';
	cmt_level := 1;
	count := 2;
      end
    else                                              (* New line, old comment. *)
      count := 0;
    repeat
      if count = token_size then
	error (er_cmt_len);                      (* Comment line too long. *)
      count := count + 1;
      text_buffer [count] := get_char (c);
      if c = '(' then
	if get_char (c) = '*' then begin
	  if count = token_size then
	    error (er_cmt_len);                   (* Comment line too long. *)
	  count := count + 1;
	  text_buffer [count] := '*';
	  cmt_level := cmt_level + 1;
	end
	else
	  put_back (c)
      else if c = '*' then
	if get_char (c) = ')' then begin
	  if count = token_size then
	    error (er_cmt_len);                   (* Comment line too long. *)
	  count := count + 1;
	  text_buffer [count] := ')';
	  cmt_level := cmt_level - 1;
	end
	else
	  put_back (c)
      else if c = eof_ch then
	error (er_cmt_eof);                    (* Eof in comment. *)
    until (cmt_level = 0) or (c = eol);            (* Scan for end of comment or line. *)
    token_kind := comment;
    token_text := substr(text_buffer,1,count);
  end (* scan_comment *);
$PAGE get_token:  main routine
begin
                                            (* start *) 
100:

  if cmt_level <> 0 then
    scan_comment
  else begin
    token_text := get_char (c);
    case uppercase (c) of

      'A'..'Z', '_', '#':
	begin
	  count := 1;
	  text_buffer[1] := c;
	  while uppercase(get_char(c)) in alphanumerics do
	    begin
	      if count = token_size then
		error (er_tok_len);             (* Token too long. *)
	      count := count + 1;
	      text_buffer [count] := c;
	    end;
	  token_kind := symbolic;
	  token_text := substr(text_buffer,1,count);
	  put_back (c);
	end;

      '''':
	if pascal_switch in cmd_options.option_list then
	  begin
	    count := 0;
	    repeat
	      repeat
		if (c = eol) or (c = eof_ch) then
		  error (er_lit_eol);           (* End of line in literal string. *)
		if count = token_size then
		  error (er_lit_len);           (* Literal string too long. *)
		count := count + 1;
		text_buffer [count] := c;
	      until get_char (c) = '''';        (* Scan for a quote mark. *)
	      if count = token_size then
		error (er_lit_len);             (* Literal string too long. *)
	      count := count + 1;
	      text_buffer [count] := '''';
	    until get_char (c) <> '''';         (* Repeat while it is double. *)
	    token_kind := normal;
	    token_text := substr(text_buffer,1,count);
	    put_back (c);
	  end
	else
	  token_kind := normal;

      '<':
	if get_char(c) = ':' then
	  begin
	    token_kind := lquote;
	    token_text := '<:';
	  end
	else
	  begin
	    token_kind := normal;
	    put_back (c);
	  end;

      '(':
	if (get_char(c) = '*') and (pascal_switch in cmd_options.option_list) then
	  scan_comment
	else
	  begin
	    token_kind := lparen;
	    put_back (c);
	  end;

      ':':
	if get_char(c) = '>' then
	  begin
	    token_kind := rquote;
	    token_text := ':>';
	  end
	else
	  begin
	    token_kind := normal;
	    put_back (c);
	  end;

      '[':
	token_kind := lparen;

      ')',']':
	token_kind := rparen;

      ',':
	token_kind := arg_separator;

      eof_ch:
	token_kind := finish;

      '&':
	token_kind := break_flag;

      '"':
	token_kind := eval_flag;

      space, tab, eol:
	token_kind := spacing;

      others:
	token_kind := normal

    end (* case uppercase(c) *);
  end (* cmt_level = 0 *);
end (* get_token *);
$PAGE collect_args
(*  COLLECT_ARGS is called when the CALL_MACRO procedure has read the left
    parenthesis of a macro call argument list.  It has the job of scanning
    the argument list up to its closing parenthesis, collecting all the
    arguments with NEW_ARG and PUT_STRING calls, and evaluating any
    arguments which are marked with an evaluation flag (") by pushing them
    back into the input stack, terminated with an eof, and calling SCANNER
    to evaluate them.  *)

procedure collect_args;

  var
    eval_arg: boolean;                               (* Used to remember an evaluation flag. *)
    end_of_list: boolean;                   (* Used to remember the final right paren. *)
    plevel: string_index;                        (* Counts the parenthesis nesting level. *)
    qlevel: string_index;                   (* Counts the text quote nesting level. *)

begin
  repeat                                    (* Scan to the end of the argument list. *)
    new_arg;
    get_token;
    eval_arg := (token_kind = eval_flag);    (* Evaluated argument? *)
    if eval_arg then                         (* Yes--discard the flag character. *)
      get_token;
    plevel := 0;
    while (plevel <> 0) orif not (token_kind in [arg_separator,rparen]) do
      begin                                     (* Scan one argument. *)
	if token_kind = lparen then
	  plevel := plevel + 1
	else if token_kind = rparen then
	  plevel := plevel - 1
	else if token_kind = lquote then
	  begin                                 (* Copy quoted text verbatim. *)
	    qlevel := 1;
	    while qlevel <> 0 do begin
	      put_string (token_text);
	      get_token;
	      if token_kind = lquote then
		qlevel := qlevel + 1
	      else if token_kind = rquote then
		qlevel := qlevel - 1
	      else if token_kind = finish then
		error (er_str_eof);
	    end;
	  end
	else if token_kind = finish then
	  error (er_arg_eof);                   (* End of file in argument list. *)
	if token_kind <> comment then           (* Don't save comments in arg lists. *)
	  put_string (token_text);
	get_token;
      end;
    end_of_list := (token_kind = rparen);
    if eval_arg then begin                   (* Are we supposed to evaluate it? *)
      put_back (eof_ch);                    (* Yes--mark the end of it with an eof. *)
      arg_rescan;                           (* Put the argument back in the input. *)
      scanner;                                       (* Evaluate it. *)
    end;
  until end_of_list;
end (* collect_args *);
$PAGE call_macro
(*  CALL_MACRO is called when the name of a defined macro is read.  A
    call to NEW_CALL establishes the call stack data for the macro.  If
    it is a user-defined literal, it is immediately expanded.  Otherwise,
    the input is scanned for the first token which is not a space, eol,
    or comment.  If this token is not a left parenthesis, then there is
    no argument list, so the text which has been scanned is saved.  If the
    token is a left parenthesis, then the intervening text is discarded,
    and the argument list is scanned.  In either case, the macro call is
    then expanded.  *)

procedure call_macro ( defn: definition );

begin
  new_call (defn);
  if defn^.kind <> user_literal then begin       (* The macro may take an argument list. *)

    (*  Scan for the next significant token.  *)

    get_token;
    while token_kind in [spacing,comment] do
      begin
	tsave (token_text);
	get_token;
      end;

    (*  Do we have an argument list?  *)

    if token_kind = lparen then begin
      tscratch;                                    (* Yes--discard the intervening garbage. *)
      collect_args;                                (* Scan the argument list. *)
    end
    else begin
      tsave (token_text);                       (* No--save the token, whatever it was. *)
      tkeep;                                        (* Save all the text that came before it. *)
    end;
  end;
  expand (defn);                                (* Now expand the macro call. *)
end (* setup_call *);
$PAGE scanner
(*  SCANNER is the central routine of the program.  It reads tokens from
    input, performs macro processing as appropriate, and writes the tokens
    to output.  *)

procedure scanner;

  var
    defn: definition;
    quote_level: string_index;

begin
  cmt_level := 0;
  get_token;
  while token_kind <> finish do begin
    if token_kind = symbolic then
      begin
	defn := lookup(uppercase(token_text));
	if defn = nil
	  then put_string (token_text)
	  else call_macro (defn);
      end
    else if token_kind = lquote then
      begin
	quote_level := 1;
	loop
	  get_token;
	  if token_kind = lquote then
	    quote_level := quote_level + 1
	  else if token_kind = rquote then
	    quote_level := quote_level - 1
	  else if token_kind = finish then
	    error (er_str_eof);                 (* Eof in quoted text string. *)
	exit if quote_level = 0;
	  put_string (token_text);
	end;
      end
    else if token_kind <> break_flag then      (* Break flags just disappear. *)
      put_string (token_text);
    get_token;
  end (* while token_kind <> finish *);
end (* scanner *).
  