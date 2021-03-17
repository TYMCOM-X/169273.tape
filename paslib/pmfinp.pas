$TITLE pmfinp -- character input and stacking routines
$LENGTH 43

module input_module;

$INCLUDE pmf.typ
$PAGE pmfput.inc file
$INCLUDE pmfput.inc
$PAGE pmferr.inc file
$INCLUDE pmferr.inc
$PAGE input description
(*  All character input to the macro program is done through the pmfinp
    module, via the 'get_char' function.  Normally, successive characters
    from the input file are returned.  (Special characters are returned
    to indicate the ends of lines and of the file.)  It is possible to
    save characters on a stack, however, so that characters from the stack
    will be returned in lieu of input file characters.  This feature has
    several main uses.  The lexical (token) routine can call 'get_char' to
    look at the next character, and then call 'put_back' to put that char-
    acter back in the input to be looked at again later if it is not of any
    immediate interest.  The macro expansion (pmfexp) module will place
    the expansion of a macro call back into the input, so that the call is
    literally replaced by its value in the input.  This is performed using
    'str_back' and 'arg_back', which place a substring of a definition or of
    an argument on the input stack.  The input stack is also used for temp-
    orary storage of parameter information during macro definition processing,
    using 'put_back'.  Note that, if 'a' and 'b' are characters or strings,
    the effect of saving 'a' and then saving 'b' on the input stack is the
    same as the effect of saving 'b||a', rather than 'a||b'.  The character
    END_EXP is never returned when it is found in the input stack.  Instead,
    it indicates that a call should be made to the 'end_call' routine in the
    'pmfput' module to signal the end of a macro expansion.  *)
$PAGE declarations
const
  in_stk_size = 5000;

public var
  line_text: packed array [1..256] of char; (* The text of the current line. *)
  line_no: integer;                              (* The line number of the current line. *)
  line_len: 0 .. 256;                             (* The number of characters read from the current line. *)


var
  in_stk: packed array [1..in_stk_size] of char;
  in_ptr: 0 .. in_stk_size;                  (* The first available stack location. *)
  t_ptr:  0 .. in_stk_size;                        (* The last used temporary stack location. *)

public procedure inpinit;

begin
  line_no := 0;
  in_ptr := in_stk_size;
  t_ptr := 0;
end (* inpinit *);
$PAGE get_char
(*  GET_CHAR returns the next input character.  This may be a character
    directly from the input, or it may be a character which has been saved
    on the input stack.  The character is returned both in the parameter
    and as the function result.  *)

public function get_char ( var c: char ): char;

  label
  (* get next character *) 100;

begin
                                            (*  get next character *) 
100:
  if in_ptr = in_stk_size then                   (* Input stack is empty -- read a character.  *)
    if eof(input) then
      c := eof_ch
    else if eoln(input) then
      begin
	c := eol;
	readln (input);
	if line_no > 0 then
	  line_no := - line_no                  (* Flag end of line. *)
	else begin
	  line_no := line_no - 1;
	  line_len := 0;
	end;
      end
    else
      begin
	read (input,c);
	if (c < space) andif (c <> tab) then
	  goto (* get next character *) 100;
	if line_no <= 0 then begin
	  line_len := 0;
	  line_no := - line_no + 1;
	end;
	line_len := line_len + 1;
	line_text [line_len] := c;
	if (c = '&') andif (eoln(input) orif (input^ = eol)) then begin
	  readln (input);
	  line_no := - line_no;
	  goto (* get next character *) 100;
	end
      end
  else                                          (* Take the character from the input stack. *)
    begin
      in_ptr := in_ptr + 1;
      c := in_stk [in_ptr];
      if c = end_exp then begin
	end_call;                               (* Signal end of macro expansion. *)
	goto (* get next character *) 100;
      end;
    end;
  get_char := c;
end (* get_char *);
$PAGE put_back, str_back,  blk_back
(*  PUT_BACK will store the character c on the input stack so that it
    will be returned by a subsequent call to GET_CHAR.  *)

public procedure put_back ( c: char );

begin
  if in_ptr = 0 then
    error (er_inp_ofl);                         (* Input stack overflow. *)
  in_stk [in_ptr] := c;
  in_ptr := in_ptr - 1;
end (* put_back *);


(*  STR_BACK will store the characters of the selected substring of
    the string block S on the input stack.  *)

public procedure str_back ( s: str_pointer; ind, len: string_index );

begin
  if len = 0 then return;
  if in_ptr - len < 0 then
    error (er_inp_ofl);                             (* Input stack overflow. *)
  in_ptr := in_ptr - len;
  in_stk [in_ptr+1:len] := substr(s^.str_text,ind,len);
end (* str_back *);


(*  BLK_BACK will store the characters of the selected substring of the
    evaluation block text T on the input stack.  *)

public procedure blk_back ( t: eval_text; ind, len: eval_index );

begin
  if len = 0 then return;
  if in_ptr - len < 0 then
    error (er_inp_ofl);                              (* Input stack overflow. *)
  in_ptr := in_ptr - len;
  in_stk [in_ptr+1:len] := substr(t,ind,len);
end (* blk_back *);
$PAGE tsave
(*  The input stack area is also used for another function.  The pmf syntax
    rules allow the name of a macro to be separated from its argument list by
    any amount of spacing or comments (not generally good style, but allowed
    for consistency with Pascal).  Since the argument list is optional,
    this means that when a macro name is encountered, any subsequent spacing
    and comments must be saved until it is learned whether or not the next
    significant token is a left parenthesis.  If it is, all the intermediate
    text may be discarded.  Otherwise, it must all be put back into the
    input so that it can be read again after the macro has been expanded.
    This function is accomplished by 'tsave', which saves tokens at the
    top end of the input stack area; 'tscratch', which discards all the
    saved text; and 'tkeep', which copies the saved material from the top
    of the stack area into the input stack proper.  'Put_back' is never
    called between a call to 'tsave' and the next call to 'tkeep' or to
    'tscratch'.  *)


(*  TSAVE will store the characters of the string S at the other end of
    the input stack area, using t_ptr.  TSAVE is used only by SETUP_CALL
    while scanning for a parameter list.  After a series of TSAVE calls,
    either TSCRATCH or TKEEP will be called.  Therefore, PUT_BACK will
    never be called when there is data saved at the end of the stack area,
    so PUT_BACK doesn't need to worry about t_ptr.  *)

public procedure tsave ( s: string_parm );

  var
    len: string_index;

begin
  len := length(s);
  if len = 0 then return;
  if t_ptr + len > in_ptr then
    error (er_tmp_ofl);                          (* Temp stack overflow. *)
  in_stk [t_ptr+1:len] := s;
  t_ptr := t_ptr + len;
end (* tsave *);
$PAGE tscratch  tkeep
(*  TSCRATCH will be called if it is known that the data stored with
    TSAVE is useless, and may be discarded.  *)

public procedure tscratch;

begin
  t_ptr := 0
end;



(*  TKEEP will be called if the data stored with TSAVE have turned out
    to be useful after all, and are to be stored on the regular input
    stack.  *)


public procedure tkeep;

begin
  if in_ptr - t_ptr >= t_ptr then           (* No overlap -- simple copy. *)
    begin
      in_ptr := in_ptr - t_ptr;
      in_stk [in_ptr+1:t_ptr] := in_stk;
    end
  else                                         (* Fields overlap -- copy backwards. *)
    for t_ptr := t_ptr downto 1 do begin
      in_stk [in_ptr] := in_stk [t_ptr];
      in_ptr := in_ptr - 1;
    end;
  t_ptr := 0;
end (* tkeep *).
 