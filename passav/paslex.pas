(*   +--------------------------------------------------------------+
     I                                                              I
     I                        P A S L E X                           I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+

     PURPOSE: Lexical analyzer.  Maintains source files and generates
        source   listing.  Processes  directive  lines,  and  handles
        conditional comments.

     ENTRY POINTS: 

        lex_init   initializes package and opens main source file.

        scan       returns next token in input.

        lex_line   returns text of an input line if available.

        lex_token  returns text of  a  token as  it  appears  in  the
                   input line, if available.

     ---------------------------------------------------------------- *)
$PAGE includes
$include pascal.inc
$include pasist.inc
$include paspt.typ
$include paserr.inc
$include pasfil.inc
$include pasutl.inc
$INCLUDE pasval.inc
$include corout.inc
$include pasrdr.inc
$PAGE declarations

public var      (* information available to rest of parse *)
  token: token_type;                    (* last token scanned *)
  last_token: token_type;               (* previous token *)

static var
  lindex: line_index;                   (* line scanning cursor *)
$PAGE local definitions
(* Lexical analysis state information and type definitions. *)

const max_comment_level = 8;

static var
  comment_level: 0..max_comment_level;  (* current depth of nested comments *)
  comment_start: array [0..max_comment_level] of source_id;
                                        (* line at which each level of comment begins *)
$PAGE lexicon

$INCLUDE pasrw.inc
$PAGE lex_init
(* LEX INIT initializes the lexical analysis state information. *)

public procedure lex_init;
 begin
  line := '  ';                 (* prime next line to get line on first call *)
  lindex := 1;
  literal_line := '';

  with token do begin
    sym := badsymbol;                   (* field tested on entry to scan *)
    dummy := false;                     (* no text for token *)
    source := cur_source;               (* file 0, page 0, line 0 *)
    next := nil;
    defn := nil;
  end;
  last_token := token;

  comment_level := 0;

 end;
$PAGE scan_number
(* SCAN NUMBER parses an integer or real number, and sets up the token
   information for what it sees. *)

procedure scan_number;

 procedure scan_digits;
  begin
   lindex := lindex + verify (substr (line, lindex), ['0'..'9']) - 1;
  end;

 var
   ndigits: int_type;

 begin
  token.sym := intconst;		(* assume integer to start with *)
  scan_digits;                                (* get integer part, if any *)
  ndigits := lindex - token.column;
  if line[lindex] = 'B' then begin (* octal constant: nnnnnnB *)
    token.value := cst_int (substr (line, token.column, ndigits), 8, token.column);
    lindex := lindex + 1;
  end
  else begin (* decimal integer or real constant *)
    if (line[lindex] = '.')           (* check if there is a decimal point *)
	and (line[lindex+1] <> '.') then begin        (* but not an elipsis *)
      token.sym := realconst;
      lindex := lindex + 1;
      scan_digits;			(* scan fraction *)
      ndigits := lindex - token.column - 1;
    end;
    if line[lindex] = 'E' then begin  (* has an exponent field *)
      token.sym := realconst;
      lindex := lindex + 1;
      if line[lindex] in ['+', '-'] then lindex := lindex + 1;
      scan_digits;
    end;
    with token do begin
      if sym = intconst
	then value := cst_int (substr (line, column, ndigits), 10, column)
	else value := cst_real (substr (line, column, lindex - column), ndigits);
    end;
  end;
 end;
$PAGE scan
(* SCAN reads the next token of input, bypassing white space and comments
   and fills in the corresponding information in the public var TOKEN. *)

public function scan: symbols;

  label
    100 (* eof *),
    200 (* reserved word found *);


  (* Entered when an EOF is encountered on the master file. This sets
     the token info to eof and forces an exit of scan. *)

  procedure set_eof;
   begin
    (* leave "cur_source" at logical end of main file for other errors *)
    token.dummy := true;                (* eof token has no text, anyway *)
    token.sym := eofsy;
    if comment_level > 0 then           (* comment unclosed *)
      err_print (err_unclosed_comment, comment_start[comment_level], '', 0);
    goto 100  (* EOFABORT *)
   end;


  (* Procedure to read a new logical line into the line buffers. Filters and
     processes directive lines including $INCLUDE. Returns if a new line can
     be found. Calls SETEOF if there is no remaining text. *)

  procedure next_line;
   begin
     if end_of_file then set_eof;               (* does not return *)
     call (reader);                             (* fetch new line *)
     lindex := 1;
     token.source := cur_source;
   end;


  (* PUSH COMMENT enters a level of comment nesting by incrementing the
     comment level counter and recording the statement at which the comment
     starts *)

  procedure push_comment;
   begin
    if comment_level = max_comment_level then begin
      error (err_comments_too_deep);
      stop
    end;
    comment_level := comment_level + 1;
    comment_start [comment_level] := cur_source
   end;
$PAGE scan mainline
 var l: line_index;
     idx: int_type;
     word: rwordtype;
     text: line_string;
     radix: int_type;

 begin
  last_token := token;                          (* save for error recovery *)
  token.dummy := false;                         (* assume token has text *)
  if token.sym <> eofsy then begin              (* once in eof state, stay there *)
    loop (* until good symbol found *);

      (*  Ignore white space and comments.  *)

      loop
        loop
          l := verify (substr (line, lindex), [' ']);
        exit if l <> 0;
          next_line
        end;
        lindex := lindex + l - 1;
      exit if (line [lindex] <> '(') or (line[lindex+1] <> '*');
        push_comment;
        lindex := lindex + 2;
        if (lindex <= length (line)) andif (line[lindex] = '$') then
          err_print (err_cmt_dollar_option, cur_source, '$', lindex);
        repeat                                  (* until back at starting level *)
          loop                                  (* search for comment delims *)
            l := search (substr (line, lindex), ['(','*']);
          exit if l <> 0;
            next_line
          end;
          lindex := lindex + l - 1;             (* position at first delim *)
          if (line[lindex] = '(') and (line[lindex+1] = '*')
            then begin
              push_comment;
              lindex := lindex + 2;
              if (lindex <= length (line)) andif (line[lindex] = '$') then
                err_print (err_cmt_dollar_option, cur_source, '', lindex);
            end
          else if (line[lindex] = '*') and (line[lindex + 1] = ')')
            then begin
              comment_level := comment_level - 1;
              lindex := lindex + 2;
            end
          else lindex := lindex + 1             (* not comment start or stop, reconsider next char *)
        until comment_level = 0
      end;

      token.column := lindex;                   (* remember where token appears *)
      token.length := 1;                        (* set in case of errors *)
      case line [lindex] of

        'A'..'Z','_','$':                       (* identifier or reserved word *)
            begin
              l := verify (substr (line, lindex), ['A'..'Z','_','$','0'..'9']) - 1;
              lindex := lindex + l;
              if l < upperbound (frw) then begin
                word := substr (line, lindex-l, l);   (* pad and align for quick comparison *)
                for idx := FRW[l] to FRW[l+1]-1 do
                  if RW[idx] = word then begin
                    token.sym := RSY[idx];
                    if ROP[idx] <> noop
                      then token.op := ROP[idx];
                    goto 200
                  end
              end;                              (* exit if too long, or not found in RW list *)
              token.sym := ident;
              token.name := entername (substr (line, lindex - l, l));
            200: end;

        '''':
            begin                               (* character string constant *)
              token.sym := stringconst;
              lindex := lindex + 1;
              text := '';
              loop
                l := index (substr (line, lindex), '''');
                if l <> 0
                  then begin                    (* found second quote *)
                    text := text || substr (literal_line, lindex, l-1);
                    lindex := lindex + l
                  end
                  else begin                    (* unbalanced quote *)
                    token.sym := badsymbol;
                    err_print (err_unbalanced_quote, cur_source, '', length (line));
                    lindex := length (line)             (* force fallout *)
                  end;
              exit if line[lindex] <> '''';             (* exit if not doubled quote *)
                text := text || '''';
                lindex := lindex + 1
              end;
              if length (text) = 1
                then token.value := cst_scalar (ord (text[1]))
                else token.value := cst_string (text)
            end;

        '0'..'9':
            scan_number;                        (* entered in other places too *)

        '.':
            begin
              if line[lindex+1] = '.'
                then begin
                  token.sym := elipsis;
                  lindex := lindex + 2
                end
              else if line[lindex+1] in ['0'..'9']
                then scan_number                (* starts with a decimal point *)
              else
                begin
                  token.sym := period;
                  lindex := lindex + 1
                end
            end;

	'#':
	    begin
	      lindex := lindex + 1;
	      case line[lindex] of
		'B': radix := 2;
		'O': radix := 8;
		'D': radix := 10;
		'H': radix := 16;
		others: radix := 0
	      end;
	      if radix = 0 then
		token.sym := badsymbol
	      else begin
		token.sym := intconst;
		lindex := lindex + verify (substr (line, lindex + 1), ['0'..'9', 'A'..'F']);
		token.value := cst_int (substr (line, token.column + 2, lindex - token.column - 2),
					radix, token.column);
	      end;
	    end;

        ':':
            begin
              lindex := lindex + 1;
              if line[lindex] = '='
                then begin
                  token.sym := becomes;
                  lindex := lindex + 1
                end
                else token.sym := colon
            end;

        '<','>':
            begin
              token.sym := relop;
              token.op := sop[line[lindex]];
              lindex := lindex + 1;
              if line[lindex] = '=' then begin          (* case of >= <= *)
                if token.op = ltop
                  then token.op := leop
                  else token.op := geop;
                lindex := lindex + 1
              end
            else if (line [lindex] = '>') and (token.op = ltop)
              then begin
                token.op := neop;
                lindex := lindex + 1
              end
            end;

        '*':
            begin
              lindex := lindex + 1;
              if line[lindex] = '*' then begin
                token.sym := powerop;
                token.op := expon;                      (* case of ** *)
                lindex := lindex + 1;
              end
              else if line[lindex] = ')' then
                error (err_unopened_comment)
              else begin
                token.sym := mulop;
                token.op := mul;
              end;
            end;

        '|':
          begin
            lindex := lindex + 1;
            if line[lindex] = '|' then begin    (* concatenation operator *)
              token.sym := addop;
              token.op := catop;
              lindex := lindex + 1
            end
            else begin                          (* | not allowed *)
              token.sym := badsymbol;
            end
          end;

        others:                                 (* fetch data from table *)
            begin
              token.sym := ssy [line[lindex]];
              if sop[line[lindex]] <> noop
                then token.op := sop [line[lindex]];
              lindex := lindex + 1
            end

      end;                                      (* of case on line character *)
    exit if token.sym <> badsymbol;             (* of loop to get symbol *)
      err_print (err_bad_token, token.source,
                 substr (line, token.column, token.length), token.column)
    end
  end;                                          (* of if on eofsy *)
  token.length := lindex - token.column;        (* record length *)

(* EOFABORT *) 100:                             (* on abort, token.sym has been set *)
  scan := token.sym
 end.
