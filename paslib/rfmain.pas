$TITLE rfmain
$LENGTH 42
$OPTIONS special
$PAGE includes and externals
$include rfdata.inc

external procedure readline ( var command_words; var line_type );

external procedure justify ( var command_words; var line_type; points );

external procedure put_page;

external procedure put_skip (line_count);

external procedure put_need (line_count);

external procedure put_line (line_type);

external procedure wrline ( line_type );

external function rfword ( line_type; var line_index ): command_words;
$PAGE process_command

function process_command ( line: line_type ): boolean;

 label 100;				(* error exit *)
 var lindex: line_index;		(* parsing cursor *) 


 procedure error (msg: string);
  begin
   writeln (tty, msg);
   goto 100;
  end;


 procedure skip_blanks;
  begin
   with line^ do begin
     while (lindex <= textlen) andif (text[lindex].ch <= ' ')
       do lindex := lindex + 1
   end
  end;


 function endline: boolean;
  begin
   skip_blanks;
   with line^ do
     endline := (lindex > textlen);
  end;


 procedure check_end;
  begin
   if not endline
     then error ('Command not terminated.')
  end;


 function word: command_words;
  begin
   skip_blanks;
   word := rfword (line, lindex);
  end;


  (* SIGNED VALUE checks if an optionally signed valued is present. The
     value of the number and a sign indicator are returned.  (If no value
     is present, 0 and false are returned.)  The value of the function
     indicates if a value is present.  This errors only if no number appears
     after a sign. *)

  function signed_value (var num: integer; var signed: boolean): boolean;
   var sign: integer;
   begin
    with line^ do begin
      signed_value := false;		(* assume failure *)
      num := 0; signed := false;
      sign := 1;			(* if unsigned *)
      if endline then return;		(* this also skips blanks *)
      if text[lindex].ch in ['+','-'] then begin
	signed := true;			(* do have signed value *)
	if text[lindex].ch = '+' then sign := 1 else sign := -1;
	lindex := lindex + 1;
	if endline orif (not (text[lindex].ch in ['0'..'9'])) then begin
	  error ('Number expected.');
	  return
	end;
      end;
      if not (text[lindex].ch in ['0'..'9']) then return;
      while (lindex <= textlen) andif (text[lindex].ch in ['0'..'9']) do begin
	num := (num * 10) + (ord (text[lindex].ch) - ord ('0'));
	lindex := lindex + 1;
      end;
      num := num * sign;
      signed_value := true;
    end (* with line *) ;
   end;


  (* VALUE checks if an unsigned value is present.  Errors if there
     is a signed value.  Returns the number found, or if not found zero. *)

  function value (var num: integer): boolean;
   var signed: boolean;
   begin
    value := signed_value (num, signed);
    if signed then error ('Signed value not allowed here.');
   end;
$PAGE command dispatching loop

var
  new_state: state;
  num: integer;
  signed: boolean;

begin					(* process commands *)
  process_commands := false;		(* not done until we determine otherwise *)
  lindex := 2;			(* bypass the "$" *)

  case rfword (line, lindex) of	(* get and bypass the imperative *)

    justifywrd:
      begin
	if endline then cur_state := alignmode	(* just "justify" *)
	else begin				(* get which form *)
	  case word of
	    left:      new_state := fillmode;
	    right:     new_state := rightalignmode;
	    others:    error ('Missing mode.')
	  end;
	  check_end;
	  cur_state := new_state;
	end
      end;

    center:
      begin
	check_end;
	cur_state := centermode;
      end;

    verbatim:
      begin
        check_end;
        cur_state := literalmode;
      end;

    right:
      begin				(* obsolete form *)
        check_end;
	cur_state := rightalignmode;
      end;

    paragraph:
      begin
	if signed_value (num, signed) then ;
	check_end;
	if ((page.left_margin + left_indentation + num) < 0) or
	   ((page.left_margin + left_indentation + num) > (page.width - page.right_margin - right_indentation))
	  then error ('Value out of range.')
	  else par_indentation := num;
      end;

    indent:
      begin
	case word of
	  left:	  begin
		    if signed_value (num, signed) then ;
		    if signed then num := left_indentation + num;
		    if ((page.left_margin + num) > (page.width - page.right_margin - right_indentation))
			or (num < 0)
		      then error ('Value out of range.')
		      else left_indentation := num;
		  end;
	  right:  begin
		    if signed_value (num, signed) then ;
		    if signed then num := right_indentation + num;
		    if ((page.left_margin + left_indentation) > (page.width - page.right_margin - num))
			or (num < 0)
		      then error ('Value out of range.')
		      else right_indentation := num;
		  end;
	  others: error ('Right/left expected.')
        end;
        check_end;
      end;

    spacing:
      begin
	if not value (num) then num := 1;
	if (num < 1) or (num > (page.length div 5))
	  then error ('Spacing not allowed.');
	check_end;
	line_spacing := num - 1;
      end;

    skip:
      begin
	if not value (num) then num := 1;
	if (num < 0) or (num > page.length - page.top_margin - page.bottom_margin)
	  then error ('Line count too large.');
	check_end;
	put_skip (num);
      end;

    need:
      begin
	if not value (num) then num := 1;
	if (num < 0) or (num > page.length - page.top_margin - page.bottom_margin)
	  then error ('Line count too large.');
	check_end;
	put_need (num);
      end;

    pagewrd:
      put_page;				(* permit arbitrary comment on page line *)

    title, footnotes, number:		(* ignore for now *)
      ;

    endoffile:				(* explicit $ENDOFFILE command *)
      begin
	check_end;
	process_command := true;	(* indicate that we are done *)
      end;

    others:				(* invalid commands *)
      error ('Invalid command.')

  end (* case *) ;

100 (* error return *) :
  dispose (line);
end;
$PAGE rfmain
public procedure rfmain;

 var
   line: line_type;
   cmdkind: command_words;
   done: boolean;
   effective_width: points;
   local_width: points;

 begin
  repeat					(* until done processing *)
    effective_width := page.width - page.left_margin - page.right_margin
			  - left_indentation - right_indentation;
    loop				(* process input lines *)
      local_width := effective_width - par_indentation;
      case cur_state of

	alignmode, fillmode:		(* justify and justify left *)
	  justify (cmdkind, line, local_width);	(* justify does it all *)

	centermode:			(* center *)
	  begin				(* read a literal line and center it *)
	    readline (cmdkind, line);
	    with line^ do		(* ignored if command *)
	      indentation := (local_width - total_width) div 2;
	  end;

	literalmode:			(* verbatim *)
	  readline (cmdkind, line);	(* print line as is *)

	rightalignmode:			(* justify right, or right *)
	  begin				(* read a literal line, and align it to the right margin *)
	    readline (cmdkind, line);
	    with line^ do		(* ignored if command *)
	      indentation := local_width - total_width;
	  end

      end (* case *) ;
    exit if cmdkind <> nonword;
      with line^ do begin
	indentation := indentation + left_indentation + par_indentation;
	spacing := line_spacing;
      end;
      if page.length > 0
	then put_line (line)		(* give it to page formatter *)
	else begin			(* no pagination, put it out directly *)
	  wrline (line);
	  dispose (line);
	end;
      par_indentation := 0;		(* always canceled after one line *)
    end;
    par_indentation := 0;		(* before next command too *)

  exit if (line = nil);			(* end of file reached *)
    done := process_command (line);	(* parse and interpret the command *)
  until done;
 end.
  