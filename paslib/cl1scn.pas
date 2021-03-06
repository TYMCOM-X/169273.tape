$title scanner
$length 45
module scanner;
$include cl1.inc

(*
 *  scanner - lexical analyzer for cl/1.  returns one
 *  token each time called.
 *  
 *  note that file tty must be opened and rewritten externally.
 *)

public function scanner: token_rec;

type set_of_char = set of char;
   digit_array = array ['0'..'9'] of 0..9; (* for conversion of constants *)

const alphanum: set_of_char := ['A'..'Z', '0'..'9']; (* legal in id's *)
   digit_val: digit_array := (0,1,2,3,4,5,6,7,8,9); (* for const conversion *)

static var looked_ahead: boolean := false;
      (* flag set when we have to look ahead one character *)
   ch: char;  (* current character *)

var token: token_rec;  (* token being formed *)
   i: 0..max_int;

$page get_char
(*
 *  function get_char returns the first non-blank character
 *  or a blank if at end of line.  Note that all alphabetic
 *  characters are converted to upper case.
 *)

function get_char:char;
   static var first_call: boolean := true;
      eoln_flag: boolean := true;
   
   var ch: char;

   begin if first_call orif (eoln_flag = true) then readln(tty);
      eoln_flag := false;
      repeat
         if not eoln(tty) then read(tty,ch)
         else begin
            ch := ' ';
            eoln_flag := true
         end
      until (ch <> ' ') orif (eoln_flag = true);
      first_call := false;
      get_char := uppercase(ch);
   end;
$page scan_err
(*
 *  scan_err - procedure to handle any lexical errors.
 *)
procedure scan_err;
   begin token.t_type := error;
      writeln(tty,'''',ch,''' illegal in this context');
      break(tty)
   end;
$page scanner
(*
 * begin scanner code.
 *)
begin if not looked_ahead then ch := get_char;
   looked_ahead := false;

   with token do
   case ch of
      'A'..'Z':   (* identifier *)
      begin 
         i := 0;
         id_text := '';
         repeat   (* read until non-alphanumeric read *)
            i := i + 1;
            if i <= max_id_len then id_text := id_text || ch;
            ch := get_char
         until not (ch in alphanum);
         t_type := id;
         looked_ahead := true;
      end;
      '0'..'9':  (* constant *)
      begin
         cons_val := 0;
         repeat   (* read until non-digit read *)
            cons_val := cons_val*10 + digit_val[ch];
            ch := get_char
         until not (ch in ['0'..'9']);
         t_type := cons;
         looked_ahead := true;
      end;

      ')':   t_type := r_paren;  (* right paren *)
      '(':   t_type := l_paren;  (* left paren *)
      '+':   t_type := plus; (* plus *)
      '-':   t_type := minus;  (* minus *)
      '*':   t_type := times;  (* times *)
      '/':   t_type := divide;  (* divide *)
      ':':   
      begin  (* assignment operator *) 
         ch := get_char;
         if ch <> '=' then scan_err
         else t_type := assign_op
      end;
      ' ':   t_type := eol;  (* end of line *)
      others:   scan_err  (* else - error *)
   end (* case *) ;
   scanner := token
end.
    