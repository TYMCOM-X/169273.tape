$TITLE DEBIO - Pascal Debugger Teletype I/O routines

$HEADER debio

module debio$  options nocheck, special (word);
  
$INCLUDE debug.typ
  
$INCLUDE debasm.inc
$INCLUDE debug.inc

const
  cr = chr(15b);
  lf = chr(12b);
  esc = chr(33b);
$PAGE read$line - input from terminal
(* READ$LINE - routine to read a line from the terminal. *)

public procedure read$line (var line:     string[*];
			    var too_long: boolean);
  
  var
    ch: char;
  
  begin
    line := '';
    too_long := false;
    loop
      tt$in (ch);
     exit if (ch = cr) or (ch = esc) do tt$in (ch); (* eol - eat lf and quit *)
      if length (line) = upperbound (line) then
	too_long := true
      else
	line := line || ch
    end
  end;
$PAGE writ$str, writ$eol, writ$nl - output to terminal
(* WRIT$STR - write string to terminal. *)

public procedure writ$str (    str: packed array [1..*] of char);
  
  var i: 1..maximum (char_range);
  
  begin
    for i :=  1 to upperbound (str) do
      tt$out (str[i])
  end;


  
  
  
(* WRIT$EOL - terminate out line. *)

public procedure writ$eol;
  
  begin
    tt$out (cr);
    tt$out (lf)
  end;
  
  
  
  
  
  
(* WRIT$NL - write string and terminate line. *)

public procedure writ$nl (    str: packed array [1..*] of char);
  
  begin
    writ$str (str);
    writ$eol
  end;
$PAGE writ$int - numeric output
(* WRIT$INT - routine to write integer number in desired radix. *)

public procedure writ$int (    int:           machine_word;
			       desired_radix: radix_type);

  var
    int_text: id_string;

  begin
    case desired_radix of
      decimal_radix:
	putstring (int_text, int:0);
      octal_radix:
	putstring (int_text, int:max_octal_width:o);
      hex_radix:
	putstring (int_text, int:max_hex_width:h);
      others:
	a$$ert (false)
    end;
    
    (* strip leading zeroes *)

    int_text := substr (int_text, verify (int_text, ['0'], length (int_text)));

$IF P10
    if (desired_radix = octal_radix) and (length (int_text) > 6) then
      int_text := substr (int_text, 1, length (int_text) - 6) || ',,' ||
		  substr (int_text, length (int_text) - 5, 6);
$ENDIF

    writ$str (int_text)
  end.
   