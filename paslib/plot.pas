program plot;

  static var plotting: boolean;		(* indicates if plotter is turned on *)
$PAGE includes and externals

$include lookup.typ[52250,245]
$include cmdutl.typ[52250,245]
$include pltutl.inc[52250,240]

type cnvtorstr = packed array[1..132] of char;	(* real number converter *)
external procedure cnvtor ( str: cnvtorstr; var r: real );
$PAGE symbol and keyword definitions

(* The symbols defined here include both reserved words and punctuation.  As we
   use the lookup procedure to process keywords, the keyword symbols must come
   first in the scalar type. *)

type
  symbols =
     (	ploton_cmd, plotoff_cmd, move_cmd, draw_cmd, color_cmd, speed_cmd,
	write_cmd, circle_cmd, arc_cmd, scale_cmd, rotate_cmd, position_cmd,
	window_cmd, layout_cmd, begin_cmd, end_cmd, reset_cmd,
	device_cmd,
	red_rw, green_rw, blue_rw, black_rw, none_rw,	(* color names *)
	real_cst, string_cst, comma, identifier, nl  );

  plotcmds = ploton_cmd..none_rw;		(* subrange containing just the reserved words *)
  plotcmdlist = array [plotcmds] of cmdlist;

const
  plotcmd_names: plotcmdlist :=
    (	('PLOTON    ', 6),
	('PLOTOFF   ', 7),
	('MOVE      ', 1),
	('DRAW      ', 1),
	('COLOR     ', 2),
	('SPEED     ', 2),
	('WRITE     ', 1),
	('CIRCLE    ', 2),
	('ARC       ', 2),
	('SCALE     ', 1),
	('ROTATE    ', 1),
	('POSITION  ', 1),
	('WINDOW    ', 3),
	('LAYOUT    ', 3),
	('BEGIN     ', 5),
	('END       ', 3),
	('RESET     ', 5),
	('DEVICE    ', 3),
	('RED       ', 3),
	('GREEN     ', 5),
	('BLUE      ', 4),
	('BLACK     ', 5),
	('NONE      ', 4)  );

type color_array = array[red_rw..none_rw] of pen_colors;
const color_map: color_array := ( red, green, blue, black, none );

external function lookup_rws
     (	line: cmdline; var lindex: cmdlineidx;
	list: plotcmdlist;  maxcmd: plotcmds;
	var rwidx: plotcmds			  ): boolean;
$PAGE error
label 100;	(* point at which to reenter command loop *)

procedure error (msg: string);
 begin
  plt_off;			(* make sure plotter is closed down *)
  plotting := false;
  writeln (tty);		(* print message on new line *)
  writeln (tty, msg);
  writeln (tty);
  goto 100
 end;
$PAGE get_input
(* GET INPUT does a get on the current file. *)

static var
  input2, input3, input4: text;		(* stack of input files, tty is level 1 by default *)
  inlevel: 1..4;

procedure get_input;
 begin
  case inlevel of
    1:  begin
	  if eoln (tty) then begin
	    write (tty, '*'); break;
	    get (tty);
	  end
	  else get (tty)
	end;
    2:  begin get (input2) end;
    3:  begin get (input3) end;
    4:  begin get (input4) end
  end
 end;


(* INCHAR returns the current character in the input file *)

function inchar: char;
 begin
  case inlevel of
    1: inchar := tty^;
    2: inchar := input2^;
    3: inchar := input3^;
    4: inchar := input4^
  end;
 end;



(* INEOF returns the eof flag for the current file *)

function ineof: boolean;
 begin
  case inlevel of
    1: ineof := eof (tty);
    2: ineof := eof (input2);
    3: ineof := eof (input3);
    4: ineof := eof (input4)
  end
 end;


(* INEOLN returns the eoln flag for the current file *)

function ineoln: boolean;
 begin
  case inlevel of
    1: ineoln := eoln (tty);
    2: ineoln := eoln (input2);
    3: ineoln := eoln (input3);
    4: ineoln := eoln (input4)
  end
 end;


(* INOPEN opens a new input file. *)

procedure inopen ( name: string );
 begin
  inlevel := inlevel + 1;
  case inlevel of
    2: open (input2, '.plt ' || name);
    3: open (input3, '.plt ' || name);
    4: open (input4, '.plt ' || name)
  end;
  if ineof then error ('Cannot open file: ' || name || '.');
 end;


(* INCLOSE closes the current input file *)

procedure inclose;
 begin
  case inlevel of
    2: begin close (input2); writeln (tty); open (tty) end;
    3: close (input3);
    4: close (input4)
  end;
  inlevel := inlevel - 1;
 end;
$PAGE scan
(* SCAN reads the input file and tokeninzes the input.  It returns one token at
   a time, in the global variable token. *)

var token:
      record
	case kind: symbols of
	  identifier:	(  name: string );
	  real_cst:	(  value: real  );
	  string_cst:	(  str: string  )
      end;

procedure scan;

var
  text: cmdline;			(* for reading reserved words *)
  number: packed array[1..132] of char;	(* buffer for converting input numbers *)
  idx: cmdlineidx;			(* index in above buffers *)


  (* local procedure to read a number, entered in different parts of the case *)
  procedure scan_number;
   begin
    idx := 1;
    while inchar in ['0'..'9','.','-','+','E'] do begin	(* scan integer part *)
      number [idx] := inchar;
      idx := idx + 1;
      get_input
    end;
    number [idx] := ' ';			(* cnvtor requires a blank *)
    token.kind := real_cst;
    cnvtor (number, token.value);
   end;

begin
  loop
    if (token.kind = nl) and ineoln then get_input;
  exit if not ineof;
    inclose
  end;
  while inchar = ' ' do begin			(* discard white space *)
    if ineoln then begin
      token.kind := nl;
      return;					(* <<<<---- sideways exit *)
    end;
    get_input
  end;

  case uppercase (inchar) of

    '0'..'9','-','.':
	begin
	  scan_number;				(* read and convert number *)
	  return
	end;

    '''':
	begin					(* read a string *)
	  token.kind := string_cst;
	  token.str := '';
	  loop
	    get_input;
	    if ineoln then begin		(* string extends across a line *)
	      error ('String extends across multiple lines.');
	    end;
	    if inchar = '''' then begin		(* may be end of string *)
	      get_input;
	      if inchar <> '''' then return;	(* string finished, <<<<<---- exit sideways *)
	    end;
	    token.str := token.str || inchar
	  end
	end;

    'A'..'Z':
	begin					(* reserved word *)
	  text := '';
	  repeat				(* get text of word *)
	    text := text || inchar;
	    get_input;
	  until not (uppercase (inchar) in ['A'..'Z']);
	  idx := 1;				(* need by lookup *)
	  if not lookup_rws (text, idx, plotcmd_names, maximum (plotcmds), token.kind)
	    then begin
	      token.kind := identifier;
	      token.name := text
	    end;
	end;

    ';':
	begin
	  get_input;
	  token.kind := nl
	end;

    ',':
	begin
	  get_input;
	  token.kind := comma;
	end;

    others:
	error ('Invalid character in input.')

  end (* case *) ;
end;
$PAGE scanning functions

(* SCANSYM scans for tokens in a specific set of symbols, errors otherwise *)

type symbol_set = set of symbols;

procedure scansym ( symset: symbol_set );
 begin
  scan;
  if not (token.kind in symset) then error ('Syntax error.');
 end;



(* GET POINT attempts to read a point specification from the input.  At least,
   the x and y coordinates must be specified, the z coordinate if omitted is
   assumed to be zero. *)

function get_point: point;
 const zero_pt: point := ( 0, 0, 0 );
 begin
  get_point := zero_pt;
  scansym ([real_cst]);
  get_point.x := token.value;
  scansym ([real_cst]);
  get_point.y := token.value;
  scan;
  if token.kind = real_cst then begin
    get_point.z := token.value;
    scan
  end
 end;
$PAGE do_plot - run a plot program
procedure do_plot;

var pt, pt2: point;
    err: boolean;
    saved_state: plt_state;
    text: string[32];

begin
 loop			(* exits by sideways return, or goto the main *)
  scan;
  case token.kind of

    device_cmd:
	begin
	  scansym ([identifier]);
	  plt_init (token.name, err);
	  if err then error ('Invalid device name.');
	  scan
	end;

    begin_cmd:			(* stack state, at end, state will be reestablished *)
	begin
	  saved_state := plt;
	  scansym ([nl]);
	  do_plot;		(* recurse, looking for end *)
	  plt := saved_state;
	end;

    end_cmd:			(* return to caller to reestablish state *)
	begin
	  scansym ([nl]);
	  return
	end;

    ploton_cmd:
	begin
	  if not plotting then plt_on;
	  plotting := true;
	  scan
	end;

    plotoff_cmd:
	begin
	  if plotting then plt_off;
	  plotting := false;
	  scan;
	end;

    move_cmd:
	plt_move (get_point);

    draw_cmd:
	plt_draw (get_point);

    speed_cmd:
	begin
	  scansym ([real_cst]);
	  plt_speed (token.value);
	  scan
	end;

    scale_cmd:
	plt_scale (get_point);

    rotate_cmd:
	plt_rotate (get_point);

    position_cmd:
	plt_translate (get_point);

    color_cmd:
	begin
	  scansym ([red_rw..none_rw]);
	  plt_color (color_map [token.kind]);
	  scan;
	end;

    layout_cmd:
	begin
	  pt := get_point;
	  if token.kind = comma then begin	(* if not, fall through and error *)
	    pt2 := get_point;
	    plt_layout (pt.x, pt.y, pt2.x, pt2.y);
	  end
	end;

    window_cmd:
	begin
	  pt := get_point;
	  if token.kind = comma then begin
	    plt_window (pt, get_point)
	  end
	end;

    reset_cmd:		(* reestablish top-level transform, i.e. the window *)
	begin
	  saved_state := plt;
	  saved_state.transform := saved_state.window;
	  plt := saved_state;
	  scan
	end;

    nl:	(* no action *) ;

    identifier:
	begin
	  text := token.name;
	  scansym ([nl]);
	  inopen (text)
	end;

    others:
	error ('Invalid statement.')

  end (* case *);
  if token.kind <> nl then error ('Statement not terminated.');
 end (* loop *);
end;
$PAGE mainline

begin
  rewrite (tty); open (tty);

100 (* return from error or do_plot *) :

  inlevel := 1;					(* read from tty *)
  plotting := false;
  token.kind := nl;
(*  loop
    scan;
    case token.kind of
	identifier:	writeln (tty, 'IDENTIFIER = ', token.name);
	comma:		writeln (tty, 'comma');
	nl:		writeln (tty, 'newline');
	real_cst:	writeln (tty, 'REAL = ', token.value);
	string_cst:	writeln (tty, 'STRING = ', token.str);
	others:		writeln (tty, plotcmd_names[token.kind].name)
    end;
    break;
  end; *)
  do_plot;
end.
 