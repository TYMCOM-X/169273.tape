	       (*****************************
		*                           *
		*   SCRIBE-10 LINE READER   *
		*                           *
		* PERFORMS DECAPITALIZATION *
		*                           *
		*        UNDERLINING        *
		*                           *
		*            AND            *
		*			    *
		*     CHARACTER COMMAND     *
		*        PROCESSING         *
		*                           *
		*****************************)

module reader;

$include stdtyp.inc

$include levels.typ
external   (* must immediately precede levels.var *)
$include levels.var

external var
    inf: array [1..8] of text;
    inf_depth: 1..8;
    dodecap, (* flags $decap on in effect *)
    dounder, (* flags $underline on in effect *)
    nodecap, (* on if currently within at-sign string
		and $decap is on *)
    docontrol: boolean; (* flags $control on in effect *)
    under_state, (* retains state of underlining due
		    to & -- if within ampersand string,
		    set to [underline], otherwise null *)
    current_state: attribute_set; (* retains state of other
				     character commands between lines *)

external procedure error(code: integer);
$PAGE static variable declarations

const
  tab = chr(11b); (* this is the only non-
		     printing character passed through
		     by the reader *)
  eolchar = chr(15b); (* character returned at end of line
			 by character getting routines *)
  endnum = 10; (* length of line returned at eof *)

type end_type = packed array[1..endnum] of char;

const endline: end_type := '$XXHACKEOF'; (* text of line returned
					    at eof *)
      endinclude: end_type := '$EOINCLUDE'; (* text of line returned
						at eof(include file) *)

static var 
  lineno: linenum; (* saves current line number
		      (for line numbered files) *)
  linecnt: integer; (* saves current line count
		      (for unnumbered files) *)
$PAGE initreader -- main initialization routine

public procedure initreader;

(* This routine sets the reader package to the ground state, i.e.,
   sets all processing modes to their defaults and initializes
   other state variables. *)

begin
  dodecap := false;  (* $decap off *)
  dounder := true;   (* $underline on *)
  under_state := [];
  docontrol := true; (* $control on *)
  current_state := [];
  linecnt:= 0; (* initialize line counter variables *)
  lineno:= '-----'; (* assume not a line numbered file *)
end (*initreader*);



public procedure getlnr( var ln: linenum; (* the current line number *)
			 var lc: integer); (* the current line count *)

(* This routine returns the current values of the line number and
   line count static variables.  It is called by routines external to
   this package when such information is needed, e.g., when error messages
   are printed *)

  var i: 1..5; (* for loop in routine *)

begin
  ln:= lineno; (* return line number *)
  if lineno <> '-----' then begin (* if file is numbered... *)
    lc:= 0; (* convert number and return as line count *)
    for i:= 1 to 5 do
      lc:= 10*lc + ord(lineno[i]) - ord('0')
  end
  else lc:= linecnt (* otherwise, just return count *)
end (*getlnr*);
$PAGE store_section -- routine to generate and insert section number

(* This sneaky routine is passed the routine it should use to store
   generated characters, so it may be called from READIT and DECAPIT and
   yet may store into the local buffer of each *)

type store_routine = procedure (char);

procedure store_section( store_ch: store_routine );

  var levnum: string[5];   (* conversion buffer for level numbers *)
      i: 1..maxlevels;     (* for looping through levels *)
      j: 1..upperbound(levnum);  (* for looping through levnum *)

begin
  for i:= 1 to clevel do begin
    putstring(levnum, curlevnum[i]:0 (*no leading blanks*) );
    for j:= 1 to length(levnum) do store_ch( levnum[j] );   (* store # *)
    if i<>clevel then store_ch('.')   (* and . if not last level *)
  end
end (* store_section *);
$PAGE readline -- the reader itself

public procedure readline( var f: text; (* the input file *)
			   var outline: line; (* the output line *)
			   var outlen: lineptr; (* length of output line *)
			   var cmdflg: boolean); (* set if line is command *)

(* The following are static variables which could very well
   be on the stack, but are not, to minimize access time, and
   therefore to maximize speed. *)

static var
  outnum, (* the current length of outline,
	     the output line parameter to readline *)
  inptr, (* pointer to current character in outline
	    being processed *)
  outptr: lineptr; (* pointer to last character output *)

(* Note that all processing operations either hold constant or reduce
   the number of characters in the output line -- hence, both inptr
   and outptr may safely point into outline.  Note further that 
   routines are local to readline so that they may directly refer to
   outline, thus avoiding buffering and copying of lines. *)

  ch: chtype; (* the character returned by all
		 local character getters, hence,
		 used for nearly all processing *)
  omit_under, (* set by readit, flagging whether calls *)
  omit_control: boolean; (* to underit/do_char_cmds may be omitted *)
$PAGE nextch -- the ubiquitous character getting routine

  procedure nextch;
  begin
    inptr:= inptr+1; (* inptr points to previous character
			on call to routine *)
    if inptr>outnum then ch.value:= eolchar (* if character after end
						of line is sought *)
    else ch:= outline[inptr]
  end (*nextch*);


  procedure next_under_ch;

  (* This routine is called by do_char_cmds if underline ampersand
     processing must also be performed.  The determination is made
     by readline, and the routine for do_char_cmds to use is passed. *)

  begin
    nextch;
    if ch.value='&' then begin (* check for double char *)
      nextch;
      if ch.value <> '&' then
	if under_state = [] then under_state:= [underline]
	else under_state:= []
    end;
    ch.attr:= under_state
  end (*next_under_ch*);
$PAGE readit -- read a line before underline/command processing

  procedure readit;

  (* This routine reads an input line in preparation for underlining
     and/or character command processing.  To minimize unnecessary
     processing, it keeps its eyes peeled for occurances of '^'
     '|' and '&', and indicates whether do_char_cmds and/or underit
     will have to be called subsequently by setting omit_control and omit_under.
     If one or the other must be called, inptr points to the character
     before the first one to be processed. *)

    const null = chr(0);   (* ground value for character holders *)

    var start_state: attribute_set;   (* attributes to store with each character *)
	ch: char;   (* the current character *)
	lastch: char;   (* the next character, if nextch does lookahead *)
	seen_control: boolean;   (* indicates we have seen ^ or | *)
	seen_ampersand: boolean;   (* indicates we have seen & *)

  procedure storech( ch: char );   (* local routine to store character *)
  begin
    if (ch>=' ') orif (ch=tab) then begin   (* ignore other chars *)
      if outnum>=maxlinelen then error(101);   (*fatal-->no return*)
      outnum:= outnum+1;
      with outline[outnum] do begin
	value:= ch;
	attr:= start_state
      end
    end
  end (*storech*);

  procedure nextch;  (* local routine to get char, handles section #'s *)
  begin
    if lastch<>null then begin  (* return saved character *)
      ch:= lastch;
      lastch:= null
    end
    else if eoln(f) then ch:= eolchar  (* return cr on end of line *)
    else begin   (*read and check for ^# *)
      read(f,ch);
      if eoln(f) then lastch:= eolchar  (* cr next, # not possible *)
      else if ch='^' then begin
	read(f,ch);   (*check next char*)
	if ch='#' then begin (* section number *)
	  store_section( storech );   (* store it into output buffer *)
	  nextch   (* and call ourself for next character *)
	end
	else begin   (*nope, not section #, save character *)
	  lastch:= ch;
	  ch:= '^'
	end
      end
    end
  end (*nextch*);

  begin   (*readit*)
    outnum:= 0; (* length of line read *)
    lastch:= null;
    ch:= null;    (* initialize our character holders *)
    seen_control:= false;
    seen_ampersand:= false;   (* initialize our special char flags *)

    (* following variables provide for early-out or omission of call(s)
       to underit/do_char_cmds. *)

    omit_under:= dounder; (* if true on exit, no call needed *)
    omit_control:= docontrol;
    inptr:= 0;

    start_state:= under_state + current_state;

    while (ch<>eolchar) and (outnum<maxlinelen) do begin
      nextch;
      if omit_under andif (ch='&') then begin (* our first & *)
	if not seen_control then begin (* no ^ or | seen yet ... *)
	  inptr:= outnum; (* save index of char before & *)
	  start_state:= current_state
	end
	else start_state:= [];
	seen_ampersand:= true;
	omit_under:= false
      end
      else if omit_control andif ( (ch='^') orif (ch='|') ) then begin
	if not seen_ampersand then begin (* no & seen yet *)
	  inptr:= outnum;
	  start_state:= under_state
	end
	else start_state:= [];
	seen_control:= true;
	omit_control:= false
      end;
      storech(ch)
    end;

    if eoln(f) then readln(f)
    else error(6); (* input line too long *)

    omit_under:= (not dounder) or omit_under;
    omit_control:= (not docontrol) or omit_control

  end (*readit*);
$PAGE decapit -- routine to read line and perform decapitalization

  procedure decapit;

  (* This routine reads an input line and decapitalizes it.  Unlike
     READIT, which is called instead when $decap is off, it does not
     attempt to eliminate unnecessary calls to underit and do_char_cmds,
     as the percentage to be saved here is far less. *)

    var ch: char;

    procedure storech(chrctr: char);

    (* This routine stores characters into outline *)

    begin
      if chrctr<>eolchar then begin (* don't store end of line *)
	if outnum>=maxlinelen then error(101);   (*fatal-->no return*)
	outnum:= outnum+1;
	with outline[outnum] do begin
	  value:= chrctr;
	  attr:= []
	end
      end
    end (*storech*);

    procedure nextch; (* decapit has its own, which reads directly *)
    begin
      while (f^ < ' ') and (f^ <> tab) and (not eoln(f)) do
	get(f);    (* get a good character into the buffer or eoln *)
      if (not eoln(f)) and (outnum<maxlinelen) then begin
	read(f,ch);
      end
      else begin
	ch:= eolchar;
	if eoln(f) then readln(f)
	else error(6) (* input line too long *)
      end
    end (*nextch*);
$PAGE decapit_2 -- body of routine decapit
  begin		    (*DECAPIT*)
    omit_control:= not docontrol; (* when decapitalizing, don't attempt to *)
    omit_under:= not dounder;      (* eliminate unnecessary calls to *)
    inptr:= 0;               (* the underlining and command routines *)

    outnum:= 0;
    nextch;
    while ch<>eolchar do
    begin
      if ch = '@' then begin (* check for toggling of decapping *)
	nextch; (* check for double *)
	if ch = '@' then begin (* @@ => @ *)
	  storech('@');
	  nextch
	end
	else nodecap:= not nodecap (* otherwise...change state *)
      end
      else if (ch='*') andif (not nodecap) then begin
	nextch; (* pass the next char through as is *)
	storech(ch); (* note that store of eolchar is caught *)
	if ch<>eolchar then nextch (* and get the next one *)
      end
      else if ch = '#' then begin
	nextch;
	if ch in ['['..'^'] then begin
	  storech( chr(ord(ch) + 40b) );
	  nextch
	end
	else storech('#') (* and keep ch for next time around *)
      end
      else if ch='^' then begin   (* check for section number *)
	nextch;
	if ch='#' then begin
	  store_section( storech );
	  nextch
	end
	else begin
	  storech ('^');
	  if ch = '^' then begin	(* skip over if "^^" *)
	    storech('^');
	    nextch
	  end
	end
      end
      else if nodecap then begin
	storech(ch);
	nextch
      end
      else begin (* decap the character *)
	storech( lowercase(ch) );
	nextch
      end
    end;
  end (*decapit*);
$PAGE underit -- routine to perform underline processing alone

  procedure underit;

  (* This routine performs ampersand underline processing when no
     character commands have been noticed by readit.  It starts
     processing with the character after inptr. *)

    procedure storech_and_mark;	(* STORES CH AND CURRENT STATE INFO *)
    begin
      if ch.value<>eolchar then
      begin
	outptr := outptr + 1;
	with outline[outptr] do begin
	  value := ch.value;
	  attr:= under_state + ch.attr
	end
      end
    end (*storech*);

  begin		    (*UNDERIT*)
    outptr := inptr;
    nextch;
    while ch.value <> eolchar do
    begin
      if ch.value = '&' then
      begin
	nextch;	    (* check for double *)
	if ch.value <> '&' then
	  if under_state = [] then under_state:= [underline]
	  else under_state:= []
      end;
      storech_and_mark;
      nextch
    end;
    outnum:= outptr (* set the new length *)
  end (*underit*);
$PAGE do_char_cmds -- processes ^ and | character commands

  type formal = procedure ; (* any parameterless procedure *)

  procedure do_char_cmds( nextch: formal );

  (* This procedure processes character commands preceded by ^ or |.
     Its character input routine is passed, and is either nextch, if
     no ampersand underline processing is required, or is next_under_ch,
     if such processing is required.  Searching for character commands
     commences with the character after inptr *)

    type
      setofchar = set of char;
      attr_vector = array[attributes] of char;	(* maps attributes to chars *)

    var
      line_state: attribute_set;	(* length specified attributes *)
      state_length: array[attributes] of lineptr;	(* length to apply *)
      temp_length: lineptr;   (* used to obtain apply length *)
      saveinptr: lineptr;	(* used to save inptr *)
      j: lineptr; (* loop variable used within *)
      i: attributes;


    const
      valid_attrs: setofchar := ['U', 'A', 'B', 'O', 'D'];
      attr_chars: attr_vector := ('U','A', 'B', 'O', 'D');
$PAGE do_char_1 -- local do_char_cmds procedures

    procedure change_state;   (* checks if line_state attributes still in effect *)

      var i: attributes; (* loop variable *)

    begin
      for i := minimum(attributes) to maximum(attributes) do
	if i in line_state then
	  if outptr > state_length[i] then
	  begin
	    current_state := current_state - [i];
	    line_state := line_state - [i];
	    state_length[i] := 0
	  end
    end (*change_state*);


    procedure storech_plus_state(chrctr: chtype);
    begin
      if line_state <> [] then
	change_state; (* returns  a new current_state and new line_state *)
      if chrctr.value <> eolchar then
      begin
	outptr := outptr + 1;
	with outline[outptr] do
	begin
	  value := chrctr.value;
	  attr := current_state + chrctr.attr
	end	    (* of with *)
      end	    (* of not eol char *)
    end (*storech_plus_state*);
$PAGE do_char_2 -- body of do_char_cmds

  begin		    (* do_char_cmds *)
    outptr := inptr;
    line_state := [];
    nextch;
    while ch.value <> eolchar do
    begin
      saveinptr := inptr;
      if ch.value = '^' then
      begin
	nextch;	    (* check for double *)
	if ch.value = eolchar then	(* just store arrow *)
	  storech_plus_state(outline[saveinptr])
	else	    (* and fall down to bar check *)
	if ch.value <> '^' then	(* check for attribute *)
	begin
	  if uppercase(ch.value) in valid_attrs then
	    for i := minimum(attributes) to maximum(attributes) do
	      exit if (attr_chars[i] = uppercase(ch.value)) do
		current_state := current_state + [i]
	  else	    (* check for number *)
	  begin
	    temp_length := 0;
	    while ch.value in ['0'..'9'] do
	    begin
	      temp_length := temp_length*10 + ord(ch.value) - ord('0');
	      nextch
	    end;
	    if uppercase(ch.value) in valid_attrs then
	    begin
	      for i := minimum(attributes) to maximum(attributes) do
		exit if (attr_chars[i] = uppercase(ch.value)) do
		begin
		  current_state := current_state + [i];
		  state_length[i] := temp_length + outptr - 1;
		  line_state := line_state + [i]
		end
	    end
	    else	    (* copy chars *)
	      for j:= saveinptr to inptr  do
		 storech_plus_state(outline[j])
	  end
	end	    (* of not double '^' *)
	else
	  storech_plus_state(ch);
      end		    (* of char = '^' *)
      else	    (* check for '|' *)
      if ch.value = '|' then
      begin
	nextch;	    (* check for double *)
	if ch.value = eolchar then
	  storech_plus_state(outline[saveinptr])
	else
	if ch.value <> '|' then
	begin
	  if uppercase(ch.value) in valid_attrs then
	  begin
	     (* determine attribute *)
	    for i := minimum(attributes) to maximum(attributes) do
	      exit if (attr_chars[i] = uppercase(ch.value)) do
	      begin
		current_state := current_state - [i];
		line_state := line_state - [i]
	      end
	  end
	  else	    (* copy chars *)
	    for j:= saveinptr to inptr  do
	      storech_plus_state(outline[j]);
	end
	else
	  storech_plus_state(ch)
      end		    (* of ch = '|' *)
      else
	storech_plus_state(ch);
      nextch;
    end;		    (* of while *)
    current_state := current_state - line_state;
    outnum := outptr
  end (*do_char_cmds*);
$PAGE readline_2 -- the body of readline

begin
  if not eof(f) then begin
(*    getlinenr(lineno); (* update line information *) *)
    linecnt:= linecnt+1;
    if dodecap then decapit (* either read and decap or just read *)
    else readit;
    if omit_control then begin
      if not omit_under then underit
    end
    else
      if omit_under then do_char_cmds(nextch) (* do cmds with no under *)
      else do_char_cmds(next_under_ch); (* do cmds with underlining *)
    outlen:= outnum; (* set returned length *)
    cmdflg:= (outlen>0) andif (outline[1].value = '$')
  end (* not eof *)
  else begin (* on eof(inf[1]), return $endoffile command    if inf_depth = 1 then begin
      for outptr:= 1 to endnum do
	with outline[outptr] do begin
	  value:= endline[outptr];
	  attr:= []
	end;
      outlen:= endnum;
      cmdflg:= true
      end
    else begin
      (* On eof(include file), send back $ENDINC command *)
      for outptr:= 1 to endnum do
	with outline[outptr] do begin
	  value:= endinclude[outptr];
	  attr:=[]
	end;
      outlen := endnum;
      cmdflg:= true
    end
  end
end (*readline*).
   W %ç