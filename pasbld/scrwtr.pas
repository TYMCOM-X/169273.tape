                  (*************************
                   *                       *
                   * SCRIBE-10 LINE WRITER *
                   *                       *
                   * PERFORMS  UNDERLINING *
                   *                       *
		   *  AND OTHER  CHARACTER *
		   *			   *
		   *       PROCESSING      *
		   *                       *
                   *************************)
 
module writer;

$include stdtyp.inc

type script_char = string [5]; (* for commands controlling sub/superscripting *)
     script_list = array [terminal_type] of script_char;

external var repeat_cnt: lineptr;		(* /R:n switch parameter *)

const (* various and sundry special characters *)
  esc := chr(33b); (* special command character *)
  null := chr(0);   (* filler character--assumed not to effect carriage *)
  cr := chr(15b); (* to return carriage *)
  bs := chr(10b); (* to backspace one position *)
  sp := ' '; (* to advance one position *)
  ul := '_'; (* the under/overline character *)
  lf := chr(12b); (* to advance carriage one line *)
  supchar: script_list := (* output characters to superscript *)
     (  esc || 'S', (* diablo *)
	'', (* nospecial *)
	esc || 'D'  ); (* xerox *)
  subchar: script_list := (* output characters to subscript *)
     (  esc || 's', (* diablo *)
	'', (* nospecial *)
	esc || 'U'  ); (* xerox *)
  boldchar: script_list := (* output characters for boldface *)
     (  '', (* diablo *)
	'', (* nospecial *)
	esc || '3' || sp || esc || '4'  ); (* xerox *)
  unboldchar: script_list := (* output characters to cancel boldface *)
     (  '', (* diablo *)
	'', (* nospecial *)
	esc || '3' || bs || esc || '4'  ); (* xerox *)
  revlf: script_list := (* output characters to overline *)
     (  chr(32b), (*diablo*)
	'',    (*nospecial*)
	esc || lf );     (*xerox*)
$PAGE wrtline -- heading and local variables of wrtline

public procedure wrtline( var f: text; (* the output file *)
			   var inline: line; (* the input line *)
			   innum: lineptr; (* length of input line *)
			   dounder: boolean; (* flags $underline on/off *)
			   backspace: boolean; (* flags /b switch *)
			   map: trans_table; (* $translate table *)
			   terminal: terminal_type ); (* /t switch setting *)

  var
    outbuf, (* character buffer for line text *)
    underbuf, (* character buffer for underline text *)
    overbuf, (* for overline text *)
    boldbuf: (* for boldface text *)
      packed array [1..maxlinelen] of char;
    i: lineptr;	(* loop variable *)
  static var (* for efficiency of access *)
    undernum, overnum, boldnum: lineptr; (* lengths of the buffers *)
    inptr: lineptr; (* pointer to current character in inline *)
    ch: chtype; (* for holding input characters *)
    super_or_sub, (* flags any super/subscripting in input *)
    special_ter: boolean; (* flags diablo terminal *)
    scrstate: (normal, super, sub); (* for remembering carriage state *)
$PAGE process_ch -- character writer for subscripting and/or superscripting

  procedure rest_state;

  (* This routine returns the carriage to its normal position. *)

    begin
    case scrstate of
      super: write(f,subchar[terminal]);
      sub: write(f,supchar[terminal]);
      others: (* do nothing *)
    end (* case *);
    scrstate := normal
    end (*rest_state*);


  procedure process_ch ( ch: char;  (* the character to write *)
			 attr: attribute_set );	(* and its attributes *)

  (* This routine writes a character, performing any necessary
     subscripting and/or superscripting *)

  begin
  if ch <> sp then  (* and don't worry about mode for SP *)
    begin	    (* change mode if necessary *)
    if [superscript,subscript] <= attr then
      rest_state    (* super and sub is normal *)
    else if superscript in attr then
      begin
      if scrstate <> super then
	begin	    (* must change to super *)
	rest_state;
	write(f, supchar[terminal]);
	scrstate := super
	end
      end
    else if subscript in attr then
      begin	    (* same deal for subscript *)
      if scrstate <> sub then
	begin
	rest_state;
	write(f, subchar[terminal]);
	scrstate := sub
	end
      end
    else rest_state (* neither sub or super, goto normal *)
    end (* if special processing needed *);

  write(f,ch)
  end (*process_ch*);
$PAGE do_backspace -- performs underlining by backspacing

  procedure write_ch( ch: char; (* character to write *)
		      attr: attribute_set ); (* its attributes (ignored) *)

  (* This procedure is passed to do_backspace as its output routine
     if no super or subscripting is to be performed.  It merely writes
     the character passed. *)

  begin
    write(f, ch)
  end (*write_ch*);


  type formal = procedure ( char; attribute_set );

  procedure do_backspace( writer: formal );

  (* This procedure writes the underline line (underbuf) and the
     line text (outbuf) by backspacing to print the underlining.
     It uses a passed routine to output characters, which performs
     the super and/or subscripting, if any. *)

    var under, bold: boolean;
	i, inptr, underidx, boldidx, index: lineptr;

  begin
    under := false;
    bold := false;
    underidx := 0;
    boldidx := 0;

    for inptr := 1 to innum+1 do begin

      (* if underline shift, backspace and print if necessary *)

      if (inptr > innum) orif (under <> (underbuf[inptr] <> sp)) then begin
	if under then begin
	  for index:= underidx to inptr-1 do write(f,bs); (* back up *)
	  for index:= underidx to inptr-1 do
	    writer(underbuf[index], inline[index].attr)
	end;
	under := not under;
	underidx := inptr;
      end;

      (* if boldspace shift, backspace and boldface if necessary *)

      if (inptr > innum) orif (bold <> (boldbuf[inptr] <> sp)) then begin
	if bold then
	  for i := 1 to repeat_cnt do begin
	    for index:= boldidx to inptr-1 do write (f,bs); (* back up *)
	    write (f, boldchar[terminal]); (* shift over 1/60 inch *)
	    for index:= boldidx to inptr-1 do
	      if boldbuf[index] = '\' then writer (' ',inline[index].attr)
	      else writer (boldbuf[index], inline[index].attr);
	    write (f, unboldchar[terminal]); (* shift back *)
	end;
	bold := not bold;
	boldidx := inptr;
      end;

      (* print the input character *)

      if inptr <= innum then
	writer ( outbuf[inptr], inline[inptr].attr );
    end (* for inptr *);
  end (*do_backspace*);
$PAGE wrtline_2 -- body of wrtline routine

begin

  if innum<=0 then return; (* a rather expensive nop *)

  undernum:= 0; (* initialize lengths of various buffers *)
  overnum:= 0;
  boldnum:= 0;
  super_or_sub:= false; (* and the two flags *)
  special_ter := (terminal <> nospecial);

  (* loop through the input line, and set up outbuf, underbuf, overbuf *)

  for inptr:= 1 to innum do begin
    ch:= inline[inptr]; (* fetch the current input character *)
    outbuf[inptr]:= map[ch.value]; (* perform translation *)

    (* store either a space or an underbar in underbuf *)

    if dounder andif (ch.value<>sp) andif (underline in ch.attr) then begin
      underbuf[inptr]:= ul;
      undernum:= inptr (* save highest underlined position *)
    end
    else underbuf[inptr]:= sp;

    (* if terminal is diablo, check for overlining and scripting *)

    if special_ter then begin
      if (ch.value<>sp) andif (overline in ch.attr) then begin
	overbuf[inptr]:= ul;
	overnum:= inptr (* remember highest index here too *)
      end
      else overbuf[inptr]:= sp;
      super_or_sub:= super_or_sub orif
		     ( ([subscript,superscript]*ch.attr) <> [] )
    end;

    if (ch.value <> sp) andif (ch.value <> '\') andif (boldface in ch.attr) then begin
      boldbuf[inptr] := ch.value;
      boldnum:= inptr;
    end
    else boldbuf[inptr]:= sp;
  end (*for*);

  scrstate:= normal; (* assume we are at ground zero *)

  if super_or_sub then begin (* print character by character *)
    if (undernum = 0) and (boldnum = 0) then (* just print text *)
      for inptr:= 1 to innum do process_ch(outbuf[inptr], inline[inptr].attr)
    else if not backspace then begin (* underline and overprint by lines *)
      for inptr:= 1 to innum do process_ch(outbuf[inptr], inline[inptr].attr);
      if undernum <> 0 then begin
	write(f,cr); (* return the carriage *)
	for inptr:= 1 to undernum do (* print the underline line *)
	  process_ch(underbuf[inptr], inline[inptr].attr)
      end;
      if boldnum <> 0 then begin
	write(f,cr,boldchar[terminal]); (* return the carriage *)
	for inptr:= 1 to boldnum do (* print the boldface line *)
	  process_ch(boldbuf[inptr], inline[inptr].attr);
      end;
    end
    else do_backspace( process_ch );

    if overnum <> 0 then begin (* do the overlining *)
      write(f,cr,revlf[terminal]); (* position above first character in line *)
      for inptr:= 1 to overnum do (* this we only do by overprinting *)
	process_ch(overbuf[inptr], inline[inptr].attr);
      write(f, lf) (* and reposition carriage *)
    end;
    rest_state (* and finally, return carriage to normal position *)
  end

  else begin (* if not scripting, can print full lines *)
    if (undernum = 0) and (boldnum = 0) then
      write(f, outbuf:innum)
    else if not backspace then begin
      write(f, outbuf:innum);
      if undernum <> 0 then
	write(f, cr, underbuf:undernum);
      if boldnum <> 0 then
	for i := 1 to repeat_cnt do write(f, cr, boldbuf:boldnum);
    end
    else do_backspace( write_ch );

    if overnum <> 0 then (* overline *)
      write(f, cr, revlf[terminal], overbuf:overnum, lf)
  end
end (*wrtline*).
  