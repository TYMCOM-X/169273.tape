program mmdebm options special (word);

const
  no := 0;				(* relocation specifier bits *)
  right := 1;
  left := 2;
  both := 3;

type
  relbyte = 0 .. 3b;			(* relocation word which precedes *)
  relword = packed array [0..17] of relbyte;	(* every group of words *)

  addrrange = 0 .. 777777b;

  l10itemhdr = packed record		(* LINK10 item header word *)
    item: addrrange;
    count: addrrange
    end;

  symtype = packed record		(* RAD50 symbol def record *)
    code: 0 .. 17b;			(* external, intern or whatever *)
    name: 0 .. 37777777777b		(* RAD50 name *)
    end;

  l10overlay = packed record		(* every possible interpretation *)
    case integer of			(* wouldn't ya know *)
      0: (data: machine_word);		(* for doing I/O *)
      1: (itemhdr: l10itemhdr);		(* for the first word of item *)
      2: (relocation: relword);		(* as per LINK10 manual *)
      3: (hword: packed array [0..1] of addrrange);	(* when needed *)
      4: (symword: symtype)		(* for interpreting symbol defs *)
    end (* l10overlay *);

const
  maxnam := 200;			(* maxnum of debugger pseudonyms *)

var
  str: file_name;
  itemtype, itemsize: integer;
  reloc_bytes: relword;
  reloc_idx: -1 .. 17;
  rel_buffer: l10overlay;
  relfile, outfile: file of machine_word;
  names, psnames: array [1 .. maxnam] of symtype;
  pscnt: integer;
  referenced: set of 1 .. maxnam;
$PAGE getbuf, putbuf, and nextbuf
procedure getbuf;
begin
  rel_buffer.data := relfile^;
  get (relfile)
end (* procedure getbuf *);

procedure putbuf;
begin
  outfile^ := rel_buffer.data;
  put (outfile)
end (* procedure putbuf *);

procedure nextbuf;
begin
  putbuf;
  getbuf
end (* procedure nextbuf *);
$PAGE next_item and next_datum
procedure next_item;

(* NEXT_ITEM assumes that the output file is completely up to date, and that
   the input file is positioned at the start of the next item *)

begin
  getbuf;				(* loads first word of item *)
  itemtype := rel_buffer.itemhdr.item;
  itemsize := rel_buffer.itemhdr.count;
  nextbuf;				(* loads first relocation word *)
  reloc_bytes := rel_buffer.relocation;
  reloc_idx := -1
end (* procedure next_item *);


procedure next_datum;

(* NEXT_DATUM gets the next datum word, checking the relocation *)

begin
  itemsize := itemsize - 1;
  if reloc_idx >= 17 then begin
    nextbuf;
    reloc_bytes := rel_buffer.relocation;
    reloc_idx := -1
    end;
  reloc_idx := reloc_idx + 1;
  nextbuf;
  if (reloc_bytes [ reloc_idx ] >= 2) then  (* left half relocated *)
    if rel_buffer.hword [0] < 400000b then
      writeln (tty, '%Left half loseg relocation ', cursor (relfile):6:O)
    else rel_buffer.hword [0] := rel_buffer.hword [0] - 400000b;
  if odd (reloc_bytes [ reloc_idx ]) then
    if rel_buffer.hword [1] < 400000b then
      writeln (tty, '%Right half loseg relocation ', cursor (relfile):6:O)
    else rel_buffer.hword [1] := rel_buffer.hword [1] - 400000b;
end (* procedure next_datum *);
$PAGE asctorad50 converts string symbol names into LINK-10 form
const
  tab: packed array [1..40] of char :=
    ' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ.$%' ;

function asctorad50 (s: string[6]): machine_word;

(* ASCTORAD50 converts an ascii string to its equivalent RAD50 form,
   used by .REL files to express symbol names.  Interestingly, all we
   have to do in this program is convert the text in the runtime symbol
   file to RAD50 and store it in a table.  Then, when a RAD50 name
   comes along, we just look in our table for it, substituting for it
   if we see it. *)

var
  idx: integer;

begin
  asctorad50 := 0;
  for idx := 1 to length (s) do
    asctorad50 := (asctorad50 * 50b) +
      index (tab, s[idx] ) - 1
end (* function asctorad50 *);


function radtoasc (i: machine_word): string [6];

(* Simply, RADTOASC reverses the above process. *)

var
  val: machine_word;

begin
  radtoasc := '';
  val := i;
  while val > 0 do begin
    radtoasc := tab [ ( val mod 50b ) + 1 ] || radtoasc;
    val := val div 50b
    end
end (* function radtoasc *);
$PAGE getlin in do_syms reads lines from the symbol file
procedure do_syms;

(* DO_SYMS reads the symbol file used by ODMS to vector the debugger's
   runtime requirements.  We only use the third section of the file; we
   borrow thr routine getrtl from MDLPRO. *)

var
  s: string[80];
  f: text;
  comma: integer;

  procedure getlin;

  (* GETLIN is borrowed from MDLPRO in ODMS system. *)

  var semi: integer;

  begin
    repeat
      readln (f, s);
    if s = '' then return;		(* blank line is fine *)
      semi := index (s, ';', length (s) + 1);
      s := uppercase (substr (s, 1, semi - 1));
      until s <> '';
    while s [length (s)] = ' ' do
      s := substr (s, 1, length (s) - 1)
  end (* procedure getlin *);
$PAGE do_syms builds up table from runtime symbol file
begin					(* do_syms *)
  reset (f, 'ODMSRT.SEX');		(* assume on same account! *)
  if eof (f) then begin
    writeln (tty, '?Can''t get runtime symbol file.');
    stop
    end
  else begin
    pscnt := 0;
    repeat getlin until s = '';		(* gotta skip first two sections *)
    repeat getlin until s = '';

    loop
      getlin;
    exit if s = '';
      pscnt := pscnt + 1;		(* maintains number of pseudonyms *)
      comma := index (s, ',');
      if comma <> 0 then begin		(* real pseudonym *)
	names [pscnt].name := asctorad50 (substr (s, 1, comma-1));
	names [pscnt].code := 0;
	psnames [pscnt].name := asctorad50 (substr (s, comma+1));
	psnames [pscnt].code := 0
	end
      else begin			(* debugger public *)
	names [pscnt].name := asctorad50 (s);
	names [pscnt].code := 0;
	psnames [pscnt].name := asctorad50 (s);
	psnames [pscnt].code := 0
	end
      end
    end;
  close (f);
end (* procedure do_syms *);
$PAGE do_fix does the job
procedure do_fix;

(* DO_FIX does the actual work.  It controls the reading of the file,
   letting the subordinates do the relocation, but intercepts the
   symbol records and checks them for a fix. *)

var
  tmpbrk: machine_word;
  i: integer;
  found: boolean;

begin
  repeat
    next_item;
    case itemtype of

      2: while itemsize > 0 do begin
	next_datum;
	if rel_buffer.symword.code >= 14b then begin   (* a request *)
	  found := false;
	  for i := 1 to pscnt do
	    exit if names [i].name = rel_buffer.symword.name do begin
	      rel_buffer.symword.name := psnames [i].name;
	      referenced := referenced + [i];
	      found := true
	      end;
	  if not found then
	    writeln (tty, '?Unknown symbol ', radtoasc (rel_buffer.symword.name),
		' requested.');
	  end;
	next_datum
	end (* type 2 -- symbol records *);

      5: begin
	write (tty,' END record ');
	next_datum;
	tmpbrk := rel_buffer.data;
	rel_buffer.data := 400000b;
	next_datum;
	rel_buffer.data := tmpbrk;
	if not eoln (ttyoutput) then writeln (tty)
	end;

      others: while itemsize > 0 do next_datum
      end;

    putbuf
    until itemtype = 5
end (* do_fix *);
$PAGE main program
begin
  rewrite (ttyoutput); open (tty);
  referenced := [];
  write (tty, 'Output file: '); break (tty); readln (tty);
  read (tty, str);
  rewrite (outfile, '.REL ' || str);
  if not eof (outfile) then begin
    writeln (tty, '?Can''t rewrite output file.');
    stop
    end;
  do_syms;

  loop
    write (tty, 'Input file: '); break (tty); readln (tty);
    read (tty, str);
  exit if str = '';
    reset (relfile, '.REL ' || str);
    if eof (relfile) then
      writeln (tty, '?Can''t find input file.')
    else begin
      repeat do_fix until eof (relfile);
      close (relfile)
      end
    end;

  for itemsize := 1 to pscnt do
    if not (itemsize in referenced) then begin
      if names [itemsize].name = psnames [itemsize].name then
	write (tty, '%Debugger entry ')
      else write (tty, '%Runtime name ');
      writeln (tty, radtoasc (names [itemsize].name),
	' never referenced.')
      end;

  close
end (* program *).
    