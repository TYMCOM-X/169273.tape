module mdlpro options special (coercions);

(* MDLPRO main program, invokes recursive descent processor. *)

$SYSTEM MMSSYM.TYP
$SYSTEM MMSMDL.TYP

external var
  lastsym: symptr;
  lastmod: modptr;
  lastarea: areaptr;
  mdlerror: boolean;
  newmdl: ^pnode;
  currid: mdlid;
  compfn: file_name;
  js: jobrec;

public var
  curtok: mdltoken;
  intval: integer;
  mdlfile: text;

var
  infile, outfile: file_name;
  tlen: integer;

$INCLUDE RUN.INC
external var auto_run: boolean;
$INCLUDE INFPAC.INC
external procedure usepro;
external procedure hpsave (file_name; ^pnode);

$INCLUDE MMTCOR.INC

external function newsym: symptr;
external function newname (namekind; symptr) (* don't use PTR, fake it *)
  : nameptr;
external function find (namekind; ^pnode): boolean;

$PAGE  declarations for post_proces
procedure post_process;

(* Once the MDL has been successfully processed, we can assign final
   program addresses for areas and symbols and stuff.  We can also
   verify that any symbol in a contains clause is also on the MTV
   somewhere.  *)

var
  m: modptr;
  a: areaptr;
  oldfirst, s: symptr;
  loseg: integer;			(* we're allocating it *)
  shrcnt, ncnt: integer;		(* for counting modules *)
  resptr: integer;			(* size of resident LTV *)
  f: text;				(* for reading symbol file *)
  rtline: string [80];			(* same *)
$PAGE getrtl, do_sym, and poserr -- in post_process
  procedure getrtl;

  (* GETRTL reads in a line from the runtime symbol file.  It finds one
     that contains non-comment text.  The runtime symbol file is defined
     as three sections, each one terminated with an entirely blank line.
     Semicolon is the comment delimiter within the file; anything to the
     right of a semi is commentary, but a line whose first nonblank char
     is a semi does NOT count as a section separator.  Each section in
     the file is described below, in POST_PROCESS, as it is read.  *)

    var semi: integer;			(* to find one in the line *)

  begin
    repeat
      readln (f, rtline);		(* assume blank line at EOF! *)
    if rtline = '' then return;		(* blank line is perfectly good *)
      semi := index (rtline, ';', length (rtline) + 1 );
      rtline := uppercase (substr (rtline, 1, semi - 1) );  (* left of 1st semi *)
      until rtline <> '';
    while rtline [length(rtline)] = ' ' do	(* shave off trailing blanks *)
      rtline := substr (rtline, 1, length (rtline) - 1  )
  end (* procedure getrtl *);

  procedure poserr (s: string [60]);

  (* POSERR is called with an error message from post_process *)

  begin
    writeln (tty, '?ODMPOS -- ', s, '.');
    mdlerror := true
  end (* procedure poserr *);

  procedure do_sym (str: string [80]);

  begin					(* just consolidates bookkeeping *)
    s := newsym;
    s^.stype := proc;
    s^.mnext := newmdl^.slist;
    newmdl^.slist := s;
    currid := str
  end (* procedure do_sym *);
$PAGE post_process initialize, read the runtime symbol file
begin					(* procedure post_process *)
  resptr := 400012b;			(* for symbols in MAIN *)
  if newmdl^.multiseg then begin	(* if sharables, put MTV in loseg *)
    newmdl^.mtvaddr := 140b;
    loseg := newmdl^.mtvsize + newmdl^.mtvaddr
    end
  else begin				(* no sharables except MAIN *)
    newmdl^.mtvaddr := 400012b;
    loseg := 140b
    end;

  reset (f, 'ODMSRT.SEX' || js.progdir);  (* access the symbol file *)
  if eof (f) then poserr ('Can''t open runtime symbol file')
  else begin
    oldfirst := newmdl^.slist;		(* remember to fix up later *)

    (* the first set of symbols in the symbol file are those symbols
       used for communication with the debugger, which are emitted in
       the inline code.  Such symbols should appear on the MTV only if
       the system is in DEBUG mode.  Currently the only such symbol is
       STMT., but we may as well be ready for others. *)

    loop
      getrtl;
    exit if rtline = '';
      do_sym (rtline);
      if newmdl^.debug then
	if find (symname, newmdl) then
	  poserr ('Control symbol ' || currid || ' defined by user')
	else s^.name := newname (symname, s)
      end;

    (* the next set of symbols are the so-called vectored runtime
       symbols.  In essence, anytime a runtime routine stores an
       address which may be expected to survive across an overlay
       operation (and thus a possible GETSEG), the routine must store
       a transfer vector address instead.  The convention for the
       runtime names is that xxx.AC is the actual address, while
       xxx.TV is the transfer vector.  Thus the routines store xxx.TV.
       We will define a transfer vector xxx.TV::jrst xxx.AC, while
       a non-overlaid program will have xxx.TV=:xxx.AC (equate them).
       The file contains the xxx name, one per line. *)

    loop
      getrtl;
    exit if rtline = '';
      if newmdl^.multiseg then begin	(* install the vectored symbols *)
	do_sym (rtline || '.AC');
	if find (symname, newmdl) then
	  poserr ('Runtime vector ' || currid || ' defined by user')
	else s^.name := newname (symname, s); (* put it there *)
	currid := rtline || '.TV';	(* the PSEUDONYM *)
	if find (symname, newmdl) then
	  poserr ('Runtime pseudonym ' || currid || ' defined by user')
	else s^.psname := newname (symname, s)
	end
      end;

    (* The next section of the symbol file is the list of debugger
       runtime symbols, followed by their pseudonyms, separated by
       a comma.  *)

    loop
      getrtl;
    exit if rtline = '';
      ncnt := index (rtline, ',');	(* borrow an integer to find comma *)
      if newmdl^.multiseg 		(* if we need to do it *)
      and (ncnt <> 0) then begin	(* must have a real pseudonym line *)
	do_sym (substr (rtline, 1, ncnt-1));
	if newmdl^.debug then begin
	  if find (symname, newmdl) then	(* user could be using it too *)
	    s^.name := lastsym^.name		(* so don't duplicate node *)
	  else s^.name := newname (symname, s);
	  currid := substr (rtline, ncnt + 1);	(* rest of it *)
	  if find (symname, newmdl) then
	    poserr ('Debugger pseudonym ' || currid || ' defined by user')
	  else s^.psname := newname (symname, s)  (* the pseudonym *)
	  end
	end
      end;

    (* The final section of the symbol file is the list of low segment
       resident routines which must be placed on the transfer vector.
       These have to be treated differently, since they are not included
       on the LTV's of sharable segments, in fact they never appear there. *)

    loop
      getrtl;
    exit if rtline = '';
      if newmdl^.multiseg then begin
	do_sym (rtline);
	if find (symname, newmdl) then
	  poserr ('Low segment symbol ' || currid || ' defined by user')
	else s^.name := newname (symname, s);
	s^.stype := loseg_entry
	end
      end;

    (* Finally, we have to make DEBUG$ one of these syms. *)

    if newmdl^.debug then begin
      do_sym ('DEBUG$');
      if find (symname, newmdl) then poserr ('Symbol DEBUG$ defined by user')
      else s^.name := newname (symname, s)
      end
    else do_sym ('');			(* a filler for it *)
    s^.stype := loseg_entry;

    close (f);				(* we're done with it *)

    s := oldfirst;			(* fix the chain, skip new ones *)
    while s^.mnext <> oldfirst do s := s^.mnext;  (* find old last *)
    s^.mnext := newmdl^.slist		(* and make it point to new first *)
    end (* file not at eof *);
$PAGE post_process the symbol definitions
  if newmdl^.multiseg then begin	(* sharable, define LTV for res *)
    s := newmdl^.slist;			(* symbols in each segment *)
    resptr := 400012b;			(* LTV of sharable, id and stcheck *)
    repeat
      if (s^.smod = nil) and (s^.stype <> loseg_entry) then begin
	s^.ltvloc := resptr;
	resptr := resptr + 1
	end;
      s := s^.mnext
      until s = newmdl^.slist
    end (* resident LTV definitions *);

  m := newmdl^.mlist;			(* assign static areas in loseg *)
  newmdl^.losegbreak := loseg;		(* remember for skipping later *)
  shrcnt := -1;  ncnt := 0;
  while m <> nil do
    with m^ do begin
      storig := loseg;			(* assign origin *)
      loseg := loseg + stsize;		(* allocate needed size *)
      if marea = nil then begin		(* count as sharable *)
	shrcnt := shrcnt + 1;
	modid := - shrcnt
	end
      else begin
	ncnt := ncnt + 1;
	modid := ncnt
	end;
      m := next				(* advance pointer *)
      end;

  loseg := newmdl^.losegbreak + newmdl^.stsize;	(* skip all static areas *)
  a := newmdl^.alist;			(* walk non-sharable areas to alloc *)
  while a <> nil do
    with a^ do begin			(* if any, indeed *)
      loseg := ((loseg + 777b) div 1000b) * 1000b;	(* page bdy *)
      aorig := loseg;
      loseg := loseg + asize;
      a := next
      end;

  loseg := ((loseg + 777b) div 1000b) * 1000b;	(* round up *)
  newmdl^.losegbreak := loseg;		(* and remember *)

  s := newmdl^.slist;			(* allocate MTV addresses *)
  loseg := newmdl^.mtvaddr;		(* just use it as integer *)
  repeat
    with s^ do begin
      mtvloc := loseg;
      if (smod <> nil) and (stype in [proc,func,unknown]) then
	loseg := loseg + 2		(* two for proc/func in ovl *)
      else loseg := loseg + 1;
      if (stype in [variable, constant]) and
	( (smod = nil) orif (smod^.name^.text <> 'MAIN') ) then
	poserr ('symbol ' || name^.text ||
	  ' -- vars and consts in MAIN only.');
      s := mnext			(* next on master list *)
      end
    until s = newmdl^.slist;

  if loseg > newmdl^.mtvaddr + newmdl^.mtvsize then begin
    poserr ('MTV total size exceeded.');
    return
    end;

  m := newmdl^.mlist;			(* assign LTV in all modules *)
  while m <> nil do begin		(* also check locals not on MTV *)
    if m^.marea = nil then loseg := resptr	(* where to start *)
      else loseg := m^.marea^.aorig + 12b;	(* + 1 for mod ID word *)
    s := m^.syms;			(* start with mod's syms *)
    if s <> nil then repeat
      with s^ do begin			(* assign and check *)
	s := lnext;
	if stype in [proc, func, unknown] then begin
	  ltvloc := loseg;		(* assign *)
	  loseg := loseg + 1;
	  if (mtvloc = -1) and (name <> nil) then	(* MTV never assigned above *)
	    poserr ('symbol ' || name^.text ||	(* name = NIL => FILLER in CONTAINS clause *)
	      ' not defined in SYMBOLS section')
	  end
	end (* single symbol *);
      until s = m^.syms;
    m := m^.next
    end (* each module *);
end (* procedure post_process *);
$PAGE mdlpro the main program
public procedure mdlpro;

begin
  infile := compfn;
  outfile := '';
  tlen := index (infile, '=');
  if tlen <> 0 then begin
    outfile := substr (infile, 1, tlen - 1);	(* before equals *)
    infile := substr (infile, tlen + 1)	(* and the rest *)
    end;
  reset (mdlfile, '.MDL ' || infile);
  if outfile = '' then outfile := filename (mdlfile) || '[,].MDO'
  else outfile := '[,].MDO ' || outfile;
  if eof (mdlfile) then writeln (tty, '?MDLPRO Can''t get input file.')
  else begin
    rewrite (output, outfile);
    if not eof (output) then writeln (tty, '?MDLPRO Can''t write output.')
    else begin
      outfile := filename (output);
      scratch (output);
      usepro;
      close (mdlfile);
      if not mdlerror then post_process;
      if mdlerror then begin
	writeln (tty, '?MDLPRO Errors detected.');
	outfile := '';		(* for going to ODMS *)
	end
      else hpsave (outfile, newmdl)
      end
    end
end.
 