MODULE mdlpro OPTIONS special (coercions);
(* MDLPRO main program, invokes recursive descent processor. *)

$SYSTEM MMSSYM.TYP
$SYSTEM MMSMDL.TYP

EXTERNAL VAR
lastsym: symptr;
lastmod: modptr;
lastarea: areaptr;
mdlerror: BOOLEAN;
newmdl: ^pnode;
currid: mdlid;
compfn: FILE_NAME;
js: jobrec;
PUBLIC VAR
curtok: mdltoken;
intval: INTEGER;
mdlfile: TEXT;

VAR
infile, outfile: FILE_NAME;
tlen: INTEGER;
$INCLUDE INFPAC.INC
EXTERNAL
PROCEDURE usepro;
EXTERNAL
PROCEDURE hpsave (FILE_NAME; ^pnode);
$INCLUDE MMTCOR.INC
EXTERNAL
FUNCTION newsym: symptr;
EXTERNAL
FUNCTION newname (namekind; symptr) (* don't use PTR, fake it *)
: nameptr;
EXTERNAL
FUNCTION find (namekind; ^pnode): BOOLEAN;
$PAGE  declarations for post_proces

PROCEDURE post_process;
(* Once the MDL has been successfully processed, we can assign final
   program addresses for areas and symbols and stuff.  We can also
   verify that any symbol in a contains clause is also on the MTV
   somewhere.  *)
VAR
m: modptr;
a: areaptr;
oldfirst, s: symptr;
loseg: INTEGER; (* we're allocating it *)
shrcnt, ncnt: INTEGER; (* for counting modules *)
resptr: INTEGER; (* size of resident LTV *)
f: TEXT; (* for reading symbol file *)
rtline: STRING [80]; (* same *)
$PAGE getrtl, do_sym, and poserr -- in post_process

  PROCEDURE getrtl;
  (* GETRTL reads in a line from the runtime symbol file.  It finds one
       that contains non-comment text.  The runtime symbol file is defined
       as three sections, each one terminated with an entirely blank line.
       Semicolon is the comment delimiter within the file; anything to the
       right of a semi is commentary, but a line whose first nonblank char
       is a semi does NOT count as a section separator.  Each section in
       the file is described below, in POST_PROCESS, as it is read.  *)
  VAR
  semi: INTEGER; (* to find one in the line *)
  BEGIN
    REPEAT
      READLN (f, rtline); (* assume blank line at EOF! *)
      IF rtline = '' THEN RETURN; (* blank line is perfectly good *)
      semi := INDEX (rtline, ';', LENGTH (rtline) + 1 );
      rtline := UPPERCASE (SUBSTR (rtline, 1, semi - 1) ); (* left of 1st semi *)
    UNTIL rtline <> '';
    WHILE rtline [LENGTH(rtline)] = ' ' DO (* shave off trailing blanks *)
    rtline := SUBSTR (rtline, 1, LENGTH (rtline) - 1 )
  END (* procedure getrtl *);

  PROCEDURE poserr (s: STRING [60]);
  (* POSERR is called with an error message from post_process *)
  BEGIN
    WRITELN (TTY, '?ODMPOS -- ', s, '.');
    mdlerror := TRUE
  END (* procedure poserr *);

  PROCEDURE do_sym (str: STRING [80]);
  BEGIN (* just consolidates bookkeeping *)
    s := newsym;
    s^.stype := proc;
    s^.mnext := newmdl^.slist;
    newmdl^.slist := s;
    currid := str
  END (* procedure do_sym *);
$PAGE post_process initialize, read the runtime symbol file
BEGIN (* procedure post_process *)
  resptr := 400012b; (* for symbols in MAIN *)
  IF newmdl^.multiseg THEN BEGIN (* if sharables, put MTV in loseg *)
    newmdl^.mtvaddr := 140b;
    loseg := newmdl^.mtvsize + newmdl^.mtvaddr
  END
  ELSE BEGIN (* no sharables except MAIN *)
    newmdl^.mtvaddr := 400012b;
    loseg := 140b
  END;
  RESET (f, 'ODMSRT.SEX' || js.progdir); (* access the symbol file *)
  IF EOF (f) THEN poserr ('Can''t open runtime symbol file')
  ELSE BEGIN
    oldfirst := newmdl^.slist; (* remember to fix up later *)
    (* the first set of symbols in the symbol file are those symbols
	   used for communication with the debugger, which are emitted in
	   the inline code.  Such symbols should appear on the MTV only if
	   the system is in DEBUG mode.  Currently the only such symbol is
	   STMT., but we may as well be ready for others. *)
    LOOP
      getrtl;
      EXIT IF rtline = '';
      do_sym (rtline);
      IF newmdl^.debug THEN IF find (symname, newmdl)
	THEN poserr ('Control symbol ' || currid || ' defined by user')
      ELSE s^.name := newname (symname, s)
    END;
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
    LOOP
      getrtl;
      EXIT IF rtline = '';
      IF newmdl^.multiseg THEN BEGIN (* install the vectored symbols *)
	do_sym (rtline || '.AC');
	IF find (symname, newmdl)
	  THEN poserr ('Runtime vector ' || currid || ' defined by user')
	ELSE s^.name := newname (symname, s); (* put it there *)
	currid := rtline || '.TV'; (* the PSEUDONYM *)
	IF find (symname, newmdl)
	  THEN poserr ('Runtime pseudonym ' || currid || ' defined by user')
	ELSE s^.psname := newname (symname, s)
      END
    END;
    (* The next section of the symbol file is the list of debugger
	   runtime symbols, followed by their pseudonyms, separated by
	   a comma.  *)
    LOOP
      getrtl;
      EXIT IF rtline = '';
      ncnt := INDEX (rtline, ','); (* borrow an integer to find comma *)
      IF newmdl^.multiseg (* if we need to do it *)
      AND (ncnt <> 0) THEN BEGIN (* must have a real pseudonym line *)
	do_sym (SUBSTR (rtline, 1, ncnt-1));
	IF newmdl^.debug THEN BEGIN
	  IF find (symname, newmdl) THEN (* user could be using it too *)
	  s^.name := lastsym^.name (* so don't duplicate node *)
	  ELSE s^.name := newname (symname, s);
	  currid := SUBSTR (rtline, ncnt + 1); (* rest of it *)
	  IF find (symname, newmdl)
	    THEN poserr ('Debugger pseudonym ' || currid || ' defined by user'
	    )
	  ELSE s^.psname := newname (symname, s) (* the pseudonym *)
	END
      END
    END;
    (* The final section of the symbol file is the list of low segment
	   resident routines which must be placed on the transfer vector.
	   These have to be treated differently, since they are not included
	   on the LTV's of sharable segments, in fact they never appear there. *)
    LOOP
      getrtl;
      EXIT IF rtline = '';
      IF newmdl^.multiseg THEN BEGIN
	do_sym (rtline);
	IF find (symname, newmdl)
	  THEN poserr ('Low segment symbol ' || currid || ' defined by user')
	ELSE s^.name := newname (symname, s);
	s^.stype := loseg_entry
      END
    END;
    (* Finally, we have to make DEBUG$ one of these syms. *)
    IF newmdl^.debug THEN BEGIN
      do_sym ('DEBUG$');
      IF find (symname, newmdl) THEN poserr ('Symbol DEBUG$ defined by user')
      ELSE s^.name := newname (symname, s)
    END
    ELSE do_sym (''); (* a filler for it *)
    s^.stype := loseg_entry;
    CLOSE (f); (* we're done with it *)
    s := oldfirst; (* fix the chain, skip new ones *)
    WHILE s^.mnext <> oldfirst DO s := s^.mnext; (* find old last *)
    s^.mnext := newmdl^.slist (* and make it point to new first *)
  END (* file not at eof *);
$PAGE post_process the symbol definitions
  IF newmdl^.multiseg THEN BEGIN (* sharable, define LTV for res *)
    s := newmdl^.slist; (* symbols in each segment *)
    resptr := 400012b; (* LTV of sharable, id and stcheck *)
    REPEAT
      IF (s^.smod = NIL) AND (s^.stype <> loseg_entry) THEN BEGIN
	s^.ltvloc := resptr;
	resptr := resptr + 1
      END;
      s := s^.mnext
    UNTIL s = newmdl^.slist
  END (* resident LTV definitions *);
  m := newmdl^.mlist; (* assign static areas in loseg *)
  newmdl^.losegbreak := loseg; (* remember for skipping later *)
  shrcnt := -1;
  ncnt := 0;
  WHILE m <> NIL DO WITH m^ DO BEGIN
    storig := loseg; (* assign origin *)
    loseg := loseg + stsize; (* allocate needed size *)
    IF marea = NIL THEN BEGIN (* count as sharable *)
      shrcnt := shrcnt + 1;
      modid := - shrcnt
    END
    ELSE BEGIN
      ncnt := ncnt + 1;
      modid := ncnt
    END;
    m := next (* advance pointer *)
  END;
  loseg := newmdl^.losegbreak + newmdl^.stsize; (* skip all static areas *)
  a := newmdl^.alist; (* walk non-sharable areas to alloc *)
  WHILE a <> NIL DO WITH a^ DO BEGIN (* if any, indeed *)
    loseg := ((loseg + 777b) DIV 1000b) * 1000b; (* page bdy *)
    aorig := loseg;
    loseg := loseg + asize;
    a := next
  END;
  loseg := ((loseg + 777b) DIV 1000b) * 1000b; (* round up *)
  newmdl^.losegbreak := loseg; (* and remember *)
  s := newmdl^.slist; (* allocate MTV addresses *)
  loseg := newmdl^.mtvaddr; (* just use it as integer *)
  REPEAT
    WITH s^ DO BEGIN
      mtvloc := loseg;
      IF (smod <> NIL) AND (stype IN [proc,func,unknown])
	THEN loseg := loseg + 2 (* two for proc/func in ovl *)
      ELSE loseg := loseg + 1;
      IF (stype IN [variable, constant]) AND ( (smod = NIL) ORIF
	(smod^.name^.TEXT <> 'MAIN') )
	THEN poserr ('symbol ' || name^.TEXT || ' -- vars and consts in MAIN only.'
	);
      s := mnext (* next on master list *)
    END
  UNTIL s = newmdl^.slist;
  IF loseg > newmdl^.mtvaddr + newmdl^.mtvsize THEN BEGIN
    poserr ('MTV total size exceeded.');
    RETURN
  END;
  m := newmdl^.mlist; (* assign LTV in all modules *)
  WHILE m <> NIL DO BEGIN (* also check locals not on MTV *)
    IF m^.marea = NIL THEN loseg := resptr (* where to start *)
    ELSE loseg := m^.marea^.aorig + 12b; (* + 1 for mod ID word *)
    s := m^.syms; (* start with mod's syms *)
    IF s <> NIL THEN REPEAT
      WITH s^ DO BEGIN (* assign and check *)
	s := lnext;
	IF stype IN [proc, func, unknown] THEN BEGIN
	  ltvloc := loseg; (* assign *)
	  loseg := loseg + 1;
	  IF (mtvloc = -1) AND (name <> NIL) THEN (* MTV never assigned above *)
	  poserr ('symbol ' || name^.TEXT || (* name = NIL => FILLER in CONTAINS clause *)
	  ' not defined in SYMBOLS section')
	END
      END (* single symbol *);
    UNTIL s = m^.syms;
    m := m^.next
  END (* each module *);
END (* procedure post_process *);
$PAGE mdlpro the main program
PUBLIC
PROCEDURE mdlpro;
BEGIN
  infile := compfn;
  outfile := '';
  tlen := INDEX (infile, '=');
  IF tlen <> 0 THEN BEGIN
    outfile := SUBSTR (infile, 1, tlen - 1); (* before equals *)
    infile := SUBSTR (infile, tlen + 1) (* and the rest *)
  END;
  RESET (mdlfile, '.MDL ' || infile);
  IF outfile = '' THEN outfile := FILENAME (mdlfile) || '[,].MDO'
  ELSE outfile := '[,].MDO ' || outfile;
  IF EOF (mdlfile) THEN WRITELN (TTY, '?MDLPRO Can''t get input file.')
  ELSE BEGIN
    REWRITE (OUTPUT, outfile);
    IF NOT EOF (OUTPUT) THEN WRITELN (TTY, '?MDLPRO Can''t write output.')
    ELSE BEGIN
      outfile := FILENAME (OUTPUT);
      SCRATCH (OUTPUT);
      usepro;
      CLOSE (mdlfile);
      IF NOT mdlerror THEN post_process;
      IF mdlerror THEN BEGIN
	WRITELN (TTY, '?MDLPRO Errors detected.');
	outfile := ''; (* for going to ODMS *)
      END
      ELSE hpsave (outfile, newmdl)
    END
  END
END.
   