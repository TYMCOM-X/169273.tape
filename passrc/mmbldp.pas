module mmbldp;

$Include MMSSYM.TYP
$INCLUDE INFPAC.INC
$PAGE external declarations for support package
external var
  res_str: string [255];		(* user resident string *)
  js: jobrec;				(* contains hiseg PPN *)

external function setupcode (file_name;file_name): boolean;
external procedure hiseg;
external procedure loseg;
external procedure portal (symptr);
external procedure jrst (mdlid);
External Procedure JRSTADDR ( Integer );
external procedure argnam (mdlid);
external procedure mtvent (mdlid; integer; integer);
external procedure alloc (integer);
external procedure padout;
external procedure fword (integer);
external procedure hwords (integer; integer);
external procedure extern (mdlid);
external procedure entry (mdlid);
external procedure define (mdlid; integer);
external procedure nhword (mdlid; mdlid);
External Procedure SHWORD ( Integer ; MDLID );
external procedure drop_text (integer; string[60]);
external procedure do_label (mdlid);
external procedure comment (string[60]);
External Procedure DROP_ADDR ( String [ 10 ] );
External Procedure JSP1 ( String [ 10 ] );
External Procedure PUSHJ17 ( String [ 10 ] );
external procedure finishcode;
$PAGE do_mtv  routine to emit master transfer vector
procedure do_mtv (mdl: ^pnode);

(* DO_MTV takes care of the dirty work involved in emitting the master
   transfer vector. *)

var
  count: integer;			(* for computing pad at end *)
  s: symptr;				(* for walking list *)
  str: string[6];			(* for converting to octal *)

begin
  if mdl^.multiseg then count := 0
  else begin
    count := 2;
    extern ( 'OVL.AC' );
    extern ( 'OVL.TV' );
  end;
  extern ( 'EX.OVL' );		(* force exception messages in *)
  s := mdl^.slist;
  repeat with s^ do begin
    if smod <> nil then begin		(* not resident *)
      if (smod <> mdl^.mlist) and (name <> nil) then
	entry (name^.text);		(* define it here *)
      if stype in [proc, func, unknown] then begin	(* gotta drop pushj *)
	if name = nil then begin		(* filler in module *)
	  JSP1 ( 'MOE.TV' );
	  fword (0)			(* extra word after error call *)
	  end
	else if smod <> mdl^.mlist then	(* not MAIN symbol *)
	  mtvent (name^.text, smod^.modid, ltvloc)
	else begin			(* homebrew pushj for main entry *)
	  PUSHJ17 ( 'OVL.TV' );
	  extern (name^.text);		(* out there in MAIN link *)
	  DROP_ADDR ( NAME^.TEXT );	(* MAIN module,, addr of routine *)
					(*   no need to fool with LTV *)
	  end;
	count := count + 2		(* both kinds two word entries *)
	end
      else if name = nil then		(* must be filler in MAIN *)
	JSP1 ( 'MOE.TV' )
      else begin			(* must be var in MAIN *)
	extern (name^.text);
	argnam (name^.text);		(* just drop the pointer *)
	count := count + 1
	end;
      end
    else if name = nil then		(* resident filler *)
      JSP1 ( 'MOE.TV' )
    else begin				(* resident symbol *)
      if mdl^.multiseg and (stype <> loseg_entry)
	Then JRSTADDR ( LTVLOC )
      else begin			(* normal pgm, go right to sym *)
	str := name^.text;
	extern (str);			(* it's not in this assembly *)
	jrst ( str )
      End;
      comment ('Resident symbol ' || name^.text);	(* just for drills *)
      count := count + 1
      end;

    s := mnext				(* next on master list *)
    end (* with *);
    until s = mdl^.slist;		(* once around the loop *)

  alloc (mdl^.mtvsize - count)		(* alloc what we didn't fill *)
end (* procedure do_mtv *);
$PAGE do_tables
procedure do_tables (
  dbname: file_name;			(* default database name *)
  files: integer;			(* size of file table *)
  channels: integer;			(* max # of channels *)
  mdl: ^pnode );			(* MDL pointer *)

(* DO_TABLES drops the tables needed by the overlay manager. *)

var
  shr: integer;				(* for counting *)
  a: areaptr;				(* for the area table *)
  str: string[6];			(* for constructing labels *)
  nsh: integer;				(* too *)
  m: modptr;				(* for walking *)

begin
  entry ('OVSTA.');			(* so manager can find it *)
  shr := 0;				(* count up non-sharables *)
  nsh := 0;				(* and non-sharables *)
  m := mdl^.mlist;
  while m <> nil do begin		(* walk module list *)
    if m^.modid < 0 then shr := shr + 1
    else if m^.modid > 0 then nsh := nsh + 1;	(* don't count MAIN *)
    m := m^.next
    end;

  comment ('Constant storage for overlay runtime');
  fword (0);				(* current overlay is MAIN *)
  fword (1);				(* initial MRU count is 1 *)
  alloc (2);				(* dispatch word, current channel *)
  comment ('Non-sharable overlay table size');
  fword (nsh * 4);			(* size of non-shr table to follow *)
  m := mdl^.mlist;
  while m <> nil do begin
    if m^.modid > 0 then begin		(* for all non-sharable overlays *)
      comment ('  Module ' || m^.name^.text);
      putstring (str, '$.A', (m^.marea^.aorig div 1000b):3:O);
      nhword (str, '$.F000');		(* area and file pointers *)
      hwords (m^.storig, m^.storig + m^.stsize - 1);	(* start and finish *)
      alloc (2)				(* version and database pointer *)
      end;
    m := m^.next
    end;

  m := mdl^.mlist;			(* now for the sharables *)
  comment ('Sharable module table');
  fword ((shr + 1) * 2);		(* size of sharable table *)
  if not mdl^.multiseg then alloc (2)	(* don't walk if only MAIN *)
  else while m <> nil do begin
    if m^.modid <= 0 then begin		(* for MAIN too *)
      comment ('  Overlay ' || m^.name^.text);
      putstring (str, '$.F', (1-m^.modid):3:O);	(* for file table entry *)
      If M^.MODID = 0
	Then SHWORD ( 400000b , STR )
      Else SHWORD ( 0 , STR );
      hwords (m^.storig, m^.storig + m^.stsize - 1)
      end;
    m := m^.next
    end;

  a := mdl^.alist;			(* drop the area table *)
  shr := 0;				(* gotta count them first *)
  while a <> nil do begin
    a := a^.next; shr := shr + 1
    end;

  comment ('Area table');
  fword (shr);
  a := mdl^.alist;			(* after size, drop area table *)
  while a <> nil do begin		(* with names, of course *)
    comment ('Area ' || a^.name^.text);
    putstring (str, '$.A', (a^.aorig div 1000b):3:O);
    do_label (str);
    hwords (a^.aorig, a^.asize);
    a := a^.next
    end;

  comment ('I/O control tables');
  fword (channels);			(* max channels *)
  fword (0);				(* cur channels *)
  comment ('File table');
  fword (files);			(* file table count *)

  nsh := 1;				(* always one file *)
  do_label ('$.F000');
  hwords (400000b,0);			(* database, no buffer *)
  fword (1);				(* initially associated *)
  fword (length (dbname));
  drop_text (8, dbname);

  m := mdl^.mlist;			(* every sharable overlay gets one *)
  if mdl^.multiseg then
    while m <> nil do begin
      if m^.modid <= 0 then begin
	nsh := nsh + 1;			(* keep a count *)
	putstring (str, '$.F', (1-m^.modid):3:O);
	do_label (str);
	fword (0); fword (1);		(* pointer and channel, MRU=1 *)
	if m = mdl^.mlist then str := mdl^.pname
	else str := m^.name^.text;	(* MAIN gets program name, others *)
	fword ( length (str) + 4);	(* extra for ext *)
	drop_text (8, str || '.EXE')
	end;
      m := m^.next
      end;

  if nsh > files then writeln (tty, '?ODMBLD -- Need larger file table.')
  else if nsh < files then alloc ( 11 * (files - nsh) );

  (* doseg *)
end (* procedure do_tables *);
$PAGE bld_main   build for the main program
public procedure bld_main
  ( mdl: ^pnode;			(* current mdl *)
    files: integer;			(* max # of files *)
    channels: integer;			(* max # of channels *)
    dbfile: file_name;			(* database file name *)
    using_str: string[25];		(* USING string *)
    assmfile: file_name );		(* For assembly, '' if no file *)

(* BLD_MAIN generates the files for creating the central executable for
   an overlaid system.  This .CMD file includes the overlay manager
   module as well as the low segment debugger, if needed.  If the system
   contains sharable overlays, then MAIN should be considered as a
   sharable overlay, and have its own LTV in the high segment.  If
   there are no sharable overlays, then the MTV can go in the high
   segment. *)

var
  f: text;
  s: symptr;
  a: areaptr;
  m: modptr;
  i: integer;

begin
  rewrite (f, mdl^.pname || ' [,].CMD');
  if not eof (f) then
    writeln (tty, '?MDLBLD -- Can''t create LINK command file.')
  else with mdl^ do begin		(* let's write command file *)
    writeln (f, pname || '.SRL');	(* our symbol file *)
    if kicode then
    writeln (f, '/s OVLMKI', js.progdir) 	(* the overlay manager *)
    else
    writeln (f, '/s MMOVLM', js.progdir);	(* the overlay manager *)
    if multiseg then begin		(* include the segment swapper *)
      writeln (f, 'MMDSEG', js.progdir);  (* if needed *)
      if debug then			(* same with the low segment *)
	begin if kicode then
     	    writeln (f, '/s LSDBKI', js.progdir)   (* debugger *)
	  else
	    writeln (f, '/s LSGDEB', js.progdir)   (* debugger *)
	end
    end;
    writeln (f, using_str);		(* user link string *)
    if res_str <> '' then
      writeln (f, res_str);		(* user resident component *)
    writeln (f, mdl^.pname, '/SSAVE/G'); (* finish up *)
    close (f);

    if not setupcode (pname , assmfile ) then
      writeln (tty, '?MDLBLD -- Can''t create assembly file.')
    else begin
      comment ('');
      comment ('Symbol file for MAIN of system ' || pname);
      comment ('');
      hiseg;
      fword (0);			(* MAIN is always zero *)
      hwords (0,0);			(* hope no one checks static! *)
      
      if multiseg then begin	(* emit PORTALS for residents *)
	comment ('');
	comment ('PORTALS to resident routines');
	comment ('');
	s := slist;
	repeat
	  if (s^.smod = nil) and (s^.stype <> loseg_entry)
	  then begin			(* it's a resident *)
	    if s^.psname <> nil then	(* a pseudonym to define *)
	      entry (s^.psname^.text);
	    portal (s)
	    end;
	  s := s^.mnext
	  until s = slist;

	comment ('');			(* LTV for MAIN *)
	comment ('LTV for symbols in MAIN segment');
	comment ('');
	s := mlist^.syms;		(* the MAIN local list *)
	if s <> nil then repeat		(* could be nil --> no way back *)
	  if s^.stype in [proc, func, unknown] then
	    If S^.NAME = Nil
	      Then JSP1 ( 'MOE.TV' )
	    else begin
	      extern (s^.name^.text);
	      jrst (s^.name^.text)
	      end;
	  s := s^.lnext
	  until s = mlist^.syms;
	loseg				(* put MTV in loseg! *)
	end
      Else extern ('MOE.TV' );		(* If not multiseg then *)

      comment ('');
      comment ('Master transfer vector (MTV)');
      comment ('');
      do_mtv (mdl);

      loseg;
      i := 0;
      m := mlist;			(* allocate static for overlays *)
      while m <> nil do begin
	comment (''); comment ('Static for module ' || m^.name^.text);
	comment ('');
	alloc (m^.stsize);
	i := i + m^.stsize;
	m := m^.next
	end;

      if mdl^.stsize > i then alloc (mdl^.stsize - i);

      if alist <> nil then begin	(* reserve overlay areas *)
	padout;
	a := alist;
	repeat
	  comment ('');comment ('Overlay area ' || a^.name^.text);comment('');
	  alloc ( ((a^.asize + 777b) div 1000b) * 1000b );
	  a := a^.next
	  until a = nil
	end;

      comment (''); comment ('Tables for overlay manager'); comment ('');
      do_tables (dbfile, files, channels, mdl);

      finishcode
      end
    end
end (* procedure bld_main *);
$PAGE bld_mod   build for an overlay module
public procedure bld_mod
  ( mdl: ^pnode;			(* current mdl *)
    curmod: modptr;			(* module to build *)
    using_str: string [255];		(* USING string *)
    assmfile: file_name );		(* For assembly listing, '' if no list *)

(* BLD_MOD performs the BUILD functions for a module BUILD.  The LINK
   command file is created here.  If the module is sharable, we include
   the user resident string RES_STR.  Then we create the .SYM file for
   MACRO.  Again, if the module is sharable, we walk the master symbol
   list, emitting PORTALs for resident routines.  Then we walk the local
   list, emitting JRSTs for the module's symbols.  Finally, we DEFINE the
   MTV locations for all symbols not covered in the above cases.  *)

var
  f: text;				(* for writing .CMD file *)
  s: symptr;				(* for walking local sym list *)

begin
  rewrite (f, curmod^.name^.text || ' [,].CMD');	(* create command file *)
  if not eof (f) then 
    writeln (tty, '?MDLBLD -- Can''t create LINK command file.')
  else with curmod^ do begin		(* jump right in *)
    writeln (f, '/nosymbols');		(* no link s.t. in non-main *)
    writeln (f, '/SET:.LOW.:', storig:6:O);	(* loseg reloc'n counter *)
    if marea <> nil then
      writeln (f, '/SET:.HIGH.:', marea^.aorig:6:O);	(* hiseg if not shar *)
    writeln (f, name^.text, '.SRL');	(* include symbol fakeup *)
    if marea = nil then			(* sharables must include ovlm *)
      begin if mdl^.kicode then
          writeln (f, 'OVLMKI', js.progdir) 
	else
          writeln (f, 'MMOVLM', js.progdir) 
      end;
    writeln (f, using_str);		(* the user's USING string *)
    if marea = nil then begin		(* special stuff for sharables *)
      if res_str <> '' then writeln (f, res_str);	(* user resident *)
      writeln (f, name^.text,'/SSAVE/G') (* LINK to produce .EXE *)
      end
    else begin
      if mdl^.kicode then
	writeln(f, '/S INSTRS', js.progdir);
      writeln (f, name^.text, '/SAVE/G')      (* non-shar for others *)
      end;
    close (f);				(* done with command file *)

    if not setupcode (name^.text,assmfile) then	(* can we do it? *)
      writeln (tty, '?MDLBLD -- Can''t create assembly file.')
    else begin				(* generate sym file *)
      comment ('');
      comment ('Symbol BUILD file for module ' || name^.text);
      comment ('');
      hiseg;				(* all in high segment *)
      fword (modid);			(* module identification cell *)
      hwords (storig, storig+stsize-1);	(* limits of mod's static storage *)
      if marea = nil then begin		(* if sharable then *)
	extern ( 'INITP.' );		(*    force rt monitor in and *)
	extern ( 'EX.OVL' );		(*    force excep msgs in *)
      end;

      comment ('');			(* header for resident section *)
      if marea = nil then
	comment ('Resident PORTALs for LTV of sharable overlay')
      else comment ('Resident definitions for non-sharable overlay');
      comment ('');

      s := mdl^.slist;			(* do something with residents *)
      repeat
	if (s^.smod = nil) and (s^.stype <> loseg_entry) then  (* a resident *)
	  if marea = nil then begin	(* for sharables, emit PORTAL *)
	    if s^.stype in [proc,func, unknown] then begin
	      if s^.psname <> nil then	(* if it has a pseudonym, use it *)
		entry (s^.psname^.text);
	      portal (s)
	      end
	    end
	  else if s^.name <> nil then	(* non-shar, define MTV value *)
	    define (s^.name^.text, s^.mtvloc);
	s := s^.mnext
	until s = mdl^.slist;

      comment ('');
      comment ('LTV for this module''s symbols');
      comment ('');

      s := syms;			(* this module's symbols *)
      repeat
	if s^.name = nil then		(* emit the local *)
	  JSP1 ( 'MOE.TV' )
	else begin
	  extern (s^.name^.text);
	  jrst (s^.name^.text)
	  end;
	s := s^.lnext
	until s = syms;			(* once around the ring *)

      comment ('');
      comment ('Definitions for symbols in other overlays');
      comment ('');

      s := mdl^.slist;			(* now define syms in other modules *)
      repeat
	if (s^.smod <> curmod) and 	(* not local *)
	  (s^.name <> nil) and		(* not a filler *)
	  ( (s^.smod <> nil) orif (s^.stype = loseg_entry) ) then
	  define (s^.name^.text, s^.mtvloc);	(* neither local nor res *)
	s := s^.mnext
	until s = mdl^.slist;

      finishcode			(* all done *)
      end
    end
end (* procedure bld_mod *).
  