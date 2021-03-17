program odms options special (coercions);

(* the main program file for ODMS -- contains user command parser *)

$SYSTEM MMSSYM.TYP
$SYSTEM CMDUTL.TYP
$SYSTEM LOOKUP.TYP
$SYSTEM RUNUTL.INC
$SYSTEM MMTCOR.INC
$SYSTEM INFPAC.INC
$SYSTEM TIMUTL.INC
$SYSTEM JOBNUM.INC
$PAGE type declarations for this module
type
  keywordlist = array [keyword] of cmdlist;

const
  user_commands: kwdset := [use_cmd .. exit_cmd];
  info_kwd: kwdset := [area_cmd .. all_cmd];	(* INFO options *)

var
  names: keywordlist := (		(* the LOOKUP table *)
    ('USE',		1),
    ('COMPILE',		1),
    ('UPDATE',		2),
    ('DELETE',		2),
    ('RENUMBER',	1),
    ('PACK',		2),
    ('BUILD',		1),
    ('PRINT',		1),
    ('VERIFY',		4),
    ('HELP',		1),
    ('STOP',		2),
    ('QUIT',		1),
    ('EXIT',		1),
    ('DATABASE',	1),
    ('OVERLAY',		1),
    ('ASSEMBLY',	2),
    ('VERSION',		1),
    ('NEWVERSION',	1),
    ('FILES',		1),
    ('CHANNELS',	2),
    ('USING',		5),
    ('INFO',		1),
    ('AREAS',		1),
    ('MODULES',		1),
    ('PROGRAM',		3),
    ('ALL',		3),
    ('SYMBOLS',		1),
    ('RESIDENT',	8)
  );
$PAGE vars and labels
label	100,				(* to restart when no MDL active *)
	200;				(* to restart at other times *)

var
  cmd: cmdline;				(* current input line *)
  stidx: cmdlineidx;			(* indices into line *)
  dbfile, ovfile: file_name;		(* string parameters D= and O= *)
  assmfile : file_name;			(* File for ASSEMBLY option *)
  i: integer;				(* random integer *)
  symmod: modptr;			(* results from SYMB=modname *)
  files, channels: integer;		(* also parsed integers *)
  info_item: keyword;			(* keyword for INFO option *)
  using_str: string[255];		(* big string for USING option *)
  tmpcorstr: packed array [1..50] of char;	(* for calling TMPCOR *)

  curcmd,				(* current command *)
  cur_item: keyword;			(* word being parsed *)
  kwds_seen: kwdset;			(* to detect extraneous or missing *)
  prompt: string[8];			(* prompt can change. . . *)

external var
  curmdl: ^pnode;
  currid: mdlid;
  lastsym: symptr;
  lastmod: modptr;
  lastarea: areaptr;
  res_str: string[255];
  js: jobrec;
  dbvers, dbnvers: integer;
  curmod: modptr;
  compfn: file_name;
  mdlerror: boolean;

external function lookup (
  cmdline; var cmdlineidx; var keywordlist; keyword; var keyword): boolean;
external function hpload (file_name; boolean): ^pnode;
external function find (namekind; ^pnode): boolean;
external procedure bld_main (^pnode;integer;integer;file_name;cmdline;file_name);
external procedure bld_mod (^pnode; modptr; cmdline; file_name );
external procedure upd_ovl (file_name; file_name);
external procedure del_ovl (file_name);
external procedure ren_ovl (file_name);
external procedure prdbvers (file_name);
external procedure prtsym (modptr);
external procedure prtinf (keyword; ^pnode);
external procedure packdb (file_name; ^pnode);
external procedure vfyext (file_name);
external procedure vfdbvers (file_name);
external procedure vfyvers (file_name);
external procedure dbclose;
external procedure mdlpro;
$PAGE little helpers for everyone
procedure comerr (msg: string[80]);

(* COMERR is called with the substance of a message to be printed
   on the user's terminal. *)

begin
writeln (tty, cmd);			(* reprint the line *)
writeln (tty, ' ': (stidx-1), '^');	(* mark AFTER the token... *)
writeln (tty, '?ODMCOM -- ', msg, '.');
prompt := '*';				(* back to normal *)
if curmdl = nil then goto 100
else goto 200
end (* procedure comerr *);


function dolook (var x: keyword): boolean;

(* DOLOOK commonizes the LOOKUP function, advancing pointers as it goes. *)


begin
dolook := lookup (cmd, stidx, names, maximum (keyword), x)
end (* function dolook *);


public procedure io_error (fatal: boolean; s: string[80]);

(* IO_ERROR is called from the database manipulation routines to terminate
   the current operation.  If FATAL is true, the user must be warned that
   the database integrity might have been lost. *)

begin
  writeln (tty, '?ODMDIO -- Database I/O error -- ', s, '.');
  if fatal then writeln ('?ODMFAT -- Database integrity questionable.');
  dbclose;
  goto 200
end (* procedure io_error *);
$PAGE getstr and getcid helpers for getting components
function getstr: cmdline;

(* GETSTR extracts a string from the command line, starting at STIDX.
   The string is capitalized, and STIDX is incremented. *)

var
  loc: integer;

begin
  loc := index (substr (cmd, stidx), ' ', length (cmd) - stidx + 2);
  getstr := uppercase (substr (cmd, stidx, loc  - 1));
  stidx := stidx + length (getstr)
end (* function getstr *);


procedure getcid;

(* GETCID is similar to GETSTR, except that the name is truncated to
   six characters into the global CURRID, and any contained underscores
   are converted to percent signs. *)

var
  loc: integer;

begin
  currid := getstr;
  loop
    loc := index (currid, '_');
  exit if loc = 0;			(* all done *)
    currid [loc] := '%'
  end
end (* procedure getcid *);
$PAGE getnum to extract numeric field from command line
function getnum: integer;

(* GETNUM extracts an integer from the command line, not unlike GETSTR.
   If the delimiting character is B, it is eaten, and the conversion
   done in octal. *)

var
  str: string[30];

begin
  str := getstr;			(* up until next blank *)
  if str [ length(str) ] = 'B' then begin	(* wants octal, eh? *)
    getstring (substr (str, 1, length(str)-1), getnum:100:O);
    if iostatus <> io_ok then comerr ('Invalid octal constant')
    end
  else begin
    getstring (str, getnum);
    if iostatus <> io_ok then comerr ('Invalid decimal constant')
    end
end (* function getnum *);
$PAGE write_mod_file helpers   write_name and find_sharable
procedure write_mod_file;

(* WRITE_MOD_FILE uses the system name from the program node, writing
   the file containing the names of the system's modules, as the scalar
   type OVL_ID. *)

var
  count_num: integer;
  f: text;
  m: modptr;

  procedure write_name (n: mdlid);

  (*  This guy writes the string passed to it, ejecting lines and inserting
      commas between repeated calls. *)

    begin
    put (f);				(* ok to write that comma! *)
    if count_num mod 3 = 0 then writeln (f);	(* three names per line *)
    write (f, '  ', n);			(* the name *)
    write (f, '_OV');			(* the suffix *)
    f^ := ',';				(* defer a comma until next time *)
    count_num := count_num + 1
    end;



  procedure find_sharable (m: modptr);
    var m1: modptr;

    (* This guy stacks the names of the sharable modules, writing them out
       in reverse order. *)

    begin
    m1 := m;
    while (m1 <> nil) andif (m1^.marea <> nil) do m1 := m1^.next;
    if m1 <> nil then begin		(* it's sharable *)
      find_sharable (m1^.next);		(* recurse *)
      write_name (m1^.name^.text)
      end
    end;
$PAGE write_mod_file the body of write_mod_file
begin					(* write_mod_file *)
  rewrite (f, '[,].MOD ' || curmdl^.pname);
  if not eof (f) then comerr ('Can''t write module name file');
  count_num := 0;
  write (f, 'type OVL_ID = (');
  f^ := ' ';				(* no comma at first *)

  find_sharable (curmdl^.mlist);	(* write names of sharables *)

  m := curmdl^.mlist;
  while (m <> nil) do begin
    if m^.marea <> nil then write_name (m^.name^.text);
    m := m^.next
    end;

  writeln (f, ');');			(* conveniently blots deferred comma *)
  close (f)
end (* procedure write_mod_file *);
$PAGE do_use helper for the use command
procedure do_use;

(* DO_USE closes and reopens files before doing a USE command.  If
   successful or not, it prints a message.  If the terminal is not
   open for input, but we were runoff > 0, then we open the terminal
   here to suppress the greeting message in GETLINE. *)

Var loc_fname : file_name;
    idx : cmdlineidx;

begin
  close;
  curmdl := nil;
  loc_fname := substr ( cmd , stidx );

  idx:= index ( loc_fname , '=' );

  if idx <> 0
    then loc_fname := '.MDO ' || substr ( loc_fname , 1 , idx - 1 )
  else loc_fname := loc_fname || ' .MDO' ;

  curmdl := hpload ( loc_fname , false );

  rewrite (ttyoutput);
  if (tty <> nilf) orif (runoff > 0) then begin
    open (tty);
    if curmdl = nil then comerr ('Unable to open MDO file')
    else if curmdl^.odmsversion <> 300 then begin
      curmdl := nil;
      comerr ('Obsolete MDO file')
      end
    else begin				(* success, message and mod file *)
      writeln (tty, '[System (', curmdl^.pname, '), compiled ',
	dc_ext (curmdl^.cre_dt), ']');
      write_mod_file
      end
    end
end (* procedure getline *);
$PAGE do_compile helper for compile command
procedure do_compile;

(* DO_COMPILE sets everything up for the COMPILE command, giving the
   rest of the input line to the TMPCOR or ### file, and then running
   MDLPRO. *)

var i: integer;				(* var for tmpcor *)

begin
  compfn := substr (cmd, stidx);
  mdlpro;
  if not mdlerror then begin
    cmd := compfn;			(* set up a USE command *)
    stidx := 1;
    do_use
    end
end (* procedure do_compile *);
$PAGE do_help helper for the HELP command
procedure do_help;

(* DO_HELP reads the external help file for the selected command. *)

var
  help_file: text;			(* the external help file *)
  help_line: cmdline;			(* string for reading it *)
  help_command: keyword;		(* lookup rest of line *)
  target: string[10];			(* to find section of file *)

begin
  reset (help_file, 'ODMS.HLP' || js.progdir );
  if eof (help_file) then comerr ('Can''t locate ODMS.HLP file');

  if substr (cmd, stidx) = '' then	(* just HELP *)
    loop
      readln (help_file, help_line);	(* starting at start of file *)
    exit if help_line = '*****';	(* the section sentinel *)
      writeln (tty, help_line)
    end
  else if not dolook (help_command) orif	(* good command? *)
    not (help_command in user_commands) then	(* good keyword only *)
    comerr ('Invalid HELP subcommand')
  else begin				(* true command! *)
    target := names [help_command].name;	(* string from lookup table *)
    repeat
      readln (help_file, help_line)
      until help_line = target;		(* sentinel is command alone on line *)
    repeat				(* now write out text *)
      writeln (tty, help_line);
      readln (help_file, help_line)
      until help_line = '*****'
    end;
  close (help_file)
end (* procedure do_help *);
$PAGE comstr and cum_str for printing command errors
function comstr (x: keyword): string[10];

(* COMSTR returns the text of an ODMS command language keyword.  It is
   useful for general error messages requiring a command to be printed,
   and for turning keyword variables into text.  We get it out of the
   LOOKUP table, removing those ghastly trailing blanks. *)

begin
  comstr := names [x].name;
  while comstr [ length(comstr) ] = ' ' do	(* trailing blank *)
    comstr := substr (comstr, 1, length (comstr) - 1)
end (* function comstr *);



function cum_str (x: kwdset): cmdline;

(* CUM_STR accumulates a string of command language keywords corresponding
   to a KWDSET parameter.  Separate by commas, please. *)

var i: keyword;				(* for the FOR loop *)

begin
  cum_str := '';
  for i := minimum (keyword) to maximum (keyword) do
    if i in x then
      cum_str := cum_str || ', ' || comstr (i);
  cum_str := substr (cum_str, 3)	(* remove initial comma *)
end (* function cum_str *);
$PAGE chk_opt and chk_req checks optional and required qualifiers
procedure chk_opt (may_be: kwdset);

(* CHK_OPT checks that the accumulated set of qualifiers is legal for
   a given command.  The dispatcher calls this routine with its set of
   legal modifiers for its command; we verify that no extras were
   specified.  This catches silly errors like "DEL O=FOO".  *)

var
  extras: kwdset;

begin
  extras := kwds_seen - may_be;
  if extras <> [] then
    comerr ('Command may not have ' || cum_str (extras) || ' keyword(s)' )
end (* procedure chk_opt *);


procedure chk_req (must_be: kwdset);

(* CHK_REQ is just like CHK_OPT above, except that it checks for qualifiers
   which MUST appear for a given command.  *)

var
  missings: kwdset;

begin
  missings := must_be - kwds_seen;
  if missings <> [] then
    comerr ('Command must have ' || cum_str (missings) || ' keyword(s)' )
end (* procedure chk_req *);
$PAGE getline to read a command line from TTY
procedure getline;

(* GETLINE gets a non-blank line from TTY, after prompting appropriately. *)

begin
  if tty = nilf then begin		(* never opened *)
    open (tty);
    writeln (tty, 'TYM-Pascal ODMS Version 2(1)-1');
    if curmdl <> nil then
      writeln (tty, '[MDL for system (', curmdl^.pname, ') loaded]');
    writeln (tty)
    end;
  repeat
    write (tty, prompt);
    break (tty);
    readln (tty);
    read (tty, cmd)
  until cmd <> '';
  while cmd [length(cmd)] = ' ' do 	(* remove trailing blanks *)
    cmd := substr (cmd, 1, length (cmd) - 1 );
  stidx := 1;
end (* procedure getline *);
$PAGE TRIM_BLANKS and TRIM_EQUALS
Procedure TRIM_LEADING_BLANKS;

Begin
  While ( STIDX <= Length ( CMD ) ) Andif ( Ord(CMD[STIDX]) <= Ord(' ') ) Do
    STIDX := STIDX + 1
End;





Procedure TRIM_EQUALS;

Begin
  TRIM_LEADING_BLANKS;
  If ( STIDX <= Length ( CMD ) ) Andif ( CMD [ STIDX ] = '=' )
    Then Begin
      STIDX := STIDX + 1;
      TRIM_LEADING_BLANKS		(* Eat blanks after equals *)
    End
  Else COMERR ( '"=" expected' )
End;
$PAGE non_mod_cmd_lookup - NONmodule command lookup

function non_mod_cmd_lookup ( var x: keyword ) : boolean;

var idx : cmdlineidx;

begin

  (* lookup the command. If it is RESIDENT, MAIN, USING it simply
     performs a dolook function. If not and a KEYWORD is matched
     there must be an equals sign following the KEYWORD, else it must
     be a module so return FALSE. *)

  idx := stidx;		(* save it so we can restore it if need be *)

  non_mod_cmd_lookup := dolook ( x );

  if non_mod_cmd_lookup
  andif not ( x in [ using_cmd , resident_cmd ] )
    then begin
      (* Since a command was found, and it was not one of the above
	 an equals sign must follow, or I will assume that the word
	 is a MODULE name. *)

      trim_leading_blanks;	(* Find the next NOBLANK character *)

      if (stidx > length(cmd) ) Orif ( cmd [ stidx ] <> '=' )
	then begin
	  stidx := idx;		(* restore things *)
	  trim_leading_blanks;	(* Lookup has this effect even if it fails *)
	  non_mod_cmd_lookup := false
	end
    end

end;	(* non_mod_cmd_lookup *)
$PAGE no_shar routine to prohibit sharable overlays
procedure no_shar;

(* Certain ODMS operations cannot be performed on sharable overlays.
   This routine checks for that and fires if not. *)

begin
  if curmod^.marea = nil then
    comerr ('Module ' || curmod^.name^.text || ' is sharable -- ' ||
      comstr (curcmd) || ' illegal')
end (* procedure no_shar *);
$PAGE mainline -- get an MDL to work with

begin					(* ODMS *)
tty := nilf;
curmdl := nil;				(* must get one *)
prompt := '*';				(* for 'Using: ' prompt *)
rewrite (ttyoutput);			(* open that error message thingy *)
jobinfo (js);				(* to get hiseg PPN *)

100:					(* try for a startup *)
repeat					(* until we get something *)
if runoff > 0 then begin
  i := 50;
  if tmpcor ('ODM', tmpcor_df, ord(address(tmpcorstr)), i) then
    if tmpcorstr = '' then begin	(* error -- inhibit greeting *)
      open (tty);
      writeln (tty, '?ODMINI -- No MDL active.');
      getline
      end
    else cmd := 'USE ' || tmpcorstr
  else begin				(* no tmpcor, try ### *)
    reset (input, jobnum || 'ODM.TMP');
    if not eof (input) then begin
      read (input, tmpcorstr);		(* just like a tmpcor *)
      cmd := 'USE ' || tmpcorstr;
      close (input);
      rewrite (input, jobnum || 'ODM.TMP');
      scratch (input);
      if tmpcorstr = '' then begin	(* no MDL file, we're canned *)
	open (tty);			(* inhibit greeting *)
	writeln (tty, '?ODMINI -- No MDL active.');
	getline
	end
      end
    else getline
    end
  end
else getline;				(* as usual *)

if not dolook (curcmd) orif		(* don't recognize it *)
  not (curcmd in [use_cmd, compile_cmd, help_cmd..exit_cmd]) then
  comerr ('no MDL active')		(* or not legal command *)
else if curcmd = compile_cmd then do_compile	(* just do it *)
else if curcmd = help_cmd then do_help	(* this one too *)
else if curcmd = use_cmd then do_use
else stop				(* one of the exit commands *)
until curmdl <> nil;			(* gotta get something *)
$PAGE mainline -- got MDL, do commands

200:					(* here if MDL in effect *)
loop					(* forever, use STOP to quit *)
  getline;				(* input from wherever *)
  kwds_seen := [];			(* initialize parse variables *)
  curmod := nil; symmod := nil;
  dbfile := ''; ovfile := '';
  assmfile := '';			(* Empty unless programmed *)
  dbvers := -1; dbnvers := -1;
  files := -1; channels := 0;
  info_item := use_cmd;			(* something nonsensical *)

  if not dolook (curcmd) orif		(* unknown string *)
    not (curcmd in user_commands) then	(* known, but not command *)
    comerr ('unknown command');
  if curcmd = compile_cmd then do_compile	(* catch it now *)
  else if curcmd = help_cmd then do_help	(* this one too *)
  else if curcmd = use_cmd then do_use		(* and this one *)
  else begin				(* something else, get pars *)

    while stidx <= length (cmd) do begin	(* get all known words *)
      if not NON_MOD_CMD_LOOKUP ( cur_item )
	orif (cur_item = resident_cmd)	(* or RESIDENT keyword *)
	orif (stidx > length (cmd))	(* or no room for anything *)
	then begin			(* assume modulename if not resident *)
	if cur_item = resident_cmd then begin
	  if (resident_cmd in kwds_seen) or (curmod <> nil) then
	    comerr ('multiple BUILD module/RESIDENT specification')
	  else kwds_seen := kwds_seen + [resident_cmd]
	  end
	else begin
	  getcid;				(* get CURRID from line *)
	  if not find (modname, curmdl) then
	    comerr ('unknown module name [' || currid || ']')
	  else if curmod <> nil then
	    comerr ('unknown keyword [' || currid || ']')
	  else curmod := lastmodend
	end

      else begin			(* a good identifier *)
	If CUR_ITEM <> USING_CMD
	  Then TRIM_EQUALS;
	if stidx > length (cmd) then	(* oops *)
	  comerr ('no parameter value found');

	if cur_item in kwds_seen then 		(* eek -- already have one *)
	  comerr ('Multiple ' || comstr (cur_item) || ' keyword')
	else kwds_seen := kwds_seen + [cur_item];
	case cur_item of		(* whatever it is, get its param *)

	  overlay_cmd: ovfile := getstr;
	  database_cmd: dbfile := getstr;
	  assembly_cmd : Begin
			   assmfile := getstr;
			   While (Length(assmfile)>0) Andif (Ord(assmfile[1])<=Ord(' ')) Do
			     If Length ( assmfile ) = 1
			       Then assmfile := ''
			     Else assmfile := Substr ( assmfile , 2 );
			   if assmfile = ''
			     then comerr ( 'File name expected' )
			 End;

	  version_cmd:
	    if not dolook (cur_item) then
	      dbvers := getnum			(* unknown, numeric? *)
	    else if cur_item <> all_cmd then	(* only ALL suboption here *)
	      comerr (comstr (cur_item) || ' not valid here')
	    else kwds_seen := kwds_seen + [all_cmd];

	  newversion_cmd: dbnvers := getnum;
	  files_cmd: files := getnum;
	  channels_cmd: channels := getnum;

	  symbol_cmd: begin		(* so much for the simple ones *)
	    getcid;			(* isolate string *)
	    if not find (modname, curmdl) then
	      comerr ('unknown module name [' || currid || ']')
	    else symmod := lastmod
	    end (* symbol_cmd case *);

	  info_cmd:			(* get another keyword *)
	    if not dolook (info_item) then
	      comerr ('unknown INFO suboption'); (* else just leave it there *)

	  using_cmd: begin		(* munch rest of line *)
	    prompt := 'Using: ';
	    using_str := '';		(* init it *)
	    loop
	      using_str := using_str || substr (cmd, stidx);
	    exit if not (using_str [length(using_str)]  (* last char *)
		in ['%', '&'] ) do stidx := length (cmd) + 1;
	      using_str := substr (using_str, 1, length(using_str)-1 );
	      (* after removing last char, may want to add CRLF *)
	      if cmd [length (cmd)] = '&' then	(* amp means new line *)
		using_str := using_str || chr(15b) || chr(12b);  (* HACK! *)
	      getline;			(* check next input line for length *)
	      if length (using_str) + length (cmd) > 255 then
		comerr ('USING string too long')
	      end;
	    prompt := '*'		(* reset it *)
	    end (* using_cmd case *);

	  others: comerr ('invalid as command modifier')
	  end (* case *);
	end (* good identifier *);
      end (* while *);
$PAGE mainline -- dispatcher
(* now we've got the command all parsed.  Of course, we have to check for
   required parameters, as well as invalid parameters.  For example,
   the command "PACK V=23" is no good, although we haven't detected
   things like that yet. *)

    case curcmd of			(* the big dispatch *)

      build_cmd: if resident_cmd in kwds_seen then begin
	chk_opt ( [resident_cmd, using_cmd] );
	chk_req ( [resident_cmd, using_cmd] );
	res_str := using_str
	end
      else if curmod = nil then
	comerr ('BUILD command needs module ID or RESIDENT')
      else if curmod^.name^.text = 'MAIN' then begin
	chk_opt([using_cmd,database_cmd,files_cmd,channels_cmd,assembly_cmd]);
	chk_req ([using_cmd]);
	if files = -1 then files := 8;	(* set up defaults *)
	if dbfile = '' then dbfile := curmdl^.pname || '.ODB';
	bld_main (curmdl, files, channels, dbfile, using_str, assmfile)
	end
      else begin			(* regular module build *)
	chk_opt ( [using_cmd , assembly_cmd ] );
	chk_req ( [using_cmd] );
	if curmod^.syms = nil
	  then comerr ( 'No symbols defined for ' || curmod^.name^.text )
	else bld_mod (curmdl, curmod, using_str, assmfile )
	end (* build_cmd *);

      update_cmd:
	if curmod = nil then comerr ('UPDATE command needs module ID')
	else begin
	  chk_opt ([database_cmd, overlay_cmd, version_cmd]);
	  no_shar;			(* just checking up *)
	  if dbvers = -1 then dbvers := 0;	(* set up defaults *)
	  if dbfile = '' then dbfile := curmdl^.pname;
	  if ovfile = '' then ovfile := curmod^.name^.text;
	  upd_ovl (dbfile, ovfile)
	  end (* update command *);

      quit_cmd, exit_cmd, stop_cmd: stop;	(* your wish is my command *)


      delete_cmd: begin
	if curmod = nil then comerr ('DELETE command needs module ID');
	chk_opt ([database_cmd, version_cmd, all_cmd] );
	no_shar;
	if not (all_cmd in kwds_seen) andif (dbvers = -1) then 
	  dbvers := 0;
	if dbfile = '' then dbfile := curmdl^.pname;
	del_ovl (dbfile)
	end (* delete_cmd *);

      renumber_cmd: begin
	if curmod = nil then comerr ('RENUMBER command needs module ID');
	chk_opt ( [database_cmd, version_cmd, newversion_cmd] );
	chk_req ( [newversion_cmd] );
	no_shar;
	if dbvers = -1 then dbvers := 0;
	if dbfile = '' then dbfile := curmdl^.pname;
	ren_ovl (dbfile)
	end (* renumber_cmd *);

      print_cmd:
	if (curmod <> nil) orif (kwds_seen <= [database_cmd])
	then begin			(* module, or just PRINT DB *)
	  if curmod <> nil then no_shar;  (* sharables not allowed *)
	  chk_opt ( [database_cmd] );	(* the only allowable option *)
	  if dbfile = '' then dbfile := curmdl^.pname;
	  if curmod <> nil then prdbvers (dbfile)  (* just print the one *)

	  else begin			(* loop through non-sharables *)
	    curmod := curmdl^.mlist;
	    writeln (tty);
	    while curmod <> nil do begin
	      if curmod^.marea <> nil then begin (* header *)
		writeln (tty, 'Module ', curmod^.name^.text);
		prdbvers (dbfile);
		writeln (tty)
		end;
	      curmod := curmod^.next
	      end (* while *)
	    end (* loop of nonshars *)
	  end

	else if symmod <> nil then begin (* SYMB=<module> seen *)
	  chk_opt ( [symbol_cmd] );	(* only one allowed *)
	  prtsym (symmod)
	  end
	else begin			(* must be INFO option *)
	  chk_opt ( [info_cmd] + info_kwd );
	  chk_req ( [info_cmd] );
	  if not (info_item in info_kwd) then
	    comerr ('invalid INFO suboption');
	  prtinf (info_item, curmdl)
	  end (* print_cmd *);

      pack_cmd: begin
	if curmod <> nil then comerr ('Module not required');
	chk_opt ( [database_cmd] );
	if dbfile = '' then dbfile := curmdl^.pname;
	packdb (dbfile, curmdl)
	end (* pack_cmd *);

      verify_cmd: begin
	if curmod = nil then comerr ('VERIFY command needs module ID');
	if curmod^.marea = nil		(* SHARABLE *)
	  then begin
	    if not ( overlay_cmd in kwds_seen )
	      then comerr ( 'File must be specified for sharable overlays');
	    chk_opt ( [ overlay_cmd ] );
	    vfyext ( ovfile )
	  end
	else if overlay_cmd in kwds_seen then begin  (* external file *)
	  chk_opt ( [overlay_cmd] );
	  vfyext (ovfile)
	  end
	else begin
	  chk_opt ( [database_cmd, version_cmd, all_cmd] );
	  if dbfile = '' then dbfile := curmdl^.pname;
	  if all_cmd in kwds_seen then vfdbvers (dbfile)
	  else begin
	    if dbvers = -1 then dbvers := 0;
	    vfyvers (dbfile)
	    end
	  end
	end (* verify_cmd *);

      others: comerr (comstr (curcmd) || ' command not implemented')

      end (* case on command *);
    end (* got an interesting command *);
  end (* loop forever *);
end (* program odms *).
   @	4¡