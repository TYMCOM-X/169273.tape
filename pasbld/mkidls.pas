module mmmdls options special (ptr);

(* support for ODMS USE command -- input and symbol table primitives. *)

$INCLUDE MMSSYM.TYP
$INCLUDE MMSMDL.TYP
$INCLUDE TIMUTL.INC
$PAGE tables for scanner
const
  start_for_len: array [1..10] of 1..100 := (
    1, 2, 4, 7, 9, 13, 17, 22, 27, 29 );

  token_in_slot: array [1..28] of mdlwords := (
    idtok,
    intok, idtok,
    vartok, endtok, idtok,
    sizetok, idtok,
    areatok, constok, debtok, idtok,
    systok, filltok, kitok, idtok,
    symtok, machtok, modtok, stortok, idtok,
    shartok, functok, containstok, restok, idtok,
    proctok, idtok );

var
  string_for_slot: array [1..28] of string[9] := (
    '',
    'IN', '',
    'VAR', 'END', '',
    'SIZE', '',
    'AREAS', 'CONST', 'DEBUG', '',
    'SYSTEM', 'FILLER', 'KICODE', '',
    'SYMBOLS', 'MACHINE', 'MODULES', 'STORAGE', '',
    'SHARABLE', 'FUNCTION', 'CONTAINS', 'RESIDENT', '',
    'PROCEDURE', '' );
$PAGE externals and storage for module
external var
  mdlfile: text;			(* declared in main datamod *)
  currid: mdlid;			(* most recently read ID *)
  newmdl: ^pnode;			(* where to build new MDL *)
  curtok: mdltoken;			(* what we just scanned *)
  intval: integer;			(* most recently scanned integer *)
  mdlerror: boolean;			(* true if invalid MDL read *)
  lastsym: symptr;			(* last symbol record looked up *)
  lastmod: modptr;			(* last module record *)
  lastarea: areaptr;			(* and last area record looked up *)

var
  inline: string[255];
  stidx, endidx: 0..256;
  linenum: integer;
$PAGE lookup scans reserved words
procedure lookup;

(* LOOKUP works just like the PASCAL compiler's reserved word lookup
   routine (also like INDENT).  Essentially, insert the token to be
   classified into the table, then scan all known words of that
   length.  If no others match, the one we put into the table will,
   with a corresponding ID type of ID (not reserved word). *)

var
  i, j: integer;
  s: string[255];			(* to hold uppercased ID *)

begin
  i := endidx - stidx;			(* length of ID *)
  s := uppercase (substr (inline, stidx, i));	(* get it uppercased *)
  if (i > 9) or (i < 2) then curtok := idtok	(* not right size *)
  else begin
    string_for_slot [start_for_len [i+1] -1] := s;

    for j := start_for_len [i] to start_for_len [i+1] -1 do
      exit if string_for_slot [j] = s
	do curtok := token_in_slot [j]	(* will match last if no others *)
    end
end (* procedure lookup *);
$PAGE wrtline to assist in error messages
public procedure wrtline;

(* WRTLINE writes out the original user line, the line number, and
   a pointer to the var ENDIDX, which in general is the position
   being scanned at the moment. *)

begin
  mdlerror := true;
  writeln (tty);
  writeln (tty, '**     ', inline);
  writeln (tty, linenum:5, ' ': endidx, '^')
end (* procedure wrtline *);
$PAGE gettoken helpers to read lines and flag errors
public procedure gettoken;

(* GETTOKEN is called from the recursive descent parser to read in
   some more text and classify the next token.  Fairly straightforward. *)

label 100,				(* eof *)
      200;				(* bad character seen *)

  procedure getline;

  (* GETLINE reads a nonblank lne into INLINE, and catches EOF. *)

  begin
    if eof (mdlfile) then begin
      curtok := eoftok;
      goto 100
      end
    else begin
      repeat
	readln (mdlfile, inline);
	linenum := linenum + 1;
      until inline <> '';
      stidx := 1; endidx := 1
      end
  end (* procedure getline *);

  procedure scanerr (x: string[100]);

  (* SCANERR writes an error message from the scanner, sets the
     error flag, and restarts the scanner past the error. *)

  begin
    wrtline;
    writeln (tty, '?MDLSCN -- ', x, '.');
    goto 200
  end (* procedure scanerr *);
$PAGE gettoken the body of gettoken
begin					(* start of gettoken *)
  200:					(* to restart after error *)
  stidx := endidx;			(* after end of old one *)

  if stidx > length (inline) then getline;	(* prime the pump *)
  endidx := verify (substr (inline, stidx), [' ']);
  if endidx = 0 then begin		(* end of old line *)
    getline;				(* guaranteed non-blank *)
    stidx := verify (inline, [' '])	(* so just set it up *)
    end
  else stidx := stidx + endidx - 1;	(* stidx is index into original *)

  if (inline [stidx] = '(') andif (stidx < length (inline)) andif
    (inline [stidx + 1] = '*') then begin	(* opening comment *)
    stidx := stidx + 2;			(* go past it *)
     loop				(* to eat the whole thing *)
       if stidx > length (inline) then getline;	(* let eof get hit *)
       endidx := index (substr (inline, stidx), '*');
     exit if (endidx <> 0) andif
       (endidx + stidx - 1 < length (inline)) andif
       (inline [endidx + stidx] = ')') do
	 stidx := stidx + endidx + 1;		(* get past delimiter *)
       if endidx = 0 then getline	(* whole line is no good *)
	 else stidx := endidx + stidx	(* get past asterisk *)
      end (* loop *);
    endidx := stidx;
    goto 200
    end (* within a comment *);

  endidx := stidx;			(* all set to scan something *)
  case inline [stidx] of
    'A'..'Z', 'a'..'z', '$', '.', '%', '_':	(* the words *)
      begin
	endidx := endidx + verify (substr (inline, endidx + 1),
	  ['A'..'Z', 'a'..'z', '$', '.', '%', '_', '0'..'9'],
	  length (inline) - endidx + 1); (* whew -- get end of token *)
	lookup;				(* reserved word or not *)
	if curtok = idtok then		(* if not reserved, save text *)
	  begin
	  currid := uppercase (substr (inline, stidx, endidx-stidx));
	  loop
	    stidx := index (currid, '_');	(* turn _ into % *)
	  exit if stidx = 0;
	    substr (currid, stidx, 1) := '%'
	    end
	  end
      end (* identifier or other word case *);

    '0'..'9':				(* the numbers *)
      begin
	curtok := numtok;
	endidx := endidx + verify (substr (inline, endidx + 1),
	  ['0'..'9'], length (inline) - endidx + 1);	(* same thing *)
	if (endidx <= length (inline)) andif
	  (uppercase (inline [endidx]) = 'B') then begin (* octal *)
	  getstring (substr (inline, stidx, endidx - stidx), intval:40:o);
	  if iostatus <> io_ok then scanerr ('bad octal constant');
	  endidx := endidx + 1		(* skip over the 'B' *)
	  end
	else begin			(* decimal *)
	  getstring (substr (inline, stidx, endidx - stidx), intval);
	  if iostatus <> io_ok then scanerr ('bad decimal constant')
	  end
      end (* digit case *);

    ',':				(* yes, a comma is a token *)
      begin
	curtok := commatok;
	endidx := endidx + 1
      end;

    others:				(* i don't understand *)
      begin
	endidx := endidx + 1;		(* skip it *)
	scanerr ('invalid character')
      end

    end (* case *);
  100:					(* eof, just leave scanner *)
end (* procedure gettoken *);
$PAGE newsym, newmod, and newarea initializing routines
(* These routines centralize initialization of table records *)

public function newsym: symptr;

begin
new (newsym);
with newsym^ do begin
  mtvloc := -1; ltvloc := -1;
  stype := unknown;
  name := nil;  psname := nil;
  smod := nil;
  mnext := nil;
  lnext := nil
  end
end (* function newsym *);


public function newmod: modptr;

begin
new (newmod);
with newmod^ do begin
  stsize := 0;   storig := -1;
  name := nil; modid := -1;
  marea := nil; manext := nil;
  next := nil;
  syms := nil;
  end
end (* function newmod *);


public function newarea: areaptr;

begin
new (newarea);
with newarea^ do begin
  asize := -1;   aorig := -1;
  name := nil;
  next := nil;
  mods := nil
  end
end (* function newarea *);
$PAGE newname insertion into name table
public function newname (t: namekind; p: ptr): nameptr;

(* NEWNAME places CURRID into the name table with the class T, linked
   to whatever pointer is passed in as P.  To do so, do a search down
   the tree, until we step onto a nil node.  That is where we insert.
   The helper routine INSTALL either builds the new node, or walks us
   down another level. *)

var
  done: boolean;			(* tells us when to quit *)

  function install (var hook: nameptr): nameptr;
  begin
  if hook = nil then begin		(* right here, babe *)
    new (hook); done := true;
    with hook^ do begin
      left := nil; right := nil;
      text := currid; kind := t;
      aptr := p
      end
    end;
  install := hook
  end (* helper install *);

begin					(* function newname *)
done := false;
newname := install (newmdl^.ntree);	(* incase tree is empty *)
while not done do
  if newname^.text < currid then newname := install (newname^.right)
  else if newname^.text > currid then newname := install (newname^.left)
  else if newname^.kind < t then newname := install (newname^.right)
  else if newname^.kind > t then newname := install (newname^.left)
  else assert (false)
end (* function newname *);
$PAGE tree_delete delete name tree
procedure tree_delete (var x: nameptr);

(* TREE_DELETE is a quick recursive routine to dispose a name tree. *)

begin					(* note -- x is never nil!! *)
if x^.left <> nil then tree_delete (x^.left);
if x^.right <> nil then tree_delete (x^.right);
dispose (x);
x := nil
end (* procedure tree_delete *);
$PAGE mdldel delete mdl data structure
public procedure mdldel (var x: ^pnode);

(* MDLDEL deletes an MDL data structure, the lists of area, module, and
   symbol nodes, and the name tree. *)

  var
    ta: areaptr;
    tm: modptr;
    ts: symptr;

begin
with x^ do begin
  while alist <> nil do begin
    ta := alist^.next;
    dispose (alist);
    alist := ta
    end;

  while mlist <> nil do begin
    tm := mlist^.next;
    dispose (mlist);
    mlist := tm
    end;

  while slist <> nil do begin
    ts := slist^.mnext;
    dispose (slist);
    slist := ts
    end;

  if ntree <> nil then tree_delete (ntree)
  end;

dispose (x);
x := nil
end (* procedure mdldel *);
$PAGE mdlinit initialize program node before mdl processing
public procedure mdlinit;

(* MDLINIT initializes the fields of the program node for MDL processing.
   In addition, the predefined names are added here.  Finally, the
   scanner is initialized.  *)

var
  colon, dot: integer;			(* for isolating MDL name *)

begin
linenum := 0;
inline := '';
stidx := 1; endidx := 1;
new (newmdl);
with newmdl^ do begin
  cre_dt := daytime;
  mdlname := filename (mdlfile);	(* save string form *)
  colon := index (mdlname, ':');
  dot := index (mdlname, '.');
  pname := substr (mdlname, colon+1, dot-colon-1);
  odmsversion := 300;			(* version 3.0, if you will *)
  alist := nil;
  slist := nil;
  ntree := nil;
  stsize := 0;
  mtvsize := -1;
  mtvaddr := -1;
  debug := false;
  kicode := true;
  multiseg := false;
  mlist := newmod;			(* get a mod record *)
  currid := 'MAIN';			(* fake up to predefine module *)
  mlist^.name := newname (modname, mlist)
  end
end (* procedure mdlinit *).
 