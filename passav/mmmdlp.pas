module mmmdlp options special (ptr);

(* MDLPRO program, recursive descent parser for MDL. *)

$INCLUDE MMSSYM.TYP
$INCLUDE MMSMDL.TYP
$PAGE external declarations for helper routines
external function newsym: symptr;
external function newmod: modptr;
external function newarea: areaptr;
external function newname (namekind; ptr): nameptr;
external function find (namekind; ^pnode): boolean;

external procedure gettoken;
external procedure wrtline;

external procedure mdlinit;

external var
  lastsym: symptr;
  lastmod: modptr;
  lastarea: areaptr;
  mdlerror: boolean;
  newmdl: ^pnode;
  currid: mdlid;
  curtok: mdltoken;
  intval: integer;
$PAGE mdlerr parsing error message writer
procedure mdlerr (x: string[100]);

(* MDLERR writes a message to tty for a parsing error.  The character '&'
   occuring in the input string is to be replaced with CURRID, the most
   recently parsed identifier. *)

  var i: integer;

begin
mdlerror := true;
wrtline;
i := index (x, '&');
if i = 0 then writeln (tty, '?MDLPAR -- ', x, '.')
else writeln (tty, '?MDLPAR -- ', substr (x, 1, i-1), currid,
  substr (x, i+1), '.')
end (* procedure mdlerr *);
$PAGE storspec
function storspec (var st: integer): boolean;

(* <storspec> ::= <id> [SIZE] <num> *)

  var m: modptr;

begin
st := 0;
if curtok <> idtok then storspec := false
else begin
  storspec := true;
  if not find (modname, newmdl) then begin
    mdlerr ('undefined module &');
    m := nil
    end
  else m := lastmod;
  gettoken;
  if curtok = sizetok then gettoken;
  if curtok <> numtok then mdlerr ('numeric expected')
  else begin
    if m <> nil then m^.stsize := intval;
    st := intval;
    gettoken
    end
  end
end (* function storspec *);
$PAGE storsect
function storsect: boolean;

(* <storsect> ::= <storhead> <storlist>
   <storhead> ::= STORAGE [SIZE] <num>
   <storlist> ::= <storspec> [<storlist>]   *)

  var i, sum: integer;

begin
if curtok <> stortok then storsect := false
else begin
  storsect := true;
  gettoken;
  if curtok = sizetok then gettoken;
  if curtok <> numtok then mdlerr ('numeric expected')
  else begin
    newmdl^.stsize := intval;
    gettoken;
    sum := 0;
    while storspec (i) do sum := sum + i;	(* count them up *)
    if sum > newmdl^.stsize then mdlerr ('total storage exceeded')
    end
  end
end (* function storsect *);
$PAGE inclause
function inclause (var m: modptr): boolean;

(* <inclause> ::= IN <id>
	      ::= IN RESIDENT     *)

begin
m := nil;
if curtok <> intok then inclause := false
else begin
  inclause := true;
  gettoken;
  if curtok = restok then gettoken	(* all set, just eat it *)
  else if curtok <> idtok then mdlerr ('module or RESIDENT expected')
  else begin
    if not find (modname, newmdl) then mdlerr ('undefined module &')
    else m := lastmod;
    gettoken
    end
  end
end (* function inclause *);
$PAGE symspec
function symspec (var s: symptr): boolean;

(* <symspec> ::= <id> ( VAR | CONST | PROCEDURE | FUNCTION ) *)

begin
s := nil;
if curtok <> idtok then symspec := false
else begin
  symspec := true;
  if find (symname, newmdl) then begin	(* it's there *)
    if lastsym^.mnext <> nil then	(* on MTV, multiply defined *)
      mdlerr ('symbol & multiply defined')
    else s := lastsym
    end
  else begin
    s := newsym;
    s^.name := newname (symname, s)
    end;
  gettoken;
  if not (curtok in [proctok, functok, vartok, constok]) then
    mdlerr ('unknown symbol class')
  else begin
    if s <> nil then
      with s^ do
	if curtok = proctok then stype := proc
	else if curtok = functok then stype := func
	else if curtok = vartok then stype := variable
	else stype := constant;
    gettoken
    end
  end
end (* function symspec *);
$PAGE symdef
function symdef (var s: symptr): boolean;

(* <symdef> ::= ( FILLER | <symspec> ) [<inclause>]   *)

var
  c: symptr;				(* for chaining LTV chain *)
  m: modptr;				(* in case sym is nil *)

begin
if curtok = filltok then begin
  s := newsym;
  gettoken
  end
else if not symspec (s) then begin
  symdef := false;
  return
  end;
symdef := true;

if inclause (m) then begin
  if s = nil then return		(* otherwise process normally *)
  end
else if (s <> nil) andif (s^.stype in [variable, constant]) then
  m := newmdl^.mlist;			(* vars and consts default to MAIN *)

if s <> nil then begin
  s^.smod := m;
  if m <> nil then
    if s^.lnext = nil then begin	(* if not on LTV previously *)
      if m^.syms = nil then		(* the first *)
	m^.syms := s
      else begin			(* find end and chain it on *)
	c := m^.syms;
	while c^.lnext <> m^.syms do c := c^.lnext;
	c^.lnext := s			(* slip it on *)
	end;
      s^.lnext := m^.syms		(* make last point to first *)
      end
  end
end (* function symdef *);
$PAGE symsect
function symsect: boolean;

(* <symsect> ::= SYMBOLS [ [SIZE] <num> ]  <symdeflist>
   <symdeflist> ::= <symdef> [<symdeflist>]   *)

  var
    send, stmp: symptr;

begin
if curtok <> symtok then symsect := false
else begin
  symsect := true;
  gettoken;
  newmdl^.mtvsize := -1;
  if curtok = sizetok then begin
    gettoken;
    if curtok <> numtok then mdlerr ('numeric expected')
    else begin
      newmdl^.mtvsize := intval;
      gettoken
      end
    end
  else if curtok = numtok then begin
    newmdl^.mtvsize := intval;
    gettoken
    end;
  if not symdef (stmp) then mdlerr ('Null symbols section');
  newmdl^.slist := stmp;		(* the first one *)
  stmp^.mnext := stmp;			(* and the last, pointing to first *)
  send := stmp;				(* monitor last always *)
  while symdef (stmp) do 
    if stmp <> nil then begin
      stmp^.mnext := send^.mnext;	(* fix pointer of new one *)
      send^.mnext := stmp;		(* now point old last to new one *)
      send := stmp			(* finally, new last to chain onto *)
      end
  end
end (* function symsect *);
$PAGE cclause
function cclause (m: modptr): boolean;

(* <cclause> ::= CONTAINS <idlist>
   <idlist> ::= ( <id> | FILLER ) [, <idlist>]   *)

  var cend: symptr;

begin
if curtok <> containstok then cclause := false
else begin
  cclause := true;
  cend := newsym;
  m^.syms := cend;			(* dummy for chaining *)
  repeat
    gettoken;
    if curtok = filltok then begin
      cend^.lnext := newsym;
      cend := cend^.lnext;
      cend^.lnext := m^.syms^.lnext;	(* always points to first valid *)
      gettoken
      end
    else if curtok <> idtok then mdlerr ('identifier expected')
    else if find (symname, newmdl) then mdlerr ('symbol & multiply defined')
    else begin
      cend^.lnext := newsym;
      cend := cend^.lnext;
      cend^.lnext := m^.syms^.lnext;
      cend^.name := newname (symname, cend);
      gettoken
      end
    until curtok <> commatok;
  cend := m^.syms;			(* remove dummy *)
  m^.syms := cend^.lnext;
  dispose (cend)
  end
end (* function cclause *);
$PAGE moddef
function moddef (var m: modptr): boolean;

(* <moddef> ::= <id> <arearef> [<cclause>]
   <arearef> ::= ( IN <id> | SHARABLE )   *)

var
  mend: modptr;

begin
m := nil;
if curtok <> idtok then moddef := false
else begin
  moddef := true;
  if find (modname, newmdl) then begin
    mdlerr ('module & multiply defined');
    m := lastmod
    end
  else begin
    m := newmod;
    m^.name := newname (modname, m)
    end;
  gettoken;
  if curtok = shartok then begin	(* we know to set multiseg now *)
    gettoken;
    newmdl^.multiseg := true
    end
  else if curtok <> intok then mdlerr ('area or SHARABLE expected')
  else begin
    gettoken;
    if curtok <> idtok then mdlerr ('area expected')
    else begin
      if not find (areaname, newmdl) then mdlerr ('undefined area &')
      else begin			(* put mod on end of area *)
	m^.marea := lastarea;
	mend := m^.marea^.mods;		(* current mod chain *)
	if mend = nil then m^.marea^.mods := m	(* first one *)
	else begin
	  while mend^.manext <> nil do mend := mend^.manext;	(* find end *)
	  mend^.manext := m
	  end
	end;
      gettoken
      end
    end;
  if cclause (m) then ;
  end
end (* function moddef *);
$PAGE modsect
function modsect: boolean;

(* <modsect> ::= MODULES <moddeflist>
   <moddeflist> ::= <moddef> [<moddeflist>]   *)

  var mend: modptr;

begin
if curtok <> modtok then modsect := true
else begin
  modsect := true;
  gettoken;
  mend := newmod;
  mend := newmdl^.mlist;		(* hook onto MAIN entry *)
  while moddef (mend^.next) do mend := mend^.next;
  end
end (* function modsect *);
$PAGE areadef
function areadef (var a: areaptr): boolean;

(* <areadef> ::= <id> [SIZE] <num>    *)

begin
if curtok <> idtok then areadef := false
else begin
  areadef := true;
  if find (areaname, newmdl) then begin
    mdlerr ('area & multiply defined');
    a := lastarea
    end
  else begin
    a := newarea;
    a^.name := newname (areaname, a)
    end;
  gettoken;
  if curtok = sizetok then gettoken;
  if curtok <> numtok then mdlerr ('numeric expected')
  else begin
    a^.asize := intval;
    gettoken
    end
  end
end (* function areadef *);
$PAGE areasect
function areasect: boolean;

(* <areasect> ::= AREAS <areadeflist>
   <areadeflist> ::= <areadef> [<areadeflist>]   *)

  var aend: areaptr;

begin
if curtok <> areatok then areasect := false
else begin
  areasect := true;
  gettoken;
  aend := newarea;
  newmdl^.alist := aend;		(* dummy for chaining *)
  while areadef (aend^.next) do aend := aend^.next;
  aend := newmdl^.alist;		(* now remove dummy *)
  newmdl^.alist := aend^.next;
  dispose (aend)
  end
end (* function areasect *);
$PAGE syssect and machsect
function syssect: boolean;

(* <syssect> ::= SYSTEM <id> [DEBUG] [KICODE] *)

begin
if curtok <> systok then syssect := false
else begin
  syssect := true;
  gettoken;
  if curtok <> idtok then mdlerr ('identifier expected')
  else begin
    newmdl^.pname := currid;
    gettoken
    end;
  if curtok = debtok then begin
    newmdl^.debug := true;
    gettoken
    end;
  if curtok = kitok then begin
    newmdl^.kicode := true;
    gettoken
    end
  end
end (* function syssect *);


function machsect: boolean;

(*  <machsect> ::= MACHINE <id>   *)

begin
if curtok <> machtok then machsect := false
else begin
  machsect := true;
  gettoken;
  if curtok <> idtok then mdlerr ('identifier expected')
  else gettoken
  end
end (* function machsect *);
$PAGE usepro
public procedure usepro;

begin
mdlerror := false;
mdlinit;
gettoken;
if machsect then;			(* who cares *)
if syssect then;			(* same *)
if areasect then;
if not modsect then mdlerr ('modules section expected');
if not symsect then mdlerr ('symbols section expected');
if not storsect then mdlerr ('storage section expected');
if curtok <> endtok then mdlerr ('end expected')
else begin
  gettoken;
  if curtok <> eoftok then mdlerr ('end of input expected')
  end
end (* procedure usepro *).
    