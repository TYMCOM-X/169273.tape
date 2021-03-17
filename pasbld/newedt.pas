$title edt.pas, last modified 2/16/84, zw
program edt options storage(3072);
(*tym-pascal text editor*)
$system versio.inc
$system filutl.inc
$system infpac.inc
$system cmdutl.typ
$system cmdutl.inc
$system wio.typ
$system wio.inc
$system query.inc
(*error codes*)
type
qerrcode = (
qok,(*normal return*)
qfatal,(*internal error -- fatal situtation*)
qbadln,(*non-existant line number passed*)
qbadlb,(*non-existant lower bound passed*)
qbadub,(*non-existant upper bound passed*)
qbadrn,(*lower bound > upper bound*)
qbadargno,(*wrong number of la's in ld*)
qbadcmd,(*bad command name parsed*)
qnocmd,(*cmd invalid in context used*)
qbadfile,(*bad filename parsed*)
qnofile,(*non-existant file*)
qnoclose,(*no closing delimiter in string pattern*)
qnodefault,(*default pattern has not beed defined*)
qno2ndla,(*no second line address after comma*)
qnoinparen,(*invalid field within parens of string predicate*)
qnoclosparen,(*no closing parenthesis*)
qnonot_op,(*invalid field after "not" operator*)
qnortop,(*invalid field after "and" or "or" operator*)
qoffirst,(*signed offset found as first part of la*)
qnonumf,(*no numeric field found after '+' or '-'*)
qonlyfirst,(*'*','$', or '.' found not as first part of la*)
qnoprevious,(*'*' only allowed in second half of ld*)
qspnotfnd,(*string predicate not found in range*)
qoutrange,(*evaluated ld out of buffer or special range*)
qstupid,(*unmatchable predicate detected in parse*)
qlnlong,(*line too long, from split, substitute*)
qquit,(*quit command given*)
qnomark,(*no line matching markstring was found*)
qbadsubst,(*syntax error in substitute patterns*)
qbadopt,(*bad option was parsed*)
qextratxt,(*extraneous text follows command*)
qtoobig,(*integer too big to be parsed as line number*)
qnofindpred,(*no spattern given in find command*)
qnomovela,(*1 la expected after move, 0 or 2 found*)
qbadmovela,(*la was within text being moved*)
qnotopen,(*output command was issued with no open file*)
qbadparam,(*missing or invalid set parameter*)
qnoparamval,(*no value given for set parameter*)
qjointoofew,(*fewer than two lines given to join*)
qnosplitpat,(*invalid or missing pattern for split*)
qempty,(*buffer is empty*)
qtooshort,(*not enough leading space to indent*)
qwrterr,(*eof(f) false after write statement*)
qla1notfnd,(*spred in first la not found*)
qla2notfnd,(*spred in second la not found*)
qla1outrange,(*first la out of range*)
qla2outrange,(*second la out of range*)
qheapfull,(*heap overflow*)
qbadnth,(*bad nth occurrance pattern*)
qnoindno,(*amount to indent not specified*)
qmovoutrange,(*destination of move out of range*)
qnoinfile,(*failure to open input file*)
qnooutfile,(*failure to open output file*)
qnocopyla,(*1 la expected after copy, 0 or 2 found*)
qnolabel);(*no label found for _ kludge*)
const qstringlen = 254;
type
qstring = string [qstringlen];
qstringidx = 0..255;
(*string pattern definitions represent a parsed edt string matching pattern.
details may be found in the 'definition of edt.'*)
type
spattern_forms =(*as determined from delimiter*)
( simple,(*match anywhere within line*)
left,(*matches at the beginning of the line*)
right,(*matches at the end of the line*)
leadstring,(*match chars following leading whitespace*)
token,(*match a delimited string*)
nullspat );(*represents invalid default pattern*)
side_type = (
leftside,(*star on the left side of pattern*)
rightside,(*etc.*)
bothsides,
neither );
spattern =
record(*internal reprsentation of a string pattern*)
stype: spattern_forms;(*matching boundary conditions*)
wildcard: boolean;(*true if string contains a '*'*)
sidestars: side_type;(*positions of side stars*)
slength: qstringidx;(*length of pattern minus wildcards*)
list: ^spatrec;(*pointer to head of list of patterns*)
lastrec: ^spatrec(*pointer to last token in pattern list*)
end;
spatlist = ^spatrec;
spatrec =
record
wildchar: boolean;(*true if string contains a '?'*)
sstring: qstring;(*string to be matched*)
next: spatlist;(*pointer to next pattern token*)
prevtok: spatlist(*pointer to previous pattern token*)
end;
(*string predicate definitions reprenting a parsed edt string predicated.
for details see the 'definition of edt'.*)
type
spred_kinds =
( pattern_spop,(*match a line containing pattern*)
not_spop,(*match a line not matching predicate operand*)
and_spop,(*match a line matching both predicate operands*)
or_spop );(*match a line matching either predicate operands*)
spred = ^ spred_node;(*represented as a tree of nodes*)
spred_node =
record
case predkind: spred_kinds of
not_spop:(*not <noper>*)
( noper: spred );
and_spop, or_spop:(*<loper> op <roper>*)
( loper, roper: spred );
pattern_spop:
( pattern: spattern )
end;
toktyp = (and_tok, or_tok, not_tok);
(*edt type definitions*)
const qmaxlines = 99999;(*maximum line number*)
type
qlinep = ^qline;(*ptr to line descriptor*)
qlineno = 0..qmaxlines;(*edt line number*)
qlnoffset = -99999..qmaxlines;(*line offset*)
(*buffer descriptor - callers should only access the following fields:
curlineno, lbound, hbound, lastlineno, curfile, curfileok, mark, changes.
all other fields are implementation specific.*)
qbuffer = record
firstlinep,(*ptr to dummy 0th line desc*)
lastlinep,(*to last line desc*)
getlinep: qlinep;(*ptr to last line gotten*)
getlineno: qlineno;(*line number of last line gotten*)
curlineno,(*current line number*)
lastlineno: qlineno;(*last line number*)
garblinep: qlinep;(*lines to be deleted*)
lbound,
hbound: qlineno;(*bounded range*)
lboundp,
hboundp: qlinep;(*pointers to bounds*)
offset: qlineno;(*anc line bounding offset*)
oldoffset: qlineno;(*offset previous to above*)
mark: spred;(*boundary mark*)
curfile: file_id;(*name of defaul file*)
curfileok: boolean;(*valid filename in above ?*)
changes: boolean;(*unwritten changes in buffer*)
s940: boolean(*true if current file is 940 file*)
end;
type
qline = packed record(*edt line descriptor*)
prevlinep,(*previous line pointer*)
nextlinep: qlinep;(*next line pointer*)
source: qstring(*text of line*)
end;(*note: users of edt routines
should never mess with qlines*)
(*line designator defintions. an ld is represented as a linked list of nodes
giving the atomic parts of a line designator such as predicates, symbolic
addresses, and punctuation. details may be found in the 'definition of edt.'*)
type
ld_forms =
( num_ld,(*line offset from last addressed line*)
forward_ld,(*address specified by predicate*)
backward_ld,(*address specified by ^predicate*)
dot_ld,(*current line*)
dollar_ld,(*last line*)
star_ld,(*first line of range*)
comma_ld );(*range separator*)
ldchain = ^ ldpart;(*node on ldchain list*)
ldpart =
record
next: ldchain;(*pointer to next node on list, or nil*)
case ldkind : ld_forms of(*various atomic parts*)
num_ld: ( offset: qlnoffset );(*offset or absolute lineno*)
forward_ld,backward_ld: ( pred: spred );(*predicates to search for*)
dot_ld,dollar_ld,star_ld,comma_ld: ()
end;
ldcount = 0..2;(*number of addresses in an ld*)
ldrange =(*evaluated ld chain, gives limits of range*)
record
lbound: qlineno;(*first line addressed*)
hbound: qlineno(*second line, same as first if ld not range*)
end;
type
edtcmds =
(append, change, delete, insert, edit, modify, load, print, substitute,
after, before, writecmd, save, find, gotocmd, resetcmd, join, copy,
move, transfer, bound, list, eqcmd, number, opencmd, outputcmd, closecmd,
setcmd, split, quit, exitcmd, uparrow, why, indent, underbar, readcmd);
edt_cmd_set = set of edtcmds;
set_params =
(del_param, lcnt_param, mark_param, tab_param, wild_param, case_param);
set_param_set = set of set_params;
type
split_options = (number_splitop, confirm_splitop, print_splitop,
all_splitop, delete_splitop);
split_opt_set = set of split_options;
type
sub_options =
( confirm_sop, all_sop, print_sop, number_sop );
sub_opt_set = set of sub_options;
const
token_chars : set of char = ['a' .. 'z'];
type
rangetypes = (one, dollar, dot, dotp1, lb, lbp1);
rangelist = record
lbound, hbound1, hbound2: rangetypes;
required, permitted: 0..2
end;
cmd_range = 1 .. ord (maximum (edtcmds)) + 1;
sub_opt_range = 1 .. ord (maximum (sub_options)) + 1;
set_param_range = 1 .. ord (maximum (set_params)) + 1;
split_opt_range = 1 .. ord (maximum (split_options)) + 1;
caller_range = 1 .. ord (maximum (toktyp)) + 1;
qcmdlist = array [cmd_range] of cmd_lookup_record;
sub_opt_list = array [sub_opt_range] of cmd_lookup_record;
set_par_list = array [set_param_range] of cmd_lookup_record;
split_op_list = array [split_opt_range] of cmd_lookup_record;
caller_list = array [caller_range] of cmd_lookup_record;
defrangelist = array [edtcmds] of rangelist;
type typecode=(fnerror,fnok,fndevice);(*for 940 syntax routine*)
external procedure fn940to10( file_id;(*940 style name*)
var file_id;(*pdp 10 style name*)
var typecode);(*fnerror,fnok,fndevice*)
public procedure wfileconvert(file_name: file_id;(*name to convert*)
file_modifier: wmodifier;(*type of conversion l s d b 6 7*)
var pdp_name: file_id;(*resultant name*)
var wcode: wcodes;(*error codes*)
var drconvert: boolean);(*further convert on name req.*)
type integer= 0..99999;
wl_or_d= (lnumber,dnumber);(*link number or documentation*)
var code: typecode;
werror: boolean;
wnumber: integer;
tmodifier: wmodifier;
function wuser(wnumber: integer;(*link number to determine user*)
wnumber_type: wl_or_d(*lnumber or dnumber given*)
): wmodifier;
var zero: 0..9999;
digit: 0..7;
acct,n: 0..99999;
begin
(*the following computes the acct number for link names and
link documentation names based on files per account and
the number of accounts.
links 1-9 user 42164
links 10-19 user 42165
| |
links 90-99 user 42175 in other words add to the base acct the tens digit
links 100-109 user 42217
links 200-209 user 42252 add the hundreds digit*27 to the base acct
the above pattern repeats for links above 999
documentation files are arranged 30 per file for 26 user names
and then repeats through the user names for the next 780 files*)
if wnumber_type=lnumber then acct:=17524+((wnumber div 100)mod 10)*27
+((wnumber div 10) mod 10)
else acct:=( ( ( wnumber - 1 ) mod 780 ) div 30 ) + 9046;
wuser:='';(*start string as null string*)
zero:=ord( '0' );(*need internal rep of zero*)
while acct<>0 do
begin
n:= acct div 8;(*determine octal user number*)
digit:= acct - n*8;(*this gives digits from right to left*)
wuser:= chr(zero + digit) || wuser;
acct:= n
end;
end;(*wuser*)
procedure wvalidate_number(file_name: file_id;(*contains only the number*)
var wnumber: integer;(*number to be returned*)
var wnew_name: file_id;(*number with .940 added*)
var wnum_error: boolean);(*error if not num or bad*)
var i,n: integer;
j: -9999..9999;
begin
wnum_error:= false;
n:=length(file_name);
if n>4 then wnum_error:=true
else begin
wnumber:= 0;
for i:= 1 to n do
begin
wnew_name:=wnew_name||file_name[i];
j:= ord(file_name[i])-ord('0');
if(j<0) or (j>9) then wnum_error:=true
else wnumber:=wnumber*10+j
end;
wnew_name:=wnew_name||'.940';
end
end;(*wvalidate*)
begin(*this is the start of wfileconvert*)
drconvert:= false;
wcode:= wok;
pdp_name:='';
if file_modifier='' then
begin
if (length(file_name)>4) andif
(substr(file_name,length(file_name)-3,4)='.940') andif
(not query ('7 bit file, ok')) then
begin
wcode:=wbadname;
return;
end;
wcode:=wtenfile;
pdp_name:=file_name
end
else begin
if file_modifier[1]='6' then
begin
fn940to10(file_name,pdp_name,code);
if code=fnok then wcode:=wok
else wcode:=wbadname
end
else if file_modifier[1]='7' then 
begin
wcode:=wtenfile;
pdp_name:=file_name
end
else begin
wvalidate_number(file_name,wnumber,pdp_name,werror);
if werror then wcode:=wbadname
else
begin
tmodifier:=uppercase (file_modifier);
case tmodifier[1] of
'l': pdp_name:= pdp_name||'[50122,'||
wuser(wnumber,lnumber)||']<007>';
's': pdp_name:=pdp_name||'[50122,34321]<007>';
'b': pdp_name:=pdp_name||'[50122,34124]<007>';
'd': begin
pdp_name:='';
case length(tmodifier) of
1: pdp_name:=file_name;
2: begin
if ((tmodifier[2]<>'f') and (tmodifier[2]<>'g') and
(tmodifier[2]<>'r')) then wcode:=wbadname
else pdp_name:=file_name||tmodifier[2]
end;
3: begin
if ((tmodifier[2]='f') or (tmodifier[2]='g'))
and (tmodifier[3]='r') then 
pdp_name:=file_name||substr(tmodifier,2)
else wcode:=wbadname
end;
others: wcode:=wbadname
end;
if wcode<>wbadname then
begin
pdp_name:=pdp_name||'.940[50127,'||
wuser(wnumber,dnumber)||']<007>';
if search(tmodifier,['r'])=0 then drconvert:= true;
end;
end;
others: wcode:=wbadname
end
end;
end;
end
end;(*wfileconvert*)
(*qread is the actual read routine used by edt 1.6 and editread .*)
external function qread
: qstring;(*result*)
(*editread (1.6) reads a line of input. the oldline input is just a stub
that in version 1.5 was an argument string for intra-line editing.*)
external function editread
( oldline: qstring 
): qstring;(*result*)
(*
external function lineedit ( oldline: qstring ): qstring;
*)
public function editread (oldline: qstring): qstring;
begin
editread := qread;
end;
(*the following is edt 1.5 editread code . 
label 100;
var newline: packed array[1..qstringlen] of char;(*packed for speed*)
nll: qstringidx;
begin
nll := 0;(*have no chars yet*)
if eoln (tty) then readln (tty);(*if at end of line, get new line*)
if eof (tty) then begin(*erroneous condition*)
open (tty, '');(*must reopen it*)
readln (tty)
end;
while not eoln (tty) do begin(*read each character*)
if nll < length (newline) then begin(*room left in buffer*)
nll := nll + 1;
newline [nll] := tty^;
get (tty)
end
else goto 100(*have overflowed buffer, return what we have*)
end;
if eoln(tty) andif (tty^=chr(7)(*control-g*)) then begin
if nll>0 then begin
writeln(tty); break;
editread:= substr(newline,1,nll);(*to avoid rumored bug passing substr*)
editread:= lineedit(editread)
end
else editread:= lineedit(oldline);
return
end;
100:
editread := substr (newline, 1, nll);(*return varying string*)
end;
*)
(*qsetcase sets the case flag in qspat.*)
external procedure qsetcase (x:boolean);
(*spatparse extracts a string pattern from an input line. the parse begins
at a caller supplied index. preceding blanks are ignored. if the first 
nonblank character is not a valid string pattern delimiter, then false is
returned. the index is not advanced. if a valid delimiter is found, then
the line is scanned to complete the pattern. an error occurs if there is
no closing delimiter or if the default pattern is used and there it has no
current value. if the pattern is valid, the pattern record is filled in;
the parsing index is advanced to the character past the closing delimiter;
and true is returned. if there is an error, true is returned with an error
code set. the index is set to the openning delimiter; the pattern record is
not modified.*)
external function spatparse
( line: cmdline;(*line to be parsed*)
var idx: cmdlineidx;(*parsing cursor, advanced if true returned*)
var pat: spattern;(*pattern description, set if complete pattern found*)
wildswitch: boolean;(*wildcarding set switch*)
var err: qerrcode(*error code, set if something found*)
): boolean;
(*spatmatch attempts to find a substring matching a pattern somewhere within
a caller supplied line. if no match is found, then false is returned. if
a match is found, then the starting index and length of the substring are
set, and true is returned.*)
external function spatmatch
( line: qstring;(*line to scan for a matching substring*)
pat: spattern;(*pattern to be matched*)
var pos, len: qstringidx;(*starting position and length of substring*)
var err: qerrcode(*error report*)
): boolean;
(*spatdispose disposes of the doubly-linked list in the given spattern*)
external procedure spatdispose (pattern: spattern);
(*spatsetdefault sets the default string pattern - i.e. // or equivalent.
initially it has no value; an attempt to reference it causes an error to
be reported by spatmatch.*)
external procedure spatsetdefault (pattern: spattern);
(*spatgetdefault returns the value of the current
default spattern. useful for preventing sub-commands of find from
changing it. will not report an error if the pattern
is null.*)
external function spatgetdefault: spattern;
(*qtokenget searches for the next non-blank character in line and sets idx
to that position. the function returns false if no token is found*)
external function qtokenget(line : cmdline; var idx : cmdlineidx) : boolean;
var
opener, closer : char;
tmpidx : cmdlineidx;
static var defpat: spattern := (nullspat, false, neither, 0, nil, nil);
nodefault: boolean := false;
caseflg: boolean;
type charset = set of char;
const legalopen: charset := 
['#', ':', '@', '!', '/', '&', '''', '"', '\', '[', '%',
'<', '`' ];
public procedure qsetcase (x:boolean);
(*this procedure sets the caseflg flag.*)
begin
caseflg := x
end;
public procedure spatdispose (var pat: spattern);
(*this function disposes of a spattern record. will not dispose
of the default pattern*)
var patp: spatlist;
begin
if (pat.list <> defpat.list) then(*don't throw away the default pattern!*)
with pat do
while list <> nil do begin
patp := list;
list := list^.next;
dispose (patp)
end
end;(*spatdispose*)
function copypat (original: spattern): spattern;
(*function to copy a spattern record*)
var orig, copy, pat: spatlist;(*temporary pointer vars for copy*)
begin
copypat := original;(*first, copy main record*)
orig := original.list;(*top of list of original pattern*)
if orig <> nil then begin
new (copy);(*create a copy record*)
copypat.list := copy;(*chain it into the return record*)
copy^ := orig^;(*copy the contents of the default*)
orig := orig^.next;(*advance the original ptr*)
(*now that the copy has been chained onto the return record, copy...*)
while orig <> nil do begin
new (pat);
pat^ := orig^;
copy^.next := pat;(*chain it on*)
pat^.prevtok := copy;
copy := copy^.next;(*advance the pointers*)
orig := orig^.next
end;
copypat.lastrec := copy
end
end(*copypat*);
public procedure spatsetdefault ( pattern : spattern);
(*this function set default pattern*)
var tempat : spattern;
begin
if not nodefault then begin
tempat := defpat;
defpat := copypat (pattern);
spatdispose (tempat)
end
end;(*spatsetdefault*)
public function spatgetdefault: spattern;
(*this function returns a copy of the default pattern.*)
begin
spatgetdefault := copypat (defpat)
end;(*spatgetdefault*)
public function qtokenget(line : cmdline; var idx : cmdlineidx) : boolean;
(*this function returns true and places idx at the next token in line
if another token exists. else false*)
begin
qtokenget := false;
while (not qtokenget) and (idx <= length(line) ) do
if (line[idx] = ' ') or (line[idx] = tab) then
idx := idx + 1
else
qtokenget := true
end;
(*please see qspat.inc for specifications of spatparse and spatsetdefault*)
public function spatparse ( line : cmdline; var idx : cmdlineidx;
var pat : spattern; wildswitch: boolean; var err : qerrcode ) : boolean;
function delete_star(var pattern: qstring): side_type;
(*this function deletes stars on the far right or left of a pattern
and returns a side_type indicating where they were deleted from*)
begin
delete_star := neither;
while pattern[1] = '*' do begin
delete_star := leftside;
pattern := substr(pattern, 2)
end;
if (pattern[length(pattern)] = '*') then
if delete_star = neither then delete_star := rightside
else delete_star := bothsides;
while pattern[length(pattern)] = '*' do
pattern := substr(pattern, 1, length(pattern)-1)
end;(*delete_star*)
var
ch: char;
pattern: qstring;
sidestar: side_type;
wildcard: boolean;
temprec, trec: spatlist;
ind: qstringidx;
begin(*parsing the string pattern*)
tmpidx := idx;
spatparse := false;
err := qok;
if qtokenget(line,tmpidx) then
begin
ch := line[tmpidx];
if (ch in legalopen) orif (ch = '{') orif 
(ch = '|') orif (ch = chr(#o176)) then begin
(*we have an opening delimiter*)
idx := tmpidx;
spatparse := true;
tmpidx := tmpidx + 1;
opener := line[idx];
case opener of
'[': closer := ']';
'{': closer := chr(#o175);
'<': closer := '>';
others: closer := opener
end;
if tmpidx > length(line) then
tmpidx := 0
else
tmpidx := index( substr(line,tmpidx), closer);
if tmpidx = 0 then begin
err := qnoclose;(*leave idx at opener*)
spatparse := false
end
else begin(*we got a closing delimiter*)
pattern := substr (line, idx+1, tmpidx-1);(*chop off delimiters*)
(*flag patterns w/ no "real" characters*)
if (verify(pattern, ['?','*']) = 0) andif
wildswitch andif
(length(pattern) <> 0) andif
(verify(pattern, ['?']) <> 0) then err := qstupid(*wow!*)
else begin(*to build a pattern record*)
(*a spattern pattern is stored internally as a doubly-linked
list of pattern strings separated by implicit stars. this
parsing is done here to avoid a slow wildcard matcher. 
for example, the pattern 'foo*bah*fum' is stored 
internally as a doubly linked list like:
list^->'foo'<->'bah'<->'fum'<-lastrec^*)
wildcard := (index (pattern, '*') <> 0) and wildswitch;
new (temprec);
pat.list := temprec;
pat.wildcard := wildcard;
temprec^.next := nil;
temprec^.prevtok := nil;
if not wildcard then begin(*a one-element list*)
pat.slength := length(substr(line, idx+1, tmpidx-1));
temprec^.sstring := substr(line, idx+1, tmpidx-1);
temprec^.wildchar := (index(temprec^.sstring, '?') <> 0) and wildswitch
end
else begin(*add to the linked list*)
pat.sidestars := delete_star (pattern);
pat.slength := length (pattern);
ind := index(pattern, '*');
if ind <> 0 then begin
pat.slength := pat.slength - 1;
temprec^.sstring := substr(pattern, 1, ind-1)
end
else temprec^.sstring := pattern;
temprec^.wildchar := (index(temprec^.sstring, '?') <> 0) and wildswitch;
if ind <> 0 then
repeat(*build a doubly-linked list of patterns*)
(*separated by implicit stars*)
while pattern[ind+1] = '*' do begin
ind := ind + 1;
pat.slength := pat.slength - 1
end;
pattern := substr(pattern, ind + 1);
ind := index(pattern, '*');
if ind <> 0 then pat.slength := pat.slength - 1;
new(trec);
temprec^.next := trec;
trec^.next := nil;
trec^.prevtok := temprec;
if ind = 0 then trec^.sstring := pattern
else trec^.sstring := substr(pattern, 1, ind-1);
trec^.wildchar := (index(trec^.sstring, '?') <> 0) and wildswitch;
temprec := temprec^.next
until ind = 0;
pat.lastrec := temprec
end(*if-then-else*)
end;(*if verify then-else*)
case opener of
':' :
pat.stype := left;
'#' :
pat.stype := leadstring;
'@' :
pat.stype := right;
'!' :
pat.stype := token;
others :
pat.stype := simple
end(*case*)
end;
idx := idx + tmpidx + 1
end
end
end;
public function spatmatch (line: qstring; spat: spattern;
var pos, lenpatss: qstringidx; var err: qerrcode): boolean;
(*helpers*)
function equal(line: qstring; idx, len:qstringidx; pattern : spatrec)
: boolean;
(*
this function returns true if the substring in line starting at 
position idx and of length len equals the pattern. otherwise
false*)
var
i : qstringidx;
begin
if len > length(line)-idx+1 then
equal := false
else begin
equal := true;
i := 0;
if not pattern.wildchar then
if caseflg then equal := substr(line,idx,len) = pattern.sstring
else equal := uppercase(substr(line,idx,len)) = uppercase(pattern.sstring)
else
while (i < len) and (equal) do
if (pattern.sstring[i+1] = '?') orif
( (caseflg and (pattern.sstring[i+1] = line[idx+i])) or
(not caseflg and (uppercase(pattern.sstring[i+1]) =
uppercase(line[idx+i]))) ) then
i := i + 1
else
equal := false
end
end;
function column( line: qstring; pat: spatrec; backward: boolean): qstringidx;
(*
this function returns the column number of the substring in line
that matches pat. if backward is true, then the line is scanned
from the end of the line to the beginning. otherwise, from the
beginning of the line to the end; 
returns 0 on no match found.*)
var
column_limit: qstringidx;
inc: -1..1;
function within_limit: boolean;
begin
if backward then within_limit := column >= column_limit
else within_limit := column <= column_limit
end;(*within_limit*)
begin
if (pat.wildchar or backward) then begin
if backward then begin
column := length(line) - length(pat.sstring) + 1;
inc := -1;
column_limit := 1
end
else begin
column := 1;
inc := 1;
column_limit := length(line) - length(pat.sstring) + 1;
end;
while within_limit andif
not (equal (line,column,length(pat.sstring),pat)) do
column := column + inc;
if (column = column_limit+inc) then
column := 0
end
else
if caseflg then column:= index(line,pat.sstring)
else column:= index(uppercase(line),uppercase(pat.sstring))
end;
function wildmatch ((*to find the first occurrance of a wildcard pattern*)
var pos, len: qstringidx;(*return values, pos also input*)
line: qstring;(*line to be searched*)
backward: boolean(*flag for direction of scan*)
): boolean;(*true for a good find*)
(*this function will attempt to match the first or last occurrance of
a spattern wildcard pattern after/before pos in the line. 
pos must be input as the starting position of the search.*)
var
pattern: spatlist;
templine: qstring;
patlen, temppos: qstringidx;
begin(*body of wildmatch*)
wildmatch := false;
len := 0;
if backward then begin(*backward scan*)
templine := substr (line, 1, pos);
pos := column (templine, spat.lastrec^, backward);
if pos <> 0 then temppos := pos - 1
else temppos := 0;
pattern := spat.lastrec^.prevtok;
templine := substr (templine, 1, temppos);
if temppos <> 0 then
(*match pattern parts starting from the end of the list
to the beginning, chopping off the end of the line
on each match*)
while pattern <> nil do begin
pos := column (templine, pattern^, backward);
exit if pos = 0;
templine := substr (templine, 1, pos-1);
pattern := pattern^.prevtok
end;
wildmatch := pos <> 0;
len := temppos - pos + 1 + length(spat.lastrec^.sstring)
end
else begin(*forward scan*)
templine := substr (line, pos);
temppos := column (templine, spat.list^, backward);
pos := pos + temppos - 1;
pattern := spat.list^.next;
patlen := length(spat.list^.sstring);
if temppos <> 0 then
(*match pattern parts starting from the beginning of the list
to the end, chopping off the beginning of the line 
on each match*)
while pattern <> nil do begin
templine := substr(templine, temppos + 1);
temppos := column (templine, pattern^, backward);
exit if (temppos = 0) orif (templine = '');
len := len + temppos;
if temppos >= patlen then begin(*be sure to skip over last pattern*)
patlen := length (pattern^.sstring);
pattern := pattern^.next
end
else patlen := patlen - temppos
end;
wildmatch := temppos <> 0;
len := len + length(spat.lastrec^.sstring)
end;
wildmatch := (pattern = nil) and wildmatch
end;(*function wildmatch*)
var
cond: boolean;
tpos, len, patidx: qstringidx;
alpha: set of char;
pat: spattern;
templine: qstring;
temppos: qstringidx;
templen: qstringidx;
function delimiter (idx: qstringidx): boolean;
(*this function returns true if line[idx] and line[idx-1]
are delimiters.*)
begin
delimiter := not((uppercase(line[idx]) in alpha) and
(uppercase(line[idx-1]) in alpha))
end;
begin
spatmatch := false;(*result in case of errors*)
err := qok;
pat := spat;
alpha := ['a'..'z', '0'..'9', '_', '%', '$'];
len := pat.slength;
if length (line) >= len then
with pat do
case stype of
simple: 
if not wildcard then begin
if len = 0 then(*use the default pattern*)
if defpat.stype = nullspat then begin
err := qnodefault;
return
end
else begin
spatmatch := spatmatch(line, defpat, pos, lenpatss, err);
return
end;
pos := column(line,list^, false);
spatmatch := (pos <> 0)
end
else begin(*to find the shortest match*)
pos := 0;
tpos := 0;
len := maximum (qstringidx);
templine := line;
temppos := 1;
while wildmatch (temppos, templen, templine, false ) do begin
tpos := tpos + temppos;
if templen < len then begin(*save the shortest match*)
pos := tpos;
len := templen
end;
templine := substr (templine, temppos + 1);
exit if length(templine) < slength;
temppos := 1
end;(*while*)
spatmatch := pos <> 0
end;
left:
if not wildcard then begin
if len = 0 then begin
spatmatch := true;
pos := 1
end
else if equal(line,1,len,list^) then begin
spatmatch := true;
pos := 1
end
end
else begin(*left wildcard match*)
pos := 1;
if (sidestars in [leftside, bothsides] ) then begin
spatmatch := wildmatch(pos, len, line, false);
len := pos + len - 1;
pos := 1
end
else spatmatch := wildmatch(pos, len, line, false) andif
(pos = 1);
end;
right:
if not wildcard then begin
if len = 0 then begin
spatmatch := true;
pos := length (line) + 1
end
else if equal(line,length(line)-len+1,len,list^) then begin
pos := length (line) - len + 1;
spatmatch := true
end
end
else begin(*right wildcard match*)
pos := length(line);
if (sidestars in [rightside, bothsides] ) then begin
spatmatch := wildmatch(pos, len, line, true);
len := length(line) - pos + 1
end
else spatmatch := wildmatch(pos, len, line, true) andif
((pos + len - 1) = length(line))
end;
leadstring: begin
pos := 1;
while (pos <= length(line)) andif
((line[pos] = ' ') orif (line[pos] = tab)) do
pos := pos + 1;(*skip over leading whitespace*)
if pos > length(line) then pos := 0;
if len = 0 then
spatmatch := verify(line, [' ', tab]) <> 0
else begin
patidx := 1;
while (patidx <= len ) andif (pos > 0) andif (list^.sstring[patidx] = ' ')
do begin(*to make sure that the pattern fits*)
patidx := patidx + 1;
pos := pos - 1
end;
if (pos <> 0) then begin
if not wildcard then
spatmatch := equal(line,pos,len,list^)
else begin(*leadstring wildcard*)
temppos := pos;
spatmatch := wildmatch (temppos, len, line, false);
if (sidestars in [leftside, bothsides] ) then
len := temppos - pos + len
else spatmatch := temppos = pos
end;
(*if the above fails, then check for '?' wildcards*)
if (list^.wildchar) and (not spatmatch) then begin
loop
cond := (not spatmatch) andif ( (patidx <= len) andif
(pos > 1) andif (list^.sstring[patidx] in [' ','?']) );
exit if not cond;
patidx := patidx + 1;
pos := pos - 1;
if not wildcard then
spatmatch := equal(line,pos,len,list^)
else begin
temppos := pos;
spatmatch := wildmatch (temppos, len, line, false);
if (sidestars in [leftside, bothsides] ) then
len := temppos - pos + len
else spatmatch := temppos = pos
end(*if not wildcard*)
end(*loop*)
end(*if (list^.wildchar)*)
end(*if (pos <> 0)*)
end(*if len = 0*)
end;
token:
if len = 0 then begin
spatmatch := true;
pos := 1
end
else begin
pos := 1;
if (not wildcard) or (sidestars = neither) then
repeat(*regular token searching--no critical wildcards*)
if not wildcard then
patidx:= column(substr(line,pos),list^, false)
else begin(*regular token wildcarding*)
temppos := pos;
if wildmatch (temppos, len, substr(line,pos), false) then
patidx := temppos
else patidx := 0
end;
spatmatch:= patidx<>0;
if spatmatch then begin(*check if it is a token*)
pos:= patidx+pos-1;
if pos = 1 then begin
if len <> length(line) then
if not delimiter(pos+len) then begin
pos := pos + 1;
spatmatch := false
end
end
else begin(*check closer, if one*)
if pos < length(line) - len + 1 then
if not delimiter(pos+len) then begin
pos := pos + 1;
spatmatch := false
end;
if spatmatch then
if not delimiter(pos) then begin
pos := pos + 1;
spatmatch := false
end
end(*if pos = 1*)
end(*if spatmatch*)
else
pos:= length(line)+1(*to force repeat loop termination*)
until (pos > length(line)-len+1) or (spatmatch)
else begin(*special token search--wildcards on outside edges of pattern*)
pos := 0;
tpos := 0;
len := maximum (qstringidx);
templine := line;
templen := 0;
temppos := 1;
while wildmatch(temppos, templen, templine, false) do begin
tpos := tpos + temppos;
temppos := tpos;
if (sidestars in [leftside, bothsides] ) then
while (not delimiter(temppos)) and (temppos > 1) do begin
(*expand match to delimiters*)
temppos := temppos - 1;
templen := templen + 1
end;
if (sidestars in [rightside, bothsides] ) then
while (not delimiter(temppos+templen)) and
((temppos+templen) <= length(line)) do(*expand*)
templen := templen + 1;
if templen < len then begin
len := templen;
pos := temppos
end;
templine := substr(line, tpos + 1);
temppos := 1
end;(*while wildmatch*)
spatmatch := pos <> 0
end(*if not wildcard or sidestars*)
end(*if len = 0*)
end;(*case*)
if spatmatch then
spatsetdefault(pat);
lenpatss:= len(*return length of pattern*)
end;
(*spredparse extracts a string predicate from an input line. the parse begins
at a caller supplied index. if the first token at or following the index is
not a valid initial token for a string predicate, false is returned. no
additional parameters, including the line index, are modified. if the
token is acceptable, an attempt is made to parse a complete string predicate
an error occurs if there is a syntactic error, an error in a contained
string pattern, or an and operator whose operands are both ::, @@, or ##
form patterns. if there is no error, then a pointer to the base of the
predicate tree is filled in; the index is set to the character position
following the end of the predicate; and true is returned. if there is an
error, true is returned with a nonnull error code. the index is set to
the start of the token at which the error occurred. the predicate tree
base is not set.*)
external function spredparse
( line: qstring;(*string to be parsed*)
var idx: qstringidx;(*parsing cursor, advanced if something found*)
var pred: spred;(*predicate tree, set if complete pattern found*)
wildswitch: boolean;(*on/off switch for wildcarding*)
var err: qerrcode(*error code, set if something found*)
): boolean;
(*spredmatch attempts to match a line against a string predicate. the rules
for interpreting a predicate is given in the 'definition of edt'. a flag is
returned indicating whether or not there was a match.*)
external function spredmatch
( line: qstring;(*line to match against*)
pred: spred;(*predicate to match*)
var err: qerrcode(*error report*)
): boolean;(*true if match found*)
(*spreddispose deletes the nodes of a string predicate tree. it is passed
a possibly nil pointer to the base of a tree.*)
external procedure spreddispose (pred: spred);
external const lexcon : caller_list;
var
whichfnd : toktyp;
public procedure spreddispose(pred : spred);
begin
if pred <> nil then with pred^ do begin
case predkind of
not_spop:
spreddispose(noper);
and_spop, or_spop: begin
spreddispose(loper);
spreddispose(roper)
end;
pattern_spop:
spatdispose (pattern)
end;(*case*)
dispose(pred)
end;
end;
public function spredparse( line : cmdline; var idx : cmdlineidx;
var pred : spred; wildswitch : boolean; var err : qerrcode) : boolean;
label 1;
(*outsiders see only this wrapper--real work is done down below*)
procedure error (derr : qerrcode; zap : spred);
begin
err := derr;
spreddispose(zap);
pred := nil;
goto 1(*kickout of recursion*);
end;
(*getspred parses <spred> ::= [ <spred> ( and | or ) ] <subpred>*)
function getspred (line : cmdline; var idx : cmdlineidx; var pred : spred;
var err : qerrcode) : boolean;
forward;
(*get sub pred parses
<subpred> ::= not <subpred> | <spat> | "(" <spred> ")"*)
function getsubpred(line : cmdline; var idx : cmdlineidx; var pred : spred;
var err : qerrcode) : boolean;
var
temptr : spred;
pat : spattern;
match : integer;
begin
pred := nil;
getsubpred := false;
err := qok;
if qtokenget(line, idx) then begin
if cmd_lookup(line, idx, token_chars, lexcon, match) then begin
whichfnd := toktyp (match);
if whichfnd = not_tok then begin
if (not getsubpred(line, idx, temptr, err)) then
error(qnonot_op, nil);
new(pred, not_spop);
pred^.noper := temptr;
getsubpred := true
end
end
else begin(*something but not a 'not' keyword*)
if line[idx] = '(' then begin
idx := idx + 1;
if not getspred(line, idx, temptr, err) then
error(qnoinparen, temptr);
if (not qtokenget(line, idx)) orif (line[idx] <> ')' ) then
error(qnoclosparen, temptr);
pred := temptr;
idx := idx + 1;
getsubpred := true
end
else begin(*not even a paren'd spred*)
if spatparse(line, idx, pat, wildswitch, err) then
if (err = qok) then begin
new(pred, pattern_spop);
pred^.pattern := pat;
getsubpred := true
end
else
error(err, nil)
end
end
end
end;
function andoror(line : cmdline; var idx : cmdlineidx;
var which : spred_kinds) : boolean;
var save_idx: cmdlineidx;
match: integer;
begin
andoror := false;
save_idx := idx;
if cmd_lookup( line, idx, token_chars, lexcon, match) then begin
whichfnd := toktyp (match);
if (whichfnd = or_tok) then begin
andoror := true;
which := or_spop
end
else if (whichfnd = and_tok) then begin
andoror := true;
which := and_spop
end
else(*whichfnd = not_tok*) begin
idx := save_idx(*back up, not interested in not here*)
end
end;
end;
function stupid(pred:spred) : boolean;
(*to catch those ridiculous unmatchable predicates*)
begin
stupid := false;(*assume the best*)
if pred <> nil then
if pred^.predkind = and_spop then
with pred^ do
if (loper^.predkind = pattern_spop) andif
(roper^.predkind = pattern_spop) then
stupid := (not(loper^.pattern.stype in [simple, token])) andif
(loper^.pattern.stype = roper^.pattern.stype);
end;(*whew*)
function getspred;
var
ltemp, rtemp : spred;
which : spred_kinds;
begin
err := qok;
pred := nil;
getspred := getsubpred(line, idx, pred, err);
if getspred then
while andoror(line, idx, which) do
if not getsubpred (line, idx, rtemp, err) then
error(qnortop, pred)
else begin
if which = or_spop then
new(ltemp, or_spop)
else
new(ltemp, and_spop);
ltemp^.loper := pred;
ltemp^.roper := rtemp;
pred := ltemp;
if stupid(pred) then
error(qstupid, pred)
end;
end;
(*mainline for procedure spredparse*)
begin
spredparse := getspred(line, idx, pred, err);
1:
end;
public function spredmatch(line : qstring; pred : spred; var err : qerrcode)
: boolean;
var
pos, len: qstringidx;(*discarded after pattern match*)
begin
err := qok;
case pred^.predkind of
not_spop: begin
spredmatch := not spredmatch (line, pred^.noper, err);
if err <> qok then
spredmatch := false
end;
and_spop:
spredmatch := spredmatch (line, pred^.loper, err) andif
spredmatch (line, pred^.roper, err);
or_spop: begin
if not spredmatch (line, pred^.loper, err) then begin
if err <> qok then
spredmatch := false
else
spredmatch := spredmatch (line, pred^.roper, err)
end
else
spredmatch := true
end;
pattern_spop:
spredmatch := spatmatch (line, pred^.pattern, pos, len, err)
end;
end;
type edterrlevel = 1..10;
external procedure edterror
( var f: text;(*file to write error message to*)
code: qerrcode;(*error to report*)
level: edterrlevel );(*level of message desired*)
type edterrlevel = 1..10;
var errfile: text;
public procedure edterror (var f: text; code: qerrcode; level: edterrlevel);
var msgcode: 0..255;(*code read from file*)
msglevel: edterrlevel;(*level readfrom files*)
word: string[32];(*assembled word from message*)
column: 0..255;(*output column*)
errfile_name: file_name;(*complete error file name*)
jobstuff: jobrec;(*for error file ppn*)
(*writes text to tty, filling to width of terminal*)
procedure out;
begin
if (column + length (word)) > 72(*typical terminal width*) then begin
writeln (f);
column := 0;
if word = '' then return
end;
column := column + length (word);
write (f, word)
end;
begin
column := 0;
if (level = 1) and (code > qfatal) then begin(*brief message*)
writeln (f, '?'); break;
return
end;
jobinfo (jobstuff);
errfile_name := 'edterr.msg' || jobstuff.progdir;
open (errfile, errfile_name );(*get file*)
if eof (errfile) then begin(*fatal situation*)
writeln (f, 'edt error file missing.'); break;
return
end;
loop(*search for message in file*)
repeat readln (errfile) until not eoln (errfile) or eof(errfile);(*skip blanks before start of msg*)
if eof (errfile) then begin(*code + level not found*)
writeln (f, 'error not found.', ord (code):5); break;
return
end;
if errfile^ = '*'(*get errcode from file*)
then begin(*'*' matches any code*)
msgcode := ord (code);(*force match*)
get (errfile);(*eat comma following asterisk*)
get (errfile)(*and get first digit of following number*)
end
else read (errfile, msgcode);
read (errfile, msglevel);(*get level from file*)
exit if (msgcode = ord (code)) and (msglevel >= level);
repeat readln (errfile) until eoln (errfile);(*skip til blank line following msg*)
end;
repeat(*output msg*)
while errfile^ <= ' ' do get (errfile);(*skip control chars at start of msg*)
while not eoln (errfile) do begin(*extract words from line*)
if errfile^ > ' ' then begin
word := '';
repeat
word := word || errfile^;
get (errfile)
until errfile^ <= ' ';
out; word := ' '; out;(*write word followed by blank*)
end
else get (errfile);(*ignore control chars, i.e. white space*)
end;
readln (errfile);(*go to next line*)
until eoln (errfile);(*message terminated by blank line*)
writeln (f); break;(*terminate msg*)
close (errfile);(*clean up our dirty laundry*)
end;
(*qldparse extracts a line designator from an input line. it returns a count
of line addresses seen, and a representation of the ld. the parse begins
with the first token at or following a caller supplied index. if no ld at
all appears, a zero count and nil ld chain are returned. there is no error.
if the start of an ld appears, then the parse continues until the first
token, not a part of an ld, is seen or an error is detected. if a valid ld
is found, the count of address (one for a single address, two for a
range) and the ld chain built up are returned with a null error code. an
error occurs if there is a syntactic error in a contained string predicate,
star is used improperly, or a line address or offset is out of range. in
such a case, the count and ld chain parameters are not returned; and a
nonnull error code is returned. the index is set to the start of the first
token following the ld if there is no error; otherwise is set to point to
the start of the erroneous token. the caller should insure that the ld
chain has been initialized by qlddisposeing any records on it, and then
setting ld to nil.*)
external procedure qldparse
( line: cmdline;(*line to be parsed*)
var idx: cmdlineidx;(*parsing cursor*)
var nld: ldcount;(*number of line addresses seen*)
var ld: ldchain;(*representation*)
wildswitch: boolean;(*switch for wildcarding*)
var err: qerrcode );(*indicates success or nature of failure*)
(*qldeval resolves a line designation within the range of lines specified,
and returns the line numbers of the addressed lines. the rules governing
the evaluation of an ld are given in the 'definition of edt'. if an error
is detected, the error code is set nonnull, and the range left undefined.*)
external procedure qldeval
( var buf: qbuffer;(*buffer to search*)
ld: ldchain;(*parsed ld to evaluate*)
searchrange: ldrange;(*limits of search*)
var range: ldrange;(*contains returned line addresses*)
var err: qerrcode );(*indicates success or nature of failure*)
(*qlddispose frees the storage associated with an ld chain. it is passed a
pointer to the start of the chain to free. this pointer may be nil.*)
external procedure qlddispose (ld: ldchain );
public procedure qlddispose(ld : ldchain);
begin
if ld <> nil then
begin
qlddispose(ld^.next);
if (ld^.ldkind = forward_ld) orif (ld^.ldkind = backward_ld) then
spreddispose(ld^.pred);
dispose(ld)
end
end;
public procedure qldparse ( line : cmdline;
var idx : cmdlineidx;
var nld : ldcount;
var ld : ldchain;
wildswitch : boolean;
var err : qerrcode) ;
var
firstld: ldchain;(*where we build new chain*)
lastld, ldtemp: ldchain;
idxtemp: cmdlineidx;
backward: boolean;(*flag for backward searches*)
(*and now for a few helpers*)
label 1;
procedure error(derr : qerrcode);
begin
err := derr;
qlddispose(firstld);
nld := 0;
goto 1
end;
function getint ( line : cmdline; var idx : cmdlineidx) : qlnoffset;
var okflag : boolean;
begin
getint := 0;
loop
okflag := false;
if idx <= length(line) then
if line[idx] in ['0'..'9'] then
begin
if (getint*10) > maximum(qlineno) then error(qtoobig);
getint := getint*10 + ( ord(line[idx]) - ord('0') );
idx := idx + 1;
okflag := true
end;
exit if not okflag
end
end;
procedure chain ( var one, two : ldchain);
begin
if one <> nil then
one^.next := two;
one := two;
two^.next := nil
end;
function laparse (line : cmdline;
var idx : cmdlineidx;
var first, last : ldchain ): boolean;
var
pred: spred;
firstpass, kickout: boolean;
offsetsign: -1..1;
linetmp: qlineno;
temprec: ldchain;
begin
pred := nil;
firstpass := true;
kickout := false;
err := qok;
offsetsign := 1;
loop
if not qtokenget(line, idx) then
kickout := true
else case line[idx] of
'0'..'9', '+', '-' :begin
if line[idx] in ['+','-'] then
begin
if firstpass then error(qoffirst);
if line[idx] = '-' then
offsetsign := -1;
idx := idx + 1;
if (not qtokenget(line,idx)) orif
(not (line[idx] in ['0'..'9'])) then
error(qnonumf)
end;
linetmp := getint(line, idx);
new(temprec, num_ld);
temprec^.offset := linetmp * offsetsign;
offsetsign := 1
end;
'*', '$', '.' :begin
if not firstpass then error(qonlyfirst);
case line[idx] of
'*' :begin
if nld = 0 then
error(qnoprevious);
new(temprec, star_ld)
end;
'$' :new(temprec, dollar_ld);
'.' :new(temprec, dot_ld)
end;(*little case*)
idx := idx + 1
end;
others :begin
if (line[idx] = '^') then begin
backward := true;
idx := idx + 1;
if not qtokenget(line, idx) then
kickout := true
else kickout := not spredparse
(line, idx, pred, wildswitch, err)
end
else begin
backward := false;
kickout := not spredparse
(line, idx, pred, wildswitch, err)
end;
if err <> qok then error(err);
if not kickout then
begin
if backward then new (temprec, backward_ld)
else new (temprec, forward_ld);
temprec^.pred := pred;
pred := nil
end
end
end;(*case*)
exit if kickout;
chain(last, temprec);
if first = nil then
first := temprec
else if firstpass then
first^.next := temprec;
firstpass := false
end;(*loop*)
laparse := not firstpass
end;
begin(*mainline for qldparse*)
err := qok; nld := 0; firstld := nil; lastld := nil;
idxtemp := idx;
if laparse(line, idx, firstld, lastld) then
begin
nld := nld + 1;
if qtokenget (line, idx) andif (line[idx] = ',') then
begin
new(ldtemp, comma_ld);
ldtemp^.ldkind := comma_ld;
chain(lastld, ldtemp);
idx := idx + 1;
if not laparse(line, idx, lastld, ldtemp)
then error (qno2ndla)
else nld := nld + 1
end;
ld := firstld
end
else begin
ld := nil;
idx := idxtemp
end;
1: end;
public procedure qldeval ( var buf : qbuffer;
ld : ldchain;
searchrange : ldrange;
var range : ldrange;
var err : qerrcode);
label 1;
procedure error(derr : qerrcode);
begin
err := derr;
goto 1
end;
function search (pred : spred;
range : ldrange;
var buf : qbuffer;
var where : qlineno) : boolean;
var found : qlineno;
linetemp : cmdline;
increment : -1..1;
backward : boolean;
function within_limit : boolean;(*function to check limit in search*)
begin
if backward then within_limit := found > range.hbound
else within_limit := found < range.hbound
end;(*function within_limit*)
begin
search := false;
backward := range.lbound > range.hbound;
if not backward then increment := 1
else increment := -1;
found := range.lbound;
while (not search) and within_limit do
begin
found := found + increment;
linetemp := qgetline (buf, found, err);
if err <> qok then error(err);
search := spredmatch (linetemp, pred, err);
if err <> qok then error(err)
end;
if search then where := found
end;
var firsttoken, lowbdalso : boolean;
ldtemp : ldchain;
range_limit, predrange, addrrange : ldrange;
notfinderr, outrangeerr: qerrcode;
begin
ldtemp := ld;
err := qok;
range := searchrange;
range_limit.lbound := qfirst_val (buf);(*allowed search limits- this narrows*)
range_limit.hbound := searchrange.hbound;(*down as the search continues*)
addrrange.lbound := 0;(*limits for non-predicate addressing*)
addrrange.hbound := qdollar_val (buf);
predrange.lbound := buf.curlineno;(*line before search beginning*)
firsttoken := true;
lowbdalso := true;
notfinderr := qla1notfnd;
outrangeerr := qla1outrange;
while ldtemp <> nil do
begin
case ldtemp^.ldkind of
forward_ld,
backward_ld :begin
if ldtemp^.ldkind = backward_ld then begin
predrange.hbound := range_limit.lbound;(*last search line*)
range_limit.hbound := predrange.lbound - 1(*limits narrow down!*)
end
else begin
predrange.hbound := range_limit.hbound;(*like above*)
range_limit.lbound := predrange.lbound + 1
end;
if search(ldtemp^.pred, predrange, buf, range.hbound) then
begin
predrange.lbound := range.hbound
end
else begin
if not(firsttoken and lowbdalso) or
(searchrange.lbound=searchrange.hbound) then
error(notfinderr);
if ldtemp^.ldkind = forward_ld then begin
predrange.lbound := qfirst_val (buf) - 1;
range_limit.lbound := predrange.lbound;(*search limits change*)
range_limit.hbound := buf.curlineno + 1;(*on wrap-around*)
predrange.hbound := range_limit.hbound
end
else begin
predrange.lbound := searchrange.hbound + 1;
range_limit.lbound := buf.curlineno - 1;
predrange.hbound := range_limit.lbound;
range_limit.hbound := predrange.lbound
end;
if not search(ldtemp^.pred, predrange, buf, range.hbound) then
error(notfinderr);
predrange.lbound := range.hbound
end;
firsttoken := false
end;
comma_ld :begin
lowbdalso := false;
notfinderr := qla2notfnd;
outrangeerr := qla2outrange;
firsttoken := true;
range_limit.lbound := range.lbound;
range_limit.hbound := searchrange.hbound;
predrange.lbound := range.lbound;
predrange.hbound := searchrange.hbound;
addrrange.hbound := searchrange.hbound
end;
others :begin
case ldtemp^.ldkind of
dollar_ld :range.hbound := qdollar_val (buf);
star_ld :range.hbound := range.lbound;
dot_ld :range.hbound := buf.curlineno;
num_ld :if firsttoken then
range.hbound := ldtemp^.offset
else if (range.hbound + ldtemp^.offset > addrrange.hbound)
or (range.hbound + ldtemp^.offset < addrrange.lbound) 
then
error(outrangeerr)
else range.hbound := range.hbound+ldtemp^.offset
end;(*little case*)
if range.hbound = 0 then
predrange.lbound := 1
else if range.hbound >= maximum(qlineno) then error(outrangeerr)
else predrange.lbound := range.hbound ;
firsttoken := false
end
end;(*big case*)
if lowbdalso then
range.lbound := range.hbound;
if (range.hbound < addrrange.lbound) or
(range.hbound > addrrange.hbound) then
if lowbdalso or (ldtemp^.ldkind = comma_ld) then
error(qla1outrange)
else error(qla2outrange);
ldtemp := ldtemp^.next
end;(*while*)
if (range.lbound < searchrange.lbound) then error(qla1outrange)
else if (range.hbound > searchrange.hbound) then error(qla2outrange);
1: end;
(*qread reads a line of input, but will no longer allow intraline editing.
naturally assumes that the terminal is open. this routine will also
print a '?' when a control-g is type at the terminal.*)
external function qread : qstring;
public function qread: qstring;
var newline: qstring;(*a convenient input buffer*)
junkline: boolean;(*if line should be ditched*)
begin
qread := '';(*nothing yet*)
repeat(*until line legitimately complete*)
junkline := false;(*looks good so far*)
if eoln (tty)
then
begin(*get more input, if necessary*)
if (tty^ <> cr) and (tty^ <> esc)(*include prior eoln character if not cr or esc*)
then qread := qread || tty^;
readln (tty)
end
else junkline := true;(*may be phony wraparound*)
read (tty, newline: qstringlen - length (qread));
if length (newline) > 0
then(*concatenate new stuff onto end*)
begin
if eoln (tty) and (tty^ = lf) and (newline[length (newline)] = cr)
then
begin(*crlf ==> cr. fake it for line end*)
tty^ := cr;
if length (newline) > 1
then(*definitely a real line*)
begin
qread := qread || substr (newline, 1, length (newline) - 1);
junkline := false(*so make sure we keep it*)
end
end
else
begin
qread := qread || newline;
junkline := false(*keep this line*)
end
end
until (eoln (tty) and ((tty^ = cr) or (tty^ = esc)) and not junkline)
or (length (qread) = qstringlen)
end;(*we got enough for now*)
external procedure qinitbuf(*call once for each buffer*)
( var buffer: qbuffer);(*buffer to be created*)
external procedure qdelbuf(*clear and re-initialize buffer*)
( var buffer: qbuffer);(*buffer to be purged*)
external function qgetline(*returns text of specified line*)
( var buffer: qbuffer;(*working buffer*)
l: qlineno;(*number of line to be gotten*)
var err: qerrcode(*what went wrong?*)
): qstring;(*text of line*)
external procedure qmodline(*change text of specified line*)
( var buffer: qbuffer;(*in this buffer*)
l: qlineno;(*line to be changed*)
newtext: qstring;(*new line's text*)
var err: qerrcode);(*in case of trouble*)
external procedure qdellines(*delete specified range of lines*)
( var buffer: qbuffer;(*from this buffer*)
f, l: qlineno;(*from f to l*)
var err: qerrcode);(*everything qok?*)
external procedure qaddline(*add line of text to specified place*)
( var buffer: qbuffer;(*in this buffer*)
l: qlineno;(*after this line*)
text: qstring;(*line to add*)
var err: qerrcode);(*did it work?*)
external function qbuflength(*return length of bounded buffer*)
( var buffer: qbuffer
): qlineno;
external function qdollar_val(*return the value of a $ ld*)
( var buffer: qbuffer
): qlineno;
external function qfirst_val(*return the value of the 1st bounded line*)
( var buffer: qbuffer
): qlineno;
external procedure qmovelines(*relocate a section of the buffer*)
( var buffer: qbuffer;(*working buffer*)
first, last: qlineno;(*range of lines to be moved*)
dest: qlineno;(*where to move them to*)
var err: qerrcode);(*error report*)
external procedure qcopylines
( var buffer: qbuffer;(*working buffer*)
first, last: qlineno;(*range of lines to copy*)
dest: qlineno;(*where to copy them to*)
var err: qerrcode);(*error report*)
external procedure qsetoffset(*sets offset for addressing bounded lines*)
( newoffset: qlineno;(*new buffer offset*)
var buffer: qbuffer);(*working buffer*)
external procedure qsetbounds(*set up bounded region in buffer*)
( var buffer: qbuffer;(*working buffer*)
f, l: qlineno;(*bounds (in bounded linenos)*)
absolute: boolean;(*flag for line addressing mode*)
var err: qerrcode);(*any problem?*)
external procedure qunbound(*resets buffer bounds to whole buffer*)
( var buffer: qbuffer;(*working buffer*)
var err: qerrcode);(*done?*)
external procedure qfileappend(*read text from file*)
( var buffer: qbuffer;(*in this buffer*)
s940id: file_id;(*from this file*)
wmod: wmodifier;(*940 file type modifier*)
where: qlineno;(*append to this line*)
var cnt: qlineno;(*number of lines appended*)
var err: qerrcode);(*and report any problems*)
external procedure qttyappend(*append text from the tty*)
( var buffer: qbuffer;(*in this buffer*)
where: qlineno;(*append to this line*)
var cnt: qlineno;(*number of lines appended*)
var err: qerrcode);(*report failures*)
external procedure qfilewrite(*write text to file*)
( var buffer: qbuffer;(*buffer to write from*)
s940id: file_id;(*file to write to*)
wmod: wmodifier;(*940 file type modifier*)
fno, lno: qlineno;(*range to write*)
confirm: boolean;(*new/old file prompting desired?*)
var err: qerrcode);(*error report*)
(*algorithm: line descriptors are maintained in a doubly linked
list with header information in the passed buffer descriptor.
the line descriptors in turn contain edt strings.
notes: since any of the edt line operations may be broken out
of, care is taken in the management of the line chain.
while storage may be lost as the result of breaks, the
following strategy insures that these routines will never
leave the line chain in a confused state, i.e., will never
partially complete operations.
1. lastlineno is always correct.
2. getlinep and lastlinep may not be defined (are nil),
even if the buffer contains text. whenever an operation
changes getlineno or lastlineno, the corresponding xxxlinep
is niled first, then updated after the xxxlineno is changed.
3. the back line chain may be broken, i.e., a qline other
than the first one may have prevlinep=nil. however, the
forward chain is always complete.
4. the definitive test of an empty buffer is lastlineno=0,
in which case, the other contents of the buffer are assumed
to be arbitrary.*)
(*move is the ultimate procedure for all line manipulations, including
additions, deletions, and real moves. it handles all the bookkeeping
for updating the special line numbers and pointers. it is assumed that
all line numbers and pointers passed to this routine are reasonable.*)
procedure move
( var buf: qbuffer;(*buffer to manipulate*)
fln: qlineno;(*addr of first line of section to be moved, if
zero, lines are new additions from garblist*)
flp: qlinep;(*ptr to above line*)
lln: qlineno;(*addr of last line to be moved, if fln = 0 then
this is #lines - 1 to yield proper count*)
llp: qlinep;(*ptr to above line*)
tln: qlineno;(*addr of line after which text is to be moved*)
tlp: qlinep );(*ptr to above line, if nil, lines are added to
the list of lines to be discarded*)
var cnt: qlineno;
tlno: qlineno;(*tln adjusted for movements*)
begin
with buf do begin
cnt := lln - fln + 1;(*count of lines to move*)
tlno := tln;
mask(attention);
(*slice out source lines from buffer if move or delete*)
if fln <> 0 then begin(*check for lines in buffer*)
flp^.prevlinep^.nextlinep := llp^.nextlinep;(*take off chain*)
if llp^.nextlinep <> nil then 
llp^.nextlinep^.prevlinep := flp^.prevlinep;
if fln <= lbound then(*adjust special line numbers*)
if lbound > lln then lbound := lbound - cnt
else begin(*lbound in lines moved, new lbound follows*)
lbound := fln;
lboundp := llp^.nextlinep
end;
if fln <= getlineno then
if getlineno > lln then getlineno := getlineno - cnt
else begin(*getlineno in lines move, new before them*)
getlineno := fln - 1;
getlinep := flp^.prevlinep
end;
if fln <= hbound then
if hbound > lln then hbound := hbound - cnt
else begin(*hbound in lines moved, new before them*)
hbound := fln - 1;
hboundp := flp^.prevlinep
end;
if fln <= lastlineno then
if lastlineno > lln then lastlineno := lastlineno - cnt
else begin(*last line in lines moved, new at new end of buffer*)
lastlineno := fln - 1;
lastlinep := flp^.prevlinep
end;
if fln < tlno then tlno := tlno - cnt(*addr of target may be affected too*)
end
(*if appending new lines, remove from garb list*)
else if llp = garblinep(*quick check to see that line is on list*)
then garblinep := flp^.prevlinep;
(*if deleting, add to list to be discarded*)
if tlp = nil then begin
flp^.prevlinep := garblinep;(*garb chain is backwards*)
garblinep := llp
end
(*if moving or appending, add after target line*)
else begin
llp^.nextlinep := tlp^.nextlinep;(*thread source to target*)
flp^.prevlinep := tlp;
if tlp^.nextlinep <> nil then(*thread target to source*)
tlp^.nextlinep^.prevlinep := llp;
tlp^.nextlinep := flp;
if lastlineno = tlno then lastlinep := llp;(*adjust special hooks*)
lastlineno := lastlineno + cnt;
if tlno <= hbound then begin
if tlno = hbound then hboundp := llp;
hbound := hbound + cnt;
if tlno = lbound - 1 then lboundp := flp
else if tlno < lbound then lbound := lbound + cnt
end
end;
changes := true;
unmask(attention);
end
end;
(*cleangarb removes deleted (or unused) lines from the so-called garb list.
the list is scanned backwards and one line at a time is deleted. this
code runs unmasked; if interrupted, at most one line will be lost (i.e.
unchained, but not disposed.)*)
procedure cleangarb ( var buffer: qbuffer );
var lp: qlinep;
begin
with buffer do begin
while garblinep <> nil do begin(*scan list and delete one at a time*)
lp := garblinep;(*save current ptr in temp*)
garblinep := garblinep^.prevlinep;
dispose (lp)(*now delete, after stepping over it in chain*)
end
end
end;
(*internal procedure to find the pointer to a passed lineno*)
procedure findlinep(var buf: qbuffer; lno: qlineno; var lp: qlinep);
(*assumes that l is a good number*)
procedure setlp(tp: qlinep);(*sets findlinep return value*)
begin
with buf do begin(*update buf info first*)
mask(attention);
getlineno := lno;
getlinep := tp;
unmask(attention)
end(*with*);
lp:= tp(*now return pointer*)
end(*setlp*);
procedure search(beglineno: qlineno; beglinep: qlinep;
endlineno: qlineno; endlinep: qlinep);
var tp: qlinep; i: qlineno;(*used in line search*)
begin
(*determine search direction*)
if ((endlineno - lno) <= (lno - beglineno)) and (endlinep <> nil)
then begin(*search backward from endlineno to lno*)
tp:= endlinep;
for i:= endlineno-1 downto lno do
tp:= tp^.prevlinep
end
else begin(*search forward from beglineno to lno*)
tp:= beglinep;
for i:= beglineno+1 to lno do
tp:= tp^.nextlinep
end;
setlp(tp)(*update getline information*)
end(*search*);
begin(*findline*)
with buf do
if lno < getlineno
then if lno < lbound
then search (0, firstlinep, getlineno, getlinep)
else if lno < hbound
then search (lbound, lboundp, getlineno, getlinep)
else search (hbound, hboundp, getlineno, getlinep)
else if lno < lbound
then search (getlineno, getlinep, lbound, lboundp)
else if lno <= hbound
then search (getlineno, getlinep, hbound, hboundp)
else search (getlineno, getlinep, lastlineno, lastlinep)
end(*findline*);
(*function to transform bounded linenos into absolute ones*)
function map
( var buffer: qbuffer;(*working buffer*)
line: qlineno(*line number to transform*)
): qlineno;(*mapped result*)
begin
map := line + buffer.lbound - buffer.offset
end;(*map*)
(*procedure to check a line number*)
function chkline(var buf: qbuffer; l: qlineno; var err: qerrcode): boolean;
begin
err:= qok;
if (l < buf.lbound) or (l > buf.hbound) then err:= qbadln;
chkline:= (err=qok)
end(*chkline*);
(*procedure to check a line range*)
function chkrange(var buf: qbuffer; f,l: qlineno; var err: qerrcode): boolean;
begin
err:= qok;
if f > l then err:= qbadrn
else if l > buf.hbound then err:= qbadub
else if f < buf.lbound then err:= qbadlb;
chkrange:= (err=qok)
end(*chkrange*);
(*procedure to create a edt line record, does not chain it in*)
function makeline (var buf: qbuffer; line: qstring): qlinep;
type
synlinep = ^synline;(*synthetic line*)
synline =
packed record
prevlinep, nextlinep: qlinep;
strng: packed array[1..*] of char
end;
var synp: synlinep;
np: qlinep;
begin
new (synp, length (line));(*alloc line of appropriate length*)
synp^.strng[1:length(line)]:=line;(*copy only to length allocated*)
np := address (synp^);(*coerce the pointer*)
with np^ do begin
nextlinep := nil;
prevlinep := buf.garblinep;(*add to dispose list, in case we lose it*)
if buf.garblinep <> nil then
buf.garblinep^.nextlinep := np;
buf.garblinep := np
end;
makeline := np
end;(*makeline*)
(*procedure to delete a buffer*)
public procedure qdelbuf(var buf: qbuffer);
var err: qerrcode;(*we need it but we ignore them*)
begin(*qdelbuf*)
with buf do begin
if lastlineno>0 then begin(*something to release*)
move (buf, 1, firstlinep^.nextlinep, lastlineno, lastlinep, 0, nil);(*move lines to garb list*)
cleangarb (buf);(*dispose the lines*)
end;
dispose (firstlinep);(*get rid of zeroth line*)
spreddispose (mark);(*dispose mark predicate*)
mark := nil(*for good measure*)
end(*with*)
end(*qdelbuf*);
(*procedure to initialize buffer for first time*)
public procedure qinitbuf(var buf: qbuffer);
begin
with buf do
begin
lastlineno := 0;
getlineno := 0;
lbound := 1;
offset := 1;
oldoffset := 1;
hbound := 0;
curlineno := 0;
new (firstlinep);(*dummy zeroth line to make things easier*)
with firstlinep^ do begin
prevlinep := nil;
nextlinep := nil
end;
lastlinep := firstlinep;
getlinep := firstlinep;
lboundp := nil;
hboundp := firstlinep;
garblinep := nil;
curfile := '';
curfileok := false;
changes := false;
mark := nil;
end
end;(*qinitbuf*)
(*function to return text of line*)
public function qgetline(var buf: qbuffer; l: qlineno; var err: qerrcode): qstring;
var
lp: qlinep;
lno: qlineno;
begin
lno := map (buf, l);
if not chkline(buf, lno, err) then qgetline:= ''
else begin
findlinep(buf, lno, lp);
qgetline := substr (lp^.source, 1, length (lp^.source))
end
end(*qgetline*);
public procedure qmodline(var buf: qbuffer; l: qlineno; newtext: qstring;
var err: qerrcode);
var
lp,np: qlinep;
lno: qlineno;
begin
lno := map (buf, l);
if chkline(buf, lno, err) then begin
findlinep(buf, lno, lp);
np := makeline (buf, newtext);
mask(attention);
with np^ do begin
buf.garblinep := prevlinep;(*remove new from garb list*)
prevlinep := lp^.prevlinep;(*chain new line to neighbors of old line*)
nextlinep := lp^.nextlinep
end;
with lp^ do begin(*chain neighbors to new line*)
prevlinep^.nextlinep := np;(*make forward chain*)
if nextlinep <> nil(*build backward chain*)
then nextlinep^.prevlinep := np
else buf.lastlinep := np;
if lno = buf.lbound then buf.lboundp := np;(*if this was special line, reset ptr*)
if lno = buf.hbound then buf.hboundp := np;
if lno = buf.getlineno then buf.getlinep := np;
end;
lp^.prevlinep := buf.garblinep;(*put old on garb list to dispose*)
buf.garblinep := lp;
buf.changes := true;
unmask(attention);
cleangarb (buf)(*dispose old line*)
end
end(*qmodline*);
(*procedure to add a line to a buffer*)
public procedure qaddline(var buf: qbuffer; l: qlineno; text:qstring;
var err: qerrcode);
var
lno: qlineno;
np, lp: qlinep;
begin
err := qok;(*assume success*)
lno := map (buf, l);
if lno > buf.hbound then err := qbadln
else begin
findlinep (buf, lno, lp);(*find line to append to*)
np := makeline (buf, text);(*create a line with text*)
move (buf, 0, np, 0, np, lno, lp);(*move from garblist to buffer*)
end
end(*qaddline*);
(*procedure to delete line(s) from buffer*)
public procedure qdellines (var buf: qbuffer; f,l: qlineno; var err: qerrcode);
var
fp,
lp: qlinep;
fno,
lno: qlineno;
begin
fno := map (buf, f);
lno := map (buf, l);
if chkrange (buf, fno, lno, err) then begin
findlinep (buf, fno, fp);(*find addressed lines*)
findlinep (buf, lno, lp);
move (buf, fno, fp, lno, lp, 0, nil);(*move to garb list*)
cleangarb (buf);(*and dispose*)
err := qok
end
end(*qdellines*);
public function qbuflength ( var buf: qbuffer ): qlineno;
begin
with buf do begin
qbuflength := hbound - lbound + 1
end
end;
public function qdollar_val ( var buf: qbuffer ): qlineno;
begin
qdollar_val := qbuflength (buf) + buf.offset - 1
end;(*qdollar_val*)
public function qfirst_val ( var buf: qbuffer ): qlineno;
begin
qfirst_val := buf.offset
end;(*qfirst_val*)
public procedure qmovelines
( var buffer: qbuffer;(*working buffer*)
first, last: qlineno;(*range of lines to be moved*)
dest: qlineno;(*where to move them to*)
var err: qerrcode);(*error report*)
var
fno,
lno,
dno: qlineno;(*for line number mapping*)
firstp,
lastp: qlinep;(*temporary pointers*)
destp: qlinep;(*where to re-attach lines*)
begin
fno := map (buffer, first);
lno := map (buffer, last);
dno := map (buffer, dest);
if not chkrange (buffer, fno, lno, err) then return;
if not ((dno = buffer.lbound - 1) orif (chkline (buffer, dno, err))) then return;
if (fno <= dno) and (dno <= lno) then begin(*target within lines to be moved*)
err := qbadmovela;
return
end;
findlinep (buffer, fno, firstp);
findlinep (buffer, lno, lastp);
findlinep (buffer, dno, destp);
move (buffer, fno, firstp, lno, lastp, dno, destp);(*do it*)
end;
public procedure qcopylines
( var buffer: qbuffer;(*working buffer*)
first, last: qlineno;(*range of lines to copy*)
dest: qlineno;(*where to copy them to*)
var err: qerrcode);(*error report*)
var
source: qstring;(*to hold text of lines to be copied*)
idx: qlineno;(*counter for lines*)
fno,
lno,
dno: qlineno;(*for line number mapping*)
firstp,
lastp,
destp: qlinep;(*working pointers*)
begin
fno := map (buffer, first);
lno := map (buffer, last);
dno := map (buffer, dest);
if not chkrange (buffer, fno, lno, err) then return;
if not ((dno = buffer.lbound - 1) orif (chkline (buffer, dno, err))) then return;
(*construct copy of lines to move on garb list*)
cleangarb (buffer);(*not really necessary, but good form*)
firstp := nil;(*to check if first line copied*)
for idx := first to last do begin(*copy lines, use relative #s with qgetline*)
source := qgetline (buffer, idx, err);(*get text of line*)
if err <> qok then return;
lastp := makeline (buffer, source);(*append copy to garb list*)
if firstp = nil then firstp := lastp(*remember start*)
end;
(*move copy of lines into buffer*)
findlinep (buffer, dno, destp);
move (buffer, 0, firstp, lno-fno, lastp, dno, destp)
end;(*qcopylines*)
(*routine to set the buffer offset for addressing bounded lines*)
public procedure qsetoffset (newoffset: qlineno; var buffer: qbuffer);
begin
buffer.oldoffset := buffer.offset;
buffer.offset := newoffset
end;
public procedure qsetbounds (var buffer: qbuffer; low, high: qlineno;
absolute: boolean; var err: qerrcode);
var
tempoffset: qlineno;
tempp: qlinep;(*temporary storage*)
fno,
lno: qlineno;(*for bound conversion*)
begin
tempoffset := buffer.offset;
if absolute then buffer.offset := buffer.lbound
else buffer.offset := buffer.oldoffset;
fno := map (buffer, low);
lno := map (buffer, high);
if chkrange (buffer, fno, lno, err) then
with buffer do
begin
mask(attention);
findlinep (buffer, fno, tempp);
findlinep (buffer, lno, hboundp);
lboundp := tempp;
lbound := fno - offset + 1;
hbound := lno - offset + 1;
unmask(attention)
end;
buffer.offset := tempoffset;
buffer.curlineno := qfirst_val (buffer)
end(*qsetbounds*);
public procedure qunbound (var buffer: qbuffer; var err: qerrcode);
begin
err := qok;
mask(attention);
with buffer do
begin
lbound := 1;
lboundp := firstlinep^.nextlinep;
hbound := lastlineno;
offset := 1;
hboundp := lastlinep
end;
unmask(attention)
end(*qunbound*);
var
f: text;(*kludge around brain-damage*)
public procedure qfileappend
( var buffer: qbuffer;(*working buffer*)
s940id: file_id;(*file to read text from*)
wmod: wmodifier;(*940 file modifier*)
where: qlineno;(*where to append text*)
var cnt: qlineno;(*number of lines appended*)
var err: qerrcode);(*error report*)
var
idx: qstringidx;
ch: char;
whereno: qlineno;(*mapped address*)
tline, rline: qstring;
first, last, wherep: qlinep;
pdp10id: file_id;(*converted name of file*)
rflag: boolean;(*kludgy documentation revision switch*)
werr: wcodes;(*return codes from 940 i/o routines*)
s940: boolean;(*true if file is 940 file*)
wchan: wchannel;(*channel number from which to read*)
junkline: boolean;(*true if line should be ditched*)
begin
err := qok;(*start with a clean slate*)
whereno := map (buffer, where);
if whereno > buffer.hbound
then
begin(*no reading outside file limits!*)
err := qbadln;
return
end;
cnt := 0;(*no lines read yet*)
s940 := false;
pdp10id := s940id;
(*
* convert the file name and figure out what kind of a file we're
* dealing with. if it is a 940 file, use the special 940 file
* opening routine; otherwise, just use standard i/o.
*)
mask(attention);
wfileconvert (s940id, wmod, pdp10id, werr, rflag);
unmask(attention);
if werr = wbadname
then err := qnofile(*stop obvious garbage early*)
else if werr = wok
then
begin(*file is 940 file*)
wchan := getchannel;
wopen (wchan, werr, winput, pdp10id);
if werr <> wok
then
begin
freechannel (wchan);(*open failed. return channel to pool*)
err := qnofile
end
else s940 := true;(*open succeeded. leave the word*)
end
else
qopenfile (f, pdp10id, '', qinput_mode, [qio_ascii], err);
if err = qok
then
begin(*so far, so good*)
first := nil;(*initialize new chain*)
werr := wok;
if not s940
then f^ := cr;(*initialize normal file buffer*)
loop(*the main read loop starts here*)
mask(attention);
if s940(*940 files have special read routine*)
then winline (wchan, werr, tline)
else
begin(*normal files are a bit more arduous*)
tline := '';(*start out fresh*)
(*
* although there are a number of characters which will end lines
* as far as pascal is concerned, the only acceptable line end
* characters for us are a cr-lf pair and cr by itself (not
* recognized by pdp-10 i/o as a line end). accordingly, we
* watch out for all other cases and force them to wrap around
* for up to qstringlen characters. eof forces a halt, regardless
* of the character in the buffer.
*
* note that a line of qstringlen characters should not generate a
* zero length line following it. thus, if the character in f^ is a
* cr, the line must be thrown out if the previous line had qstringlen
* characters and the current line has only one (the cr). this is
* accomplished with a hack. the hack can be removed if the run time
* code ever treats cr-lf as a line end;
*)
repeat(*until we fill a edt line*)
junkline := false;(*line looks good so far*)
if eoln (f)(*fetch a line, if needed*)
then
begin
if f^ <> cr(*force nonstandard lines to wrap*)
then tline := tline || f^;
readln (f)
end
else junkline := true;(*this might be a phony null line*)
read (f, rline: qstringlen - length (tline));(*don't get too much*)
if eof (f)
then f^ := cr;(*always stop on eof*)
if length (rline) > 0
then(*we got something*)
begin
if eoln (f) and (f^ = lf) and (rline[length (rline)] = cr)
then
begin(*chop trailing junk cr, if present*)
f^ := cr;(*and set cr as fake line end*)
if length (rline) > 1
then(*must be a legitimate line*)
begin
tline := tline || substr (rline, 1, length (rline) - 1);
junkline := false
end
end
else
begin
tline := tline || rline;(*bad line end; will wrap*)
junkline := false(*don't eat the line*)
end
end
else if (length (tline) = 0) and eof (f)
then werr := weof(*absolutely all done*)
until (eoln (f) and (f^ = cr) and not junkline) or (length (tline) = qstringlen)
end;
unmask(attention);
exit if werr <> wok;(*either eof or error*)
mask(attention);
last := makeline (buffer, tline);(*chain line into list*)
if first = nil
then first := last;(*remember first line*)
unmask(attention);
cnt := cnt + 1
end;(*main loop*)
(*
* the file has now been completely read in. if there was anything
* there, chain the new blob of lines into the proper slot. indicate
* that the buffer is unchanged if it was initially fresh. close
* the file, reenable attention interrupts, and leave.
*)
if cnt > 0
then
begin(*got something. chain it in*)
findlinep (buffer, whereno, wherep);
move (buffer, 0, first, cnt-1, last, whereno, wherep)
end;
if (buffer.lastlineno = cnt) and (err = qok)
then
begin(*buffer was empty. save file name*)
buffer.changes := false;(*buffer starts out clean*)
if s940
then
begin(*940 name may be revised for doc kludge*)
wfilename (wchan, buffer.curfile, rflag);
buffer.s940 := true(*this is now a 940 buffer*)
end
else
begin(*just use normal name for pdp-10 file*)
buffer.curfile := filename (f);
buffer.s940 := false
end;
buffer.curfileok := true
end;
if s940
then
begin(*special close logic for 940 files*)
wclose (wchan, werr);
freechannel (wchan)
end
else close (f);(*normal close for others*)
end
exception
others: begin
mask(attention);
if s940
then
begin(*special close for 940 file*)
wclose (wchan, werr);
freechannel (wchan)
end
else close (f);(*standard close for most things*)
cleangarb (buffer);(*flush unfinished stuff*)
unmask(attention);
signal();
end;
end;(*qfileappend*)
public procedure qttyappend
( var buffer: qbuffer;(*working buffer*)
where: qlineno;(*where to append text*)
var cnt: qlineno;(*number of lines appended*)
var err: qerrcode);(*error report*)
var
line: qstring;
ch: char;
linenum: qlineno;
done: boolean;
begin
break;
linenum := where;
err := qok;
(*if where = 0 then line := ''(*get text of previous line to edit*)
else line := qgetline (buffer, where, err);
if err <> qok then return; previous line editing deleted!*)
done := false;
while (err = qok) and (not done) do
begin
line := qread ;
if (length (line) = 1) andif (line [1] = '.') then done := true
else
begin
qaddline (buffer, linenum, line, err);
linenum := linenum + 1
end;
end;
if linenum > where then buffer.changes := true;
cnt := linenum - where
end(*qttyappend*);
public procedure qfilewrite(*write text to file*)
( var buffer: qbuffer;(*buffer to write from*)
s940id: file_id;(*file to write to*)
wmod: wmodifier;(*940 file modifier*)
fn, ln: qlineno;(*range to write*)
confirm: boolean;(*new/old file prompting?*)
var err: qerrcode);(*error report*)
var
fno, lno: qlineno;
flp, llp: qlinep;
lineno: qlineno;
line: qstring;
options_set: qiooption_set;
pdp10id: file_id;
s940, rflag: boolean;
werr: wcodes;
wchan: wchannel;
begin
fno := map (buffer, fn);
lno := map (buffer, ln);
if not chkrange (buffer, fno, lno, err)
then return;(*can't write outside legal range*)
if confirm
then options_set := [qio_confirm]
else options_set := [];
(*
* if we have a saved file name, use it. otherwise, convert a possible
* 940 file to a pdp-10 name. in either case, open the file for
* output, prompting for old/new file as required. if the user confirms
* the prompt, back off the open and reopen it using the special 940
* i/o if it's a 940 file.
*)
mask(attention);
if wmod = '*'(*if 940 name was already converted*)
then
begin
pdp10id := s940id;(*don't bother to reconvert*)
werr := wok(*but remember it was 940*)
end
else wfileconvert (s940id, wmod, pdp10id, werr, rflag);
if werr = wbadname
then err := qnofile
else qopenfile (f, pdp10id, '', qoutput_mode, options_set, err);
unmask(attention);
if err = qok
then
begin(*file looks legitimate*)
if werr = wok
then
begin(*got a 940 file. special open*)
close (f);
wchan := getchannel;
wopen (wchan, werr, woutput, pdp10id);
if werr <> wok
then
begin(*no can do. release everything*)
freechannel (wchan);
err := qnofile;
return;
end;
s940 := true(*success. remember it*)
end
else s940 := false;(*normal file signal*)
(*
* the file has now been properly opened. figure out where the first
* and last lines of the output block are; then write them one at a time.
*)
findlinep (buffer, fno, flp);
findlinep (buffer, lno, llp);
repeat(*here comes the main output loop*)
mask(attention);
if s940
then
begin(*special output for 940 files*)
woutline (wchan, werr, flp^.source);
if werr <> wok
then err := qwrterr(*acknowledge write error*)
end
else
begin(*standard output for normal files*)
writeln (f, flp^.source);
if not eof (f)
then err := qwrterr(*should be positioned at file end*)
end;
unmask(attention);
exit if flp = llp;(*no pointer shuffle after we're done*)
flp := flp^.nextlinep(*advance to next line*)
until err <> qok;(*end of main loop*)
(*
* the file (or a hunk of it) has now been completely written.
* if it was, in fact, the whole file, save the file name and
* type for later defaulting. then close the file.
*)
if (fno = 1) and (lno = buffer.lastlineno) and (err = qok)
then
begin(*whole file written. remember where*)
buffer.changes := false;(*buffer is now clean*)
if s940
then
begin(*get 940 file name from special code,*)
rflag := false;(*avoiding doc revision kludge*)
wfilename (wchan, buffer.curfile, rflag);
buffer.s940 := true(*all hail, xds*)
end
else
begin(*use standard code for normal file*)
buffer.curfile := filename (f);
buffer.s940 := false
end;
buffer.curfileok := true(*we now have a saved name*)
end;
if s940
then
begin
wclose (wchan, werr);(*the usual special 940 close*)
freechannel (wchan)
end
else close (f);(*the usual stuff for normal files*)
end
else if err = qnofile(*don't treat confirm failure as error*)
then err := qok;
exception
others: begin
mask(attention);
if s940
then
begin(*file is 940 file. special close*)
wclose (wchan, werr);
freechannel (wchan)
end
else close (f);(*standard close for normal file*)
writeln (tty, 'warning--output file write incomplete.');
break;
unmask(attention);
signal();
end;
end;(*qfilewrite*)
external procedure qmarkmatch(*matches lines to the markstring*)
( var buffer: qbuffer;(*in this buffer*)
mark: spred;(*predicate to match*)
sect_name: spred;(*name predicate to and in*)
start: qlineno;(*place to start looking*)
var fno, lno: qlineno;(*return args, new bounds*)
backward: boolean;(*search direction flag*)
wrap: boolean;(*wrap/no-wrap around buffer flag*)
var err: qerrcode);(*error report*)
public procedure qmarkmatch
( var buffer: qbuffer;(*working buffer*)
mark: spred;(*markstring to search for*)
sect_name: spred;(*in conjunction with mark*)
start: qlineno;(*where to start looking*)
var fno, lno: qlineno;(*limits of the marked section*)
backward: boolean;(*backward search flag*)
wrap: boolean;(*wrap/no-wrap around flag*)
var err: qerrcode);(*error report*)
var
lineno: qlineno;
line: qstring;
increment: -1..1;
begin
lineno := start;
if backward then increment := -1
else increment := 1;
loop
line := qgetline (buffer, lineno, err);
if err <> qok then return;
exit if spredmatch (line, mark, err) andif spredmatch (line, sect_name, err);
if err <> qok then return;
lineno := lineno + increment;
if (lineno > buffer.hbound) and wrap then lineno := buffer.lbound
else if (lineno < buffer.lbound) and wrap then lineno := buffer.hbound;
if lineno = start then
begin
err := qnomark;
return
end
end;
fno := lineno;
if lineno <> buffer.hbound then
begin
loop
lineno := lineno + 1;
line := qgetline (buffer, lineno, err);
if err <> qok then return;
exit if spredmatch (line, mark, err) do lineno := lineno - 1;
exit if lineno = buffer.hbound;
if err <> qok then return
end;
end;
lno := lineno
end;(*qmarkmatch*)
external procedure prline (var outfile: text; line: qstring;
tab_print: boolean; var err: qerrcode);
(*outputs the line to the specified file, converting
all control characters except &g through &m to &x form.*)
external procedure qlistlines (var buffer: qbuffer; low, high: qlineno;
var f: text; ctl_char, number, tab_print: boolean; var err: qerrcode);
(*qlistlines outputs the specified range of lines to
the tty. uses prline.*)
public procedure prline
( var outfile: text;(*file to write to*)
line: qstring;(*text to write out*)
tab_print: boolean;(*flag for &i printing*)
var err: qerrcode);(*checks for write errors*)
var
i: qlineno;
begin
for i := 1 to length (line) do
begin
if (ord(line[i]) > #o37) orif (tab_print and (line[i] = tab))
then write(outfile, line[i])
else write(outfile, '&', chr(ord(line[i]) + #o100));
exit if not eof(outfile) do err := qwrterr
end;
writeln (outfile);
if not eof(outfile) then err := qwrterr
end;
public procedure qlistlines
( var buffer: qbuffer;(*working buffer*)
low,
high: qlineno;(*range of lines to print*)
var outfile: text;(*to this file*)
ctl_char,
number,(*flags to modify printing*)
tab_print: boolean;(*flag for &i printing*)
var err: qerrcode);(*set if write errors occur*)
var
i: qlineno;
line: qstring;
begin
i := low;
err := qok;
while (err = qok) and (i <= high) do
begin
line := qgetline (buffer, i, err);
if err = qok then
begin
if number then begin
write( outfile, i:5, tab);
if not eof(outfile) then err := qwrterr
end;
if err = qok then begin
if ctl_char then prline (outfile, line, tab_print, err)
else begin
writeln(outfile, line);
if not eof(outfile) then err := qwrterr
end;
i := i + 1
end
end
end
end;
(*qsubstitute replaces zero or more occurrences of a pattern within a line
with a caller supplied string. the caller may select among the following
options:
confirm - display the line, bracketing the matched pattern with backslash,
and query the user to confirm the substitution. it is assumed that
the teletype has been opened.
all - substitute for all occurences of the pattern in the line; if not
selected, only the first occurrence may be replaced.
print - after applying all substitutions, display the resulting line
if modifications have been made.
number - if a match for the pattern is found, display a caller supplied
line number before the first substitution.
this routine returns the modified line, a count of the number of substitutions
performed, and a flag indication that a match was found for the pattern. the
flag may be true when the count is zero if the substitutions were not confirmed
by the user.
an error is reported if insertion of the replacement string would result in
a line that is too long. if on return, the error code is nonnull, then
the flag function value is true, but the values of the other values are
undefined. 
n_par is the nth occurrance parameter. this parameter instructs qsubst on 
which occurrance of matching the pattern substitution(s) should begin.
variable nth is a count of the number of matches found and not 
substituted for. if nth is less than n_par, then no substitution will be
made and qsubst will return false.
cmd must be either substitute, before, or after edtcmd. this instructs
qsubst whether to substitute or insert the replacement string in the
line.*)
external function qsubstitute
( var line: qstring;(*line to be modified*)
lineno: qlineno;(*number of above*)
pat: spattern;(*pattern to be replaced*)
rplmtstr: qstring;(*string to replace pattern*)
option_list: sub_opt_set;
var cnt: qstringidx;(*count of substitutions performed*)
cmd : edtcmds;(*parsed command*)
var nth : qlineno;(*count of occurrances*)
n_par : qlineno;(*occurrance parameter*)
var err: qerrcode(*indicates if result too long*)
): boolean;(*indicates match found*)
public function qsubstitute(*substitute one string for another in a third*)
( var line: qstring;(*line in which substitution is to be made*)
lineno: qlineno;(*above's line number*)
pat: spattern;(*pattern to search for*)
rplmtstr: qstring;(*string to replace it with*)
opts: sub_opt_set;(*various action-modifiers*)
var cnt: qstringidx;(*number of substitutions made*)
cmd : edtcmds;(*parsed command*)
var nth : qlineno;(*count of occurrances*)
n_par : qlineno;(*occurrance parameter*)
var err: qerrcode(*error report*)
): boolean;(*flag indicating success of substitution*)
label 1;
var
tempstr: qstring;
numberit,
doit: boolean;
pos,
len,
idx: qstringidx;
repstr : qstring;
begin
err := qok;
idx := 1;
cnt := 0;
nth := 0;
qsubstitute := false;
numberit := (lineno <> 0) and (number_sop in opts);
loop
tempstr := substr (line, idx);(*bug fix ?*)
doit := spatmatch (tempstr, pat, pos, len, err);
exit if (not doit) or (err <> qok);
nth := nth +1;
if nth >= n_par then begin
qsubstitute := true;
if numberit then
begin
writeln (tty, lineno:5);
numberit := false
end;
if confirm_sop in opts then
begin
writeln (tty, substr (line, 1, idx + pos - 2), '\', substr (line, idx + pos -1, len),
'\', substr (line, idx + pos + len - 1));
doit := query ('ok')
end;
if doit then
begin
if (cmd = substitute) then repstr := rplmtstr
else if (cmd = before) then repstr := rplmtstr||substr(line, idx+pos-1, len)
else if (cmd = after) then repstr := substr(line, idx+pos-1, len)||rplmtstr
else err := qbadcmd;
if idx + pos - 1 + length (repstr) - 1 +
length (substr (line, idx + pos - 1 + len)) <= qstringlen then begin
line := substr (line, 1, idx + pos - 2) || repstr ||
substr (line, idx + pos + len - 1);
cnt := cnt + 1;
idx := idx + pos + length (repstr) - 1(*advance past inserted string*)
end
else err := qlnlong;
end
else idx := idx + pos + len - 1;(*advance past matched string*)
if not ((all_sop in opts) and (pat.stype in [simple, token])) then goto 1
end(*if nth begin*)
else idx := idx + pos + len - 1;(*advance past the matched string*)
exit if err <> qok
end;(*loop*)
1:
if (err = qok) and (cnt > 0) and (print_sop in opts) then prline (ttyoutput, line, true, err)
end;(*qsubstitute*)
external procedure qjoinlines(*turn two (or more) lines into one*)
( var buffer: qbuffer;(*working buffer*)
first,
last: qlineno;(*range of lines to join*)
contmark: qstring;(*string to replace crs with*)
var err: qerrcode);(*error report*)
public procedure qjoinlines(*turn two (or more) lines into one*)
( var buffer: qbuffer;(*working buffer*)
first,
last: qlineno;(*range of lines to join*)
contmark: qstring;(*string to replace crs with*)
var err: qerrcode);(*error report*)
var
source: qstring;(*text of lines to be joined*)
result: qstring;(*line formed by joining others*)
lineno: qlineno;(*counter to step through lines joined*)
begin
if first >= last
then begin
err := qjointoofew;
return
end;
result := qgetline (buffer, first, err);
if err <> qok then return;
for lineno := (first + 1) to last do
begin
source := qgetline (buffer, lineno, err);
if err <> qok then return;
if length (result || source) > qstringlen
then begin
err := qlnlong;
return
end;
result := result || contmark || source
end;
qmodline (buffer, first, result, err);
if err <> qok then return;
qdellines (buffer, first + 1, last, err)
end;(*qjoinlines*)
external function qsplitlines(*split selected lines into smaller ones*)
( var buffer: qbuffer;(*working buffer*)
lineno: qlineno;(*addr of line to split*)
pat: spattern;(*where to split each line*)
option_list: split_opt_set;(*various action-controllers*)
var cnt: qlineno;(*number of splits - new lines created*)
var err: qerrcode(*error report*)
): boolean;(*true if a match for pat found*)
public function qsplitlines(*split selected lines into smaller ones*)
( var buffer: qbuffer;(*working buffer*)
lineno: qlineno;(*line to split*)
pat: spattern;(*where to split each line*)
opts: split_opt_set;(*various action-controllers*)
var cnt: qlineno;(*number of splits done, i.e. number of new lines*)
var err: qerrcode(*error report*)
): boolean;(*set true if a match for pat found*)
label 1;
var
pos,
len: qstringidx;(*for spatmatch*)
source: qstring;(*text of lines to be split*)
tempstr: qstring;(*temporary for building lines*)
doit,
didit,
numberit: boolean;(*conditionals*)
newlineno: qlineno;(*line counters*)
idx: qstringidx;(*position at which to search for pat*)
begin
cnt := 0;
err := qok;
didit := false;
qsplitlines := false;
newlineno := lineno;
idx := 1;
source := qgetline (buffer, lineno, err);
if err <> qok then return;
numberit := (number_splitop in opts);
loop
tempstr := substr (source, idx);
doit := spatmatch (tempstr, pat, pos, len, err);
exit if (not doit) or (err <> qok);
qsplitlines := true;
if numberit
then begin
writeln (tty, lineno:5);
numberit := false
end;
if confirm_splitop in opts
then begin
writeln (tty, substr (source, 1, idx + pos - 1 - 1), '\', substr (source, idx + pos - 1, len),
'\', substr (source, idx + pos - 1 + len));
doit := query ('ok')
end;
if doit
then begin
(*we take care here to complete this single split operation, so
that in the event of a escape being issued to the next
confirmation prompt, everything is as it should be up to the
point that the escape is issued*)
if delete_splitop in opts
then tempstr := substr (source, 1, idx + pos - 1 - 1)
else tempstr := substr (source, 1, idx + pos - 1 + len - 1);
source := substr (source, idx + pos - 1 + len);
qaddline (buffer, newlineno, source, err);
if err <> qok then return;
qmodline (buffer, newlineno, tempstr, err);
if err <> qok then return;
didit := true;
newlineno := newlineno + 1;
cnt := cnt + 1;
idx := 1
end
else idx := idx + pos - 1 + len;
if not ((all_splitop in opts) and (pat.stype in [simple, token]))
then goto 1
end;(*loop*)
1:
if (print_splitop in opts) and didit
then qlistlines (buffer, lineno, newlineno, ttyoutput, true, false, true, err);
end;(*qsplitlines*)
type
qiomode = (qinput_mode, qoutput_mode);
qiooptions = (qio_append, qio_confirm, qio_ascii);
qiooption_set = set of qiooptions;
external procedure qopenfile ( var f: text;(*file to open*)
fid: file_name;(*name of file*)
ext: string[3];(*default extension*)
mode: qiomode;(*i/o mode*)
opts: qiooption_set;(*options*)
var err: qerrcode );(*error code*)
type
qiomode = (qinput_mode, qoutput_mode);
qiooptions = (qio_append, qio_confirm, qio_ascii);
qiooption_set = set of qiooptions;
(*
this module contains the routine qopenfile, modelled after rdlib's
open_file.
*)
(*qopenfile opens a text file for input or output. the mode is specified by
the caller. for an output file the user can also request append mode and
old/new file prompting. the caller may supply a default extension for the
file name. err is returned indicating if the open was successful.*)
public procedure qopenfile
( var f: text;
fid: file_name; ext: string[3];
mode: qiomode;
option_set: qiooption_set;
var err: qerrcode );
var question: query_string;
lext: packed array[1..5] of char;
begin
err := qok;
lext := ' ';
lext := '.' || ext;
case mode of 
qinput_mode:
begin
if (option_set - [qio_ascii]) <> [] then begin
err := qnoinfile;
return
end;
if qio_ascii in option_set
then open (f, lext || fid, [ascii])
else open (f, lext || fid);
if iostatus <> io_ok then err := qnoinfile;
end;
qoutput_mode:
begin
if qio_confirm in option_set then begin
open (f, lext || fid);
if eof (f)
then question := 'new file: ' || fid
else begin
question := 'old file: ' || filename (f);(*used full file_id of that found*)
if f <> tty then close (f)
end;
if not query (question) then begin
err := qnofile;
return
end
end;
if qio_append in option_set
then rewrite (f, lext || fid, [preserve])
else rewrite (f, lext || fid);
if iostatus <> io_ok then err := qnooutfile
end
end;
end;
external function qlabelfind
( var buffer: qbuffer; 
start: qlineno;(*where to start search*)
top: qlineno;(*upper search limit*)
var qlabel: qstring;(*label name, if found*)
var displ: qlineno;(*disp. of label from start*)
var err: qerrcode):(*error code*)
boolean;(*true if label found, else false*)
(*qlabelfind searches the buffer from the start line backwards to the
top line for a line with a label. if one is found, the label's name
and its displacement from the start line are returned, with the value
true. otherwise, if no label is found, false is returned. 
a label begins in column one with a character in the set
['a'..'z','a'..'z','0'..'9','$'] and ends with the character
preceding the next tab, blank, or end of line.*)
public function qlabelfind
( var buffer: qbuffer; 
start: qlineno;(*where to start search*)
top: qlineno;(*upper search limit*)
var qlabel: qstring;(*label name, if found*)
var displ: qlineno;(*disp. of label from start*)
var err: qerrcode):(*error code*)
boolean;(*true if label found, else false*)
(*qlabelfind searches the buffer from the start line backwards to the
top line for a line with a label. if one is found, the label's name
and its displacement from the start line are returned, with the value
true. otherwise, if no label is found, false is returned. 
a label begins in column one with a character in the set
['a'..'z','a'..'z','0'..'9','$'] and ends with the character
preceding the next tab, blank, or end of line.*)
type
charset = set of char;
const
label_heads : charset := [ 'a'..'z', '0'..'9', '$' ];
var
lineno: qlineno;
line: qstring;
idx: qstringidx;
begin
lineno := start;
qlabelfind := false;(*until we find a label*)
while (not qlabelfind) and (lineno >= top) do begin
line := qgetline (buffer, lineno, err);
idx := 1;
exit if err <> qok;
if (idx <= length(line)) and
(uppercase (line[1]) in label_heads) then begin(*we have a label*)
qlabelfind := true;
idx := 2;
while (idx <= length(line)) andif
(line[idx] <> ' ') andif
(line[idx] <> tab) do idx := idx + 1;
qlabel := substr (line, 1, idx-1);(*pull of the label name*)
displ := start - lineno(*calculate displacement of label*)
end
else lineno := lineno - 1
end(*while*)
end(*qlabelfind*).
const
tab = chr(#o11);
lf = chr(#o12);
cr = chr(#o15);
esc = chr(#o33);
external const
qcmds: qcmdlist;
sops: sub_opt_list;
setparams: set_par_list;
splitops: split_op_list;
defrange: defrangelist;
type
qdatarec =(*set and other parameter variables*)
record
linecount,(*number of lines to print in a <cr>*)
maxdel: qlineno;(*maximum no. of lines to delete without confirm*)
tabprint: boolean;(*controls printing of tab character*)
wildswitch: boolean;(*enables/disables wildcarding*)
openfileopen: boolean;(*is the file open?*)
s940: boolean;(*if open file is 940 file*)
markstring: cmdline;(*the mark string for bound matching*)
lasterr: qerrcode;
errlevel: cmdlineidx
end;
static var
qdata: qdatarec;
openfile: text;
openchan: wchannel;
list_file: text;
saved_bounds: boolean;(*to push and pop buffer bounds*)
lbound_save,
hbound_save: qlineno;
offset_save: qlineno;
procedure pushbounds(var buffer: qbuffer; var err: qerrcode);
begin
offset_save := buffer.offset;
lbound_save := buffer.lbound;
hbound_save := buffer.hbound;
saved_bounds := true;
qunbound (buffer, err)
end;
procedure popbounds(var buffer: qbuffer);
var derr: qerrcode;(*use local code since called from error reporting utils*)
curline_save: qlineno;(*save from qsetbounds resetting*)
begin
if saved_bounds then begin(*called at errors, check if bounds pushed*)
qunbound (buffer, derr);(*save linenos are in terms of whole buffer*)
if lbound_save <= hbound_save then(*must be empty buffer - qunbound suffices to set
bounds properly; qsetbound fouls up*)
begin
buffer.offset := offset_save;
curline_save := buffer.curlineno;
qsetbounds (buffer, lbound_save, hbound_save, true, derr);
buffer.curlineno := curline_save
end
end;
saved_bounds := false
end;
public procedure qsetmarkdefault (line: cmdline);
begin
qdata.markstring := line
end;
public procedure qsettabdefault (default: boolean);
begin
qdata.tabprint := default
end;
public procedure qinit (var buffer: qbuffer);
var didx: qstringidx; derr: qerrcode;
begin with qdata do
begin
linecount := 1;
maxdel := 10;
wildswitch := false;(*new for ops version*)
s940 := false;(*no 940 file yet*)
openfileopen := false;
lasterr := qok;
errlevel := 1
end;
qsetcase(true);
didx := 1;
if spredparse (qdata.markstring, didx, buffer.mark, false, derr) then ;
end;
(*initialization routine which calls four other initialization
routines in the mandantory order*)
public procedure qinitexec( var buffer: qbuffer);
begin
qinitbuf( buffer );
qsetmarkdefault( ':$page:');
qsettabdefault( true );
qinit( buffer )
end;
public procedure qexecute
( var buffer: qbuffer;(*working buffer*)
line: cmdline;(*command line to parse*)
var lindex: cmdlineidx;(*place marker*)
var execrange: ldrange;(*limits of execution*)
var ble: qlineno;(*bottommost line examined*)
findflag: boolean;(*running under find?*)
allowed_cmds: edt_cmd_set;(*which commands are legal?*)
var err: qerrcode);(*anything wrong?*)
label 1, 2, 100;
const
confirm_file := true;(*new/old file prompting desired*)
var(*parsing routine args, etc.*)
nld: ldcount;(*number of la's in ld*)
ld: ldchain;(*pointer to parsed ld linked list*)
cmd: edtcmds;(*parsed command*)
cmdrange: ldrange;(*value of parsed ld*)
var(*command routine identifiers*)
fid: file_id;(*for file reading and writing*)
wmod: wmodifier;(*940 file modifier*)
werr: wcodes;(*i/o codes*)
rflag: boolean;(*940 revision flag*)
pdp10id: file_id;(*converted 940 file name*)
cnt: qlineno;(*line count for appends*)
confirm,
doit: boolean;(*for conditional tests throughout*)
lp: qlinep;(*for debugging*)
sop: sub_options;(*substitute storage*)
sop_set: sub_opt_set;(*for option parsing*)
splitop: split_options;(*split option parsing*)
splitop_set: split_opt_set;(*ditto*)
idx: qlineno;(*counter for running through buffer*)
pat: spattern;(*pattern parsing for substitute, find*)
predi: spred;(*for set parsing & bound*)
repstr: qstring;(*replacement string in substitute request*)
total: qlineno;(*to keep track of changes made*)
source: qstring;(*place to keep text of looked-up line*)
pos,
stridx,
len: qstringidx;(*indicies into edt-type strings*)
fno,
lno: qlineno;(*boundary markers*)
tmprange: ldrange;(*for additional ld parsing*)
findble: qlineno;(*for find to keep track with*)
find_cmds: edt_cmd_set;(*legal commands under find*)
optcmd: edtcmds;(*for option parsing in move, copy*)
setopt: set_params;(*for set parameter parsing*)
have_file: boolean;(*true if file parameter present*)
old_cmdrange: ldrange;(*temp used in find command*)
on_opt: boolean;(*for on/off parsing*)
joinstring: qstring;(*join continuation mark*)
qlabel: qstring;(*_ command label variable*)
displ: qlineno;(*_ cmd label displacement*)
match: integer;(*cmd_lookup parameter*)
procedure chkerr;
begin
if err <> qok then begin
popbounds(buffer);
goto 100
end;
end;(*chkerr*)
procedure seterr (newerr: qerrcode);
begin
err := newerr;
chkerr
end;(*seterr*)
procedure skipblanks;
begin
while (lindex <= length (line)) andif (ord (line[lindex]) <= ord (' '))
do lindex := lindex + 1
end;
procedure ck_extra_txt;
begin
skipblanks;
if (lindex <= length(line)) andif (line[lindex] <> ';') then
seterr(qextratxt);
end;(*ck_extra_txt*)
function checkpunct (ch: char): boolean;
begin
skipblanks;
if (lindex <= length (line)) andif (ch = line[lindex])
then begin
checkpunct := true;
lindex := lindex + 1
end
else checkpunct := false
end;
function parsenum (var value: qlineno; var err: qerrcode): boolean;
begin
skipblanks;
value := 0;
parsenum := false;
while (lindex <= length (line)) andif (line[lindex] in ['0'..'9']) do begin
if (value * 10) > maximum (qlineno) then seterr (qtoobig);
value := value * 10 + (ord (line[lindex]) - ord ('0'));
lindex := lindex + 1;
parsenum := true
end
end(*parsenum*) ;
function parseonoff (var on_opt: boolean;(*flag for on option*)
var err: qerrcode
): boolean;(*flag for good parse*)
var str: qstring;
begin
parseonoff := false;
skipblanks;
str := '';
while (lindex <= length(line)) do begin
if line[lindex] <> ' ' then
str := str||line[lindex];
lindex := lindex +1
end;(*while*)
if uppercase(str) = 'on' then on_opt := true
else if uppercase(str) = 'off' then on_opt := false
else err := qbadparam;
if err = qok then parseonoff := true
end;(*parseonoff*)
procedure chkrng (cmd: edtcmds; var nld: ldcount; var range: ldrange;
findflag: boolean; var err: qerrcode);
function decode (arg: rangetypes): qlineno;
begin
case arg of
one: decode := qfirst_val (buffer);
dollar: decode := qdollar_val (buffer);
dot: decode := buffer.curlineno;
dotp1: decode := buffer.curlineno + 1;
lb: decode := cmdrange.lbound;
lbp1: decode := cmdrange.lbound + 1
end(*case*)
end;(*decode*)
begin
err := qok;
if (nld = 0) and (defrange[cmd].permitted = 0) then return;
if (nld > defrange[cmd].permitted) or (nld < defrange[cmd].required)
and (not findflag) then err := qbadargno
else begin
if nld = 0 then
if findflag then
begin
range.lbound := buffer.curlineno;
range.hbound := buffer.curlineno;
nld := 1(*suppress nld=0 special cases when executing under
find, e.g. list*)
end
else
begin
range.lbound := decode (defrange[cmd].lbound);
range.hbound := decode (defrange[cmd].hbound1)
end
else if nld = 1 then range.hbound := decode (defrange[cmd].hbound2);
if not ( ((nld = 0) and (cmd in [bound, writecmd, save])) or
((range.lbound = (qfirst_val(buffer) - 1)) and ((cmd = append) or (cmd =readcmd))))
then with range do begin
if qbuflength (buffer) = 0
then err := qempty
else if lbound > hbound
then err := qbadrn
else if lbound < qfirst_val (buffer)
then err := qbadlb
else if hbound > qdollar_val (buffer)
then err := qbadub
end
end
end;(*chkrng*)
function numtochar (num: qlineno): qstring;
var value: qlineno;
begin
numtochar := '';
value := num;
repeat
numtochar := chr (ord ('0') + (value mod 10)) || numtochar;
value := value div 10
until value = 0
end;(*numtochar*)
function discard_changes: boolean;
begin
if buffer.changes
then discard_changes := query ('unwritten changes, ok')
else discard_changes := true
end;
procedure close_open_file;
begin
if qdata.openfileopen(*don't do anything if nothing open!*)
then
begin
mask(attention);(*until we can signal file closed*)
if qdata.s940(*special close for 940 file*)
then
begin
wclose (openchan,werr);(*close file*)
freechannel (openchan);(*and release the channel*)
qdata.s940 := false
end
else close (openfile);(*standard close for everybody else*)
qdata.openfileopen := false;(*leave the word*)
unmask(attention)
end
end;(*close_open_file*)
function file_parameter (var fid: file_id; var wmod: wmodifier): boolean;
var l: cmdlineidx;
function nextslug: file_id;(*a convenience function to strip*)
var loc: cmdlineidx;(*the next bunch of nonblank characters*)
begin
loc := verify (substr (line, lindex), [succ (' ')..pred (maximum (char))] - [';'],
length (line) - lindex + 2);(*grab all printing nonblanks*)
nextslug := substr (line, lindex, loc - 1);
lindex := lindex + loc -1(*adjust index over slug*)
end;
begin
fid := '';
wmod := '';(*no initial 940 modifier*)
file_parameter := false;
skipblanks;
if lindex > length (line)(*nothing on line - don't check for ;*)
then return;(*on some systems, ; is part of filename*)
l := lindex;
fid := nextslug;(*isolate file name*)
skipblanks;
wmod := nextslug;(*940 modifier follows name*)
file_parameter := l <> lindex(*if counter moved, something's here*)
end;
procedure do_append (fid: file_id; wmod: wmodifier; lno: qlineno);
var cnt: qlineno;
begin
if fid = ''
then qttyappend (buffer, lno, cnt, err)
else qfileappend (buffer, fid, wmod, lno, cnt, err);
execrange.hbound := execrange.hbound + cnt;
if lno < ble then ble := ble + cnt;
buffer.curlineno := lno + cnt;
chkerr
end;
function do_delete (range: ldrange): boolean;
var cnt: qlineno;
begin
with range do begin
cnt := hbound - lbound + 1;
do_delete := (cnt < qdata.maxdel) orif query (numtochar (cnt) || ' lines, ok');
if do_delete then begin
qdellines (buffer, lbound, hbound, err);
chkerr;
if ble > hbound
then ble := ble - cnt
else if ble >= lbound then ble := lbound - 1;
execrange.hbound := execrange.hbound - cnt;
buffer.curlineno := lbound - 1
end
end
end;
procedure print_times (num: qlineno);
begin
write (tty, numtochar (num), ' time');
if num <> 1 then write (tty, 's');
writeln (tty, '.')
end;
procedure substcmd;
var
nth, n_par : qlineno;(*variables for nth occurrance matching*)
match : integer;
begin
if not parsenum(n_par, err) then n_par := 1;
chkerr;
if not spatparse (line, lindex, pat, qdata.wildswitch, err) then err := qbadsubst;
chkerr;
if ((pat.stype <> simple) and (pat.stype <> token)) andif (n_par <> 1) then
err := qbadnth;(*nth occurrance is illegal with ::,@@, or ##*)
chkerr;
pos := index (substr (line, lindex), substr (line, lindex - 1, 1));
if pos = 0 then seterr (qbadsubst);
repstr := substr (line, lindex, pos - 1);
lindex := lindex + pos;
skipblanks;
sop_set := [];
while cmd_lookup (line, lindex, token_chars, sops, match) do begin
sop := sub_options (match);
sop_set := sop_set + [sop];
if checkpunct (',') then ;
end;
skipblanks;
if (lindex <= length (line)) andif (line[lindex] <> ';') then
begin
err := qbadopt;
return
end;
total := 0;
for idx := cmdrange.lbound to cmdrange.hbound do
begin
source := qgetline (buffer, idx, err);
chkerr;
if qsubstitute
(source, idx, pat, repstr, sop_set, cnt, cmd, nth, n_par, err) then
begin
buffer.curlineno := idx;(*set for a matching line*)
chkerr;
if cnt <> 0 then begin(*modify only if subs made*)
qmodline (buffer, idx, source, err);
chkerr;
end;
total := total + cnt
end;
chkerr
end;
if (cmdrange.lbound <> cmdrange.hbound) or (total = 0) or (all_sop in sop_set)
then print_times (total);
spatdispose (pat)
end;(*substitute*)
procedure boundcmd;

var tempat: spattern;(*save var for defpat on next option*)
next: boolean;(*flag indicating a next option*)
backward: boolean;(*flag for a backward search*)
function nextparse: boolean;(*returns true on a next option*)
var next_str: qstring;(*the word next*)
start: qlineno;(*start position of option*)
begin
next_str := 'next';
nextparse := true;
start := lindex - 1;
(*take 'next' or any abbreviation*)
while ((lindex <= length(line)) and ((lindex-start) <= 4))
and (line[lindex] <> ';') do begin
if (uppercase(line[lindex]) <> next_str[lindex-start]) then
nextparse := false;
lindex := lindex + 1
end;
if ((lindex-start) = 1) then nextparse := false;(*pointer didn't advance*)
if nextparse then begin
start := 1;
nextparse := spredparse('::', start, predi, qdata.wildswitch, err);
end
else if ((lindex-start) <> 1) then err := qbadopt
else err := qok;
next := nextparse
end;(*nextparse*)
begin
next := false;
idx := buffer.curlineno + buffer.lbound - 1;(*save curlineno in terms of unbounded linenos*)
if ((lindex <= length(line)) andif (line[lindex] = '^')) then begin
backward := true;
lindex := lindex + 1
end
else backward := false;
if spredparse (line, lindex, predi, qdata.wildswitch, err) orif nextparse then
begin
chkerr;
ck_extra_txt;
if nld <> 0 then seterr (qbadargno);
pushbounds(buffer, err);
qunbound (buffer, err);
if qbuflength (buffer) = 0 then seterr (qempty);
if (idx = qdollar_val (buffer)) and (not backward) then idx := 0
else if (idx = qfirst_val(buffer)) and backward then idx :=
qdollar_val (buffer) + 1;
if next then begin(*to save the defpat first*)
tempat := spatgetdefault;
if backward then
qmarkmatch (buffer, buffer.mark, predi, idx, fno, lno, backward, false, err)
else
qmarkmatch (buffer, buffer.mark, predi, idx+1, fno, lno, backward, false, err);
spatsetdefault(tempat);(*restore defpat*)
spatdispose(tempat);(*clean up*)
if (err = qbadln) then err := qnomark
end
else if backward then
qmarkmatch (buffer, buffer.mark, predi, idx-1, fno, lno, backward, true, err) 
else qmarkmatch (buffer, buffer.mark, predi, idx+1, fno, lno, backward, true, err);
chkerr;
qsetbounds (buffer, fno, lno, true, err);
chkerr;
saved_bounds := false;(*we have reset bounds after push*)
buffer.curlineno := qfirst_val (buffer)
end
else if nld = 0
then begin
if backward then err := qbadopt;(*no pattern or next!!*)
chkerr;
ck_extra_txt;
buffer.curlineno := idx;
qunbound (buffer, err);
chkerr
end
else
begin
ck_extra_txt;
if nld <> 2 then seterr (qbadargno);
(*a false argument here for non-absolute line address on bound*)
qsetbounds (buffer, cmdrange.lbound, cmdrange.hbound, false, err);
chkerr;
buffer.curlineno := qfirst_val (buffer)
end;
(*update the execrange bounds to permit accesses to the new
bounded section. we assume here that execrange is never smaller
than the bounded section, unless bound is prohibited.*)
execrange.lbound := 0;
execrange.hbound := qdollar_val (buffer);
end;(*boundcmd*)
procedure indentcmd;
var way: (left, column, right);
ind, parm, i, pos, col: qstringidx;
source, indstr: qstring;
ln: qlineno;
begin
if checkpunct ('-') then way := left
else if checkpunct ('+') then way := right
else way := column;
if not parsenum (parm, err) then seterr (qnoindno);
ck_extra_txt;
for ln := cmdrange.lbound to cmdrange.hbound do begin
buffer.curlineno := ln;(*set so if error occurs, line is error line*)
source := qgetline (buffer, ln, err); chkerr;
pos := 1; col := 1;(*derive first non-white column*)
while (pos <= length (source)) andif (source[pos] <= ' ') do begin
if source [pos] = ' '
then col := col + 1
else if source[pos] = tab then repeat col := col + 1 until (col mod 8) = 1;
pos := pos + 1
end;
if pos > length (source) then source := ''(*line is blank, truncate*)
else begin
case way of(*check if indentation okay*)
left: if parm >= col then seterr (qtooshort)
else ind := col-parm-1;(*and derive length of indentation*)
right: ind := col + parm - 1;
column: ind := parm
end;
indstr := '';(*build indentation string*)
for i := 1 to (ind div 8) do indstr := indstr || tab;
indstr := indstr || substr (' ', 1, ind mod 8);
if (length (source) - col + 1 + length (indstr)) > upperbound (source)
then seterr (qlnlong);
source := indstr || substr (source, pos);
end;
qmodline (buffer, ln, source, err); chkerr
end
end;
function command (line : cmdline;
var lindex : cmdlineidx;
var cmd : edtcmds) : boolean;
begin
command := true;
if cmd_lookup (line, lindex, token_chars, qcmds, match) then
cmd := edtcmds (match)
else
if cmd_check_punct (line, lindex, '=') then
cmd := eqcmd
else
if cmd_check_punct (line, lindex, '^') then
cmd := uparrow
else
if cmd_check_punct (line, lindex, '_') then
cmd := underbar
else
command := false;
end(*command*);
begin(*begin body of qexecute*)
saved_bounds := false;
1:(*each pass over following code parses and executes one
command in the command line.*)
qldparse (line, lindex, nld, ld, qdata.wildswitch, err);
chkerr;
if not command (line, lindex, cmd) then begin
if (lindex > length (line)) orif (line[lindex] = ';') then begin
if nld = 0 then begin
if findflag then
return;(*but don't do anything under find*)
if buffer.curlineno = qdollar_val (buffer) then
seterr (qbadln);(*nothing to print*)
fno := buffer.curlineno + 1;(*get range of lines to print*)
lno := buffer.curlineno + qdata.linecount;
if lno > qdollar_val (buffer) then
lno := qdollar_val (buffer);(*use remainder if too few*)
qlistlines (buffer, fno, lno, ttyoutput, true, false, qdata.tabprint, err);
chkerr;
buffer.curlineno := lno;
goto 2;(*to exit interpretation loop*)
end
else
cmd := print(*ld <eoln> - assume print and eval ld normally*)
end
else
seterr (qbadcmd);(*no known command name, not eoln*)
end;
if not (cmd in allowed_cmds) then
seterr (qnocmd);
qldeval (buffer, ld, execrange, cmdrange, err);
qlddispose (ld);
chkerr;
chkrng (cmd, nld, cmdrange, findflag, err);
chkerr;
skipblanks;
case cmd of
after, before:
begin
substcmd
end;
readcmd,
append:
begin
if file_parameter (fid, wmod) then ;
ck_extra_txt;
do_append (fid, wmod, cmdrange.lbound)
end;
bound:
begin
boundcmd;
end;(*bound*)
change:
begin
if file_parameter(fid, wmod) then;
ck_extra_txt;
if do_delete (cmdrange) then 
do_append (fid, wmod, cmdrange.lbound - 1)
end;
delete:
begin
ck_extra_txt;
if do_delete (cmdrange) then 
end;
closecmd:
begin
ck_extra_txt;
close_open_file
end;
edit, modify:
writeln(tty,'no longer implemented - use substitute');
list:
begin
have_file := file_parameter(fid, wmod);
if wmod <> ''
then seterr (qbadfile);(*can't list to 940 file*)
ck_extra_txt;
if have_file then begin
qopenfile (list_file, fid, '', qoutput_mode, [qio_confirm], err);
chkerr
end
else list_file:= ttyoutput;
(*listing of lines within section*)
if nld > 0 then begin
qlistlines (buffer, cmdrange.lbound, cmdrange.hbound, list_file, false, true, true, err);
chkerr;
buffer.curlineno := cmdrange.hbound
end
(*list entire file, numbered by section*)
else begin
pushbounds(buffer, err);(*save current bounds*)
idx := 0; fno := 1;
loop(*one pass certain, since no addr error => nonzero line cnt*)
repeat
idx := idx + 1
until (idx > qdollar_val (buffer)) orif
spredmatch (qgetline (buffer, idx, err), buffer.mark, err);
lno := idx - 1;
if lno >= fno then begin(*may have zero length section at start*)
qsetbounds (buffer, fno, lno, true, err);(*address new region*)
chkerr;
if fno <> 1 then begin(*page between parts, not at begin and end*)
page (list_file);
write(list_file, lf) 
end;
qlistlines (buffer, 1, lno-fno+1, list_file, false, true, true, err);
chkerr;
qunbound (buffer, err); chkerr;
end;
exit if idx > qdollar_val (buffer);
fno := idx
end;
popbounds(buffer);
end;
if list_file <> ttyoutput then(*don't close the teletype !!*)
close (list_file)
end;
find:
begin
find_cmds := allowed_cmds - [why, quit, exitcmd, resetcmd, load, save, writecmd,
opencmd, closecmd, setcmd, bound, uparrow];
cnt := 0;
if not spredparse (line, lindex, predi, qdata.wildswitch, err) then err := qnofindpred;
chkerr;
confirm := query ('confirm');
idx := cmdrange.lbound;
while idx <= cmdrange.hbound do
begin
source := qgetline (buffer, idx, err);
chkerr;
if spredmatch (source, predi, err) then
begin
pat := spatgetdefault;
findble := idx;
buffer.curlineno := idx;(*set if matched*)
doit := true;
if confirm then begin
writeln (tty, source);
doit := query ('ok')
end;
if doit then
begin
cnt := cnt + 1;(*count confirmed matches, not executions*)
stridx := lindex;
old_cmdrange := cmdrange;
qexecute (buffer, line, stridx, cmdrange, findble, true, find_cmds, err);
execrange.hbound := execrange.hbound + cmdrange.hbound -
old_cmdrange.hbound;
spatsetdefault (pat);
spatdispose (pat);
buffer.curlineno := findble;
chkerr;
end;
cmdrange.lbound := findble + 1;
idx := cmdrange.lbound;
end
else begin
chkerr;
idx := idx + 1
end
end;
spreddispose (predi);
if confirm 
then write (tty, 'found and confirmed ')
else write (tty, 'found ');
write (tty, numtochar (cnt), ' time' );
if cnt <> 1 then write ( tty, 's' );
writeln ( tty, '.' );
lindex := length (line) + 1;(*find uses rest of line*)
end;(*find*)
gotocmd:
begin
ck_extra_txt;
buffer.curlineno := cmdrange.lbound
end;
insert:
begin
if file_parameter (fid, wmod) then ;
ck_extra_txt;
do_append (fid, wmod, cmdrange.lbound - 1)
end;
indent: indentcmd;
join:
begin
if not spatparse (line, lindex, pat, false, err)
then joinstring := ''
else joinstring := pat.list^.sstring;
chkerr;
ck_extra_txt;
total := cmdrange.hbound - cmdrange.lbound + 1;
if (total <= 2) orif query (numtochar(total) || ' lines, ok') then
begin
qjoinlines (buffer, cmdrange.lbound, cmdrange.hbound, joinstring, err);
chkerr;
with cmdrange do begin
buffer.curlineno := lbound;
execrange.hbound := execrange.hbound - (hbound - lbound);
if ble > hbound
then ble := ble - (hbound - lbound)
else if ble >= lbound then ble := lbound
end
end
end;(*join*)
load:
begin
if not file_parameter (fid, wmod) then seterr (qnofile);
ck_extra_txt;
if discard_changes then begin
close_open_file;
qdelbuf (buffer);
qinitbuf (buffer);
execrange.lbound := 0;
execrange.hbound := 0;
qinit (buffer);
do_append (fid, wmod, 0)
end
end;
move,
transfer,
copy:
begin
if command (line, lindex, optcmd) then begin
if not (optcmd in [append, insert]) then
seterr (qbadopt)
end
else
optcmd := insert;
ld := nil;
qldparse (line, lindex, nld, ld, qdata.wildswitch, err);
if err = qok then begin
if nld <> 1 then begin
if cmd = copy then
err := qnocopyla
else
err := qnomovela
end
else
qldeval (buffer, ld, execrange, tmprange, err)
end;
qlddispose (ld);(*take care to dispose even if errors*)
if err = qla1outrange then
err := qmovoutrange;
chkerr;
ck_extra_txt;
if optcmd = insert then
tmprange.lbound := tmprange.lbound - 1;
cnt := cmdrange.hbound - cmdrange.lbound + 1;
if cmd in [move, transfer] then begin
qmovelines (buffer, cmdrange.lbound, cmdrange.hbound, tmprange.lbound, err);
execrange.hbound := execrange.hbound - cnt;
with cmdrange do begin
if ble > hbound then
ble := ble - cnt
else
if ble >= lbound then
ble := lbound - 1;(*in range moved*)
if tmprange.lbound > hbound then
tmprange.lbound := tmprange.lbound - cnt
end;
end
else
qcopylines (buffer, cmdrange.lbound, cmdrange.hbound, tmprange.lbound, err);
execrange.hbound := execrange.hbound + cnt;
if ble > tmprange.lbound then
ble := ble + cnt;
buffer.curlineno := tmprange.lbound + cnt
end;(*move*)
number,
eqcmd:
begin
ck_extra_txt;
writeln (tty, numtochar (cmdrange.lbound))
end;
opencmd:
begin
if file_parameter (fid, wmod)
then
begin(*something was specified*)
ck_extra_txt;
close_open_file;(*get rid of stragglers*)
wfileconvert (fid,wmod,pdp10id, werr, rflag);(*get proper file id*)
if werr = wbadfile
then seterr (qbadfile);(*note that seterr doesn't return*)
qopenfile (openfile, fid, '', qoutput_mode, [qio_confirm], err);
chkerr;
if werr = wok
then
begin(*a 940 file. use special open routine*)
close (openfile);
openchan := getchannel;
wopen (openchan, werr, woutput, pdp10id);
if werr <> wok(*trouble unlikely, but possible*)
then
begin
freechannel (openchan);(*unwind the mess, if necessary*)
seterr (qbadfile)(*this will rip us out*)
end
else qdata.s940 := true
end
else qdata.s940 := false;
qdata.openfileopen := true(*did it. remember the word*)
end
else seterr (qnofile)
end;(*opencmd*)
outputcmd:
begin
ck_extra_txt;
if qdata.openfileopen
then
begin
if qdata.s940
then
begin(*do the output here for 940 files*)
idx := cmdrange.lbound;
while (err = qok) and (idx <= cmdrange.hbound)
do
begin
source := qgetline (buffer, idx, err);
if err = qok
then
begin
mask(attention);
woutline (openchan, werr, source);(*one line at a time*)
unmask(attention);
if werr <> wok
then err := qwrterr
else idx := idx + 1
end
end
end
else qlistlines (buffer, cmdrange.lbound, cmdrange.hbound, openfile, false, false,
false, err)(*do it elsewhere for pdp-10 files*)
end
else err := qnotopen;
chkerr
end;(*outputcmd*)
print:
begin
ck_extra_txt;
qlistlines (buffer, cmdrange.lbound, cmdrange.hbound, ttyoutput, true, false, qdata.tabprint, err);
chkerr;
buffer.curlineno := cmdrange.hbound
end;(*print*)
quit,
exitcmd:
begin
ck_extra_txt;
err := qquit(*caller must decide whether to discard changes*)
end;
resetcmd:
begin
ck_extra_txt;
if discard_changes then begin
close_open_file;
qdelbuf (buffer);
qinitexec (buffer)
end
end;(*reset*)
setcmd:
begin
if cmd_lookup (line, lindex, token_chars, setparams, match) then begin
setopt := set_params (match);
skipblanks;
case setopt of
del_param:
begin
if not parsenum (idx, err) then seterr (qnoparamval);
ck_extra_txt;
qdata.maxdel := idx
end;(*del_param*)
lcnt_param:
begin
if not parsenum (idx, err) then seterr (qnoparamval);
ck_extra_txt;
qdata.linecount := idx
end;(*lcnt_param*)
mark_param:
begin
if not spredparse (line, lindex, predi, qdata.wildswitch, err) then seterr(qnoparamval);
ck_extra_txt;
spreddispose (buffer.mark);
buffer.mark := predi
end;(*mark_param*)
tab_param:
begin
if not parseonoff(on_opt, err) then seterr(err)
else if on_opt then qdata.tabprint := true
else qdata.tabprint := false
end;(*tab_param*)
wild_param:
begin
if not parseonoff(on_opt, err) then seterr(err)
else if on_opt then qdata.wildswitch := true
else qdata.wildswitch := false
end;(*wild_param*)
case_param:
begin
if not parseonoff(on_opt, err) then seterr(err)
else qsetcase(on_opt)
end
end(*case setopt*)
end
else seterr (qbadparam)
end;(*setcmd*)
split:
begin
if not spatparse (line, lindex, pat, qdata.wildswitch, err)
then if err <> qok
then chkerr
else seterr (qnosplitpat);
skipblanks;
splitop_set := [];
while cmd_lookup (line, lindex, token_chars, splitops, match) do begin
splitop := split_options (match);
splitop_set := splitop_set + [splitop];
if checkpunct (',') then ;
end;
if (lindex <= length (line)) andif (line[lindex] <> ';')
then seterr (qbadopt);
idx := cmdrange.lbound;
total := 0;
repeat(*loop over cmdrange, adjusting hbound for splits*)
if qsplitlines (buffer, idx, pat, splitop_set, cnt, err)
then begin
if ble > idx then ble := ble + cnt;
execrange.hbound := execrange.hbound + cnt;
cmdrange.hbound := cmdrange.hbound + cnt;
idx := idx + cnt;(*this adjusts for splits, does not increment*)
total := total + cnt;
buffer.curlineno := idx;
end;
chkerr;
idx := idx + 1
until idx > cmdrange.hbound;
if (cmdrange.lbound <> (cmdrange.hbound - total)) or (total = 0) or (all_splitop in splitop_set)
then print_times (total)
end;(*qsplit*)
substitute:
begin
substcmd
end;
underbar:(*an ugly, ugly hack for linkwriters*)
begin
ck_extra_txt;
if qlabelfind (buffer, cmdrange.lbound, qfirst_val (buffer), qlabel,
displ, err)
then
begin(*found a label. type line identification*)
buffer.curlineno := cmdrange.lbound;(*set the current line*)
write (tty, ':', qlabel, ':');
if displ <> 0
then writeln (tty, '+', numtochar (displ))
else writeln (tty)
end
else seterr (qnolabel)
end;(*ugly _*)
uparrow:
begin
ck_extra_txt;
if buffer.curlineno <= 1 then
begin
err := qbadlb;
goto 100
end;
buffer.curlineno := buffer.curlineno - 1;
qlistlines (buffer, buffer.curlineno, buffer.curlineno, ttyoutput, true, false, qdata.tabprint, err);
chkerr
end;(*uparrow*)
why:
with qdata do
begin
ck_extra_txt;
errlevel := errlevel + 1;
edterror (ttyoutput, lasterr, errlevel);
end;(*why*)
writecmd,
save:
begin
if not file_parameter (fid, wmod) then begin
if buffer.curfileok
then begin
fid := buffer.curfile;
if buffer.s940
then wmod := '*'(*flag saved 940 file name*)
end
else seterr (qbadfile)
end;
ck_extra_txt;
if nld > 0
then qfilewrite (buffer, fid, wmod, cmdrange.lbound, cmdrange.hbound, confirm_file,
err)
else begin(*assume user wants whole file*)
pushbounds(buffer, err);
qfilewrite (buffer, fid, wmod, 1, qdollar_val (buffer), confirm_file, err);
popbounds(buffer)
end;
chkerr
end(*write/save*)
end;(*case*)
2:
if buffer.curlineno > ble then ble := buffer.curlineno;
if lindex <= length (line) then
if line[lindex] = ';' then
begin
lindex := lindex + 1;
goto 1
end;
100:
if (err <> qok) and (err <> qquit) then begin(*save error code for why*)
qdata.lasterr := err;
qdata.errlevel := 1
end;
end;(*qexecute*)
public procedure edtcl
( var buffer: qbuffer;(*working buffer*)
allowed_cmds: edt_cmd_set);(*only commands allowed*)
var
line: cmdline;
lindex: cmdlineidx;
execrng: ldrange;
ble: qlineno;
err: qerrcode;
lp: qlinep;
emergency_heap_space: ^array[1..160] of integer;
begin
line := '';
new (emergency_heap_space);(*we get this space now so that in the
case of a heap overflow, we can dispose
of it and thus let the user save his
buffer.*)
saved_bounds := false;
loop
begin
write (tty, '*'); break;
line := qread;
execrng.lbound := 0;
execrng.hbound := qdollar_val (buffer);
lindex := 1;
ble := buffer.curlineno;
qexecute (buffer, line, lindex, execrng, ble, false, allowed_cmds, err);
exit if err = qquit;
if err <> qok then begin
clear(tty);
edterror (ttyoutput, err, 1)
end;
exception
storage_overflow: begin
err := qheapfull;
buffer.curfileok := false;
buffer.curfile := '';(*don't let him ruin his file unless he wnats to*)
writeln (tty,'?error -- the heap has overflowed.');
writeln (tty,'save any unwritten changes in a new file.');
writeln (tty,'the next heap overflow will be fatal.');
dispose (emergency_heap_space);(*give the user his last piece of
the pie...*)
if saved_bounds then popbounds(buffer)
end;
attention: begin
clear(tty); clear(ttyoutput);
writeln (tty, '__');
break;
if saved_bounds then popbounds(buffer)
end;
end;
end;
end;
var buffer: edt_buffer;
begin
open(tty, [ascii]); rewrite(ttyoutput);
writeln(tty, 'tym-pascal text editor, version ', version());
writeln(tty);
tty^ := cr;
qinitexec(buffer);
repeat
edtcl(buffer, [minimum(edtcmds) .. maximum(edtcmds)])
until (not buffer.changes) orif query('unwritten changes, ok')
end.
I  3