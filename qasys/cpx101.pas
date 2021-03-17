(*$E+*)
(*******************************************************************
: **                  PROPRIETARY INFORMATION                     **
: **                                                              **
: **  This  source code listing constitutes the proprietary pro-  **
: **  perty of TYMNET.  The recipient, by receiving this program  **
: **  listing, agrees that neither this listing nor the informa-  **
: **  tion disclosed herein nor any part thereof shall be repro-  **
: **  duced or transferred to other documents or used or dis-     **
: **  closed to others for manufacturing or for any other purpose **
: **  except as specifically authorized in writing by TYMNET.     **
: ******************************************************************
: **                   PROGRAM IDENTIFICATION                     **
: **                                                              **
: **  Version Number     : 01.02         Release Date : 12/15/86  **
: **                                                              **
: **  File Name          : cpx101.pas                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     Cpx101.pas contains the code for the cross reference,    **
: **     'x', pass which falls between pass 6 and pass 7 of the   **
: **     concurrent pascal compiler.  Option 'x' or option 'r'    **
: **     must be set for this code to be executed and to produce  **
: **     a cross listing file.  If option 'x' is set but not      **
: **     option 'r', then a file containing cross reference data  **
: **     is read in and stored in core memory.  This data is then **
: **     sorted and appended to the listing file in an            **
: **     appropriate manner.  If option 'r' is set, the same      **
: **     process occurs as when option 'x' is set, except that    **
: **     it is done in multiple passes over the cross reference   **
: **     file, each time inputing a different set of the data.    **
: **     Option 'r' was implemented in order to reduce the amount **
: **     of core space required to produce a cross reference      **
: **     listing.                                                 **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.02 12/15/86 PJH  1162     ADDED PROPRIETARY BANNER        **
: **                                                              **
: *****************************************************************)

(*$E+*)
program pasx, passx;

const
  max_index = 8512;       (* max number of spelling index *)
  noun_max = 18000;        (* maximum number of nouns allowed *)
  id_piece_length = 9;    (* length of an id_piece (see below) *)
  maxword = 100;          (* length of a table piece *)
  thispass = 0;           (* pass number for the stats *)
  
  proc_type = 1; (* tag for procedure/function difference *)
  
  listoption = 0; (* various compiler options *)
  summaryoption = 1;
  testoption = 2;
  checkoption = 3;
  codeoption = 4;
  numberoption = 5;
  xrefoption = 6;
  bstackoption = 7;
  dumpoption = 9;
  lgxrefoption = 8;

  bigtop = 102;


(*standard spelling indices*)

xundef=0;          xfalse=1;           xtrue=2;            xinteger=3;
xboolean=4;        xchar=5;            xqueue=6;           xabs=7;
xattribute=8;      xchr=9 ;            xcontinue=10;       xconv=11;
xdelay=12;         xempty=13;          xio=14;             xord=15;
xpred=16;          xstop=17;           xrealtime=18;       xsetheap=19;
xsucc=20;          xtrunc=21;          xstart=22;          xwait=23;
xreal=24;

type 
  int_file = file of integer;
  pass_range = 0..7;

  bigline = packed array[1..bigtop] of char;

(* name table types *)

spelling_index = integer;
piece = packed array[0..id_piece_length] of char;
piece_ptr = ^ id_piece;
id_piece = record
             part: piece;
             next: piece_ptr
           end;

(* inter-pass communication types *)

  pointer = ^ integer;
  tableptr = ^table;
  table = record
            nextportion: tableptr;
            contents: array[1..maxword] of integer
          end;
  option = listoption..dumpoption;
  tablesptr = ^tablepart;
  tablepart = record
                proglength,
                codelength,
                stacklength,
                varlength: integer;
                jumptable,
                blocktable,
                stacktable,
                consttable: tableptr
              end;
   passptr = ^passlink;
   passlink =
     record
     options: set of option;
     labels, blocks, constants: integer;
     resetpoint: pointer;
     tables: tablesptr
     end;


(***************)
(* externals *)
(*************)

procedure initpass(p: pass_range);  extern;

procedure nextpass(p: pass_range);  extern;

procedure wrchar(var f: text; c: char); extern;

procedure printeol(var f: text); extern;

(* This procedure reads in the xref file. *) 

procedure passx(inter_pass_ptr: passptr;
                 var list: text;
		 var temp: text;
		 var xrefile: int_file);


(* The format of the file is...

----------------
:     names    :
----------------
:      -1      :
----------------
:     noun     :
:      or      :
:     xref     :
----------------
:      -1      :
----------------
:     disps    :
----------------
:      -1      :
----------------

the subsections are detailed in the module which inputs them.

*)
const
lastpass=4;
(* constants for type kind_type *)
index_const=1;	real_const=2;	string_const=3;	variable=4;
parameter=5;	field=6;	scalar_kind=7;	syscomp_kind=8;
routine_kind=9;	set_kind=10;	program_kind=11;pointer_kind=12;
array_kind=13;	record_kind=14;	with_kind=15;	undef_kind=16;
redef_kind=17;  subr_kind=18;
  
type
  kind_type = index_const..subr_kind;

  string = packed array[1..20] of char;

  noun_index = 0..noun_max;
  line_range = 0..99999;

  kind_entry = packed record
    kind: kind_type; (* the broad class *)
    arg1, arg2: integer; (* some more stuff about it *)
  end;

  noun_ptr = ^noun_entry;
  name_ptr = ^name_entry;
  ref_ptr = ^ref_entry;

  noun_entry = packed record (* we have one of these for each noun *)
    noun_number: integer; (* the number of the beast *)
    nounkind: kind_entry; (* what sort of animal it is *)
    next: noun_ptr; (* to make a list rooted at nounlist below *)
    spix: name_ptr; (* the spelling for this one *)
    disp: integer; (* the displacement for it for Stuart *)
    defline: line_range; (* the line number where this is defined *)
    first_ref: ref_ptr; (* root of a list of references for this noun *)
  end;
  
  name_entry = record (* we have one of these for each unique spelling *)
    name: piece_ptr; (* the spelling of the name *)
    nounlist: noun_ptr; (* the list of nouns with this spelling *)
  end;
    
  ref_entry = packed record (* one of these for each reference of a noun, root at first_ref *)
    line_no: line_range; (* the line of the reference *)
    next: ref_ptr (* the next reference *)
  end;

var
  nametab: array[0..max_index] of name_ptr; (* all the spellings *)
  nountab: array[noun_index] of noun_ptr; (* all the nouns *)
  colpos: integer; (* the next character pos to be printed *)
  warnings: integer; (* set if any unreferenced variables *)
  lastnoun: noun_index; (* the last noun written is the main process *)
  xrefpass: integer;
  maxspelling, minspelling : integer;
  lowbnd, hibnd : char;
  listcomps, wrcomp, keep : boolean;
  firstptr : ^integer;
(*This function returns a table entry *)
function entry(t: tableptr;  index: integer): integer;
var
  j: integer;
  portion: tableptr;
begin
  if index = 0   (* reference to undefined routine *)
  then entry := 0
  else
    begin
      portion := t;  j := index;
      while j > maxword do
        begin
          portion := portion^.nextportion;
          j := j - maxword
        end;
      entry := portion^.contents[j]
    end
end   (* entry *) ;


procedure inputdata;
var i : integer;

(* This function returns the next word in the xref file. *)

function rdxref: integer;
var
  i: integer;
begin
  i := xrefile^;
  get(xrefile);
  rdxref := i (* return it *)
end;

(* This procedure inputs the list of names and spelling indices.  The
format of an element of this list is...

----------------
:spelling index:
----------------
:# of pieces-1 :
----------------
: 5 characters :
:      .       :
:      .       :
:      .       :
----------------

The list of elements is terminated with a -1.  
There are ten characters per piece.
*)
procedure inputnames;

var
  next: integer;		(* the next word in the file *)
  i: integer;			(* a loop index *)

(*This enters the standard names in the name table *)

procedure standard_names;
  
procedure standard_name(p: piece; spelling: spelling_index);
var
  pp: piece_ptr;
begin
  new(pp); (* make a new piece *)
  pp^.next := nil;
  pp^.part := p;
  
  new(nametab[spelling]); (* and put it in the nametable *)
  nametab[spelling]^.name := pp;
  
  new(nountab[spelling]); (* nouns and spell indices are the same for these *)
  with nountab[spelling]^ do begin
    noun_number := spelling;
    nounkind.kind := undef_kind;
    next := nil;
    spix := nametab[spelling];
    disp := -1;
    defline := 0;
    first_ref := nil
  end;

  nametab[spelling]^.nounlist := nountab[spelling];

end;

begin
      standard_name('QUEUE     ',XQUEUE);
      standard_name('REAL      ',XREAL);
      standard_name('BOOLEAN   ',XBOOLEAN);
      standard_name('INTEGER   ',XINTEGER);
      standard_name('CHAR      ',XCHAR);

  if ((xrefpass = 0) or (xrefpass = 1)) then
    begin
      standard_name('FALSE     ',XFALSE);
      standard_name('ABS       ',XABS);
      standard_name('ATTRIBUTE ',XATTRIBUTE);
      standard_name('CHR       ',XCHR);
      standard_name('CONTINUE  ',XCONTINUE);
      standard_name('CONV      ',XCONV);
      standard_name('DELAY     ',XDELAY);
      standard_name('EMPTY     ',XEMPTY);
    end;

  if ((xrefpass = 0) or (xrefpass = 2)) then
      standard_name('IO        ',XIO);

  if ((xrefpass = 0) or (xrefpass = 3)) then
    begin
      standard_name('ORD       ',XORD);
      standard_name('SUCC      ',XSUCC);
      standard_name('REALTIME  ',XREALTIME);
      standard_name('SETHEAP   ',XSETHEAP);
      standard_name('STOP      ',XSTOP);
      standard_name('START     ',XSTART);
      standard_name('PRED      ',XPRED);
    end;

  if((xrefpass = 0) or (xrefpass = 4)) then 
    begin
      standard_name('TRUE      ',XTRUE);
      standard_name('TRUNC     ',XTRUNC);
      standard_name('WAIT      ',XWAIT);
    end;
end;

(*This procedure inputs a single name/spelling index pair *)

procedure inputname;
var
  na : piece_ptr;
  spelling: spelling_index;	(* the spelling index found in the file *)
  num_pieces: integer; 		(* the length (in pieces) of the name BASED ero*)
  ending: piece_ptr;  		(* a pointer to the last piece in the piece train *)
  i: integer; 			(* loop temp, of course *)

(* This allocates and fills in a piece, returning a pointer to it *)
function getpiece: piece_ptr;
type
  convert = record
    case boolean of
      true:  (inp: array[1..2] of integer);
      false: (out: piece)
  end;
      
var
  j: integer; (* a looper *)
  p: piece_ptr; (* a temp *)
  conv: convert; (* to change form *)

begin (* getpiece *)
  new(p); (* allocate a piece *)
  p^.next := nil; (* keep our links clean *)
  for j := 1 to 2 do begin (* read the piece *)
    conv.inp[j] := rdxref;
  end;
  p^.part := conv.out; (* convert to characters *)
  getpiece := p; (* and return it *)
end;

begin 				(* input name *)
  spelling := next;  		(* this is the spelling index for this variable. *)
  num_pieces := rdxref; 	(* remember how many pieces we'll need *)
  new(nametab[spelling]);
  with nametab[spelling]^ do 
    begin
      nounlist := nil;
      if (spelling > maxspelling) then
        maxspelling := spelling;
      if (spelling < minspelling) then
        minspelling := spelling;
      name := getpiece;
      ending := name;
      for i := 1 to num_pieces do (* for each piece... *)
        begin
          ending^.next := getpiece; 	(* get a new piece *)
          ending := ending^.next; 	(* reset the ending piece *)
        end
    end
end;
  
(*input names *)

begin
  maxspelling := 0;
  minspelling := max_index; 
  standard_names; (* put the standard spellings in the table *)
  next := rdxref;(* get the next word *)
  while next <> -1 do begin (* loop until we get the terminator *)
    inputname;(* get a name *)
    next := rdxref;(* and check the terminator again *)
  end;
end;

(*This procedure will input the nouns and cross-ref nodes.

This is a bunch of records which look like...

----------------
:       1      :
----------------
:   noun rec   :
----------------
 
      or

----------------
:      2       :
----------------
:  xref node   :
----------------
*)

procedure inputnounsrefs;

var
  next: integer; (* the next word in the file *)

(*this procedure inputs a single noun record from the file 

A noun record looks like...
 
----------------
: spell index  :
----------------
:  noun index  :
----------------
:  kind info   :
----------------
*)
procedure inputnoun;

var
  noun, spix: integer; (* the numbers read from the file *)
  nounp: noun_ptr; (* points to the new noun we make *)
  delnoun: boolean;

function newnoun(noun: noun_index; spixp: name_ptr): noun_ptr;
var
  result: noun_ptr;

(*This procedure gets the kind dependant information about a noun.
No picture is given here, since this information is only zero, one,
or two other noun numbers, depending on the kind of the noun. *)

procedure getkind(var result: kind_entry);
begin (* get kind *)
  wrcomp := false;
  keep := false;
  with result do begin
    kind := rdxref;
    case kind of
      index_const:  	arg1 := rdxref;
      real_const:  	;
      string_const:	; 
      variable:     	arg1 := rdxref;
      parameter:    	begin 
			  arg1 := rdxref; 
			  arg2 := rdxref 
			end;
      field:        	begin 
			  arg1 := rdxref; 
			  arg2 := rdxref 
			end;
      scalar_kind:  	begin 
			  keep := true; 
			  arg1 := rdxref; 
			end;
      syscomp_kind: 	begin 
			  wrcomp := true; 
			  keep := true; 
			  arg1 := rdxref;  
			  arg2 := rdxref 
			end;
      routine_kind: 	begin 
			  arg1 := rdxref; 
			  wrcomp := true; 
			  arg2 := rdxref; 
			  keep := true; 
			end;
      set_kind:     	begin 
			  keep := true; 
			  arg1 := rdxref; 
			end;
      program_kind: 	begin 
			  wrcomp := true; 
			  keep := true; 
			end;
      pointer_kind: 	begin 
			  keep := true; 
			  arg1 := rdxref; 
			end;
      array_kind:  	begin 
			  keep := true;
			  arg1 := rdxref; 
			  arg2 := rdxref 
			end;
      with_kind:   	;
      record_kind: 	keep := true;
      redef_kind: 	begin 
			  keep := true;
			   arg1 := rdxref 
			end;
      subr_kind:   	begin 
			  arg1 := rdxref; 
			  keep := true;
		 	end;
      others: (* we can just ignore others *)
    end;
  end;
  if (xrefpass > lastpass) then
    keep := wrcomp or keep;
end;

(*new noun *)

begin
  new(result); (* make a new noun, and initialize it *)
  result^.next := nil;
  result^.first_ref := nil;
  result^.noun_number := noun;
  result^.spix := spixp;
  result^.disp := -1;
  result^.defline := rdxref;
  getkind(result^.nounkind);
  if noun <> 0 (* unless it's a funny case, like a constant *)
  then nountab[noun] := result; (* and keep it in the noun table *)
  newnoun := result; (* return it *)
end;
  
(*This procedure inserts a noun in a list of nouns formed through 
noun_entry.next. *)

procedure insertnoun(var list: noun_ptr; noun: noun_ptr);

begin (* insert noun *)
  noun^.next := list; (* make this the new head of the list *)
  list := noun; (* and return it *)
end;

(*input noun *)
begin
  spix := rdxref; (* remember these numbers *)
  delnoun := true;
  noun := rdxref;
  nounp := newnoun(noun, nametab[spix]); (* get a new noun node *)
  lastnoun := noun; (* the last noun written is the main process *)

  if (spix <> 0) then
    begin
      if (xrefpass = 0) then
	insertnoun(nametab[spix]^.nounlist, nounp) (* insert this noun on the list *)
      else
	 begin
	    if (nametab[spix] <> nil) then
	       if (nametab[spix]^.name <> nil) then
    	  	  if (((nametab[spix]^.name^.part[0] >= lowbnd) and 
	               (nametab[spix]^.name^.part[0] < hibnd)) or keep) then
	             begin
			delnoun := false;
		  	insertnoun(nametab[spix]^.nounlist, nounp);
		 	(* insert this noun on the list *)
		     end;
	    if ((xrefpass <> 0) and delnoun) then
	       begin
	          nountab[noun] := NIL;
  	          dispose(nounp);
	       end;
	 end;
    end
end;
(**)
procedure inputref;
(* This will input a line number/noun pair and allocate a ref node 
the format for this is...

----------------
: line number  :
----------------
:  noun index  :
----------------
*)

var
  noun, line: integer;
  refnode: ref_ptr;

begin (* input ref *)
  line := rdxref;
  noun := rdxref;
  if nountab[noun] <> nil
  then begin
    new(refnode); (* get an xref node *)
    with refnode^ do begin
      next:=nil; (* set up the ref_rec *)
  ine_no:=line
    end;
    refnode^.next:=nountab[noun]^.first_ref; (* and put it on the list *)
    nountab[noun]^.first_ref:=refnode
  end
end;

(*input nouns refs *)

begin
  next := rdxref; (* get the type of the next record *)
  lastnoun := 0;
  while next <> -1 do begin
    case next of (* do what should be done *)
      1: inputnoun;
      2: inputref
    end;
    next := rdxref;
  end
end;

(*This will input the displacement information from the file 

This is a bunch of file records each of which is...

----------------
:   noun index :
----------------
: displacement :
----------------

For routines, the displacement is the sum of the lengths of the variables
and the parameters, hence the variables usable in some sence...

*)
procedure inputdisps;
var
  next: integer;

procedure inputdisp;
(* this will input the displacement info for one noun. *)
var
 toss: integer; (* just trash... *)
begin
  if nountab[next] <> nil then (* only if we care... *)
    nountab[next]^.disp := rdxref (* get the unit *)
  else 
      toss := rdxref
end;

begin (* input displacement *)
  next := rdxref;
  while next <> -1 do begin
    inputdisp;
    next := rdxref
  end
end;


(*Main input routine *)

begin (* input data *)
  for i := 0 to max_index do
    nametab[i] := nil;
  for i := 0 to noun_max do
    nountab[i] := nil;
  reset(xrefile);
  inputnames;(* get the spellings *)
  inputnounsrefs;(* and the noun info *)
  inputdisps; (* and the displacement info *)
end;

procedure printtables;
var
  tp : name_ptr;
  np : noun_ptr;
  rp : ref_ptr;
  pp : piece_ptr;
  i  : integer;
  j  : integer;

begin
  for i := 0 to maxspelling do
    begin
      writeln(tty, 'i', i);
      if (nametab[i] <> NIL) then
	begin
	  tp := nametab[i];
	  pp := tp^.name;
	  while (pp <> NIL) do
	    begin
	      for j := 0 to id_piece_length do
		write(tty, pp^.part[j]:1);
	      pp := pp^.next;
	    end;
	  writeln(tty);
	  np := tp^.nounlist;
	  while (np <> NIL) do
	    begin
	      writeln(tty, 'noun_number', np^.noun_number, 'disp', np^.disp);
	      write(tty, 'nounkind', np^.nounkind.kind);
	      writeln(tty, np^.nounkind.arg1, np^.nounkind.arg2);
	      writeln(tty, 'defline', np^.defline);
	      rp := np^.first_ref;
	      while(rp <> NIL) do
		begin
		  writeln(tty, 'lineno', rp^.line_no);
		  rp := rp^.next;
		end;
	      np := np^.next;
	    end;
	end;
    end;
end;

(*This is the sort routine.  It's just a foolish quicksort *)

procedure sortnames;
var
  top, bottom: integer; (* used to get rid of nils in the namelist *)
  
(* This function tells if name1 is strictly less than name2 *)
function less_than(name1, name2: name_ptr): boolean;
(* lexicographic comparison of names *)
var
  result: boolean;
  p1, p2: piece_ptr;

begin
  if (name1 <> nil) and (name2 <> nil)
  then begin
    p1 := name1^.name;
    p2 := name2^.name;
    while
      (p1^.part = p2^.part) and
      (p1^.next <> nil)        and
      (p2^.next <> nil)
    do
      begin
        p1 := p1^.next;
        p2 := p2^.next
      end;
    result := (p1^.part < p2^.part) or
              ((p1^.part = p2^.part) and
               (p1^.next = nil) and
               (p2^.next <> nil));
  end
  else result := (name1 <> nil);
  less_than := result
end (* less_than *) ;
(*The quicksort itself *)

procedure quicksort(l, r: integer);
var
  i, j: integer; (* temps to keep the parameters intact *)
  temp: name_ptr; (* for doing the swap *)
  comparand: name_ptr; (* the element of choice *)
begin
  i := l;  j := r; (* save the parameters *)
  comparand := nametab[(l+r) div 2]; (* choose the middle element *)
  repeat (* move the pointers until we find a misplaced element *)
    while (less_than(nametab[i], comparand)) do
      i := succ(i);
    while (less_than(comparand, nametab[j])) do
      j := pred(j);
    if i <= j (* need to swap? *)
    then
      begin (* do a swap *)
        temp := nametab[i];
        nametab[i] := nametab[j];
        nametab[j] := temp;
        i := succ(i);  j := pred(j)
      end
  until i > j;
  if l < j then quicksort(l, j);
  if i < r then quicksort(i, r)
end (* quicksort *) ;


(*^L
Remove any intermixed nil entries to the end of the table *)

procedure condense;
var
  lower, upper : integer;
  nametemp : name_ptr;

begin
  lower := 0;
  upper := maxspelling;
  while (lower < upper) do
    begin
      while (nametab[lower] <> NIL) do
	lower := lower + 1;
      while (nametab[upper] = NIL) do
	upper := upper - 1;
      if (lower < upper) then
	begin
	  nametemp := nametab[upper];
  	  nametab[upper] := NIL;
      	  nametab[lower] := nametemp;
	end;
    end;
  maxspelling := upper;
  minspelling := 0
end;

(*^L

The main sort routine *)

begin (* sort names *)
  if (xrefpass <> 0) then
    begin
      condense;
      top := maxspelling;
      bottom := minspelling;
    end
  else
    begin
      top := max_index; (* don't bother sorting all the nils *)
      while nametab[top] = nil do
        top := pred(top);
      bottom := xundef;
      while nametab[bottom] = nil do
        bottom := succ(bottom);
    end;
  quicksort(bottom, top) (* sort 'em *)
end;

(* copy data from the first xref pass to the listing file *)
procedure copyfile;
var
  ch : char;

begin
  writeln(temp, '?');
  reset(temp);
  writeln(list);
  read(temp, ch);
  while(eoln(temp))do
    read(temp, ch);
  while (ch <> '?') do
    begin
      while(not eoln(temp)) do
        begin
          write(list, ch:1);
          read(temp, ch);
	end;
      if (ch <> '?') then
	begin
	  writeln(list, ch);
          read(temp, ch);
        end;
    end;
end;


(* prints out a header line *)
procedure wrtext(l: bigline);
var
  i: integer;
begin
  for i := 1 to bigtop do
    wrchar(list, l[i])
end;

(*This prints out a pretty header *)
procedure writeheader;
begin (* write header *)
  wrchar(list, ff); (* start off on a new page *)
  wrtext('defined  variable     type                                                           where it is (hex)');
  printeol(list);
  wrtext('   at      name            used at                                                                    ');
end;

(* This prints a cr/lf to the listing file *)
procedure printcrlf(var f : text);
begin
  colpos := 1; (* back to the left margin *)
  printeol(f)
end;

(* This prints out a number *)
procedure printnum(i: integer; var f : text);
begin
  colpos := colpos + 6;
  write(f, i:6);
end;

(* This prints out a number in hex *)
procedure printhex(i: integer; var f : text);
var
  j: integer; (* assigned to to keep our parameter clean *)
  digit: integer; (* loop index *)
  out: packed array[1..6] of char; (* the output *)
  digits: packed array[0..15] of char; (* hex digits *)

begin
  j := i; (* save the parameter *)
  if j < 0
  then begin
    write(f, '-');
    colpos := colpos + 1;
    j := -j
  end;
  digits := '0123456789abcdef'; (* set up hex digits *)
  for digit := 6 downto 1 do begin
    out[digit] := digits[j mod 16];
    j := j div 16
  end;
  digit := 1; (* now we'll get rid of zeros *)
  while (out[digit] = '0') and (digit<6) do begin (* we don't want to zap the last digit *)
    out[digit] := ' ';
    digit := succ(digit)
  end;
  write(f, out); (* write it *)
  colpos := colpos + 6
end;

(* This prints out some stuff *)
procedure wrword(w: string; var f : text);
var
  i: integer;
begin 
  if (not listcomps) then
    begin
      i := 1; (* print out only as much as told *)
      while (i<=20) do begin
        if w[i] <> '`'
         then begin
          colpos := colpos+1;
          wrchar(f, w[i]);
          i := succ(i)
       end
        else i := succ(20)
      end;
    end;    
end;

(*This prints out a name *)

procedure printname(name: name_ptr; var f : text);

var
  this_piece: piece_ptr; (* to link down the list *)
  i: integer; (* to loop down the name *)

begin (* print name *)
  this_piece := name^.name;
  while this_piece <> nil do begin
    i := 0;
    while (i<=id_piece_length)  do begin
      if this_piece^.part[i] = ' ' (* at the end? *)
      then i := succ(id_piece_length) (* force the loop to end *)
      else begin
        colpos := colpos + 1;
        wrchar(f, this_piece^.part[i]);
        i := succ(i)
      end
    end;
    this_piece := this_piece^.next;
  end
end;

  
procedure tabto(col: integer; var f : text);
(* this tries to position the "print head" so the next character will be
   at the given position. *)
begin (* tab to *)
  while colpos<col do begin
    colpos := colpos + 1;
    wrchar(f, ' ')
  end;
end;
  
(*this prints out the kind of a noun *)

procedure printkind(n: noun_entry; var f : text);

procedure printnounname(n: noun_index; var f : text);
(* This procedure prints the name of a noun.  *)

begin (* print noun name *)
  if ((nountab[n] <> nil) and (listcomps)) then
    begin
      if(nountab[n]^.spix = nil) then
        printkind(nountab[n]^, f) (* then print the sort of thing it is *)
    end;
  if ((nountab[n] = nil) and (not listcomps)) then
    wrword('???`                ', f)
  else if (not listcomps) then
      if (nountab[n]^.spix = nil) then
        printkind(nountab[n]^, f) (* then print the sort of thing it is *)
      else
        printname(nountab[n]^.spix, f); (* o'wise print the name *)
end;

(* Print kind *)

begin
  with n, n.nounkind do begin
    case kind of
      index_const:begin
		    if nountab[arg1] <> nil
		    then begin
                      wrword('constant of type `  ', f);
		      printnounname(arg1, f)
		    end
		    else wrword('enumeration constant', f);
		  end;
      real_const:   wrword('real constant`	', f);
      string_const: wrword('string constant`	', f);
      variable:   begin
		    if nountab[arg1] <> nil
		    then begin
		      wrword('variable of type `  ', f);
		      printnounname(arg1, f)
		    end
		    else begin
		      wrword('variable`           ', f); 
		      wrword(' of nameless type`  ', f)
		    end
		  end;
      parameter:  begin
                    wrword('parameter of type `	', f);
		    printnounname(arg1, f);
		    wrword(' in `               ', f);
		    printnounname(arg2, f)
		  end;
      field:      begin
		    wrword('field of type `	', f);
		    printnounname(arg1, f);
		    wrword(' in `               ', f);
		    printnounname(arg2, f)
		  end;
      scalar_kind: begin
                    wrword('enumeration type`	', f);
		  end;
      syscomp_kind:begin
	             case arg1 of
		       1: wrword('class`              ', f);
		       2: wrword('monitor`            ', f);
                       3: wrword('process`            ', f);
		     end;
		   end;
      routine_kind: begin
		  if arg1 = proc_type
		  then
		    wrword('procedure`		', f)
		  else begin
		    wrword('function of type `  ', f);
		    printnounname(arg1, f)
		  end;
		  (* since we fudge up arg2, it's only right the first time we print it *)
		  if nountab[arg2]^.nounkind.kind = syscomp_kind
		  then begin
		    wrword(' in `               ', f);
		    if nountab[arg2]^.spix = nil
		    then wrword('main `              ', f);
		    printnounname(arg2, f); (* very clever for main process *)
		    (* now we link onto a list from our parent process *)
		    nountab[noun_number]^.nounkind.arg2 := nountab[arg2]^.nounkind.arg2;
		    nountab[arg2]^.nounkind.arg2 := noun_number;
		  end
		end;
      set_kind:   begin
		    wrword('set of `            ', f);
		    printnounname(arg1, f)
		  end;
      program_kind: wrword('program`		', f);
      pointer_kind: begin
                    wrword('pointer to `	', f);
		    printnounname(arg1, f);
		    end;
      array_kind: begin 
                    wrword('array[`		', f);
		    printnounname(arg1, f);
		    while nountab[arg2]^.nounkind.kind = array_kind do begin
		      wrword(', `                 ', f);
		      arg1 := nountab[arg2]^.nounkind.arg1;
		      arg2 := nountab[arg2]^.nounkind.arg2;
		      printnounname(arg1, f);
		    end;
		    wrword('] of type `     	', f);
		    printnounname(arg2, f)
		  end;
      with_kind:    wrword('with`		', f);
      record_kind:  wrword('record`		', f);
      redef_kind: begin
	            wrword('type `              ', f);
		    printnounname(arg1, f)
		  end;
      subr_kind:  begin
	            wrword('subrange of `	', f);
		    printnounname(arg1, f)
		  end;
      others:       wrword('no idea, sir`       ', f)
    end
  end
end;

(*This prints out all the references for a noun *)

procedure printrefs(var n: noun_entry; var f : text);
var
  l: ref_ptr; (* the start of the list, usualy... *)
  revlist, temp: ref_ptr; (* used to reverse the list *)
  count: integer; (* used to keep the list nicely formatted *)

begin (* print refs *)
  l := n.first_ref;
  revlist := nil; (* we'll start out by reversing the list... *)
  while l<>nil do begin (* loop until we've eaten the list *)
    temp := l; (* get the head of the old list *)
    l := l^.next; (* advance the old list *)
    temp^.next := revlist; (* make the head of the old list point to the new *)
    revlist := temp; (* and it is now the head, as we said *)
  end;
  l := revlist; (* and now it's at the other end! *)
  n.first_ref := revlist;
  if l = nil then
    warnings := succ(warnings);

  (* and now we print it out *)
  temp := l; (* ready... *)
  count := 0; (* set... *)
  while temp <> nil do begin
    if count mod 11 = 0 (* time to space for a new line? *)
    then begin
      printcrlf(f);
      tabto(24, f);
      count := 0
    end;
    count := count + 1;
    printnum(temp^.line_no, f);
    temp := temp^.next;
  end;
end;

(* This prints out all the poop on one noun. *)
procedure outputnoun(name: piece_ptr; noun: noun_ptr; var f : text);

begin 
  if noun^.defline <> 0 (* if it's 0, it's predefined *)
  then begin
    if (listcomps) then
      begin
        printkind(noun^, f);
      end
    else 
      begin
    printcrlf(f);
    printnum(noun^.defline, f); (* print the line it was defined at *)
    wrword('     `              ', f);
    printname(noun^.spix, f); (* print the name *)
    if testoption in inter_pass_ptr^.options
    then printnum(noun^.noun_number, f);
    tabto(21, f);
    wrword('  `                 ', f);
    printkind(noun^, f); (* and the all important type *)
    if (noun^.disp <> -1) (* does it have a meaningful disp? *)
      and not (noun^.nounkind.kind in [routine_kind, syscomp_kind])
    then begin
      tabto(95, f);
      printhex(noun^.disp, f) (* AND the displacement for this one *)
    end;
    printrefs(noun^, f); (* mention everyplace it's mentioned *)
   end;
  end;
end;

(*This prints out the vitals for one spelling entry *)

procedure outputdatum(var name: name_entry; var f : text);

var
  this_noun: noun_ptr; (* for looping through the nouns of this spelling *)
  revlist: noun_ptr; (* for reversing the list of nouns *)
  
(*output datum *)

begin 
  with name do begin
    if name <> nil (* unless this is an empty entry *)
    then begin
      revlist := nil; (* first we'll put the list in the right order *)
      while nounlist <> nil do begin
	this_noun := nounlist;
	nounlist := nounlist^.next;
	this_noun^.next := revlist;
	revlist := this_noun
      end;
      nounlist := revlist; (* look in printrefs if you want more comments *)
      this_noun := nounlist;
      while this_noun <> nil do begin (* print out an entry for each noun *)
	outputnoun(name, this_noun, f);
	this_noun := this_noun^.next
      end
    end
  end
end;

(*This procedure prints out the data gathered so well by inputdata. *)
procedure outputdata(var f : text);
var
  i: integer;

begin
  if ((xrefpass = 0) or (xrefpass = 1)) then
    writeheader;(* start off with a pretty header *)
  for i := xundef to max_index do begin (* for each spelling *)
    if nametab[i] <> nil then (* print out only real entries *)
      if ((xrefpass = 0) or (xrefpass > lastpass) or
	 ((nametab[i]^.name^.part[0] >= lowbnd) and
	  (nametab[i]^.name^.part[0] < hibnd))) then
	 outputdatum(nametab[i]^, f); (* print it *)
  end;
  if ((xrefpass = 0) or (xrefpass = lastpass)) then
    printeol(f)
end;

(*This procedure prints out a list of warning messages *)
procedure printwarns(var f : text);
var
  i: integer;
  curnoun: noun_ptr;
begin (* print warns *)
  if warnings > 0 then (* do we gotta? *)
    begin
      if ((xrefpass = lastpass) or (xrefpass = 0)) then
	begin
	  wrchar(list, ff); (* a new page *)
          if warnings = 1 then
	    wrtext('Warning: this was defined but never referred to                                                       ')
          else 
	    wrtext('Warning: the following were defined but never referred to                                             ');
          if (xrefpass = lastpass) then
 	    copyfile;
        end;
    for i := xundef to max_index do  (* for each spelling *)
      begin
        if nametab[i] <> nil then
          with nametab[i]^ do
	    begin
              if name <> nil then (* if it's a real entry *)
	        if ((xrefpass = 0) or
	            ((name^.part[0] >= lowbnd) and
	             (name^.part[0] < hibnd))  ) then
                  begin
                    curnoun := nounlist;
                    while curnoun <> nil do 
	              begin
	                if curnoun^.first_ref = nil then (* never talked about? *)
      	    	            outputnoun(name, curnoun, f);
	    	        curnoun := curnoun^.next
	              end;
                  end;
	    end;
      end;
    end;
end;

(* This prints out a list of system components.  This cannot be called
before outputdata is called, since outputdata sets up the lists of procedures. *)

procedure printcomps(var f : text);
var
  i: integer;
  curnoun: noun_ptr;


procedure printfuncts(ni: noun_index; var f : text);
var
  n : integer;
  copy, list, temp: noun_index; (* we'll reverse the list using these *)

begin
  list := xundef;
  copy := ni; (* save our parameters. *)
  while nountab[copy]^.nounkind.kind = routine_kind do begin
    temp := copy; (* this is done in printrefs, too.  look there for comments *)
    copy := nountab[copy]^.nounkind.arg2; (* about reversing lists *)
    nountab[temp]^.nounkind.arg2 := list;
    list := temp
  end;
  while list <> xundef do begin
    with nountab[list]^.nounkind do begin
      printcrlf(f);
      tabto(3, f);
      if nountab[list]^.spix <> nil
      then begin
	printname(nountab[list]^.spix, f);
	if arg1 = proc_type
	then wrword(' procedure`         ', f)
	else wrword(' function`          ', f);
      end
      else wrword('initial statement`  ', f);
      tabto(45, f);
      wrword(' stack = `          ', f);
      with inter_pass_ptr^.tables^ do 
        n := entry(stacktable, nountab[list]^.disp);
      printnum(n, f);
      list := arg2;
    end;
  end;
end;

procedure printcomp(n: noun_entry; var f : text);
begin (* print comp *)
  printcrlf(f);
  printcrlf(f);
  if n.spix = nil (* is it the main process? *)
  then wrword('main`               ', f)
  else printname(n.spix, f);
  wrword(' `                  ', f);
  with n, n.nounkind do begin
    case arg1 of
       1: wrword('class`              ', f);
       2: wrword('monitor`            ', f);
       3: wrword('process`            ', f);
     end;
     tabto(45, f);
          wrword(' perm variables = ` ', f);
     printnum(disp, f);
   end;
   printfuncts(n.nounkind.arg2, f);
end;

begin (* print comps *)
  writeln(list);
  wrchar(list, ff); (* new page *)
  wrword('System components`  ', f);
  printcomp(nountab[lastnoun]^, f); (* bring out the main process *)
  for i := xundef to max_index do begin (* for each spelling *)
    if nametab[i] <> nil
    then with nametab[i]^ do
      if name <> nil (* if it's a real entry *)
      then begin
	curnoun := nounlist;
	while curnoun <> nil do begin
	  if curnoun^.nounkind.kind = syscomp_kind (* is it a component? *)
	  then printcomp(curnoun^, f);
	  curnoun := curnoun^.next
	end;
      end;
  end;
end;

(* This is the main procedure for the xref pass. *)

begin
  if xrefoption in inter_pass_ptr^.options then
    begin
      initpass(thispass); (* tell stats we're here *)
      warnings := 0; (* no warnings yet *)
      if lgxrefoption in inter_pass_ptr^.options then
	begin
	  rewrite(temp);
          for xrefpass := 1 to lastpass + 1 do
            begin
	      new(firstptr);
	      listcomps := false;
	      case xrefpass of
                1 : begin
		      lowbnd := 'A';
		      hibnd  := 'G';
		      write(tty, '.':1);
		      break(tty);
		    end;

		2 : begin
		      lowbnd := hibnd;
		      hibnd := 'M';
		      write(tty, '.':1);
		      break(tty);
		    end;

		3 : begin
		      lowbnd := hibnd;
		      hibnd := 'S';
		      write(tty, '.':1);
		      break(tty);
		    end;

		4 : begin
		      lowbnd := hibnd;
		      hibnd := '{';
		      write(tty, '.':1);
		      break(tty);
		    end;

	   others : begin
		      lowbnd := '{';
		      hibnd := '{';
		      listcomps := true;
		    end;

	        end; (* case *)
              inputdata; (* input the data *)
              sortnames; (* process the data *)
	      outputdata(list);	      
	      if (xrefpass < lastpass) then
 		begin
		  printwarns(temp);
		  dispose(firstptr);
		end;
	      if (xrefpass = lastpass) then
		begin
		  printwarns(list);
		  dispose(firstptr);
		end;
	    end; (* for *)
	  listcomps := false;
	  printcomps(list);
	  dispose(firstptr);
	end (* if *)
      else
        begin
	  xrefpass := 0;
	  reset(xrefile);
	  inputdata;
	  sortnames;
	  outputdata(list);
          printwarns(list); (* flag unrefed variables *)
          printcomps(list); (* print out a list of program components *)
	end; (* end else *)
    end;  (* end if *)
  nextpass(thispass) (* we gone *)
end;
(* Dummy block for external compilation. *)

begin
end.
    bQy0