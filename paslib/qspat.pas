$PAGE spatparse
module qspat
  options special;
(*   +--------------------------------------------------------------+
     |                                                              |
     |                        Q S P A T P                           |
     |                        - - - - - -                           |
     |                                                              |
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED:  4-Aug-77

     PURPOSE: This package contains the QED  routines  SPATPARSE  and
	SPATSETDEFAULT.

     USAGE:
	CALLING SEQUENCES:
		SPATPARSE(LINE : CMDLINE;
			  var IDX : CMDLINEIDX;
			  var PAT : SPATTERN;
			  var ERR : ERRCODE) : boolean;
		SPATSETDEFAULT (PATTERN : SPATTERN);
		SPATMATCH(LINE : QSTRING;
			  PAT : SPATTERN;
			  var POS, LEN : QLINENO;
			  var ERR : QERRCODE) : boolean;


     EFFECTS: The line index will be  advanced  over  leading  blanks
	even if no pattern is matched.
      QSPATP returns the following abnormal error codes:
	QNOCLOSE  -- Missing closing delimiter in string pattern
	QNODEFAULT-- Default pattern has been parsed but not set

     RESPONSIBLE: Software Tools

     CHANGES:
        12/11/78 smr	Made the following characters valid string
			pattern delimiters: '{', '<', '>', '|', '`'.
       12/11/78 smr	Patterns delimited by '#' now recognize tab
			characters as leading whitespace.

       7/30/79 P.Lee    Installed '*' wildcarding using the shortest
		        match algorithm. Also a wildcard switch and
			new data structure for SPATTERN.

       9/16/81 djm      Added 'SET CASE' code borrowed from P. Lee's
                        version of QSPAT.

       9/24/81 djm      Changed references to chr(11b) to tab, removed
                        const declaration of same.

     ---------------------------------------------------------------- *)

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
(* this procedure sets the caseflg flag. *)
begin
  caseflg := x
end;


(****************************************************)

public procedure spatdispose (var pat: spattern);
(* This function disposes of a SPATTERN record. Will NOT dispose
of the default pattern *)

var patp: spatlist;
begin
  if (pat.list <> defpat.list) then		(* don't throw away the default pattern! *)
   with pat do
    while list <> nil do begin
	patp := list;
	list := list^.next;
	dispose (patp)
	end
end; (* spatdispose *)

function copypat (original: spattern): spattern;

(* function to copy a SPATTERN record *)

var orig, copy, pat: spatlist;	(* temporary pointer vars for copy *)

begin
  copypat := original;		(* first, copy main record *)
  orig := original.list;		(* top of list of original pattern *)

  if orig <> nil then begin
    new (copy);				(* create a copy record *)
    copypat.list := copy;	(* chain it into the return record *)
    copy^ := orig^;			(* copy the contents of the default *)
    orig := orig^.next;		(* advance the original ptr *)

    (* now that the copy has been chained onto the return record, copy... *)
    while orig <> nil do begin
      new (pat);
      pat^ := orig^;
      copy^.next := pat;		(* chain it on *)
      pat^.prevtok := copy;
      copy := copy^.next;		(* advance the pointers *)
      orig := orig^.next
      end;
    copypat.lastrec := copy
    end
end (* copypat *);

public procedure spatsetdefault ( pattern : spattern);
(* This function sets the default pattern *)

var tempat : spattern;

begin
  if not nodefault then begin
    tempat := defpat;
    defpat := copypat (pattern);
    spatdispose (tempat)
  end
end; (* spatsetdefault *)

public function spatgetdefault: spattern;
(* This function returns a copy of the default pattern. *)

begin
  spatgetdefault := copypat (defpat)
end; (* spatgetdefault *)

(********************************************************)


(********************************************************)

public function qtokenget(line : cmdline; var idx : cmdlineidx) : boolean;
(* This function returns TRUE and places IDX at the next token in LINE
if another token exists. Else FALSE  *)

begin
  qtokenget := false;
  while (not qtokenget) and (idx <= length(line) ) do
    if (line[idx] = ' ') or (line[idx] = tab) then
      idx := idx + 1
    else
      qtokenget := true
end;
(********************************************************)


(********************************************************)
(*   please see QSPAT.INC for specifications of SPATPARSE and SPATSETDEFAULT*)

public function spatparse ( line : cmdline; var idx : cmdlineidx;
  var pat : spattern; wildswitch: boolean; var err : qerrcode ) : boolean;

  function delete_star(var pattern: qstring): side_type;
  (* this function deletes stars on the far right or left of a pattern
     and returns a side_type indicating where they were deleted from *)

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
  end; (* delete_star *)

var
    ch: char;
    pattern: qstring;
    sidestar: side_type;
    wildcard: boolean;
    temprec, trec: spatlist;
    ind: qstringidx;

begin (* Parsing the string pattern *)
  tmpidx := idx;
  spatparse := false;
  err := qok;
  if qtokenget(line,tmpidx) then
  begin
    ch := line[tmpidx];
    if (ch in legalopen) orif (ch = '{') orif  
       (ch = '|') orif (ch = chr(#o176)) then begin
					(* we have an opening delimiter *)
      idx := tmpidx;
      spatparse := true;
      tmpidx := tmpidx + 1;
      opener := line[idx];
      case opener of
        '[':  closer := ']';
        '{':  closer := chr(#o175);
        '<':  closer := '>';
        others:  closer := opener
      end;
      if tmpidx > length(line) then
	tmpidx := 0
      else
	tmpidx := index( substr(line,tmpidx), closer);
      if tmpidx = 0 then begin
	err := qnoclose; (* leave IDX at opener *)
	spatparse := false
	end
      else begin (* we got a closing delimiter *)
	pattern := substr (line, idx+1, tmpidx-1); (* chop off delimiters *)
	(* flag patterns w/ no "real" characters *)
	if (verify(pattern, ['?','*']) = 0) andif
	  wildswitch andif
	    (length(pattern) <> 0) andif
	      (verify(pattern, ['?']) <> 0) then err := qstupid (* Wow! *)
	else begin  (* to build a pattern record *)

	  (* a SPATTERN pattern is stored internally as a doubly-linked
	     list of pattern strings separated by implicit stars. This
	     parsing is done here to avoid a slow wildcard matcher. 
	     For example, the pattern 'FOO*BAH*FUM' is stored 
	     internally as a doubly linked list like:
	     LIST^->'FOO'<->'BAH'<->'FUM'<-LASTREC^    *)
	  wildcard := (index (pattern, '*') <> 0) and wildswitch;
	  new (temprec);
	  pat.list := temprec;
	  pat.wildcard := wildcard;
	  temprec^.next := nil;
	  temprec^.prevtok := nil;
	  if not wildcard then begin (* a one-element list *)
	    pat.slength := length(substr(line, idx+1, tmpidx-1));
	    temprec^.sstring := substr(line, idx+1, tmpidx-1);
	    temprec^.wildchar := (index(temprec^.sstring, '?') <> 0) and wildswitch
	    end
	  else begin (* add to the linked list *)
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
	      repeat   (* build a doubly-linked list of patterns *)
                       (* separated by implicit stars *)
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
	    end  (* if-then-else *)
	end;  (* if verify then-else *)
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
	end (*case*)
      end;
      idx := idx + tmpidx + 1
    end
  end
end;




$PAGE spatmatch helpers

public function spatmatch (line: qstring; spat: spattern;
  var pos, lenpatss: qstringidx; var err: qerrcode): boolean;


  (*HELPERS*)


  function equal(line: qstring; idx, len:qstringidx; pattern : spatrec)
    : boolean;
  (*
    This function returns TRUE if the substring in LINE starting at 
    position IDX and of length LEN equals the PATTERN. Otherwise
    FALSE *)

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
                                uppercase(line[idx+i])))       ) then
	      i := i + 1
	    else
	      equal := false
    end
  end;


  function column( line: qstring; pat: spatrec; backward: boolean): qstringidx;
  (*
    This function returns the column number of the substring in LINE
    that matches PAT. If BACKWARD is TRUE, then the line is scanned
    from the end of the line to the beginning. Otherwise, from the
    beginning of the line to the end.  
    Returns 0 on no match found. *)

  var
      column_limit: qstringidx;
      inc: -1..1;


    function within_limit: boolean;
      begin
       if backward then within_limit := column >= column_limit
       else within_limit := column <= column_limit
      end;   (* within_limit *)


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

$PAGE wildmatch
  function wildmatch (      (* to find the first occurrance of a wildcard pattern*)
			var pos, len: qstringidx; (* return values, pos also input *)
			line: qstring; (* line to be searched *)
			backward: boolean  (* flag for direction of scan *)
				): boolean; (* true for a good find *)

  (* this function will attempt to match the first or last occurrance of
     a SPATTERN wildcard pattern after/before POS in the line. 
     POS must be input as the starting position of the search. *)

  var
    pattern: spatlist;
    templine: qstring;
    patlen, temppos: qstringidx;

  begin  (* body of wildmatch *)
    wildmatch := false;
    len := 0;

    if backward then begin   (* backward scan *)
	templine := substr (line, 1, pos);
	pos := column (templine, spat.lastrec^, backward);
	if pos <> 0 then temppos := pos - 1
		    else temppos := 0;
	pattern := spat.lastrec^.prevtok;
	templine := substr (templine, 1, temppos);
	if temppos <> 0 then
	  (* match pattern parts starting from the end of the list
	     to the beginning, chopping off the end of the line
	     on each match *)
	  while pattern <> nil do begin
	    pos := column (templine, pattern^, backward);
	  exit if pos = 0;
	    templine := substr (templine, 1, pos-1);
	    pattern := pattern^.prevtok
	  end;
	wildmatch := pos <> 0;
	len := temppos - pos + 1 + length(spat.lastrec^.sstring)
	end
    else begin   (* forward scan *)
	templine := substr (line, pos);
	temppos := column (templine, spat.list^, backward);
	pos := pos + temppos - 1;
	pattern := spat.list^.next;
	patlen := length(spat.list^.sstring);
	if temppos <> 0 then
	  (* match pattern parts starting from the beginning of the list
	     to the end, chopping off the beginning of the line 
	     on each match *)
	  while pattern <> nil do begin
	    templine := substr(templine, temppos + 1);
	    temppos := column (templine, pattern^, backward);
	  exit if (temppos = 0) orif (templine = '');
	    len := len + temppos;
	    if temppos >= patlen then begin (* be sure to skip over last pattern *)
	      patlen := length (pattern^.sstring);
	      pattern := pattern^.next
	      end
	    else patlen := patlen - temppos
	  end;
	wildmatch := temppos <> 0;
	len := len + length(spat.lastrec^.sstring)
	end;
    wildmatch := (pattern = nil) and wildmatch
  end;  (* function wildmatch *)


$PAGE main body of spatmatch
var
    cond: boolean;
    tpos, len, patidx: qstringidx;
    alpha: set of char;
    pat: spattern;
    templine: qstring;
    temppos: qstringidx;
    templen: qstringidx;

      function delimiter (idx: qstringidx): boolean;
      (* this function returns true if line[idx] and line[idx-1]
	are delimiters.                           *)
	begin
	    delimiter := not((uppercase(line[idx]) in alpha) and
	      (uppercase(line[idx-1]) in alpha))
	end;

begin
  spatmatch := false; (* RESULT IN CASE OF ERRORS *)
  err := qok;
  pat := spat;
  alpha := ['A'..'Z', '0'..'9', '_', '%', '$'];
  len := pat.slength;
  if length (line) >= len then
    with pat do
      case stype of

	simple: 
	  if not wildcard then begin
	    if len = 0 then (* use the default pattern *)
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
	  else begin   (* to find the shortest match *)
	    pos := 0;
	    tpos := 0;
	    len := maximum (qstringidx);
	    templine := line;
	    temppos := 1;
	    while wildmatch (temppos, templen, templine, false ) do begin
	      tpos := tpos + temppos;
	      if templen < len then begin  (* save the shortest match *)
		pos := tpos;
		len := templen
		end;
	      templine := substr (templine, temppos + 1);
	    exit if length(templine) < slength;
	      temppos := 1
	    end; (* while *)
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
	  else begin (* left wildcard match *)
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
	  else begin (* right wildcard match *)
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
            pos := pos + 1; (* skip over leading whitespace *)
          if pos > length(line) then pos := 0;
	  if len = 0 then
	    spatmatch := verify(line, [' ', tab]) <> 0
	  else begin
	    patidx := 1;
	    while (patidx <= len ) andif (pos > 0) andif (list^.sstring[patidx] = ' ')
	      do begin (* to make sure that the pattern fits *)
		patidx := patidx + 1;
		pos := pos - 1
	      end;
	    if (pos <> 0) then begin
	      if not wildcard then
		spatmatch := equal(line,pos,len,list^)
	      else begin (* leadstring wildcard *)
		temppos := pos;
		spatmatch := wildmatch (temppos, len, line, false);
		if (sidestars in [leftside, bothsides] ) then
		  len := temppos - pos + len
		else spatmatch := temppos = pos
		end;
		(* if the above fails, then check for '?' wildcards *)
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
		    spch := wildmatch (temppos, len, line, false);
		    if (sidestars in [leftside, bothsides] ) then
			len := temppos - pos + len
		      else spatmatch := temppos = pos
		    end (* if not wildcard *)
		end (* loop *)
	      end (* if (list^.wildchar) *)
	    end (* if (pos <> 0) *)
	  end (* if len = 0 *)
	end;

	token:
	  if len = 0 then begin
	    spatmatch := true;
	    pos := 1
	  end
	  else begin
	    pos := 1;
	    if (not wildcard) or (sidestars = neither) then
	      repeat    (* regular token searching--no critical wildcards *)
		if not wildcard then
		  patidx:= column(substr(line,pos),list^, false)
		else begin (* regular token wildcarding *)
		  temppos := pos;
		  if wildmatch (temppos, len, substr(line,pos), false) then
		    patidx := temppos
		  else patidx := 0
		end;
		spatmatch:= patidx<>0;
		if spatmatch then begin (* check if it is a token *)
		  pos:= patidx+pos-1;
		  if pos = 1 then begin
		    if len <> length(line) then
		      if not delimiter(pos+len) then begin
			pos := pos + 1;
			spatmatch := false
		      end
		  end
		  else begin (* CHECK CLOSER, IF ONE *)
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
		  end (* if pos = 1 *)
		end  (* if spatmatch  *)
		else
		  pos:= length(line)+1 (*to force repeat loop termination*)
	      until (pos > length(line)-len+1) or (spatmatch)

	    else begin  (* special token search--wildcards on outside edges of pattern *)
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
		  (* expand match to delimiters *)
		    temppos := temppos - 1;
		    templen := templen + 1
		  end;
		if (sidestars in [rightside, bothsides] ) then
		  while (not delimiter(temppos+templen)) and
		    ((temppos+templen) <= length(line)) do (* expand *)
		      templen := templen + 1;
		if templen < len then begin
		  len := templen;
		  pos := temppos
		end;
		templine := substr(line, tpos + 1);
		temppos := 1
	      end; (* while wildmatch *)
	      spatmatch := pos <> 0
	    end  (* if not wildcard or sidestars *)
	  end  (* if len = 0 *)
      end; (* CASE *)
  if spatmatch then
    spatsetdefault(pat);
  lenpatss:= len (*return length of pattern*)
end.
    X@"×