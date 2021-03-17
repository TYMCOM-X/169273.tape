MODULE qcnvrtpas;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc

(* Routines to handle link, sys, sub, doc, and 6 bit files *)
(* 11/02/81 - modified by djm to add code to confirm default modifier
              value in WFILECONVERT *)

TYPE TYPECODE=(FNERROR,FNOK,FNDEVICE);		(* for 940 syntax routine *)


EXTERNAL PROCEDURE FN940TO10( FILE_ID;		(* 940 style name *)
			      VAR FILE_ID;	(* pdp 10 style name *)
			      VAR TYPECODE);	(* fnerror,fnok,fndevice *)

$PAGE
$PAGE
PUBLIC PROCEDURE WFILECONVERT(FILE_NAME: FILE_ID;   (* name to convert *)
			FILE_MODIFIER: WMODIFIER;   (* type of conversion L S D B 6 7 *)
			VAR PDP_NAME: FILE_ID;	(* resultant name *)
			VAR WCODE: WCODES;	(* error codes  *)
			VAR DRCONVERT: BOOLEAN);    (* further convert on name req. *)

TYPE INTEGER= 0..99999;
     WL_OR_D= (LNUMBER,DNUMBER);		(*link number or documentation*)

VAR CODE: TYPECODE;
    WERROR: BOOLEAN;
    WNUMBER: INTEGER;
    TMODIFIER: WMODIFIER;

$PAGE  determine the user
FUNCTION  WUSER(WNUMBER: INTEGER;		(* link number to determine user *)
		WNUMBER_TYPE: WL_OR_D		(*lnumber or dnumber given*)
		): WMODIFIER;

VAR ZERO: 0..9999;
    DIGIT: 0..7;
    ACCT,N: 0..99999;

BEGIN
  (* the following computes the acct number for link names and
     link documentation names based on files per account and
     the number of accounts.

     links 1-9    user 42164
     links 10-19  user 42165
	 |               |
     links 90-99  user 42175  in other words add to the base acct the tens digit

     links 100-109   user 42217
     links 200-209   user 42252  add the hundreds digit*27 to the base acct

     the above pattern repeats for links above 999

     documentation files are arranged 30 per file for 26 user names
     and then repeats through the user names for the next 780 files *)

  IF WNUMBER_TYPE=LNUMBER THEN ACCT:=17524+((WNUMBER DIV 100)MOD 10)*27
					+((WNUMBER DIV 10) MOD 10)

  ELSE ACCT:=( ( ( WNUMBER - 1 ) MOD 780 ) DIV 30 ) + 9046;
  WUSER:='';					(* start string as null string *)
  ZERO:=ORD( '0' );				(* need internal rep of zero *)
  WHILE ACCT<>0 DO
    BEGIN
      N:= ACCT DIV 8;				(* determine octal user number *)
      DIGIT:= ACCT - N*8;			(* this gives digits from right to left *)
      WUSER:= CHR(ZERO + DIGIT) || WUSER;
      ACCT:= N
    END;
END;						(* wuser *)


$PAGE  validate the number and generate the string || .940
PROCEDURE WVALIDATE_NUMBER(FILE_NAME: FILE_ID;	(* contains only the number *)
			VAR WNUMBER: INTEGER;	(* number to be returned *)
			VAR WNEW_NAME: FILE_ID;	(* number with .940 added *)
			VAR WNUM_ERROR: BOOLEAN);   (* error if not num or bad *)

VAR I,N: INTEGER;
    J: -9999..9999;

BEGIN
  WNUM_ERROR:= FALSE;
  N:=LENGTH(FILE_NAME);
  IF N>4 THEN WNUM_ERROR:=TRUE
  ELSE BEGIN
    WNUMBER:= 0;
    FOR I:= 1 TO N DO
    BEGIN
      WNEW_NAME:=WNEW_NAME||FILE_NAME[I];
      J:= ORD(FILE_NAME[I])-ORD('0');
      IF(J<0) OR (J>9) THEN WNUM_ERROR:=TRUE
      ELSE WNUMBER:=WNUMBER*10+J

    END;
    WNEW_NAME:=WNEW_NAME||'.940';
  END
END;						(* wvalidate *)

$PAGE wfileconvert code section
BEGIN						(* this is the start of wfileconvert *)
  DRCONVERT:= FALSE;
  WCODE:= WOK;
  PDP_NAME:='';
  IF FILE_MODIFIER='' THEN
    BEGIN
      IF (LENGTH(FILE_NAME)>4) ANDIF
         (SUBSTR(FILE_NAME,LENGTH(FILE_NAME)-3,4)='.940') ANDIF
         (NOT QUERY ('7 bit file, OK')) THEN
        BEGIN
          WCODE:=WBADNAME;
          RETURN;
        END;
      WCODE:=WTENFILE;
      PDP_NAME:=FILE_NAME
    END
  ELSE BEGIN
  IF FILE_MODIFIER[1]='6' THEN
    BEGIN
      FN940TO10(FILE_NAME,PDP_NAME,CODE);
      IF CODE=FNOK THEN WCODE:=WOK
      ELSE WCODE:=WBADNAME
    END
  ELSE IF FILE_MODIFIER[1]='7' THEN 
    BEGIN
      WCODE:=WTENFILE;
      PDP_NAME:=FILE_NAME
    END
  ELSE BEGIN
    WVALIDATE_NUMBER(FILE_NAME,WNUMBER,PDP_NAME,WERROR);
    IF WERROR THEN WCODE:=WBADNAME
    ELSE
    BEGIN
      TMODIFIER:=UPPERCASE (FILE_MODIFIER);
      CASE TMODIFIER[1] OF
	'L': PDP_NAME:= PDP_NAME||'[50122,'||
				WUSER(WNUMBER,LNUMBER)||']<007>';
	'S': PDP_NAME:=PDP_NAME||'[50122,34321]<007>';
	'B': PDP_NAME:=PDP_NAME||'[50122,34124]<007>';
	'D': BEGIN
		   PDP_NAME:='';
		   CASE LENGTH(TMODIFIER) OF
		     1:  PDP_NAME:=FILE_NAME;
		     2:  BEGIN
			   IF ((TMODIFIER[2]<>'F') AND (TMODIFIER[2]<>'G') AND
			    (TMODIFIER[2]<>'R')) THEN WCODE:=WBADNAME
			   ELSE PDP_NAME:=FILE_NAME||TMODIFIER[2]
			 END;
		     3:  BEGIN
			  IF ((TMODIFIER[2]='F') OR (TMODIFIER[2]='G'))
			   AND (TMODIFIER[3]='R') THEN 
			   PDP_NAME:=FILE_NAME||SUBSTR(TMODIFIER,2)
			   ELSE WCODE:=WBADNAME
			  END;
		     OTHERS:  WCODE:=WBADNAME
		   END;
		   IF WCODE<>WBADNAME THEN
		     BEGIN
		       PDP_NAME:=PDP_NAME||'.940[50127,'||
				WUSER(WNUMBER,DNUMBER)||']<007>';
		       IF SEARCH(TMODIFIER,['R'])=0 THEN DRCONVERT:= TRUE;
		     END;
		 END;
	OTHERS: WCODE:=WBADNAME
      END
    END;
    END;
  END
END.						(* wfileconvert *)
$PAGE spatparse
module qspat
  options special;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc

(*   +--------------------------------------------------------------+
     |                                                              |
     |                        Q S P A T P                           |
     |                        - - - - - -                           |
     |                                                              |
     +--------------------------------------------------------------+


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
				): boolean; (* true for a find *)

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
		    spatmatch := wildmatch (temppos, len, line, false);
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
(*   +--------------------------------------------------------------+
     I                                                              I
     I                        Q S P R E D                           I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+

     STARTED:  4-Aug-77

     PURPOSE: This package  contains  the  QED  routines  SPREDPARSE,
	SPREDMATCH, and SPREDDISPOSE.

     USAGE:
	CALLING SEQUENCES:
		SPREDPARSE( LINE : CMDLINE;
			    var IDX : CMDLINEIDX;
			    var PRED : SPRED;
			    var ERR : QERRCODE) : boolean;
		SPREDDISPOSE( PRED : SPRED);
		SPREDMATCH( LINE : QSTRING;
			    PRED : SPRED;
			    var ERR : QERRCODE) : boolean;

     REQUIREMENTS: This  package  uses  entry points in the QSPAT and
	QSPATP packages.  It is the second  in  the  QED  LD  parsing
	hierarchy, below  LDPARS.  Users  of SPREDDISPOSE should NIL
	the SPRED pointer after disposal.

     EFFECTS: SPREDPARSE returns a pointer to  a  tree  suitable  for
	matching by SPREDMATCH and disposal by SPREDDISPOSE.

     RESPONSIBLE: Jerry Rosen

     CHANGES: The following error codes can be returned by SPREDPARSE:
	QNONOT_OP  -- Invalid field after 'NOT' operator
	QNOINPAREN -- Invalid field after opening parenthesis
	QNOCLOSPAREN- Missing closing parenthesis
	QNORTOP    -- Invalid right-hand operator of AND or OR
	QSTUPID    -- Unmatchable predicate parsed

     SPREDPARSE can also return any error codes returned by SPATPARSE:
	QNOCLOSE   -- Missing closing delimiter for string pattern
	QNODEFAULT -- Default pattern has been parsed but not set

     CHANGES: djm 04/30/82 - changed calls to lookup to cmd_lookup.
                             Also, moved some declarations to qedtyp.typ,
                             and some initializations to qedtyp.pas.

     ---------------------------------------------------------------- *)
$PAGE declarations
module qspred
  options special;
$SYSTEM cmdutl
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM cmdutl.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM wio.typ
$SYSTEM qedtyp.typ
$SYSTEM filutl
$SYSTEM query
$SYSTEM qspat
$SYSTEM qederr
$SYSTEM qld
$SYSTEM qread
$SYSTEM qedln
$SYSTEM qmark
$SYSTEM qprint
$SYSTEM qsubst
$SYSTEM qjoin
$SYSTEM qsplit
$SYSTEM qopen
$SYSTEM qed
$IF P10
$SYSTEM infpac
$SYSTEM wio
$END
$IF VAX
$SYSTEM imgnam
$END
$SYSTEM qlabel

EXTERNAL CONST lexcon : caller_list;

var
  whichfnd : toktyp;
$PAGE spreddispose
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
    end; (*case*)
    dispose(pred)
  end;
end;
$PAGE spredparse

public function spredparse( line : cmdline; var idx : cmdlineidx;
  var pred : spred; wildswitch : boolean; var err : qerrcode) : boolean;

label 1;


    (* Outsiders see only this wrapper--real work is done down below *)




  procedure error (derr : qerrcode; zap : spred);

  begin
    err := derr;
    spreddispose(zap);
    pred := nil;
    goto 1 (*Kickout of Recursion*);
  end;


$PAGE getspred
  (* GETSPRED parses <spred> ::= [ <spred>  ( AND | OR ) ] <subpred> *)

  function getspred (line : cmdline; var idx : cmdlineidx; var pred : spred;
    var err : qerrcode) : boolean;

  forward;


  (* GET SUB PRED parses
     <subpred> ::=  NOT <subpred>  |  <spat>  |  "(" <spred>  ")"    *)

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
      else begin (*something but not a 'NOT' keyword *)
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
	else begin (*not even a paren'd spred *)
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
$PAGE andoror, stupid
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
      else (* whichfnd = not_tok *) begin
        idx := save_idx				(* back up, not interested in not here *)
      end
    end;
  end;



  function stupid(pred:spred) : boolean;
  (* to catch those ridiculous unmatchable predicates *)

  begin
    stupid := false; (* assume the best *)
    if pred <> nil then
      if pred^.predkind = and_spop then
	with pred^ do
	  if (loper^.predkind = pattern_spop) andif
	    (roper^.predkind = pattern_spop) then
	      stupid := (not(loper^.pattern.stype in [simple, token])) andif
		(loper^.pattern.stype = roper^.pattern.stype);
  end; (*whew*)
$PAGE getspred_body, spredparse_mainline
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


  (*mainline for procedure SPREDPARSE *)


begin
  spredparse := getspred(line, idx, pred, err);
  1:
end;


$PAGE spredmatch
public function spredmatch(line : qstring; pred : spred; var err : qerrcode)
  : boolean;

var
    pos, len: qstringidx; (* discarded after pattern match *)

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
end.
$PAGE QREAD -- QED Terminal Input Interface
(*   +--------------------------------------------------------------+
     |                                                              |
     |                         Q R E A D                            |
     |                         - - - - -                            |
     |                                                              |
     +--------------------------------------------------------------+


     STARTED: 29-Jul-77

     PURPOSE: Reads an input line, including control characters.

     USAGE:
	external function qread : qstring;
	var newline: qstring;

	newline := qread ;

     INPUT: 

	None.

     OUTPUT:

	newline is the line from TTY input.

     REQUIREMENTS: The files TTY and  TTYOUTPUT  must  be  open  when
	QREAD is called.

     NOTES: If  the  input  line  is  too  long,  an  end-of-line  is
	simulated  and  TTY  is  left  in the middle of a line - i.e.
	EOLN  (TTY)  is  false.  A  subsequent  call  picks  up   the
	remainder as a separate line.  Only CR and ESC will be treated
	as end of line characters.

     RESPONSIBLE: Software Tools

     CHANGES: 7/2/79 P. LEE - Changed EDITREAD to QREAD and deleted
	      intra-line editing. This header also changed.
	3/23/81 QQSV - A substantial rewrite of this code to eliminate
		special EOF code and to make line wraparound and long
		lines work properly.  Header updated to reflect change.
	3/31/81	QQSV - Added JUNKLINE logic to prevent lines of QSTRINGLEN
		characters from generating a null line if first line ends
		with CR-LF.  This is a hack; it can be eliminated if the
		run time code ever recognizes CR-LF as a line end.

        9/24/81 djm  - Removed duplicate const declarations of lf, cr, and esc,
                       which are now declared in QED.INC.

       10/05/81 djm  - Added VAX code to read a line from a text file that
                       contains control characters.

        5/07/82 djm  - Changed $IF VAX to $IFANY (VAX, M68).

     ---------------------------------------------------------------- *)

MODULE QREAD;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc

PUBLIC FUNCTION QREAD: QSTRING;

VAR	NEWLINE: QSTRING;			(* A convenient input buffer	*)
	JUNKLINE: BOOLEAN;			(* If line should be ditched	*)

BEGIN
$IFANY (VAX, M68)
  IF EOLN(TTY) THEN
    READLN(TTY);
  READ(TTY,QREAD);
  RETURN;
$END
$IF P10
QREAD := '';					(* Nothing yet	*)
  REPEAT					(* Until line legitimately complete	*)
  JUNKLINE := FALSE;				(* Looks good so far	*)
  IF EOLN (TTY)
  THEN
    BEGIN					(* Get more input, if necessary	*)
    IF (TTY^ <> CR) AND (TTY^ <> ESC)		(* Include prior EOLN character if not CR or ESC *)
    THEN QREAD := QREAD || TTY^;
    READLN (TTY)
    END
  ELSE JUNKLINE := TRUE;			(* May be phony wraparound	*)
  READ (TTY, NEWLINE: QSTRINGLEN - LENGTH (QREAD));
  IF LENGTH (NEWLINE) > 0
  THEN						(* Concatenate new stuff onto end	*)
    BEGIN
    IF EOLN (TTY) AND (TTY^ = LF) AND (NEWLINE[LENGTH (NEWLINE)] = CR)
    THEN
      BEGIN					(* CRLF ==> CR. Fake it for line end	*)
      TTY^ := CR;
      IF LENGTH (NEWLINE) > 1
      THEN					(* Definitely a real line	*)
	BEGIN
	QREAD := QREAD || SUBSTR (NEWLINE, 1, LENGTH (NEWLINE) - 1);
	JUNKLINE := FALSE			(* So make sure we keep it	*)
	END
      END
    ELSE
      BEGIN
      QREAD := QREAD || NEWLINE;
      JUNKLINE := FALSE				(* Keep this line	*)
      END
    END
  UNTIL (EOLN (TTY) AND ((TTY^ = CR) OR (TTY^ = ESC)) AND NOT JUNKLINE)
	OR (LENGTH (QREAD) = QSTRINGLEN)
$END
END.						(* We got enough for now	*)
$PAGE qlabelfind function
(* QLABEL.PAS - modified 9/24/81 by djm to change CHR(11B) to TAB *)
MODULE qlabelpas;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc

PUBLIC FUNCTION QLABELFIND
(	VAR BUFFER:	QBUFFER;	  
	START:		QLINENO;		(* where to start search *)
	TOP:		QLINENO;		(* upper search limit *)
	VAR QLABEL:	QSTRING;		(* label name, if found *)
	VAR DISPL:	QLINENO;		(* disp. of label from start *)
	VAR ERR:	QERRCODE):		(* error code *)
			BOOLEAN;		(* true if label found, else false *)

(* QLABELFIND searches the buffer from the start line backwards to the
   TOP line for a line with a label. If one is found, the label's name
   and its displacement from the START line are returned, with the value
   TRUE. Otherwise, if no label is found, FALSE is returned. 
   A label begins in column one with a character in the set
   ['A'..'Z','a'..'z','0'..'9','$'] and ends with the character
   preceding the next tab, blank, or end of line.  *)

TYPE
  CHARSET = SET OF CHAR;

CONST
  LABEL_HEADS : CHARSET := [ 'A'..'Z', '0'..'9', '$' ];

VAR
  LINENO: QLINENO;
  LINE: QSTRING;
  IDX: QSTRINGIDX;

BEGIN
  LINENO := START;
  QLABELFIND := FALSE;				(* until we find a label *)

  WHILE (NOT QLABELFIND) AND (LINENO >= TOP) DO BEGIN

    LINE := QGETLINE (BUFFER, LINENO, ERR);
    IDX := 1;

  EXIT IF ERR <> QOK;

    IF (IDX <= LENGTH(LINE)) AND
       (UPPERCASE (LINE[1]) IN LABEL_HEADS) THEN BEGIN	(* we have a label *)
      QLABELFIND := TRUE;
      IDX := 2;
      WHILE (IDX <= LENGTH(LINE)) ANDIF
	    (LINE[IDX] <> ' ')    ANDIF
	    (LINE[IDX] <> TAB) DO IDX := IDX + 1;
      QLABEL := SUBSTR (LINE, 1, IDX-1);	(* pull of the label name *)
      DISPL := START - LINENO			(* calculate displacement of label *)
      END
    ELSE LINENO := LINENO - 1

  END						(* while *)
END (* qlabelfind *).
$PAGE editread
(*   +--------------------------------------------------------------+
     |                                                              |
     |                      E D I T R E A D                         |
     |                      - - - - - - - -                         |
     |                                                              |
     +--------------------------------------------------------------+


     STARTED: 29-Jul-77

     NOTE: This is now a stub that simply calls QREAD.

     CHANGES: Previous line editing removed and a call to QREAD inserted.
              P. Lee, 7/3/79

     ---------------------------------------------------------------- *)
(* All code in the old EDITREAD version 1.5 are preserved in comments.*)
module editread
  options special;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc

(*
external function lineedit ( oldline: qstring ): qstring;
*)

public function editread (oldline: qstring): qstring;

begin

  editread := qread;

end.

(* The following is QED 1.5 EDITREAD code   .   
  label 100;
 var newline: packed array[1..qstringlen] of char;  (* PACKED FOR SPEED *)
     nll: qstringidx;

 begin
  nll := 0;					(* HAVE NO CHARS YET *)
  if eoln (tty) then readln (tty);		(* IF AT END OF LINE, GET NEW LINE *)
  if eof (tty) then begin			(* ERRONEOUS CONDITION *)
    open (tty, '');				(* MUST REOPEN IT *)
    readln (tty)
  end;
  while not eoln (tty) do begin			(* READ EACH CHARACTER *)
    if nll < length (newline) then begin	(* ROOM LEFT IN BUFFER *)
      nll := nll + 1;
      newline [nll] := tty^;
      get (tty)
    end
    else goto 100				(* HAVE OVERFLOWED BUFFER, RETURN WHAT WE HAVE *)
  end;

  if eoln(tty) andif (tty^=chr(7) (*CONTROL-G*)) then begin
    if nll>0 then begin
      writeln(tty); break;
      editread:= substr(newline,1,nll);		(*TO AVOID RUMORED BUG PASSING SUBSTR*)
      editread:= lineedit(editread)
    end
    else editread:= lineedit(oldline);
    return
  end;

 100:
  editread := substr (newline, 1, nll);		(* RETURN VARYING STRING *)
 end.
*)
/SEARCH([,])/SPECIAL(WORD)/NOCHECK/NAMES
EDT
QLD
QJOIN
QSPLIT
QEDTYP
QSUBST
QEDERR
QMARK
QPRINT
QEDLN
QOPEN
$PAGE qopenfile
module qopenpas
  options special;
TYPE
  QIOMODE = (QINPUT_MODE, QOUTPUT_MODE);
  QIOOPTIONS = (QIO_APPEND, QIO_CONFIRM, QIO_ASCII);
  QIOOPTION_SET = SET OF QIOOPTIONS;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc
(* 
     This module contains the routine QOPENFILE, modelled after RDLIB's
  OPEN_FILE.
*)



(* QOPENFILE opens a text file for input or output. The mode is specified by
   the caller. For an output file the user can also request append mode and
   old/new file prompting.  The caller may supply a default extension for the
   file name. ERR is returned indicating if the open was successful. *)

public procedure qopenfile
	    (	var f: text;
		fid: file_name; ext: string[3];
		mode: qiomode;
		option_set: qiooption_set;
		var err: qerrcode );

 var question: query_string;
     lext: packed array[1..5] of char;
 begin
  err := qok;
  lext := '     ';
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
	    then question := 'New file: ' || fid
	    else begin
		question := 'Old file: ' || filename (f);   (* used full file_id of that found *)
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
 end.
$PAGE QEDLN -- Text Buffer Manager for QED
MODULE QEDLNpas OPTIONS SPECIAL;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
(*SYSTEM qed.typ*)
(*SYSTEM qedtyp.typ*)
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
(*SYSTEM qsubst.inc*)
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc
(*   +--------------------------------------------------------------+
     |                                                              |
     |                  	q e d l n                           |
     |                  	- - - - -                           |
     |                                                              |
     +--------------------------------------------------------------+


     started: 21-apr-77

     purpose: this package contains the basic routines which manage a
	qed text buffer.

     usage:
	entry points...
	  qinitbuf.....initialize a text buffer.
		      this routine should only be called once with a
		      buffer, prior to its first use.
	  qdelbuf......delete (release) the contents of a text buffer.
		      whenever a buffer is to be reused, this routine
		      should be called, rather than qinitbuf.
	  qgetline.....return the text of a line.
	  qmodline.....change the text of a line.
	  qaddline.....add a new line to the buffer.
	  qdellines....delete line(s) from the buffer.
	  movelines...move line(s) in the buffer.
	  copylines...copy (duplicate) line(s) in the buffer.

     requirements: this  package  uses  the qed string routines,  and
	the text returned by these routines  is  in  the  qed  string
	form.

     algorithm: line  descriptors  are  maintained in a doubly linked
	list with header information in the passed buffer descriptor.
	the line descriptors in turn contain qed strings.

     notes: since  any  of  the qed line operations may be broken out
	of,  care is taken in  the  management  of  the  line  chain.
	while  storage  may  be  lost  as  the result of breaks,  the
	following strategy insures that  these  routines  will  never
	leave  the line chain in a confused state,  i.e.,  will never
	partially complete operations.

	1.  lastlineno is always correct.

	2.  getlinep and lastlinep may  not  be  defined  (are  nil),
	even  if  the  buffer  contains  text.  whenever an operation
	changes getlineno or lastlineno,  the corresponding  xxxlinep
	is niled first,  then updated after the xxxlineno is changed.

	3.  the back line chain may be broken,  i.e.,  a qline  other
	than  the  first  one  may have prevlinep=nil.  however,  the
	forward chain is always complete.

	4.  the definitive test of an empty buffer  is  lastlineno=0,
	in  which  case,  the other contents of the buffer are assumed
	to be arbitrary.

     responsible: a. kortesoja

     changes: 
	12/11/78 smr changed qfilewrite to check for write errors.
	12/11/78 smr added parameter to qfilewrite which indicates
		     whether new/old file prompting is desired.

	7/30/79 P.Lee  Changed QSETBOUNDS & MAP to use an offset in
			the buffer for bounded line addressing. Also
			the added option of using the entire buffer
			or the bounded buffer in bounding .

        9/17/81 djm    Replaced old MASK/UNMASK/PUSHESCAPE/ESCPOP/FIRESCAPE
                       attention handling with the new Exception Handling
                       constructs available in Pascal.

        9/21/81 djm    Removed $IF ANC code in procedure makeline, and removed
                       procedures QTAG, QTAGSET, and QTAGCLEAR.  This code is
                       still present in the ANC version.

        9/24/81 djm    Removed duplicate const declarations of lf and cr, 
                       which are now declared in QED.INC.

        9/25/81 djm    Added VAX code to QFILEAPPEND to read in lines from
                       a text file that contains control characters.

        9/28/81 djm    Added $IF P10 switches around certain system dependent
                       portions of the 940 file handling code.

       10/01/81 djm    Added more $IF P10 switches around more system dependent
                       portions of the 940 file handling code.

        5/17/82 djm    Added initialization of S940 flag in qfilewrite for
                       non-P10 code.

     ---------------------------------------------------------------- *)



$PAGE move
(* MOVE is the ultimate procedure for all line manipulations, including
   additions, deletions, and real moves.  It handles all the bookkeeping
   for updating the special line numbers and pointers.  It is assumed that
   all line numbers and pointers passed to this routine are reasonable. *)


PROCEDURE MOVE
     (	VAR BUF: QBUFFER; (* buffer to manipulate *)
	FLN: QLINENO;	(* addr of first line of section to be moved, if
			   zero, lines are new additions from garblist *)
	FLP: QLINEP;	(* ptr to above line *)
	LLN: QLINENO;	(* addr of last line to be moved, if fln = 0 then
			   this is #lines - 1 to yield proper count *)
	LLP: QLINEP;	(* ptr to above line *)
	TLN: QLINENO;	(* addr of line after which text is to be moved *)
	TLP: QLINEP   ); (* ptr to above line, if nil, lines are added to
			   the list of lines to be discarded *)

VAR CNT: QLINENO;
  TLNO: QLINENO;	(* tln adjusted for movements *)
BEGIN
 WITH BUF DO BEGIN
  CNT := LLN - FLN + 1;				(* count of lines to move *)
  TLNO := TLN;
  MASK(ATTENTION);


  (* slice out source lines from buffer if move or delete *)

  IF FLN <> 0 THEN BEGIN			(* check for lines in buffer *)
    FLP^.PREVLINEP^.NEXTLINEP := LLP^.NEXTLINEP;    (* take off chain *)
    IF LLP^.NEXTLINEP <> NIL THEN 
      LLP^.NEXTLINEP^.PREVLINEP := FLP^.PREVLINEP;

    IF FLN <= LBOUND THEN			(* adjust special line numbers *)
      IF LBOUND > LLN THEN LBOUND := LBOUND - CNT
	ELSE BEGIN				(* lbound in lines moved, new lbound follows *)
	  LBOUND := FLN;
	  LBOUNDP := LLP^.NEXTLINEP
	END;

    IF FLN <= GETLINENO THEN
      IF GETLINENO > LLN THEN GETLINENO := GETLINENO - CNT
	ELSE BEGIN				(* getlineno in lines move, new before them *)
	  GETLINENO := FLN - 1;
	  GETLINEP := FLP^.PREVLINEP
	END;

    IF FLN <= HBOUND THEN
      IF HBOUND > LLN THEN HBOUND := HBOUND - CNT
	ELSE BEGIN				(* hbound in lines moved, new before them *)
	  HBOUND := FLN - 1;
	  HBOUNDP := FLP^.PREVLINEP
	END;

    IF FLN <= LASTLINENO THEN
      IF LASTLINENO > LLN THEN LASTLINENO := LASTLINENO - CNT
	ELSE BEGIN				(* last line in lines moved, new at new end of buffer *)
	  LASTLINENO := FLN - 1;
	  LASTLINEP := FLP^.PREVLINEP
	END;

    IF FLN < TLNO THEN TLNO := TLNO - CNT	(* addr of target may be affected too *)
  END


  (* if appending new lines, remove from garb list *)

  ELSE IF LLP = GARBLINEP			(* quick check to see that line is on list *)
    THEN GARBLINEP := FLP^.PREVLINEP;


  (* if deleting, add to list to be discarded *)

  IF TLP = NIL THEN BEGIN
    FLP^.PREVLINEP := GARBLINEP;		(* garb chain is backwards *)
    GARBLINEP := LLP
  END


  (* if moving or appending, add after target line *)

  ELSE BEGIN
    LLP^.NEXTLINEP := TLP^.NEXTLINEP;		(* thread source to target *)
    FLP^.PREVLINEP := TLP;

    IF TLP^.NEXTLINEP <> NIL THEN		(* thread target to source *)
      TLP^.NEXTLINEP^.PREVLINEP := LLP;
    TLP^.NEXTLINEP := FLP;

    IF LASTLINENO = TLNO THEN LASTLINEP := LLP;	(* adjust special hooks *)
    LASTLINENO := LASTLINENO + CNT;
    IF TLNO <= HBOUND THEN BEGIN
      IF TLNO = HBOUND THEN HBOUNDP := LLP;
      HBOUND := HBOUND + CNT;
      IF TLNO = LBOUND - 1 THEN LBOUNDP := FLP
	ELSE IF TLNO < LBOUND THEN LBOUND := LBOUND + CNT
    END
  END;

  CHANGES := TRUE;
  UNMASK(ATTENTION);
 END
END;
$PAGE cleangarb
(* CLEANGARB removes deleted (or unused) lines from the so-called garb list.
   The list is scanned backwards and one line at a time is deleted. This
   code runs unmasked; if interrupted, at most one line will be lost (i.e.
   unchained, but not disposed.) *)

PROCEDURE CLEANGARB ( VAR BUFFER: QBUFFER );
 VAR LP: QLINEP;
 BEGIN
  WITH BUFFER DO BEGIN
   WHILE GARBLINEP <> NIL DO BEGIN		(* scan list and delete one at a time *)
     LP := GARBLINEP;				(* save current ptr in temp *)
     GARBLINEP := GARBLINEP^.PREVLINEP;
     DISPOSE (LP)				(* now delete, after stepping over it in chain *)
   END
  END
 END;
$PAGE findlinep
(*    internal procedure to find the pointer to a passed lineno    *)

PROCEDURE FINDLINEP(VAR BUF: QBUFFER; LNO: QLINENO; VAR LP: QLINEP);

(* assumes that l is a good number *)


  PROCEDURE SETLP(TP: QLINEP);			(* sets findlinep return value *)
  BEGIN
    WITH BUF DO BEGIN				(*update buf info first*)
      MASK(ATTENTION);
      GETLINENO := LNO;
      GETLINEP := TP;
      UNMASK(ATTENTION)
    END (*with*);
    LP:= TP					(*now return pointer*)
  END (*setlp*);


  PROCEDURE SEARCH(BEGLINENO: QLINENO; BEGLINEP: QLINEP;
		   ENDLINENO: QLINENO; ENDLINEP: QLINEP);
    VAR TP: QLINEP; I: QLINENO;			(*used in line search*)
  BEGIN
    (*determine search direction*)
    IF ((ENDLINENO - LNO) <= (LNO - BEGLINENO)) AND (ENDLINEP <> NIL)
      THEN BEGIN				(*search backward from endlineno to lno*)
	TP:= ENDLINEP;
	FOR I:= ENDLINENO-1 DOWNTO LNO DO
	  TP:= TP^.PREVLINEP
      END
      ELSE BEGIN				(*search forward from beglineno to lno*)
	TP:= BEGLINEP;
	FOR I:= BEGLINENO+1 TO LNO DO
	  TP:= TP^.NEXTLINEP
      END;
    SETLP(TP)					(*update getline information*)
  END (*search*);


BEGIN						(*findline*)
  WITH BUF DO
    IF LNO < GETLINENO
      THEN IF LNO < LBOUND
	THEN SEARCH (0, FIRSTLINEP, GETLINENO, GETLINEP)
	ELSE IF LNO < HBOUND
	  THEN SEARCH (LBOUND, LBOUNDP, GETLINENO, GETLINEP)
	  ELSE SEARCH (HBOUND, HBOUNDP, GETLINENO, GETLINEP)
      ELSE IF LNO < LBOUND
	THEN SEARCH (GETLINENO, GETLINEP, LBOUND, LBOUNDP)
	ELSE IF LNO <= HBOUND
	  THEN SEARCH (GETLINENO, GETLINEP, HBOUND, HBOUNDP)
	  ELSE SEARCH (GETLINENO, GETLINEP, LASTLINENO, LASTLINEP)
END (*findline*);
$PAGE utilities
(*********** text buffer manager utility routines ***********)

(* function to transform bounded linenos into absolute ones *)

FUNCTION MAP
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	LINE: QLINENO				(* line number to transform *)
		): QLINENO;			(* mapped result *)

BEGIN
  MAP := LINE + BUFFER.LBOUND - BUFFER.OFFSET
END;						(* map *)

(*    procedure to check a line number    *)

FUNCTION CHKLINE(VAR BUF: QBUFFER; L: QLINENO; VAR ERR: QERRCODE): BOOLEAN;
BEGIN
  ERR:= QOK;
  IF (L < BUF.LBOUND) OR (L > BUF.HBOUND) THEN ERR:= QBADLN;
  CHKLINE:= (ERR=QOK)
END (*chkline*);


(*    procedure to check a line range    *)

FUNCTION CHKRANGE(VAR BUF: QBUFFER; F,L: QLINENO; VAR ERR: QERRCODE): BOOLEAN;
BEGIN
  ERR:= QOK;
  IF F > L THEN ERR:= QBADRN
  ELSE IF L > BUF.HBOUND THEN ERR:= QBADUB
  ELSE IF F < BUF.LBOUND THEN ERR:= QBADLB;
  CHKRANGE:= (ERR=QOK)
END (*chkrange*);
$PAGE makeline
(* procedure to create a qed line record, does not chain it in *)

FUNCTION MAKELINE (VAR BUF: QBUFFER; LINE: QSTRING): QLINEP;
 TYPE
   SYNLINEP = ^SYNLINE;				(* synthetic line *)
   SYNLINE =
     PACKED RECORD
       PREVLINEP, NEXTLINEP: QLINEP;
       STRNG: PACKED ARRAY[1..*] OF CHAR
     END;
 VAR SYNP: SYNLINEP;
     NP: QLINEP;
 BEGIN
  NEW (SYNP, LENGTH (LINE));			(* alloc line of appropriate length *)
  SYNP^.STRNG[1:LENGTH(LINE)]:=LINE;		(* copy only to length allocated *)
  NP := ADDRESS (SYNP^);			(* coerce the pointer *)
  WITH NP^ DO BEGIN
    NEXTLINEP := NIL;
    PREVLINEP := BUF.GARBLINEP;			(* add to dispose list, in case we lose it *)
    IF BUF.GARBLINEP <> NIL THEN
      BUF.GARBLINEP^.NEXTLINEP := NP;
    BUF.GARBLINEP := NP
  END;
  MAKELINE := NP
 END;						(* makeline *)
$PAGE qdelbuf

(*    procedure to delete a buffer    *)

PUBLIC PROCEDURE QDELBUF(VAR BUF: QBUFFER);
  VAR ERR: QERRCODE;				(*we need it but we ignore them*)
BEGIN						(*qdelbuf*)
  WITH BUF DO BEGIN
    IF LASTLINENO>0 THEN BEGIN			(*something to release*)
      MOVE (BUF, 1, FIRSTLINEP^.NEXTLINEP, LASTLINENO, LASTLINEP, 0, NIL);  (* move lines to garb list *)
      CLEANGARB (BUF);				(* dispose the lines *)
    END;
    DISPOSE (FIRSTLINEP);			(* get rid of zeroth line *)
    SPREDDISPOSE (MARK);			(* dispose mark predicate *)
    MARK := NIL					(* for good measure *)
  END						(*with*)
END (*qdelbuf*);

$PAGE qgetline
(*    function to return text of line    *)

PUBLIC FUNCTION QGETLINE(VAR BUF: QBUFFER; L: QLINENO; VAR ERR: QERRCODE): QSTRING;

VAR
  LP: QLINEP;
  LNO: QLINENO;

BEGIN
  LNO := MAP (BUF, L);
  IF NOT CHKLINE(BUF, LNO, ERR) THEN QGETLINE:= ''
  ELSE BEGIN
    FINDLINEP(BUF, LNO, LP);
    QGETLINE := SUBSTR (LP^.SOURCE, 1, LENGTH (LP^.SOURCE))
  END
END (*qgetline*);

$PAGE qmodline

PUBLIC PROCEDURE QMODLINE(VAR BUF: QBUFFER; L: QLINENO; NEWTEXT: QSTRING;
  VAR ERR: QERRCODE);

VAR
  LP,NP: QLINEP;
  LNO: QLINENO;

BEGIN
  LNO := MAP (BUF, L);
  IF CHKLINE(BUF, LNO, ERR) THEN BEGIN
    FINDLINEP(BUF, LNO, LP);
    NP := MAKELINE (BUF, NEWTEXT);
    MASK(ATTENTION);
    WITH NP^ DO BEGIN
      BUF.GARBLINEP := PREVLINEP;		(* remove new from garb list *)
      PREVLINEP := LP^.PREVLINEP;		(* chain new line to neighbors of old line *)
      NEXTLINEP := LP^.NEXTLINEP
    END;
    WITH LP^ DO BEGIN				(* chain neighbors to new line *)
      PREVLINEP^.NEXTLINEP := NP;		(* make forward chain *)
      IF NEXTLINEP <> NIL			(* build backward chain *)
	THEN NEXTLINEP^.PREVLINEP := NP
	ELSE BUF.LASTLINEP := NP;
      IF LNO = BUF.LBOUND THEN BUF.LBOUNDP := NP;   (* if this was special line, reset ptr *)
      IF LNO = BUF.HBOUND THEN BUF.HBOUNDP := NP;
      IF LNO = BUF.GETLINENO THEN BUF.GETLINEP := NP;
    END;
    LP^.PREVLINEP := BUF.GARBLINEP;		(* put old on garb list to dispose *)
    BUF.GARBLINEP := LP;
    BUF.CHANGES := TRUE;
    UNMASK(ATTENTION);
    CLEANGARB (BUF)				(* dispose old line *)
  END
END (*qmodline*);
$PAGE qaddline
(*    procedure to add a line to a buffer    *)

PUBLIC PROCEDURE QADDLINE(VAR BUF: QBUFFER; L: QLINENO; TEXT:QSTRING;
  VAR ERR: QERRCODE);

VAR
  LNO: QLINENO;
  NP, LP: QLINEP;

BEGIN
  ERR := QOK;					(* assume success *)
  LNO := MAP (BUF, L);
  IF LNO > BUF.HBOUND THEN ERR := QBADLN
  ELSE BEGIN
    FINDLINEP (BUF, LNO, LP);			(* find line to append to *)
    NP := MAKELINE (BUF, TEXT);			(* create a line with text *)
    MOVE (BUF, 0, NP, 0, NP, LNO, LP);		(* move from garblist to buffer *)
  END
END (*qaddline*);
$PAGE qdellines
(*    procedure to delete line(s) from buffer    *)

PUBLIC PROCEDURE QDELLINES (VAR BUF: QBUFFER; F,L: QLINENO; VAR ERR: QERRCODE);

VAR
  FP,
  LP: QLINEP;
  FNO,
  LNO: QLINENO;
BEGIN
  FNO := MAP (BUF, F);
  LNO := MAP (BUF, L);
  IF CHKRANGE (BUF, FNO, LNO, ERR) THEN BEGIN
    FINDLINEP (BUF, FNO, FP);			(* find addressed lines *)
    FINDLINEP (BUF, LNO, LP);
    MOVE (BUF, FNO, FP, LNO, LP, 0, NIL);	(* move to garb list *)
    CLEANGARB (BUF);				(* and dispose *)
    ERR := QOK
  END
END (*qdellines*);
$PAGE qbuflength
PUBLIC FUNCTION QBUFLENGTH ( VAR BUF: QBUFFER ): QLINENO;
 BEGIN
   WITH BUF DO BEGIN
     QBUFLENGTH := HBOUND - LBOUND + 1
   END
 END;

PUBLIC FUNCTION QDOLLAR_VAL ( VAR BUF: QBUFFER ): QLINENO;
  BEGIN
    QDOLLAR_VAL := QBUFLENGTH (BUF) + BUF.OFFSET - 1
  END;						(* qdollar_val *)

PUBLIC FUNCTION QFIRST_VAL ( VAR BUF: QBUFFER ): QLINENO;
  BEGIN
    QFIRST_VAL := BUF.OFFSET
  END;						(* qfirst_val *)
$PAGE qmovelines
PUBLIC PROCEDURE QMOVELINES
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	FIRST, LAST: QLINENO;			(* range of lines to be moved *)
	DEST: QLINENO;				(* where to move them to *)
	VAR ERR: QERRCODE);			(* error report *)

VAR
  FNO,
  LNO,
  DNO:		QLINENO;			(* for line number mapping *)
  FIRSTP,
  LASTP:	QLINEP;				(* temporary pointers *)
  DESTP:	QLINEP;				(* where to re-attach lines *)

BEGIN
  FNO := MAP (BUFFER, FIRST);
  LNO := MAP (BUFFER, LAST);
  DNO := MAP (BUFFER, DEST);
  IF NOT CHKRANGE (BUFFER, FNO, LNO, ERR) THEN RETURN;
  IF NOT ((DNO = BUFFER.LBOUND - 1) ORIF (CHKLINE (BUFFER, DNO, ERR))) THEN RETURN;
  IF (FNO <= DNO) AND (DNO <= LNO) THEN BEGIN	(* target within lines to be moved *)
    ERR := QBADMOVELA;
    RETURN
  END;
  FINDLINEP (BUFFER, FNO, FIRSTP);
  FINDLINEP (BUFFER, LNO, LASTP);
  FINDLINEP (BUFFER, DNO, DESTP);
  MOVE (BUFFER, FNO, FIRSTP, LNO, LASTP, DNO, DESTP);	(* do it *)
END;
$PAGE qcopylines
PUBLIC PROCEDURE QCOPYLINES
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	FIRST, LAST: QLINENO;			(* range of lines to copy *)
	DEST: QLINENO;				(* where to copy them to *)
	VAR ERR: QERRCODE);			(* error report *)

VAR
  SOURCE:	QSTRING;			(* to hold text of lines to be copied *)
  IDX:		QLINENO;			(* counter for lines *)
  FNO,
  LNO,
  DNO:		QLINENO;			(* for line number mapping *)
  FIRSTP,
  LASTP,
  DESTP:	QLINEP;				(* working pointers *)

BEGIN
  FNO := MAP (BUFFER, FIRST);
  LNO := MAP (BUFFER, LAST);
  DNO := MAP (BUFFER, DEST);
  IF NOT CHKRANGE (BUFFER, FNO, LNO, ERR) THEN RETURN;
  IF NOT ((DNO = BUFFER.LBOUND - 1) ORIF (CHKLINE (BUFFER, DNO, ERR))) THEN RETURN;

  (* construct copy of lines to move on garb list *)

  CLEANGARB (BUFFER);				(* not really necessary, but good form *)
  FIRSTP := NIL;				(* to check if first line copied *)
  FOR IDX := FIRST TO LAST DO BEGIN		(* copy lines, use relative #s with qgetline *)
    SOURCE := QGETLINE (BUFFER, IDX, ERR);	(* get text of line *)
    IF ERR <> QOK THEN RETURN;
    LASTP := MAKELINE (BUFFER, SOURCE);		(* append copy to garb list *)
    IF FIRSTP = NIL THEN FIRSTP := LASTP	(* remember start *)
  END;

  (* move copy of lines into buffer *)

  FINDLINEP (BUFFER, DNO, DESTP);
  MOVE (BUFFER, 0, FIRSTP, LNO-FNO, LASTP, DNO, DESTP)

END;						(* qcopylines *)
$PAGE bounding utilities
(* routine to set the buffer offset for addressing bounded lines *)
PUBLIC PROCEDURE QSETOFFSET (NEWOFFSET: QLINENO; VAR BUFFER: QBUFFER);
BEGIN
  BUFFER.OLDOFFSET := BUFFER.OFFSET;
  BUFFER.OFFSET := NEWOFFSET
END;

PUBLIC PROCEDURE QSETBOUNDS (VAR BUFFER: QBUFFER; LOW, HIGH: QLINENO;
	ABSOLUTE: BOOLEAN; VAR ERR: QERRCODE);

VAR
  TEMPOFFSET: QLINENO;
  TEMPP: QLINEP;				(* temporary storage *)
  FNO,
  LNO: QLINENO;					(* for bound conversion *)

BEGIN
  TEMPOFFSET := BUFFER.OFFSET;
  IF ABSOLUTE THEN BUFFER.OFFSET := BUFFER.LBOUND
  ELSE BUFFER.OFFSET := BUFFER.OLDOFFSET;
  FNO := MAP (BUFFER, LOW);
  LNO := MAP (BUFFER, HIGH);
  IF CHKRANGE (BUFFER, FNO, LNO, ERR) THEN
  WITH BUFFER DO
  BEGIN
    MASK(ATTENTION);
    FINDLINEP (BUFFER, FNO, TEMPP);
    FINDLINEP (BUFFER, LNO, HBOUNDP);
    LBOUNDP := TEMPP;
    LBOUND := FNO - OFFSET + 1;
    HBOUND := LNO - OFFSET + 1;
    UNMASK(ATTENTION)
  END;
  BUFFER.OFFSET := TEMPOFFSET;
  BUFFER.CURLINENO := QFIRST_VAL (BUFFER)
END (* qsetbounds *);



PUBLIC PROCEDURE QUNBOUND (VAR BUFFER: QBUFFER; VAR ERR: QERRCODE);
BEGIN
  ERR := QOK;
  MASK(ATTENTION);
  WITH BUFFER DO
  BEGIN
    LBOUND := 1;
    LBOUNDP := FIRSTLINEP^.NEXTLINEP;
    HBOUND := LASTLINENO;
    OFFSET := 1;
    HBOUNDP := LASTLINEP
  END;
  UNMASK(ATTENTION)
END (* qunbound *);
$PAGE QFILEAPPEND -- Read Buffer Text from a File

VAR
  F: TEXT;					(* kludge around brain-damage *)

PUBLIC PROCEDURE QFILEAPPEND
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	S940ID: FILE_ID;			(* file to read text from	*)
	WMOD: WMODIFIER;			(* 940 file modifier	*)
	WHERE: QLINENO;				(* where to append text *)
        VAR CNT: QLINENO;                       (* number of lines appended *)
	VAR ERR: QERRCODE);			(* error report *)

VAR
  IDX: QSTRINGIDX;
  CH: CHAR;
  WHERENO: QLINENO;				(* mapped address *)
  TLINE, RLINE: QSTRING;
  FIRST, LAST, WHEREP: QLINEP;
	PDP10ID: FILE_ID;			(* Converted name of file	*)
	RFLAG:	BOOLEAN;			(* Kludgy documentation revision switch	*)
	WERR:	WCODES;				(* Return codes from 940 I/O routines	*)
	S940:	BOOLEAN;			(* True if file is 940 file	*)
	WCHAN:	WCHANNEL;			(* Channel number from which to read	*)
	JUNKLINE: BOOLEAN;			(* True if line should be ditched *)

BEGIN
ERR := QOK;					(* Start with a clean slate	*)
WHERENO := MAP (BUFFER, WHERE);
IF WHERENO > BUFFER.HBOUND
THEN
  BEGIN						(* No reading outside file limits!	*)
  ERR := QBADLN;
  RETURN
  END;
CNT := 0;					(* No lines read yet	*)
S940 := FALSE;
PDP10ID := S940ID;
$IF P10
(*
*	Convert the file name and figure out what kind of a file we're
*	dealing with.  If it is a 940 file, use the special 940 file
*	opening routine; otherwise, just use standard I/O.
*)
MASK(ATTENTION);
WFILECONVERT (S940ID, WMOD, PDP10ID, WERR, RFLAG);
UNMASK(ATTENTION);
IF WERR = WBADNAME
THEN ERR := QNOFILE				(* Stop obvious garbage early	*)
ELSE IF WERR = WOK
THEN
  BEGIN						(* File is 940 file	*)
  WCHAN := GETCHANNEL;
  WOPEN (WCHAN, WERR, WINPUT, PDP10ID);
  IF WERR <> WOK
  THEN
    BEGIN
    FREECHANNEL (WCHAN);			(* Open failed. Return channel to pool	*)
    ERR := QNOFILE
    END
  ELSE S940 := TRUE;				(* Open succeeded. Leave the word	*)
  END
ELSE
$END
  QOPENFILE (F, PDP10ID, '', QINPUT_MODE, [QIO_ASCII], ERR);
IF ERR = QOK
THEN
  BEGIN						(* So far, so good	*)
  FIRST := NIL;                                 (* Initialize new chain *)
  WERR := WOK;
  IF NOT S940
  THEN F^ := CR;				(* Initialize normal file buffer	*)
    LOOP					(* The main read loop starts here	*)
    MASK(ATTENTION);
$IF P10
    IF S940					(* 940 files have special read routine *)
    THEN WINLINE (WCHAN, WERR, TLINE)
    ELSE
      BEGIN					(* Normal files are a bit more arduous	*)
      TLINE := '';				(* Start out fresh	*)
(*
*	Although there are a number of characters which will end lines
*	as far as Pascal is concerned, the only acceptable line end
*	characters for us are a CR-LF pair and CR by itself (not
*	recognized by PDP-10 I/O as a line end).  Accordingly, we
*	watch out for all other cases and force them to wrap around
*	for up to QSTRINGLEN characters.  EOF forces a halt, regardless
*	of the character in the buffer.
*
*	Note that a line of QSTRINGLEN characters should not generate a
*	zero length line following it.  Thus, if the character in F^ is a
*	CR, the line must be thrown out if the previous line had QSTRINGLEN
*	characters and the current line has only one (the CR).  This is
*	accomplished with a hack.  The hack can be removed if the run time
*	code ever treats CR-LF as a line end.
*)
	REPEAT					(* Until we fill a QED line	*)
	JUNKLINE := FALSE;			(* Line looks good so far	*)
	IF EOLN (F)				(* Fetch a line, if needed	*)
	THEN
	  BEGIN
	  IF F^ <> CR				(* Force nonstandard lines to wrap *)
	  THEN TLINE := TLINE || F^;
	  READLN (F)
	  END
	ELSE JUNKLINE := TRUE;			(* This might be a phony null line *)
	READ (F, RLINE: QSTRINGLEN - LENGTH (TLINE));	(* Don't get too much	*)
	IF EOF (F)
	THEN F^ := CR;				(* Always stop on EOF	*)
	IF LENGTH (RLINE) > 0
	THEN					(* We got something	*)
	  BEGIN
	  IF EOLN (F) AND (F^ = LF) AND (RLINE[LENGTH (RLINE)] = CR)
	  THEN
	    BEGIN				(* Chop trailing junk CR, if present *)
	    F^ := CR;				(* and set CR as fake line end	*)
	    IF LENGTH (RLINE) > 1
	    THEN				(* Must be a legitimate line	*)
	      BEGIN
	      TLINE := TLINE || SUBSTR (RLINE, 1, LENGTH (RLINE) - 1);
	      JUNKLINE := FALSE
	      END
	    END
	  ELSE
	    BEGIN
	    TLINE := TLINE || RLINE;		(* Bad line end. Will wrap	*)
	    JUNKLINE := FALSE			(* Don't eat the line	*)
	    END
	  END
	ELSE IF (LENGTH (TLINE) = 0) AND EOF (F)
	THEN WERR := WEOF			(* Absolutely all done	*)
	UNTIL (EOLN (F) AND (F^ = CR) AND NOT JUNKLINE) OR (LENGTH (TLINE) = QSTRINGLEN)
      END;
$END
$IFANY (VAX, M68)
    READLN(F);
    IF EOF(F) THEN
      WERR := WEOF
    ELSE
      READ(F,TLINE);
$END
    UNMASK(ATTENTION);
    EXIT IF WERR <> WOK;			(* Either EOF or error	*)
    MASK(ATTENTION);
    LAST := MAKELINE (BUFFER, TLINE);		(* Chain line into list	*)
    IF FIRST = NIL
    THEN FIRST := LAST;				(* Remember first line	*)
    UNMASK(ATTENTION);
    CNT := CNT + 1
    END;					(* Main loop	*)
(*
*	The file has now been completely read in.  If there was anything
*	there, chain the new blob of lines into the proper slot.  Indicate
*	that the buffer is unchanged if it was initially fresh.  Close
*	the file, reenable attention interrupts, and leave.
*)
  IF CNT > 0
  THEN
    BEGIN					(* Got something. Chain it in	*)
    FINDLINEP (BUFFER, WHERENO, WHEREP);
    MOVE (BUFFER, 0, FIRST, CNT-1, LAST, WHERENO, WHEREP)
    END;
  IF (BUFFER.LASTLINENO = CNT) AND (ERR = QOK)
  THEN
    BEGIN					(* Buffer was empty. Save file name *)
    BUFFER.CHANGES := FALSE;			(* Buffer starts out clean	*)
    IF S940
    THEN
      BEGIN					(* 940 name may be revised for DOC kludge *)
$IF P10
      WFILENAME (WCHAN, BUFFER.CURFILE, RFLAG);
      BUFFER.S940 := TRUE			(* This is now a 940 buffer	*)
$END
      END
    ELSE
      BEGIN					(* Just use normal name for PDP-10 file	*)
      BUFFER.CURFILE := FILENAME (F);
      BUFFER.S940 := FALSE
      END;
    BUFFER.CURFILEOK := TRUE
    END;
  IF S940
  THEN
    BEGIN					(* Special close logic for 940 files	*)
$IF P10
    WCLOSE (WCHAN, WERR);
    FREECHANNEL (WCHAN)
$END
    END
  ELSE CLOSE (F);				(* Normal close for others	*)
  END
  EXCEPTION
    OTHERS: BEGIN
              MASK(ATTENTION);
	      IF S940
	      THEN
		BEGIN					(* Special close for 940 file	*)
$IF P10
		WCLOSE (WCHAN, WERR);
		FREECHANNEL (WCHAN)
$END
		END
	      ELSE CLOSE (F);				(* Standard close for most things	*)
	      CLEANGARB (BUFFER);				(* Flush unfinished stuff	*)
              UNMASK(ATTENTION);
              SIGNAL();
            END;
END;						(* qfileappend *)
$PAGE qttyappend
PUBLIC PROCEDURE QTTYAPPEND
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	WHERE: QLINENO;				(* where to append text *)
	VAR CNT: QLINENO;			(* number of lines appended *)
	VAR ERR: QERRCODE);			(* error report *)

VAR
  LINE: QSTRING;
  CH: CHAR;
  LINENUM: QLINENO;
  DONE: BOOLEAN;

BEGIN
  BREAK;
  LINENUM := WHERE;
  ERR := QOK;
(*if where = 0 then line := ''			(* get text of previous line to edit *)
  else line := qgetline (buffer, where, err);
  if err <> qok then return;    previous line editing deleted! *)
  DONE := FALSE;
  WHILE (ERR = QOK) AND (NOT DONE) DO
  BEGIN
    LINE := QREAD ;
    IF (LENGTH (LINE) = 1) ANDIF (LINE [1] = '.') THEN DONE := TRUE
    ELSE
    BEGIN
      QADDLINE (BUFFER, LINENUM, LINE, ERR);
      LINENUM := LINENUM + 1
    END;
  END;
  IF LINENUM > WHERE THEN BUFFER.CHANGES := TRUE;
  CNT := LINENUM - WHERE
END (* qttyappend *);
$PAGE qfilewrite

PUBLIC PROCEDURE QFILEWRITE			(* write text to file *)
(       VAR BUFFER: QBUFFER;			(* buffer to write from *)
	S940ID: FILE_ID;			(* file to write to	*)
	WMOD: WMODIFIER;			(* 940 file modifier	*)
	FN, LN: QLINENO;			(* range to write *)
	CONFIRM: BOOLEAN;			(* new/old file prompting? *)
	VAR ERR: QERRCODE);			(* error report *)

VAR
  FNO, LNO: QLINENO;
  FLP, LLP: QLINEP;
  LINENO: QLINENO;
  LINE: QSTRING;
  OPTIONS_SET: QIOOPTION_SET;
	PDP10ID: FILE_ID;
	S940, RFLAG: BOOLEAN;
	WERR: WCODES;
	WCHAN: WCHANNEL;

BEGIN
FNO := MAP (BUFFER, FN);
LNO := MAP (BUFFER, LN);
IF NOT CHKRANGE (BUFFER, FNO, LNO, ERR)
THEN RETURN;					(* Can't write outside legal range	*)
IF CONFIRM
THEN OPTIONS_SET := [QIO_CONFIRM]
ELSE OPTIONS_SET := [];
$IF P10
(*
*	If we have a saved file name, use it.  Otherwise, convert a possible
*	940 file to a PDP-10 name.  In either case, open the file for
*	output, prompting for old/new file as required.  If the user confirms
*	the prompt, back off the open and reopen it using the special 940
*	I/O if it's a 940 file.
*)
MASK(ATTENTION);
IF WMOD = '*'					(* If 940 name was already converted *)
THEN
  BEGIN
  PDP10ID := S940ID;				(* Don't bother to reconvert	*)
  WERR := WOK					(* But remember it was 940	*)
  END
ELSE WFILECONVERT (S940ID, WMOD, PDP10ID, WERR, RFLAG);
IF WERR = WBADNAME
THEN ERR := QNOFILE
ELSE QOPENFILE (F, PDP10ID, '', QOUTPUT_MODE, OPTIONS_SET, ERR);
      UNMASK(ATTENTION);
$END
$IFANY (VAX, M68)
  QOPENFILE (F, S940ID, '', QOUTPUT_MODE, OPTIONS_SET, ERR);
  S940 := FALSE;
$END
IF ERR = QOK
THEN
  BEGIN						(* File looks legitimate	*)
$IF P10
  IF WERR = WOK
  THEN
    BEGIN					(* Got a 940 file. Special open	*)
    CLOSE (F);
    WCHAN := GETCHANNEL;
    WOPEN (WCHAN, WERR, WOUTPUT, PDP10ID);
    IF WERR <> WOK
    THEN
      BEGIN					(* No can do. Release everything	*)
      FREECHANNEL (WCHAN);
      ERR := QNOFILE;
      RETURN;
      END;
    S940 := TRUE     				(* Success. Remember it	*)
    END
  ELSE S940 := FALSE;				(* Normal file signal	*)
$END
(*
*	The file has now been properly opened.  Figure out where the first
*	and last lines of the output block are; then write them one at a time.
*)
  FINDLINEP (BUFFER, FNO, FLP);
  FINDLINEP (BUFFER, LNO, LLP);
    REPEAT					(* Here comes the main output loop	*)
    MASK(ATTENTION);
    IF S940
    THEN
      BEGIN					(* Special output for 940 files	*)
$IF P10
      WOUTLINE (WCHAN, WERR, FLP^.SOURCE);
      IF WERR <> WOK
      THEN ERR := QWRTERR			(* Acknowledge write error	*)
$END
      END
    ELSE
      BEGIN					(* Standard output for normal files	*)
      WRITELN (F, FLP^.SOURCE);
      IF NOT EOF (F)
      THEN ERR := QWRTERR			(* Should be positioned at file end	*)
      END;
    UNMASK(ATTENTION);
    EXIT IF FLP = LLP;				(* No pointer shuffle after we're done	*)
    FLP := FLP^.NEXTLINEP			(* Advance to next line	*)
    UNTIL ERR <> QOK;				(* End of main loop	*)
(*
*	The file (or a hunk of it) has now been completely written.
*	if it was, in fact, the whole file, save the file name and
*	type for later defaulting.  Then close the file.
*)
  IF (FNO = 1) AND (LNO = BUFFER.LASTLINENO) AND (ERR = QOK)
  THEN
    BEGIN					(* Whole file written. Remember where	*)
    BUFFER.CHANGES := FALSE;			(* Buffer is now clean	*)
    IF S940
    THEN
      BEGIN					(* Get 940 file name from special code,	*)
$IF P10
      RFLAG := FALSE;				(* avoiding DOC revision kludge	*)
      WFILENAME (WCHAN, BUFFER.CURFILE, RFLAG);
      BUFFER.S940 := TRUE			(* All hail, XDS	*)
$END
      END
    ELSE
      BEGIN					(* Use standard code for normal file *)
      BUFFER.CURFILE := FILENAME (F);
      BUFFER.S940 := FALSE
      END;
    BUFFER.CURFILEOK := TRUE			(* We now have a saved name	*)
    END;
  IF S940
  THEN
    BEGIN
$IF P10
    WCLOSE (WCHAN, WERR);			(* The usual special 940 close	*)
    FREECHANNEL (WCHAN)
$END
    END
  ELSE CLOSE (F);				(* The usual stuff for normal files	*)
  END
ELSE IF ERR = QNOFILE				(* Don't treat confirm failure as error *)
THEN ERR := QOK;
EXCEPTION
  OTHERS: BEGIN
            MASK(ATTENTION);
	    IF S940
	    THEN
	      BEGIN					(* File is 940 file. Special close	*)
$IF P10
	      WCLOSE (WCHAN, WERR);
	      FREECHANNEL (WCHAN)
$END
	      END
	    ELSE CLOSE (F);				(* Standard close for normal file	*)
	    WRITELN (TTY, 'Warning--output file write incomplete.');
	    BREAK;
            UNMASK(ATTENTION);
            SIGNAL();
          END;
END.						(* QFILEWRITE	*)
$PAGE prline
(* QPRINT.PAS - modified 9/24/81 by djm to change chr(11b) to tab *)

module qprintpas
  options special;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc
public procedure prline
(	var outfile: text;			(* file to write to *)
	line: qstring;				(* text to write out *)
        tab_print: boolean;                     (* flag for &I printing *)
        var err: qerrcode);			(* checks for write errors *)

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
$PAGE qlistlines
public procedure qlistlines
(	var buffer: qbuffer;			(* working buffer *)
	low,
	high: qlineno;				(* range of lines to print *)
	var outfile: text;			(* to this file *)
	ctl_char,
	number,        			(* flags to modify printing *)
        tab_print: boolean;                     (* flag for &I printing *)
        var err: qerrcode);			(* set if write errors occur *)

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
end.
$PAGE qmarkmatch
module qmarkpas
  options special;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc
public procedure qmarkmatch
(	var buffer: qbuffer;			(* working buffer *)
	mark: spred;				(* markstring to search for *)
	sect_name: spred;			(* in conjunction with mark *)
	start: qlineno;				(* where to start looking *)
	var fno, lno: qlineno;			(* limits of the marked section *)
        backward: boolean;                      (* backward search flag *)
	wrap: boolean;				(* wrap/no-wrap around flag *)
	var err: qerrcode);			(* error report *)

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
end.						(* qmarkmatch *)
$page QEDERROR -- Some Observations
(*   +--------------------------------------------------------------+
     |                                                              |
     |                      Q E D E R R O R                         |
     |                      - - - - - - - -                         |
     |                                                              |
     +--------------------------------------------------------------+


     STARTED: 17-Aug-77

     PURPOSE: This prints messages assocaited with a QED error code.

     USAGE:
	QEDERROR (F, CODE, LEVEL);

     INPUT: 

	F	   is the file to write the message is to be written.

	CODE       is  the QED error code for which the message is to
		   be printed.

	LEVEL      is the level of the message desired.  Level 1 is a
		   brief   message;   subsequent   levels  give  more
		   information.

     REQUIREMENTS: It is assumed that the  file  TTYOUTPUT  has  been
	opened before this program is called.

     ALGORITHM: If the level specified is 1 and the code appears in a
	special list (stored locally) then  '?'  is  printed  as  the
	message.   Otherwise,  a  file  is  searched  for  the  first
	message with matching code and level numbers.  A  code  of  *
	matches  any  code.  A  level  is  matched by any level value
	greater or equal to the value  specified.  So,  the  messages
	associated with the same code should be ordered by increasing
	level number.  The format of the file is this:  a  series  of
	messages  separated  by blank line(s).  The last message must
	be followed by a blank line.  A single  message  starts  with
	<code>,<level>.  The  rest is text which is parsed into words
	and output in fill mode to fit the width of the terminal.

     RESPONSIBLE: Software Tools

     CHANGES: 09/25/81 - djm - Added VAX code for opening QEDERR.MSG file.
              05/04/82 - djm - Added M68000 code for opening QEDERR.MSG file.

     ---------------------------------------------------------------- *)


MODULE QEDERRORppas
  OPTIONS SPECIAL;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc

$PAGE QEDERROR -- The Code
TYPE QEDERRLEVEL = 1..10;

VAR ERRFILE: TEXT;

PUBLIC PROCEDURE QEDERROR (VAR F: TEXT; CODE: QERRCODE; LEVEL: QEDERRLEVEL);

 VAR MSGCODE: 0..255;				(* code read from file *)
     MSGLEVEL: QEDERRLEVEL;			(* level readfrom files *)
     WORD: STRING[32];				(* assembled word from message *)
     COLUMN: 0..255;				(* output column *)
     ERRFILE_NAME: FILE_NAME;                   (* complete error file name *)
$IF P10 JOBSTUFF: JOBREC;                       (* for error file PPN *)
$IF VAX IMGERR: IMAGE_ERROR;                    (* image_file_name error *)


 (* writes text to TTY, filling to width of terminal *)

 PROCEDURE OUT;
  BEGIN
   IF (COLUMN + LENGTH (WORD)) > 72 (* typical terminal width *) THEN BEGIN
     WRITELN (F);
     COLUMN := 0;
     IF WORD  = '' THEN RETURN
   END;
   COLUMN := COLUMN + LENGTH (WORD);
   WRITE (F, WORD)
  END;


 BEGIN
  COLUMN := 0;

  IF (LEVEL = 1) AND (CODE > QFATAL) THEN BEGIN	(* brief message *)
    WRITELN (F, '?'); BREAK;
    RETURN
  END;

$IF P10
  JOBINFO (JOBSTUFF);
  ERRFILE_NAME := 'QEDERR.MSG' || JOBSTUFF.PROGDIR;
$END
$IF VAX
  IMAGE_FILE_NAME (ERRFILE_NAME, IMGERR);
  ERRFILE_NAME := SUBSTR (ERRFILE_NAME, 1, SEARCH (ERRFILE_NAME, [']'] )) ||
                  'QEDERR.MSG';
$END
$IF M68 ERRFILE_NAME := '1000..QEDERR.MS';
  OPEN (ERRFILE, ERRFILE_NAME );   (* get file *)
  IF EOF (ERRFILE) THEN BEGIN			(* fatal situation *)
    WRITELN (F, 'QED error file missing.'); BREAK;
    RETURN
  END;

  LOOP						(* search for message in file *)
    REPEAT READLN (ERRFILE) UNTIL NOT EOLN (ERRFILE) OR EOF(ERRFILE);	(* skip blanks before start of msg *)
    IF EOF (ERRFILE) THEN BEGIN			(* code + level not found *)
      WRITELN (F, 'Error not found.', ORD (CODE):5); BREAK;
      RETURN
    END;
    IF ERRFILE^ = '*'				(* get errcode from file *)
      THEN BEGIN				(* '*' matches any code *)
	MSGCODE := ORD (CODE);			(* force match *)
	GET (ERRFILE);				(* eat comma following asterisk *)
	GET (ERRFILE)				(* and get first digit of following number *)
      END
      ELSE READ (ERRFILE, MSGCODE);
    READ (ERRFILE, MSGLEVEL);			(* get level from file *)
  EXIT IF (MSGCODE = ORD (CODE)) AND (MSGLEVEL >= LEVEL);
    REPEAT READLN (ERRFILE) UNTIL EOLN (ERRFILE);   (* skip til blank line following msg *)
  END;

  REPEAT					(* output msg *)
    WHILE ERRFILE^ <= ' ' DO GET (ERRFILE);	(* skip control chars at start of msg *)
    WHILE NOT EOLN (ERRFILE) DO BEGIN		(* extract words from line *)
      IF ERRFILE^ > ' ' THEN BEGIN
	WORD := '';
	REPEAT
	  WORD := WORD || ERRFILE^;
	  GET (ERRFILE)
	UNTIL ERRFILE^ <= ' ';
	OUT; WORD := ' '; OUT;			(* write word followed by blank *)
      END
      ELSE GET (ERRFILE);			(* ignore control chars, i.e. white space *)
    END;
    READLN (ERRFILE);				(* go to next line *)
  UNTIL EOLN (ERRFILE);				(* message terminated by blank line *)
  WRITELN (F); BREAK;				(* terminate msg *)
  CLOSE (ERRFILE);				(* clean up our dirty laundry *)
 END.
$PAGE qsubstitute
module qsubstpas
  options special;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc

public function qsubstitute			(* substitute one string for another in a third *)
(	var line: qstring;			(* line in which substitution is to be made *)
	lineno: qlineno;			(* above's line number *)
	pat: spattern;				(* pattern to search for *)
	rplmtstr: qstring;			(* string to replace it with *)
	opts: sub_opt_set;			(* various action-modifiers *)
	var cnt: qstringidx;			(* number of substitutions made *)
        cmd : qedcmds;                          (* parsed command *)
        var nth : qlineno;                      (* count of occurrances *)
        n_par : qlineno;                        (* occurrance parameter *)
	var err: qerrcode			(* error report *)
		): boolean;			(* flag indicating success of substitution *)

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
    tempstr := substr (line, idx);		(* bug fix ? *)
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
	doit := query ('OK')
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
	  idx := idx + pos + length (repstr) - 1	(* advance past inserted string *)
	  end
	else err := qlnlong;
      end
      else idx := idx + pos + len - 1;		(* advance past matched string *)
      if not ((all_sop in opts) and (pat.stype in [simple, token])) then goto 1
    end                                           (* if nth begin *)
  else idx := idx + pos + len - 1;  (* advance past the matched string *)
  exit if err <> qok
  end;						(* loop *)
1:
  if (err = qok) and (cnt > 0) and (print_sop in opts) then prline (ttyoutput, line, true, err)
end.						(* qsubstitute *)
$PAGE QEDTYP -- Public Constants for QED
(* QEDTYP.PAS - modified 10/09/81 by djm to add CASE to SETPARAMS.  *)
(*            - modified 04/29/82 by djm to change qcmdlist, sub_opt_list,
                split_op_list, and set_par_list to use the new procedure
                cmd_lookup.  *)

module qedtyp;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc

public const qcmds: qcmdlist :=
     (  ( 'APPEND',	1, ord (append) ),
	( 'CHANGE',	1, ord (change) ),
	( 'DELETE',	1, ord (delete) ),
	( 'INSERT',	1, ord (insert) ),
	( 'EDIT',	4, ord (edit) ),
	( 'MODIFY',	6, ord (modify) ),
	( 'LOAD',	1, ord (load) ),
	( 'PRINT',	1, ord (print) ),
	( 'SUBSTITUTE',	1, ord (substitute) ),
        ( 'AFTER',      2, ord (after) ),
        ( 'BEFORE',     2, ord (before) ),
	( 'WRITE',	1, ord (writecmd) ),
	( 'SAVE',	2, ord (save) ),
	( 'FIND',	1, ord (find) ),
	( 'GOTO',	2, ord (gotocmd) ),
	( 'RESET',	5, ord (resetcmd) ),
	( 'JOIN',	1, ord (join) ),
	( 'COPY',	2, ord (copy) ),
	( 'MOVE',	2, ord (move) ),
	( 'TRANSFER',	1, ord (transfer) ),
	( 'BOUND',	1, ord (bound) ),
	( 'LIST',	2, ord (list) ),
	( '=',		1, ord (eqcmd) ),
	( 'NUMBER',	1, ord (number) ),
	( 'OPEN',	4, ord (opencmd) ),
	( 'OUTPUT',	6, ord (outputcmd) ),
	( 'CLOSE',	5, ord (closecmd) ),
	( 'SET',	2, ord (setcmd) ),
	( 'SPLIT',	2, ord (split) ),
	( 'QUIT',	1, ord (quit) ),
	( 'EXIT',	2, ord (exitcmd) ),
	( '^',		1, ord (uparrow) ),
	( 'WHY',	3, ord (why) ),
	( 'INDENT',	3, ord (indent) ),
        ( '_',          1, ord (underbar) ),
        ( 'READ',       1, ord (readcmd) )   );

public const defrange: defrangelist :=
     (  ( (* APPEND *)	   dollar,	lb,	lb,	0,	1 ),
	( (* CHANGE *)	   dot,		lb,	lb,	1,	2 ),
	( (* DELETE *)	   dot,		lb,	lb,	1,	2 ),
	( (* INSERT *)	   dotp1,	lb,	lb,	0,	1 ),
	( (* EDIT *)	   dotp1,	lb,	lb,	0,	1 ),
	( (* MODIFY *)	   dotp1,	lb,	lb,	0,	1 ),
	( (* LOAD *)	   dot,		dot,	dot,	0,	0 ),
	( (* PRINT *)	   one,		dollar,	lb,	0,	2 ),
	( (* SUBSTITUTE *) dot,		lb,	lb,	0,	2 ),
        ( (* AFTER *)      dot,         lb,     lb,     0,      2 ),
        ( (* BEFORE *)     dot,         lb,     lb,     0,      2 ),
	( (* WRITE *)	   one,		dollar,	lb,	0,	2 ),
	( (* SAVE *)	   one,		dollar,	lb,	0,	2 ),
	( (* FIND *)	   one,		dollar,	lb,	0,	2 ),
	( (* GOTO *)	   dot,		lb,	lb,	1,	1 ),
	( (* RESET *)	   dot,		dot,	dot,	0,	0 ),
	( (* JOIN *)	   dot,		dot,	lbp1,	2,	2 ),
	( (* COPY *)	   dot,		lb,	lb,	0,	2 ),
	( (* MOVE *)	   dot,		lb,	lb,	0,	2 ),
	( (* TRANSFER *)   dot,		lb,	lb,	0,	2 ),
	( (* BOUND *)	   one,		dollar,	lb,	0,	2 ),
	( (* LIST *)	   one,		dollar,	lb,	0,	2 ),
	( (* = *)	   dot,		lb,	lb,	0,	1 ),
	( (* NUMBER *)	   dot,		lb,	lb,	0,	1 ),
	( (* OPEN *)	   dot,		dot,	dot,	0,	0 ),
	( (* OUTPUT *)	   dot,		lb,	lb,	0,	2 ),
	( (* CLOSE *)	   dot,		dot,	dot,	0,	0 ),
	( (* SET *)	   dot,		dot,	dot,	0,	0 ),
	( (* SPLIT *)	   dot,		lb,	lb,	0,	2 ),
	( (* QUIT *)	   dot,		dot,	dot,	0,	0 ),
	( (* EXIT *)	   dot,		dot,	dot,	0,	0 ),
	( (* ^ *)	   dot,		lb,	lb,	0,	0 ),
	( (* WHY *)	   dot,		dot,	dot,	0,	0 ),
	( (* INDENT *)     dot,		lb,	lb,	0,	2 ),
	( (* _ *)  	   dot,		lb,	lb,	0,	1 ),
	( (* READ *)       dollar,	lb,	lb,	0,	1 )   );

public const sops: sub_opt_list :=
     (	( 'CONFIRM',	1, ord (confirm_sop) ),
	( 'ALL',	1, ord (all_sop) ),
	( 'PRINT',	1, ord (print_sop) ),
	( 'NUMBER',	1, ord (number_sop) )   );

public const splitops: split_op_list :=
     (	( 'NUMBER',	1, ord (number_splitop) ),
	( 'CONFIRM',	1, ord (confirm_splitop) ),
	( 'PRINT',	1, ord (print_splitop) ),
	( 'ALL',	1, ord (all_splitop) ),
	( 'DELETE',	1, ord (delete_splitop) )   );

public const setparams: set_par_list :=
     (	( 'DELLIMIT',	3, ord (del_param) ),
	( 'LINECOUNT',	4, ord (lcnt_param) ),
	( 'MARK',	4, ord (mark_param) ),
	( 'TABS',	3, ord (tab_param) ),
        ( 'WILDCARD',   4, ord (wild_param) ),
        ( 'CASE',       4, ord (case_param) )     );

public const lexcon: caller_list :=
     (	( 'AND',	3, ord (and_tok) ),
	( 'OR',		2, ord (or_tok) ),
        ( 'NOT',        3, ord (not_tok) )     );

public procedure worthless;
begin
end.
$PAGE qsplitlines
module qsplitpas
  options special;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc
public function qsplitlines			(* split selected lines into smaller ones *)
(	var buffer: qbuffer;			(* working buffer *)
	lineno: qlineno;			(* line to split *)
	pat: spattern;				(* where to split each line *)
	opts: split_opt_set;			(* various action-controllers *)
	var cnt: qlineno;			(* number of splits done, i.e. number of new lines *)
	var err: qerrcode			(* error report *)
		): boolean;			(* set true if a match for pat found *)

label 1;

var
  pos,
  len:		qstringidx;			(* for SPATMATCH *)
  source:	qstring;			(* text of lines to be split *)
  tempstr:	qstring;			(* temporary for building lines *)
  doit,
  didit,
  numberit:	boolean;			(* conditionals *)
  newlineno:	qlineno;			(* line counters *)
  idx:		qstringidx;			(* position at which to search for pat *)

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
	doit := query ('OK')
      end;
    if doit
      then begin

	(* we take care here to complete this single split operation, so
	   that in the event of a escape being issued to the next
	   confirmation prompt, everything is as it should be up to the
	   point that the escape is issued *)

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
  end;						(* loop *)
1:
  if (print_splitop in opts) and didit
    then qlistlines (buffer, lineno, newlineno, ttyoutput, true, false, true, err);
end.						(* qsplitlines *)
$page QJOINLINES
module qjoinpas
  options special;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc
public procedure qjoinlines			(* turn two (or more) lines into one *)
(	var buffer: qbuffer;			(* working buffer *)
	first,
	last: qlineno;				(* range of lines to join *)
	contmark: qstring;			(* string to replace CRs with *)
	var err: qerrcode);			(* error report *)

var
  source:	qstring;			(* text of lines to be joined *)
  result:	qstring;			(* line formed by joining others *)
  lineno:	qlineno;			(* counter to step through lines joined *)

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
end.						(* qjoinlines *)
(*   +--------------------------------------------------------------+
     I                                                              I
     I                        L D P A R S                           I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+


     STARTED:  4-Aug-77

     PURPOSE: This package  contains  procedures  QLDPARSE,  QLDEVAL,
	and QLDDISPOSE,  for use in QED-type packages.

     USAGE:
	CALLING SEQUENCES:
		QLDPARSE(LINE : CMDLINE;
			 var IDX : CMDLINEIDX;
			 var NLD : LDCOUNT;
			 var LD : LDCHAIN;
			 var ERR : QERRCODE) ;
		QLDEVAL( var BUF : QBUFFER;
			 LD : LDCHAIN;
			 SEARCHRANGE : LDRANGE;

			 var RANGE : LDRANGE;
			 var ERR : QERRCODE);
		QLDDISPOSE (LD : LDCHAIN);

     REQUIREMENTS: These  routines  need the SPRED and SPAT packages,
	from QSPRED     and QSPATP (arse).  Note that  QLDPARSE
	may   change   the  line  index  on  an  unsuccessful  parse.
	QLDDISPOSE users should NIL the head  of  the  LDCHAIN  after
	calling.

     EFFECTS: QLDPARSE  returns an LDCHAIN suitable for evaluation by
	QLDEVAL and disposal  by  QLDDISPOSE.  This  package  is  the
	highest  in  the LD parsing hierarchy,  and needs INCLUDEs at
	compile time and the other LD parsing packages at link time.

     ALGORITHM: 


     RESPONSIBLE: Software Tools

     The following error codes may be returned by QLDPARSE:
	QOFFIRST -- Signed offset found as first part of LA
	QNONUMF  -- No numeric field after '+' or '-'
	QONLYFIRST- '*', '$', or '.' valid only as first token of LA
	QNOPREVIOUS '*' valid only in second LA
	QNO2NDLA -- No valid LA found after comma
     QLDPARSE can also return any of the codes returned by SPREDPARSE
     or SPATPARSE.

     The following error codes may be returned by QLDEVAL:
	QOUTRANGE-- Evaluated LD is out of buffer or special range
	QSPNOTFND-- String predicate not found by search algorithm


     CHANGES:
        12/14/78   smr   Changed error codes returned by QEDEVAL to
			 indicate which la was in error.

	7/30/79    P.Lee  Changed this module to parse and evaluate
			  backward search predicates.

     ---------------------------------------------------------------- *)
module qldpas
  options special;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc

$PAGE qlddispose
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
$PAGE qldparse
public procedure qldparse ( line : cmdline;
			    var idx : cmdlineidx;
			    var nld : ldcount;
			    var ld : ldchain;
			    wildswitch : boolean;
			    var err : qerrcode)   ;


var
  firstld: ldchain;				(* where we build new chain *)
  lastld, ldtemp: ldchain;
  idxtemp: cmdlineidx;
  backward: boolean;                            (* flag for backward searches *)

(*and now for a few helpers*)

label   1;

     procedure error(derr : qerrcode);
     begin
     err := derr;
     qlddispose(firstld);
     nld := 0;
     goto 1
     end;



     function getint ( line : cmdline; var idx : cmdlineidx) : qlnoffset;
     var     okflag : boolean;
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
			      var first, last : ldchain  ): boolean;

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

     '0'..'9', '+', '-'      :begin
			      if line[idx] in ['+','-'] then
				   begin
				   if firstpass then error(qoffirst);
				   if line[idx] = '-' then
					offsetsign := -1;
				   idx := idx + 1;
				   if (not qtokenget(line,idx))  orif
				      (not (line[idx] in ['0'..'9'])) then
					error(qnonumf)
				   end;
			      linetmp := getint(line, idx);
			      new(temprec, num_ld);
			      temprec^.offset := linetmp * offsetsign;
			      offsetsign := 1
			      end;

     '*', '$', '.'           :begin
			      if not firstpass then error(qonlyfirst);
			      case line[idx] of
	     '*'                     :begin
				      if nld = 0 then
					   error(qnoprevious);
				      new(temprec, star_ld)
				      end;
	     '$'                     :new(temprec, dollar_ld);
	     '.'                     :new(temprec, dot_ld)
				  end;		(*little case*)
			      idx := idx + 1
			      end;

     others                  :begin
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

			 end;			(*case*)
	  exit if kickout;
	  chain(last, temprec);
	  if first = nil then
	       first := temprec
	  else if firstpass then
		    first^.next := temprec;
	  firstpass := false
	  end;					(*loop*)
     laparse := not firstpass
     end;
$PAGE mainline for qldparse
begin						(*mainline for QLDPARSE*)
err := qok;    nld := 0;   firstld := nil;  lastld := nil;
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
$PAGE qldeval
public procedure qldeval ( var buf : qbuffer;
			   ld : ldchain;
			   searchrange : ldrange;
			   var range : ldrange;
			   var err : qerrcode);

label   1;

	procedure error(derr : qerrcode);
	begin
	err := derr;
	goto 1
	end;

	function search (pred : spred;
			 range : ldrange;
			 var buf : qbuffer;
			 var where : qlineno) : boolean;
	var     found : qlineno;
		linetemp : cmdline;
                increment : -1..1;
                backward : boolean;

	function within_limit : boolean;   (* function to check limit in search *)
          begin
            if backward then within_limit := found > range.hbound
                        else within_limit := found < range.hbound
          end;   (* function within_limit *)

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


var     firsttoken, lowbdalso : boolean;
	ldtemp : ldchain;
	range_limit, predrange, addrrange : ldrange;
        notfinderr, outrangeerr: qerrcode;

begin
ldtemp := ld;
err := qok;
range := searchrange;
range_limit.lbound := qfirst_val (buf);	(* allowed search limits- this narrows *)
range_limit.hbound := searchrange.hbound;	(* down as the search continues *)
addrrange.lbound := 0;	(* limits for non-predicate addressing *)
addrrange.hbound := qdollar_val (buf);
predrange.lbound := buf.curlineno;	(* line before search beginning *)
firsttoken := true;
lowbdalso := true;
notfinderr := qla1notfnd;
outrangeerr := qla1outrange;
while ldtemp <> nil do
     begin
     case ldtemp^.ldkind of

forward_ld,
backward_ld     :begin
		 if ldtemp^.ldkind = backward_ld then begin
		   predrange.hbound := range_limit.lbound; (* last search line *)
		   range_limit.hbound := predrange.lbound - 1 (* limits narrow down! *)
		   end
		 else begin
		   predrange.hbound := range_limit.hbound; (* like above *)
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
			range_limit.lbound := predrange.lbound;  (* search limits change *)
			range_limit.hbound := buf.curlineno + 1; (* on wrap-around *)
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

comma_ld        :begin
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

others          :begin
		 case ldtemp^.ldkind of
	dollar_ld       :range.hbound := qdollar_val (buf);
	star_ld         :range.hbound := range.lbound;
	dot_ld          :range.hbound := buf.curlineno;
	num_ld          :if firsttoken then
			      range.hbound := ldtemp^.offset
			 else if (range.hbound + ldtemp^.offset > addrrange.hbound)
			      or (range.hbound + ldtemp^.offset < addrrange.lbound) 
			      then
			      error(outrangeerr)
			      else range.hbound := range.hbound+ldtemp^.offset
		     end;			(*little case*)
		 if range.hbound = 0 then
		      predrange.lbound := 1
		 else if range.hbound >= maximum(qlineno) then error(outrangeerr)
		      else predrange.lbound := range.hbound ;
		 firsttoken := false
		 end
	end;					(*big case*)
     if lowbdalso then
	  range.lbound := range.hbound;
     if (range.hbound < addrrange.lbound) or
	(range.hbound > addrrange.hbound) then
          if lowbdalso or (ldtemp^.ldkind = comma_ld) then
            error(qla1outrange)
          else error(qla2outrange);
     ldtemp := ldtemp^.next
     end;					(*while*)
if (range.lbound < searchrange.lbound) then error(qla1outrange)
else if (range.hbound > searchrange.hbound) then error(qla2outrange);
1: end.
$PAGE Externals and Command Processing Tables
(* QEDCL.PAS - modified 9/16/81 by djm to add SET CASE code borrowed from
               P. Lee.  *)
(*           - modified 9/17/81 by djm to replace the old MASK/UNMASK/
               PUSHESCAPE/ESCPOP/FIRESCAPE attention handling with the new
               Exception Handling constructs available in Pascal.  *)
(*           - modified 9/24/81 by djm to change chr(11b) to tab and
               chr(10) to lf. *)
(*           - modified 9/28/81 by djm to add $IF P10 switches around system
               dependent portions of 940 file handling code. *)
(*           - modified 10/01/81 by djm to add more $IF P10 switches around more
               system dependent portions of 940 file handling code. *)
(*           - modified 05/06/82 by djm to add the command function to
               simplify calls to the new lookup procedure.  Also cleaned
               up the mainline code before the case in qexecute, and the
               copy, move, transfer code. *)

CONST
tab = CHR(#O11);
lf = CHR(#O12);
cr = CHR(#O15);
esc = CHR(#O33);

$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qlabel.inc

external const
  qcmds: qcmdlist;
  sops: sub_opt_list;
  setparams: set_par_list;
  splitops: split_op_list;
  defrange: defrangelist;

type
  qdatarec =	(* SET and other parameter variables *)
    record
      linecount,	(* number of lines to print in a <cr> *)
      maxdel: qlineno;	(* maximum no. of lines to delete without confirm *)
      tabprint: boolean;	(* controls printing of tab character *)
      wildswitch: boolean;	(* enables/disables wildcarding *)
      openfileopen: boolean;	(* is the file open? *)
      s940: boolean;            (* if open file is 940 file   *)
      markstring: cmdline;	(* the mark string for BOUND matching *)
      lasterr: qerrcode;
      errlevel: cmdlineidx
    end;

static var
  qdata: qdatarec;
  openfile: text;
  openchan: wchannel;
  list_file: text;
  saved_bounds: boolean;			(* to push and pop buffer bounds *)
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
  var derr: qerrcode;				(* use local code since called from error reporting utils *)
      curline_save: qlineno;			(* save from qsetbounds resetting *)
  begin
    if saved_bounds then begin			(* called at errors, check if bounds pushed *)
      qunbound (buffer, derr);			(* save linenos are in terms of whole buffer *)
      if lbound_save <= hbound_save then		(* must be empty buffer - qunbound suffices to set
						   bounds properly; qsetbound fouls up *)
        begin
          buffer.offset := offset_save;
	  curline_save := buffer.curlineno;
	  qsetbounds (buffer, lbound_save, hbound_save, true, derr);
	  buffer.curlineno := curline_save
	end
    end;
    saved_bounds := false
  end;



public procedure qexecute
(       var buffer:     qbuffer;		(* working buffer *)
	line:           cmdline;		(* command line to parse *)
	var lindex:     cmdlineidx;		(* place marker *)
	var execrange:  ldrange;		(* limits of execution *)
	var ble:        qlineno;		(* bottommost line examined *)
	findflag:       boolean;		(* running under find? *)
	allowed_cmds:   qed_cmd_set;		(* which commands are legal? *)
	var err:        qerrcode);		(* anything wrong? *)

label 1, 2, 100;

const
  confirm_file := true;				(* new/old file prompting desired *)

var						(* parsing routine args, etc. *)
  nld:          ldcount;			(* number of la's in ld *)
  ld:           ldchain;			(* pointer to parsed ld linked list *)
  cmd:          qedcmds;			(* parsed command *)
  cmdrange:     ldrange;			(* value of parsed ld *)

var						(* command routine identifiers *)
  fid:          file_id;			(* for file reading and writing *)
  wmod:         wmodifier;                      (* 940 file modifier *)
  werr:         wcodes;                         (* I/O codes         *)
  rflag:        boolean;                        (* 940 revision flag *)
  pdp10id:      file_id;                        (* converted 940 file name *)
  cnt:          qlineno;			(* line count for APPENDs *)
  confirm,
  doit:         boolean;			(* for conditional tests throughout *)
  lp:           qlinep;				(* for debugging *)
  sop:		sub_options;			(* substitute storage *)
  sop_set:	sub_opt_set;			(* for option parsing *)
  splitop:	split_options;			(* split option parsing *)
  splitop_set:	split_opt_set;			(* ditto *)
  idx:		qlineno;			(* counter for running through buffer *)
  pat:		spattern;			(* pattern parsing for substitute, find *)
  predi:        spred;                         (* for SET parsing & bound *)
  repstr:	qstring;			(* replacement string in substitute request *)
  total:	qlineno;			(* to keep track of changes made *)
  source:	qstring;			(* place to keep text of looked-up line *)
  pos,
  stridx,
  len:		qstringidx;			(* indicies into QED-type strings *)
  fno,
  lno:		qlineno;			(* boundary markers *)
  tmprange:	ldrange;			(* for additional LD parsing *)
  findble:	qlineno;			(* for FIND to keep track with *)
  find_cmds:	qed_cmd_set;			(* legal commands under FIND *)
  optcmd:	qedcmds;			(* for option parsing in MOVE, COPY *)
  setopt:	set_params;			(* for SET parameter parsing *)
  have_file:	boolean;			(* true if file parameter present *)
  old_cmdrange: ldrange;			(* temp used in find command *)
  on_opt:	boolean;                              (* for on/off parsing *)
  joinstring:	qstring;			(* JOIN continuation mark *)
  qlabel:       qstring;                        (* _ command label variable *)
  displ:        qlineno;                        (* _ cmd label displacement *)
  match:	integer;			(* CMD_LOOKUP parameter *)
$PAGE Utilities
  procedure chkerr;
  begin
    if err <> qok then begin
      popbounds(buffer);
      goto 100
    end;
  end;						(* chkerr *)

  procedure seterr (newerr: qerrcode);
  begin
    err := newerr;
    chkerr
  end;						(* seterr *)


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
  end;						(* ck_extra_txt *)

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
  end (* parsenum *) ;

function parseonoff (var on_opt: boolean;   (* flag for on option *)
                     var err: qerrcode
                                    ): boolean; (* flag for good parse *)
var str: qstring;
begin
  parseonoff := false;
  skipblanks;
  str := '';

  while (lindex <= length(line)) do begin
    if line[lindex] <> ' ' then
      str := str||line[lindex];
    lindex := lindex +1
  end;     (* while *)

    if uppercase(str) = 'ON' then on_opt := true
      else if uppercase(str) = 'OFF' then on_opt := false
        else err := qbadparam;

    if err = qok then parseonoff := true
end;     (* parseonoff *)


  procedure chkrng (cmd: qedcmds; var nld: ldcount; var range: ldrange;
    findflag: boolean; var err: qerrcode);

    function decode (arg: rangetypes): qlineno;
    begin
      case arg of
	one:    decode := qfirst_val (buffer);
        dollar: decode := qdollar_val (buffer);
	dot:    decode := buffer.curlineno;
	dotp1:  decode := buffer.curlineno + 1;
	lb:     decode := cmdrange.lbound;
	lbp1:   decode := cmdrange.lbound + 1
      end					(* case *)
    end;					(* decode *)

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
	  nld := 1				(* suppress NLD=0 special cases when executing under
						   FIND, e.g. LIST *)
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
  end;						(* chkrng *)


  function numtochar (num: qlineno): qstring;
  var value: qlineno;
  begin
    numtochar := '';
    value := num;
    repeat
      numtochar := chr (ord ('0') + (value mod 10)) || numtochar;
      value := value div 10
    until value = 0
  end;						(* numtochar *)


  function discard_changes: boolean;
  begin
    if buffer.changes
      then discard_changes := query ('Unwritten changes, OK')
      else discard_changes := true
  end;


procedure close_open_file;
begin
if qdata.openfileopen              (* Don't do anything if nothing open! *)
then
  begin
  mask(attention);                            (* Until we can signal file closed    *)
  if qdata.s940                    (* Special close for 940 file         *)
  then
    begin
$IF P10
    wclose (openchan,werr);        (* Close file                         *)
    freechannel (openchan);        (* and release the channel            *)
    qdata.s940 := false
$END
    end
  else close (openfile);           (* Standard close for everybody else  *)
  qdata.openfileopen := false;     (* Leave the word                     *)
  unmask(attention)
  end
end;                               (* CLOSE_OPEN_FILE                    *)


function file_parameter (var fid: file_id; var wmod: wmodifier): boolean;
   var l: cmdlineidx;

function nextslug: file_id;          (* A convenience function to strip       *)
var     loc: cmdlineidx;             (* the next bunch of nonblank characters *)
begin
loc := verify (substr (line, lindex), [succ (' ')..pred (maximum (char))] - [';'],
        length (line) - lindex + 2);  (* Grab all printing nonblanks    *)
nextslug := substr (line, lindex, loc - 1);
lindex := lindex + loc -1            (* Adjust index over slug                *)
end;

   begin
    fid := '';
    wmod := '';                                 (* No initial 940 modifier    *)
    file_parameter := false;
    skipblanks;
    if lindex > length (line)                   (* nothing on line - don't check for ;  *)
    then return;                                (* On some systems, ; is part of filename  *)
    l := lindex;
    fid := nextslug;                            (* Isolate file name      *)
    skipblanks;
    wmod := nextslug;                           (* 940 modifier follows name  *)
    file_parameter := l <> lindex               (* If counter moved, something's here *)
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
      do_delete := (cnt < qdata.maxdel) orif query (numtochar (cnt) || ' lines, OK');
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


$PAGE Substitute Command
procedure substcmd;

var
     nth, n_par : qlineno;  (* variables for nth occurrance matching *)
     match : integer;

      begin
        if not parsenum(n_par, err) then n_par := 1;
        chkerr;
	if not spatparse (line, lindex, pat, qdata.wildswitch, err) then err := qbadsubst;
	chkerr;
	if ((pat.stype <> simple) and (pat.stype <> token)) andif (n_par <> 1) then
	  err := qbadnth;	(* Nth occurrance is illegal with ::,@@, or ## *)
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
	    buffer.curlineno := idx;		(* set for a matching line *)
	    chkerr;
	    if cnt <> 0 then begin		(* modify only if subs made *)
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
      end;					(* substitute *)
$PAGE Bound Command
    procedure boundcmd;
     
      var tempat: spattern;       (* save var for defpat on NEXT option *)
          next: boolean;          (* flag indicating a NEXT option *)
          backward: boolean;      (* flag for a backward search *)

      function nextparse: boolean;(* returns true on a NEXT option *)
      var next_str: qstring;      (* the word NEXT *)
          start: qlineno;         (* start position of option *)

      begin
        next_str := 'NEXT';
        nextparse := true;
        start := lindex - 1;

	(* take 'NEXT' or any abbreviation *)
        while ((lindex <= length(line)) and ((lindex-start) <= 4))
	  and (line[lindex] <> ';') do begin
          if (uppercase(line[lindex]) <> next_str[lindex-start]) then
            nextparse := false;
          lindex := lindex + 1
        end;
        if ((lindex-start) = 1) then nextparse := false; (* pointer didn't advance *)

        if nextparse then begin
          start := 1;
          nextparse := spredparse('::', start, predi, qdata.wildswitch, err);
          end
        else if ((lindex-start) <> 1) then err := qbadopt
						   else err := qok;

        next := nextparse

      end;     (* nextparse *)


      begin
	next := false;
	idx := buffer.curlineno + buffer.lbound - 1;	(* save curlineno in terms of unbounded linenos *)
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
	  if next then begin (* to save the defpat first *)
	    tempat := spatgetdefault;
	    if backward then
	      qmarkmatch (buffer, buffer.mark, predi, idx, fno, lno, backward, false, err)
	    else
	      qmarkmatch (buffer, buffer.mark, predi, idx+1, fno, lno, backward, false, err);
	    spatsetdefault(tempat);	(* restore defpat *)
	    spatdispose(tempat);	(* clean up *)
	    if (err = qbadln) then err := qnomark
	    end
	  else if backward then
		qmarkmatch (buffer, buffer.mark, predi, idx-1, fno, lno, backward, true, err) 
	        else qmarkmatch (buffer, buffer.mark, predi, idx+1, fno, lno, backward, true, err);
	  chkerr;
	  qsetbounds (buffer, fno, lno, true, err);
	  chkerr;
	  saved_bounds := false;			(* we have reset bounds after push *)
	  buffer.curlineno := qfirst_val (buffer)
	end
	else if nld = 0
	  then begin
	    if backward then err := qbadopt; (* no pattern or NEXT!! *)
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
	  (* a FALSE argument here for non-absolute line address on BOUND *)
	  qsetbounds (buffer, cmdrange.lbound, cmdrange.hbound, false, err);
	  chkerr;
	  buffer.curlineno := qfirst_val (buffer)
	end;

	(* Update the execrange bounds to permit accesses to the new
	   bounded section.  We assume here that execrange is never smaller
	   than the bounded section, unless bound is prohibited. *)

	execrange.lbound := 0;
	execrange.hbound := qdollar_val (buffer);
      end;					(* boundcmd *)
$PAGE Indent Command
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
	  buffer.curlineno := ln;			(* set so if error occurs, line is error line *)
	  source := qgetline (buffer, ln, err); chkerr;
	  pos := 1;  col := 1;	(* derive first non-white column *)
	  while (pos <= length (source)) andif (source[pos] <= ' ') do  begin
	    if source [pos] = ' '
	      then col := col + 1
	      else if source[pos] = tab then repeat col := col + 1 until (col mod 8) = 1;
	    pos := pos + 1
	  end;
	  if pos > length (source) then source := ''	(* line is blank, truncate *)
	  else begin
	    case way of					(* check if indentation okay *)
	      left:  if parm >= col then seterr (qtooshort)
		       else ind := col-parm-1;		(* and derive length of indentation *)
	      right: ind := col + parm - 1;
	      column: ind := parm
	    end;
	    indstr := '';				(* build indentation string *)
	    for i := 1 to (ind div 8) do indstr := indstr || tab;
	    indstr := indstr || substr ('        ', 1, ind mod 8);
	    if (length (source) - col + 1 + length (indstr)) > upperbound (source)
	      then seterr (qlnlong);
	    source := indstr || substr (source, pos);
	  end;
	  qmodline (buffer, ln, source, err); chkerr
	end
      end;
$PAGE command
function command (line : cmdline;
                  var lindex : cmdlineidx;
                  var cmd : qedcmds) : boolean;

begin
  command := true;
  if cmd_lookup (line, lindex, token_chars, qcmds, match) then
    cmd := qedcmds (match)
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
end (* command *);
PROCEDURE get_command;
BEGIN
  qldparse (line, lindex, nld, ld, qdata.wildswitch, err);  chkerr;
  if not command (line, lindex, cmd) then begin
    if (lindex > length (line)) orif (line[lindex] = ';') then begin
      if nld = 0 then begin
	if findflag then return;        (* but don't do anything under find *)
	if buffer.curlineno = qdollar_val (buffer) then
	  seterr (qbadln);		        (* nothing to print *)
	fno := buffer.curlineno + 1;	     (* get range of lines to print *)
	lno := buffer.curlineno + qdata.linecount;
	if lno > qdollar_val (buffer) then
	  lno := qdollar_val (buffer);          (* use remainder if too few *)
	qlistlines(buffer,fno,lno,ttyoutput,true,false,qdata.tabprint,err);
	chkerr;
	buffer.curlineno := lno;
	goto 2;		(* to exit interpretation loop *)
      end
      else
	cmd := print	(* Ld <eoln> - assume print and eval Ld normally *)
    end
    else seterr (qbadcmd);	 (* no known command name, not eoln *)
  end;
  if not (cmd in allowed_cmds) then  seterr (qnocmd);
  qldeval (buffer, ld, execrange, cmdrange, err);
  qlddispose (ld); chkerr;
  chkrng (cmd, nld, cmdrange, findflag, err);  chkerr;
  skipblanks
END;
FROG
$PAGE Begin Main Command Loop
begin (* begin body of qexecute *)
  saved_bounds := false;
1: (* each pass over following code parses and executes one command in  line*)
  get_command;
  case cmd of
    after, before: begin substcmd end;
    readcmd,append: begin
	if file_parameter (fid, wmod) then ;
	ck_extra_txt; do_append (fid, wmod, cmdrange.lbound)
    end;
    bound: begin boundcmd; END;
    change: begin
        if file_parameter(fid, wmod) then;
        ck_extra_txt;
	if do_delete (cmdrange) then 
	  do_append (fid, wmod, cmdrange.lbound - 1)
    end;
    delete: begin
        ck_extra_txt; if do_delete (cmdrange) then 
    end;
    closecmd: begin
        ck_extra_txt; close_open_file
    end;
    edit, modify:
      writeln(tty,'No longer implemented - use SUBSTITUTE');
    list: begin
        have_file := file_parameter(fid, wmod);
        if wmod <> ''
        then seterr (qbadfile);         (* Can't list to 940 file         *)
        ck_extra_txt;
	if have_file then begin
	  qopenfile (list_file, fid, '', qoutput_mode, [qio_confirm], err);
	  chkerr
	end
	else list_file:= ttyoutput;
	(* listing of lines within section *)
	if nld > 0 then begin
	  qlistlines (buffer, cmdrange.lbound, cmdrange.hbound, list_file,
	    false, true, true, err);
          chkerr;
	  buffer.curlineno := cmdrange.hbound
	end
	(* list entire file, numbered by section *)
	else begin
	  pushbounds(buffer, err); (* save current bounds *)
	  idx := 0; fno := 1;
	  loop (* one pass certain, since no addr error => nonzero line cnt *)
	    repeat idx := idx + 1
	    until (idx > qdollar_val (buffer)) orif
		    spredmatch (qgetline (buffer, idx, err), buffer.mark, err);
	    lno := idx - 1;
	    if lno >= fno then begin
		(* may have zero length section at start *)
	      qsetbounds (buffer, fno, lno, true, err);   
	(* address new region *)
	      chkerr;
	      if fno <> 1 then begin 
		(* page between parts, not at begin and end *)
		page (list_file);
		write(list_file, lf)	
		end;
	      qlistlines(buffer,1,lno-fno+1,list_file,false, true, true, err);
              chkerr;
	      qunbound (buffer, err); chkerr;
	    end;
	  exit if idx > qdollar_val (buffer);
	    fno := idx
	  end;
	  popbounds(buffer);
	end;
	if list_file <> ttyoutput then  (* don't close the teletype !! *)
	  close (list_file)
    end;
    find: begin
	find_cmds := allowed_cmds - [why, quit, exitcmd, resetcmd, load, 
	  save, writecmd, opencmd, closecmd, setcmd, bound, uparrow];
	cnt := 0;
	if not spredparse (line, lindex, predi, qdata.wildswitch, err) 
	then err := qnofindpred;
	chkerr;
	confirm := query ('Confirm');
	idx := cmdrange.lbound;
	while idx <= cmdrange.hbound do
	begin
	  source := qgetline (buffer, idx, err);
	  chkerr;
	  if spredmatch (source, predi, err) then begin
	    pat := spatgetdefault;  findble := idx;
	    buffer.curlineno := idx;		(* set if matched *)
	    doit := true;
	    if confirm then begin
	      writeln (tty, source);    doit := query ('OK')
	    end;
	    if doit then  begin
	      cnt := cnt + 1;	(* count confirmed matches, not executions *)
	      stridx := lindex;
              old_cmdrange := cmdrange;
	      qexecute (buffer, line, stridx, cmdrange, findble, true,
		 find_cmds, err);
	      execrange.hbound := execrange.hbound + cmdrange.hbound -
				  old_cmdrange.hbound;
	      spatsetdefault (pat);    spatdispose (pat);
	      buffer.curlineno := findble;    chkerr;
	    end;
	    cmdrange.lbound := findble + 1;  idx := cmdrange.lbound;
	  end
	  else begin  chkerr;  idx := idx + 1 end
	end;
	spreddispose (predi);
        if confirm 
          then write (tty, 'Found and confirmed ')
          else write (tty, 'Found ');
        write (tty, numtochar (cnt), ' time' );
        if cnt <> 1 then write ( tty, 's' );
        writeln ( tty, '.' );
	lindex := length (line) + 1;		(* find uses rest of line *)
    end;					(* find *)
    gotocmd:   begin
        ck_extra_txt;
        buffer.curlineno := cmdrange.lbound
      end;
    insert:   begin
	if file_parameter (fid, wmod) then ;
        ck_extra_txt;
	do_append (fid, wmod, cmdrange.lbound - 1)
      end;
    indent: indentcmd;
    join:   begin
	if not spatparse (line, lindex, pat, false, err)
	  then joinstring := ''
	  else joinstring := pat.list^.sstring;
	chkerr;
        ck_extra_txt;
	total := cmdrange.hbound - cmdrange.lbound + 1;
	if (total <= 2) orif query (numtochar(total) || ' lines, OK') 
	then begin
	  qjoinlines(buffer,cmdrange.lbound, cmdrange.hbound, joinstring, err);
	  chkerr;
	  with cmdrange do begin
	    buffer.curlineno := lbound;
	    execrange.hbound := execrange.hbound - (hbound - lbound);
	    if ble > hbound
	      then ble := ble - (hbound - lbound)
	      else if ble >= lbound then ble := lbound
	  end
	end
      end;					(* join *)
    load:    begin
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
    move,transfer,copy:
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
	qlddispose (ld);			(* take care to dispose even if errors *)
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
		ble := lbound - 1;	(* in range moved *)
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
      end;					(* move *)

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
	begin                                    (* Something was specified *)
	ck_extra_txt;
	close_open_file;                         (* Get rid of stragglers   *)
$IF P10
	wfileconvert (fid,wmod,pdp10id, werr, rflag);  (* Get proper file ID *)
	if werr = wbadfile
	then seterr (qbadfile);                  (* Note that SETERR doesn't return *)
$END
	qopenfile (openfile, fid, '', qoutput_mode, [qio_confirm], err);
	chkerr;
$IF P10
	if werr = wok
	then
	  begin                                  (* A 940 file.  Use special open routine *)
	  close (openfile);
	  openchan := getchannel;
	  wopen (openchan, werr, woutput, pdp10id);
	  if werr <> wok                         (* Trouble unlikely, but possible *)
	  then
	    begin
	    freechannel (openchan);              (* Unwind the mess, if necessary *)
	    seterr (qbadfile)                    (* This will rip us out   *)
	    end
	  else qdata.s940 := true
	  end
	else qdata.s940 := false;
$END
	qdata.openfileopen := true               (* Did it.  Remember the word  *)
	end
      else seterr (qnofile)
      end;                                       (* opencmd *)

    outputcmd:
      begin
      ck_extra_txt;
      if qdata.openfileopen
      then
        begin
        if qdata.s940
        then
          begin                                 (* Do the output here for 940 files  *)
$IF P10
          idx := cmdrange.lbound;
          while (err = qok) and (idx <= cmdrange.hbound)
          do
            begin
            source := qgetline (buffer, idx, err);
            if err = qok
            then
              begin
              mask(attention);
              woutline (openchan, werr, source);   (* One line at a time   *)
              unmask(attention);
              if werr <> wok
              then err := qwrterr
              else idx := idx + 1
              end
            end
$END
          end
        else qlistlines (buffer, cmdrange.lbound, cmdrange.hbound, openfile, false, false,
              false, err)                          (* Do it elsewhere for PDP-10 files *)
        end
      else err := qnotopen;
      chkerr
      end;                                         (* outputcmd   *)

    print:
      begin
        ck_extra_txt;
	qlistlines (buffer, cmdrange.lbound, cmdrange.hbound, ttyoutput, true, false, qdata.tabprint, err);
        chkerr;
	buffer.curlineno := cmdrange.hbound
      end;					(* print *)

    quit,
    exitcmd:
      begin
        ck_extra_txt;
	err := qquit				(* caller must decide whether to discard changes *)
      end;

    resetcmd:
      begin
        ck_extra_txt;
	if discard_changes then begin
	  close_open_file;
	  qdelbuf (buffer);
	  qinitexec (buffer)
	end
      end;					(* reset *)

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
	    end;				(* del_param *)

	  lcnt_param:
	    begin
	      if not parsenum (idx, err) then seterr (qnoparamval);
              ck_extra_txt;
	      qdata.linecount := idx
	    end;				(* lcnt_param *)

	  mark_param:
	    begin
	      if not spredparse (line, lindex, predi, qdata.wildswitch, err) then seterr(qnoparamval);
              ck_extra_txt;
	      spreddispose (buffer.mark);
	      buffer.mark := predi
	    end;					(* mark_param *)

          tab_param:
            begin
              if not parseonoff(on_opt, err) then seterr(err)
              else if on_opt then qdata.tabprint := true
                   else qdata.tabprint := false
            end;               (* tab_param *)

	  wild_param:
	    begin
		if not parseonoff(on_opt, err) then seterr(err)
		else if on_opt then qdata.wildswitch := true
		     else qdata.wildswitch := false
	    end;			(* wild_param *)

	  case_param:
	    begin
		if not parseonoff(on_opt, err) then seterr(err)
		else qsetcase(on_opt)
	    end
	end					(* case setopt *)
	end
	else seterr (qbadparam)
      end;					(* setcmd *)

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
	repeat					(* loop over cmdrange, adjusting hbound for splits *)
	  if qsplitlines (buffer, idx, pat, splitop_set, cnt, err)
	    then begin
	      if ble > idx then ble := ble + cnt;
	      execrange.hbound := execrange.hbound + cnt;
	      cmdrange.hbound := cmdrange.hbound + cnt;
	      idx := idx + cnt;			(* this adjusts for splits, does not increment *)
	      total := total + cnt;
	      buffer.curlineno := idx;
	    end;
	  chkerr;
	  idx := idx + 1
	until idx > cmdrange.hbound;
	if (cmdrange.lbound <> (cmdrange.hbound - total)) or (total = 0) or (all_splitop in splitop_set)
	  then print_times (total)
      end;					(* qsplit *)

    substitute:
      begin
	substcmd
      end;

    underbar:                                  (* An ugly, UGLY hack for linkwriters  *)
      begin
      ck_extra_txt;
      if qlabelfind (buffer, cmdrange.lbound, qfirst_val (buffer), qlabel,
              displ, err)
      then
        begin                                  (* Found a label.  Type line identification  *)
        buffer.curlineno := cmdrange.lbound;   (* Set the current line  *)
        write (tty, ':', qlabel, ':');
        if displ <> 0
        then writeln (tty, '+', numtochar (displ))
        else writeln (tty)
        end
      else seterr (qnolabel)
      end;                                     (* Ugly _     *)

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
      end;					(* uparrow *)

    why:
      with qdata do
      begin
        ck_extra_txt;
	errlevel := errlevel + 1;
	qederror (ttyoutput, lasterr, errlevel);
      end;					(* why *)

    writecmd,
    save:
      begin
	if not file_parameter (fid, wmod) then begin
	  if buffer.curfileok
	    then begin
	      fid := buffer.curfile;
	      if buffer.s940
	      then wmod := '*'                   (* Flag saved 940 file name   *)
	      end
	    else seterr (qbadfile)
	end;
        ck_extra_txt;
	if nld > 0
	  then qfilewrite (buffer, fid, wmod, cmdrange.lbound, cmdrange.hbound, confirm_file,
                  err)
	  else begin				(* assume user wants whole file *)
	    pushbounds(buffer, err);
	    qfilewrite (buffer, fid, wmod, 1, qdollar_val (buffer), confirm_file, err);
	    popbounds(buffer)
	  end;
          chkerr
      end					(* write/save *)

  end;						(* case *)
$PAGE End Main Command Loop
2:
  if buffer.curlineno > ble then ble := buffer.curlineno;
  if lindex <= length (line) then
    if line[lindex] = ';' then
    begin
      lindex := lindex + 1;
      goto 1
    end;
100:
  if (err <> qok) and (err <> qquit) then begin	(* save error code for why *)
    qdata.lasterr := err;
    qdata.errlevel := 1
  end;
end;						(* qexecute *)

$SYSTEM VERSIO.INC
$PAGE VERSIO.INC, last modified 5/11/84, zw

TYPE
version_string = STRING [15];
EXTERNAL
FUNCTION version: version_string;
$SYSTEM filutl.inc
(**********  FILUTL.inc  last modified 6/19/78 **********)
(* PR FILE ID extracts a file title form an input string. If the title parses
   correctly, file_id information is set, the string cursor is advanced past
   the title and true is returned.  If the title is incorrectly formed, false
   is returned, and the cursor is left pointing to the character which is in
   error.  The file_id information is not changed. *)
external function pr_file_id
	    (	line: cmdline; var idx: cmdlineidx;
		var fid: file_name		      ): boolean;
(* OPEN FILE opens a text file for input or output. The mode is specified by
   the caller. For an output file the user can also request append mode and
   old/new file prompting. The caller may supply a default extension to be used
   in the file_id if none appears.  A flag is returned indicating if the open
   was successful. *)
type
  io_mode = ( input_mode, output_mode );
  io_option = ( append_mode, confirm_open );
  io_option_set = set of io_option;
external function open_file
	    (   var f: text;
		fid: file_id;
		ext: extension;
		mode: io_mode;
		option_set: io_option_set   ): boolean;
(* PATHNAME returns the actual file_id of an opened PASCAL file.  This file_id
   reflects any directory searching in effect. *)
external function pathname  ( var f: text ):  file_id;
$SYSTEM infpac.inc
(*---------------------------------------------------------------------------*)
(*
 *  INCLUDE FILE DEFINING TYPES AND ROUTINES FOR PASCAL
 *  ENVIRONMENTAL INQUIRY PACKAGE.
 *)

CONST
   MAXADDR = 777777B;

TYPE
   SEGPPN = STRING[15];
   SEGNAM = STRING[6];
   USRNAME = STRING[12];
   SEGRECD = RECORD
      LOWLEN: 0..MAXADDR;
      HIGHLEN: 0..MAXADDR;	(* ZERO IF NO HIGH SEGMENT *)
      RDONLY: BOOLEAN;		
      SHARABLE: BOOLEAN
   END;
   USTATREC = RECORD
      CRUS60: 0..MAXIMUM(INTEGER);	(* DIVIDE BY 60 TO GET CRUS *)
      DISKRDS: 0..MAXIMUM(INTEGER);	
      DISKWRS: 0..MAXIMUM(INTEGER);
      RUNTIME: 0..MAXIMUM(INTEGER);	(* 80 NANOSECOND UNITS *)
      ELAPTIME: 0..MAXIMUM(INTEGER)	(* SECONDS *)
   END;
   JOBREC = RECORD
      HSEGPPN: SEGPPN;
      HSEGNAM: SEGNAM;
      LSEGPPN: SEGPPN;
      LSEGNAM: SEGNAM;
      PROGDIR: SEGPPN; 		(* DIRECTORY PROGRAM IS RUN FROM *)
      JOBNUM: 0..MAXIMUM(INTEGER);
      PROJECTID: USRNAME
   END;
   SSTATREC = RECORD
      NLOGIN: 0..MAXIMUM(INTEGER);	(* NUMBER OF LOGGED IN USERS *)
      MAXCOR: 0..MAXADDR	(* MAXIMUM CORE AVAILABLE TO ANY JOB *)
   END;

EXTERNAL PROCEDURE SEGINFO(VAR SEGSTUFF: SEGRECD);
EXTERNAL PROCEDURE USRSTAT(VAR USRSTATS: USTATREC);
EXTERNAL PROCEDURE JOBINFO(VAR JOBSTUFF: JOBREC);
EXTERNAL PROCEDURE SYSSTAT(VAR SYSSTATS: SSTATREC);
(*---------------------------------------------------------------------------*)
$SYSTEM wio.inc
(* external procedures called by qed for 940 conversion work *)

	(* routine to convert string containing some sort of 940 file name
	   to equivalent PDP-10 file name.  If modifier specifies that file
	   is actually PDP-10 file, wtenfile is returned.  If conversion
	   fails, wbadname is returned, else wok. *)


EXTERNAL PROCEDURE WFILECONVERT( FILE_ID;	(*name to convert*)
				 WMODIFIER;	(*string indicating conversion*)
				 VAR FILE_ID;	(*converted name*)
				 VAR WCODES;	(*return code*)
				 VAR BOOLEAN);	(*add r for revision if true *)

	(* routine to open a 940 file for I/O *)

EXTERNAL PROCEDURE WOPEN( WCHANNEL;		(*channel on which to open file*)
			  VAR WCODES;		(*wok=success; wbadfile=failure*)
			  WIOMODES;		(*direction of data transfer*)
			  VAR FILE_ID);		(*PDP-10 file name*)

	(* routine to close a 940 file *)

EXTERNAL PROCEDURE WCLOSE( WCHANNEL;		(*channel on which to close file*)
			   VAR WCODES);		(*wok=success, wouterror=failure*)

	(* routine to read a line from 940 file *)

EXTERNAL PROCEDURE WINLINE( WCHANNEL;		(*channel to read from*)
			    VAR WCODES;		(*wok=success, winerror=failure,
						  weof=end of file*)
			    VAR QSTRING);	(*line read (no cr on end)*)

	(* routine to write a line *)

EXTERNAL PROCEDURE WOUTLINE( WCHANNEL;		(*channel to write to*)
			     VAR WCODES;	(*wok=success; wouterror=failure*)
			     QSTRING);		(*line to write (w/o final cr)*)

	(* routine to return PDP-10 file name open on channel *)

EXTERNAL PROCEDURE WFILENAME( WCHANNEL;
			      VAR FILE_ID;	(*name of file*)
				VAR BOOLEAN);

(**********************************************************************)

(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
(* CMDUTL.typ - last modified 8/13/77 - type declarations for command 
   utility subroutines. *)


TYPE						(* system dependent file name types *)
  FILE_ID = STRING[75];				(* maximum TOPS-10 filename with SFD's *)
  EXTENSION = PACKED ARRAY [1..3] OF CHAR;


CONST CMDLINELEN = 254;				(* command line declaration *)
TYPE
  CMDLINE = STRING[254];			(* string itself *)
  CMDLINEIDX = 0..255;				(* index of above *)

$SYSTEM cmdutl.inc
$PAGE CMDUTL.INC, last modified 5/11/84, zw

TYPE
cmd_lookup_record = RECORD
  TEXT: PACKED ARRAY [1..10] OF CHAR;
  abbrev: 1 .. 10;
  code: INTEGER;
END;
EXTERNAL
PROCEDURE cmd_skip_blanks ( PACKED ARRAY [1..*] OF CHAR; VAR INTEGER );
EXTERNAL
FUNCTION cmd_check_punct ( PACKED ARRAY [1..*] OF CHAR; VAR INTEGER; CHAR )
  : BOOLEAN;
EXTERNAL
FUNCTION cmd_token ( PACKED ARRAY [1..*] OF CHAR; VAR INTEGER; SET OF CHAR;
  VAR STRING [*] ): BOOLEAN;
EXTERNAL
FUNCTION cmd_number ( PACKED ARRAY [1..*] OF CHAR; VAR INTEGER; BOOLEAN;
  VAR INTEGER ): BOOLEAN;
EXTERNAL
FUNCTION cmd_lookup ( PACKED ARRAY [1..*] OF CHAR; VAR INTEGER; SET OF CHAR;
  ARRAY [1..*] OF cmd_lookup_record; VAR INTEGER ): BOOLEAN;
EXTERNAL
FUNCTION cmd_string ( PACKED ARRAY [1..*] OF CHAR; VAR INTEGER; CHAR;
  VAR STRING [*] ): BOOLEAN;
EXTERNAL
FUNCTION cmd_dqstring ( PACKED ARRAY [1..*] OF CHAR; VAR INTEGER; CHAR;
  VAR STRING [*] ): BOOLEAN;
EXTERNAL
FUNCTION cmd_file_name ( PACKED ARRAY [1..*] OF CHAR; VAR INTEGER; BOOLEAN;
  VAR STRING [*] ): BOOLEAN;
EXTERNAL
FUNCTION cmd_eol ( PACKED ARRAY [1..*] OF CHAR; VAR INTEGER ): BOOLEAN;
EXTERNAL
FUNCTION cmd_query ( PACKED ARRAY [1..*] OF CHAR;
  PACKED ARRAY [1..*] OF CHAR ): BOOLEAN;
EXTERNAL
PROCEDURE cmd_getline ( PACKED ARRAY [1..*] OF CHAR; VAR STRING [*];
  VAR INTEGER );
EXTERNAL
PROCEDURE cmd_display_table ( ARRAY [1..*] OF cmd_lookup_record; INTEGER;
  INTEGER );
$SYSTEM query.inc

(* QUERY asks a caller supplied question, and checks for a yes or no reply.
   Y, YES, NO, N, or <eoln> meaning yes are accepted as replys. Also,
   REPEAT is accepted as a request to repeat the question.  If no such 
   replies are received, another response is requested and processed. Notes:
   This routine appends a question mark to the question. It is assumed that
   the terminal is in the desired column (i.e. one) when called. *)

type query_string = string[256];

external function query (question: query_string): boolean;

$SYSTEM wio.typ
(*  Declarations for QED routines to read and write 940 style files *)
(*  RLD:CMDUTL.TYP must be $INCLUDEd previously to define "file_id" *)
(*  RND:QSTR.TYP must be $INCLUDEd previously to define "qstring"   *)

TYPE WCHANNEL = 0..15;				(* channel number obtained from Pascal runtime *)

     (* error codes returned by routines in this package *)

     WCODES = (WOK, WTENFILE, WBADNAME, WBADFILE, WEOF, WINERROR, WOUTERROR);

     (* following type is for the file name modifier wtring passed to
	wfileconvert.  Its length is 6 to force the old compiler to
	pass it by address for compatibility with the new compiler *)

     WMODIFIER = STRING[6];

     (* argument to wopen specifying data transfer direction *)

     WIOMODES = (WINPUT, WOUTPUT);
$SYSTEM qerr.typ
(* ERROR CODES *)
(* ----------- *)

TYPE
 QERRCODE = (
  QOK,						(*NORMAL RETURN*)
  QFATAL,					(*INTERNAL ERROR -- FATAL SITUTATION*)
  QBADLN,					(*NON-EXISTANT LINE NUMBER PASSED*)
  QBADLB,					(*NON-EXISTANT LOWER BOUND PASSED*)
  QBADUB,					(*NON-EXISTANT UPPER BOUND PASSED*)
  QBADRN,					(*LOWER BOUND > UPPER BOUND*)
  QBADARGNO,					(* WRONG NUMBER OF LA'S IN LD *)
  QBADCMD,					(* BAD COMMAND NAME PARSED *)
  QNOCMD,					(* CMD INVALID IN CONTEXT USED *)
  QBADFILE,					(* BAD FILENAME PARSED *)
  QNOFILE,					(*NON-EXISTANT FILE*)
  QNOCLOSE,					(*NO CLOSING DELIMITER IN STRING PATTERN*)
  QNODEFAULT,					(*DEFAULT PATTERN HAS NOT BEED DEFINED*)
  QNO2NDLA,					(*NO SECOND LINE ADDRESS AFTER COMMA *)
  QNOINPAREN,					(*INVALID FIELD WITHIN PARENS OF STRING PREDICATE*)
  QNOCLOSPAREN,					(*NO CLOSING PARENTHESIS*)
  QNONOT_OP,					(*INVALID FIELD AFTER "NOT" OPERATOR*)
  QNORTOP,					(*INVALID FIELD AFTER "AND" OR "OR" OPERATOR*)
  QOFFIRST,					(* SIGNED OFFSET FOUND AS FIRST PART OF LA*)
  QNONUMF,					(*NO NUMERIC FIELD FOUND AFTER '+' OR '-'*)
  QONLYFIRST,					(*  '*','$', OR '.' FOUND NOT AS FIRST PART OF LA*)
  QNOPREVIOUS,					(*  '*' ONLY ALLOWED IN SECOND HALF OF LD *)
  QSPNOTFND,					(* STRING PREDICATE NOT FOUND IN RANGE *)
  QOUTRANGE,					(* EVALUATED LD OUT OF BUFFER OR SPECIAL RANGE*)
  QSTUPID,					(* UNMATCHABLE PREDICATE DETECTED IN PARSE *)
  QLNLONG,					(*LINE TOO LONG, FROM SPLIT, SUBSTITUTE*)
  QQUIT,					(* QUIT COMMAND GIVEN *)
  QNOMARK,					(* NO LINE MATCHING MARKSTRING WAS FOUND *)
  QBADSUBST,					(* SYNTAX ERROR IN SUBSTITUTE PATTERNS *)
  QBADOPT,					(* BAD OPTION WAS PARSED *)
  QEXTRATXT,					(* EXTRANEOUS TEXT FOLLOWS COMMAND *)
  QTOOBIG,					(* INTEGER TOO BIG TO BE PARSED AS LINE NUMBER *)
  QNOFINDPRED,					(* NO SPATTERN GIVEN IN FIND COMMAND *)
  QNOMOVELA,					(* 1 LA EXPECTED AFTER MOVE, 0 OR 2 FOUND *)
  QBADMOVELA,					(* LA WAS WITHIN TEXT BEING MOVED *)
  QNOTOPEN,					(* OUTPUT COMMAND WAS ISSUED WITH NO OPEN FILE *)
  QBADPARAM,					(* MISSING OR INVALID SET PARAMETER *)
  QNOPARAMVAL,					(* NO VALUE GIVEN FOR SET PARAMETER *)
  QJOINTOOFEW,					(* FEWER THAN TWO LINES GIVEN TO JOIN *)
  QNOSPLITPAT,					(* INVALID OR MISSING PATTERN FOR SPLIT *)
  QEMPTY,					(* BUFFER IS EMPTY *)
  QTOOSHORT,					(* NOT ENOUGH LEADING SPACE TO INDENT *)
  QWRTERR,					(* EOF(F) FALSE AFTER WRITE STATEMENT *)
  QLA1NOTFND,					(* SPRED IN FIRST LA NOT FOUND *)
  QLA2NOTFND,					(* SPRED IN SECOND LA NOT FOUND *)
  QLA1OUTRANGE,					(* FIRST LA OUT OF RANGE *)
  QLA2OUTRANGE,					(* SECOND LA OUT OF RANGE *)
  QHEAPFULL,					(* HEAP OVERFLOW *)
  QBADNTH,					(* BAD Nth OCCURRANCE PATTERN *)
  QNOINDNO,					(* AMOUNT TO INDENT NOT SPECIFIED *)
  QMOVOUTRANGE,					(* DESTINATION OF MOVE OUT OF RANGE *)
  QNOINFILE,					(* FAILURE TO OPEN INPUT FILE *)
  QNOOUTFILE,					(* FAILURE TO OPEN OUTPUT FILE *)
  QNOCOPYLA,					(* 1 LA EXPECTED AFTER COPY, 0 OR 2 FOUND *)
  QNOLABEL);					(* No label found for _ kludge	*)
$SYSTEM qstr.typ
CONST QSTRINGLEN = 254;

TYPE
  QSTRING = STRING [QSTRINGLEN];
  QSTRINGIDX = 0..255;
$SYSTEM qspat.typ
(* STRING PATTERN definitions represent a parsed QED string matching pattern.
   Details may be found in the 'Definition of QED.' *)

TYPE
  SPATTERN_FORMS =				(* AS DETERMINED FROM DELIMITER *)
    (	SIMPLE,					(* MATCH ANYWHERE WITHIN LINE *)
	LEFT,					(* MATCHES AT THE BEGINNING OF THE LINE *)
	RIGHT,					(* MATCHES AT THE END OF THE LINE *)
	LEADSTRING,				(* MATCH CHARS FOLLOWING LEADING WHITESPACE *)
	TOKEN,					(* MATCH A DELIMITED STRING *)
	NULLSPAT	);			(* REPRESENTS INVALID DEFAULT PATTERN *)

  SIDE_TYPE = (
	LEFTSIDE,				(* STAR ON THE LEFT SIDE OF PATTERN *)
	RIGHTSIDE,				(* ETC. *)
	BOTHSIDES,
	NEITHER	);

  SPATTERN =
    RECORD					(* INTERNAL REPRSENTATION OF A STRING PATTERN *)
      STYPE: SPATTERN_FORMS;			(* MATCHING BOUNDARY CONDITIONS *)
      WILDCARD: BOOLEAN;			(* TRUE IF STRING CONTAINS A '*' *)
      SIDESTARS: SIDE_TYPE;			(* POSITIONS OF SIDE STARS *)
      SLENGTH: QSTRINGIDX;			(* LENGTH OF PATTERN MINUS WILDCARDS *)
      LIST: ^SPATREC;				(* POINTER TO HEAD OF LIST OF PATTERNS *)
      LASTREC: ^SPATREC				(* POINTER TO LAST TOKEN IN PATTERN LIST*)
    END;

  SPATLIST = ^SPATREC;

  SPATREC =
    RECORD
	WILDCHAR: BOOLEAN;			(* TRUE IF STRING CONTAINS A '?' *)
	SSTRING: QSTRING;			(* STRING TO BE MATCHED *)
	NEXT: SPATLIST;				(* POINTER TO NEXT PATTERN TOKEN *)
	PREVTOK: SPATLIST			(* POINTER TO PREVIOUS PATTERN TOKEN *)
    END;
$SYSTEM qspred.typ
(* STRING PREDICATE definitions reprenting a parsed QED string predicated.
   For details see the 'Definition of QED'. *)

TYPE
  SPRED_KINDS =
    (	PATTERN_SPOP,				(* MATCH A LINE CONTAINING PATTERN *)
	NOT_SPOP,				(* MATCH A LINE NOT MATCHING PREDICATE OPERAND *)
	AND_SPOP,				(* MATCH A LINE MATCHING BOTH PREDICATE OPERANDS *)
	OR_SPOP		);			(* MATCH A LINE MATCHING EITHER PREDICATE OPERANDS *)

  SPRED = ^ SPRED_NODE;				(* REPRESENTED AS A TREE OF NODES *)
  SPRED_NODE =
    RECORD
      CASE PREDKIND: SPRED_KINDS OF
	NOT_SPOP:				(* NOT <NOPER> *)
	      (  NOPER: SPRED  );
	AND_SPOP, OR_SPOP:			(* <LOPER> OP <ROPER> *)
	      (  LOPER, ROPER: SPRED  );
	PATTERN_SPOP:
	      (  PATTERN: SPATTERN  )
    END;

  TOKTYP = (AND_TOK, OR_TOK, NOT_TOK);
$SYSTEM qedln.typ
(********************************************************************* *)

		      (* QED type definitions *)
		      (* -------------------- *)


CONST QMAXLINES = 99999;			(* maximum line number *)

TYPE
  QLINEP = ^QLINE;				(* ptr to line descriptor *)
  QLINENO = 0..QMAXLINES;			(* QED line number *)
  QLNOFFSET = -99999..QMAXLINES;		(* line offset *)

  (* BUFFER DESCRIPTOR - callers should only access the following fields:
     curlineno, lbound, hbound, lastlineno, curfile, curfileok, mark, changes.
     All other fields are implementation specific. *)

  QBUFFER = RECORD
    FIRSTLINEP,					(* ptr to dummy 0th line desc *)
    LASTLINEP,					(* to last line desc *)
    GETLINEP: QLINEP;				(* ptr to last line gotten *)
    GETLINENO: QLINENO;				(* line number of last line gotten *)
    CURLINENO,					(* current line number *)
    LASTLINENO: QLINENO;			(* last line number *)
    GARBLINEP: QLINEP;				(* lines to be deleted *)
    LBOUND,
    HBOUND: QLINENO;				(* bounded range *)
    LBOUNDP,
    HBOUNDP: QLINEP;				(* pointers to bounds *)
    OFFSET: QLINENO;				(* ANC line bounding offset *)
    OLDOFFSET: QLINENO;				(* offset previous to above *)
    MARK: SPRED;				(* boundary mark *)
    CURFILE: FILE_ID;				(* name of defaul file *)
    CURFILEOK: BOOLEAN;				(* valid filename in above ? *)
    CHANGES: BOOLEAN;				(* unwritten changes in buffer *)
    S940: BOOLEAN				(* true if current file is 940 file	*)
  END;

(********************************************************************* *)
$SYSTEM qline.typ
(* QLINE.TYP - last modified 9/21/81 by djm to remove $IF ANC tag field from
               this version.  The field is still present in the ANC version. *)

TYPE
  QLINE = PACKED RECORD				(* QED line descriptor *)
    PREVLINEP,					(* previous line pointer *)
    NEXTLINEP: QLINEP;				(* next line pointer *)
    SOURCE: QSTRING				(* text of line *)
  END;						(* note: users of QED routines
						should never mess with qlines *)
$SYSTEM qld.typ
(* LINE DESIGNATOR defintions.  An LD is represented as a linked list of nodes
   giving the atomic parts of a line designator such as predicates, symbolic
   addresses, and punctuation. Details may be found in the 'Definition of QED.' *)


TYPE
  LD_FORMS =
     (	NUM_LD,					(* LINE OFFSET FROM LAST ADDRESSED LINE *)
	FORWARD_LD,				(* ADDRESS SPECIFIED BY PREDICATE *)
	BACKWARD_LD,				(* ADDRESS SPECIFIED BY ^PREDICATE *)
	DOT_LD,					(* CURRENT LINE *)
	DOLLAR_LD,				(* LAST LINE *)
	STAR_LD,				(* FIRST LINE OF RANGE *)
	COMMA_LD	);			(* RANGE SEPARATOR *)


  LDCHAIN = ^ LDPART;				(* NODE ON LDCHAIN LIST *)
  LDPART =
    RECORD
      NEXT: LDCHAIN;				(* POINTER TO NEXT NODE ON LIST, OR NIL *)
      CASE LDKIND : LD_FORMS OF			(* VARIOUS ATOMIC PARTS *)
	NUM_LD:	     (  OFFSET: QLNOFFSET  );	(* OFFSET OR ABSOLUTE LINENO *)
	FORWARD_LD,BACKWARD_LD:  (  PRED: SPRED  ); (* PREDICATES TO SEARCH FOR *)
	DOT_LD,DOLLAR_LD,STAR_LD,COMMA_LD: ()
    END;

  LDCOUNT = 0..2;				(* NUMBER OF ADDRESSES IN AN LD *)
  LDRANGE =					(* EVALUATED LD CHAIN, GIVES LIMITS OF RANGE *)
    RECORD
      LBOUND: QLINENO;				(* FIRST LINE ADDRESSED *)
      HBOUND: QLINENO				(* SECOND LINE, SAME AS FIRST IF LD NOT RANGE *)
    END;
$SYSTEM qed.typ
$PAGE QED.TYP, last modified 2/16/84, zw

TYPE
qedcmds =
  (append, change, delete, insert, edit, modify, load, print, substitute,
  after, before, writecmd, save, find, gotocmd, resetcmd, join, copy,
  move, transfer, bound, list, eqcmd, number, opencmd, outputcmd, closecmd,
  setcmd, split, quit, exitcmd, uparrow, why, indent, underbar, readcmd);
qed_cmd_set = SET OF qedcmds;
set_params =
  (del_param, lcnt_param, mark_param, tab_param, wild_param, case_param);
set_param_set = SET OF set_params;
$SYSTEM qsplit.typ
TYPE
  SPLIT_OPTIONS = (NUMBER_SPLITOP, CONFIRM_SPLITOP, PRINT_SPLITOP,
			ALL_SPLITOP, DELETE_SPLITOP);

  SPLIT_OPT_SET = SET OF SPLIT_OPTIONS;
$SYSTEM qsubst.typ
(* QSUBST.TYP - created 04/30/82 by djm *)

type
  sub_options =
    ( confirm_sop, all_sop, print_sop, number_sop  );

  sub_opt_set = set of sub_options;
$SYSTEM qedtyp.typ
(* QEDTYP.TYP - created 04/29/82 by djm *)

const
  token_chars : set of char = ['A' .. 'Z'];

type
  rangetypes = (one, dollar, dot, dotp1, lb, lbp1);

  rangelist = record
      lbound, hbound1, hbound2: rangetypes;
      required, permitted: 0..2
  end;

  cmd_range = 1 .. ord (maximum (qedcmds)) + 1;
  sub_opt_range = 1 .. ord (maximum (sub_options)) + 1;
  set_param_range = 1 .. ord (maximum (set_params)) + 1;
  split_opt_range = 1 .. ord (maximum (split_options)) + 1;
  caller_range = 1 .. ord (maximum (toktyp)) + 1;

  qcmdlist = array [cmd_range] of cmd_lookup_record;
  sub_opt_list = array [sub_opt_range] of cmd_lookup_record;
  set_par_list = array [set_param_range] of cmd_lookup_record;
  split_op_list = array [split_opt_range] of cmd_lookup_record;
  caller_list = array [caller_range] of cmd_lookup_record;
  defrangelist = array [qedcmds] of rangelist;
$SYSTEM qspat.inc
(* QSPAT.INC - modified 9/16/81 by djm to add QSETCASE declaration *)

(* QSETCASE sets the CASE flag in QSPAT. *)

EXTERNAL PROCEDURE QSETCASE (X:BOOLEAN);

(* SPATPARSE extracts a string pattern from an input line.  The parse begins
   at a caller supplied index. Preceding blanks are ignored. If the first 
   nonblank character is not a valid string pattern delimiter, then false is
   returned.  The index is not advanced.  If a valid delimiter is found, then
   the line is scanned to complete the pattern. An error occurs if there is
   no closing delimiter or if the default pattern is used and there it has no
   current value.  If the pattern is valid, the pattern record is filled in;
   the parsing index is advanced to the character past the closing delimiter;
   and true is returned. If there is an error, true is returned with an error
   code set. The index is set to the openning delimiter; the pattern record is
   not modified. *)


EXTERNAL FUNCTION SPATPARSE
	    (	LINE: CMDLINE;		(* LINE TO BE PARSED *)
		VAR IDX: CMDLINEIDX;	(* PARSING CURSOR, ADVANCED IF TRUE RETURNED *)
		VAR PAT: SPATTERN;	(* PATTERN DESCRIPTION, SET IF COMPLETE PATTERN FOUND *)
		WILDSWITCH: BOOLEAN;	(* WILDCARDING SET SWITCH *)
		VAR ERR: QERRCODE	(* ERROR CODE, SET IF SOMETHING FOUND *)
				): BOOLEAN;



(* SPATMATCH attempts to find a substring matching a pattern somewhere within
   a caller supplied line.  If no match is found, then false is returned.  If
   a match is found, then the starting index and length of the substring are
   set, and true is returned. *)


EXTERNAL FUNCTION SPATMATCH
	    (	LINE: QSTRING;		(* LINE TO SCAN FOR A MATCHING SUBSTRING *)
		PAT: SPATTERN;		(* PATTERN TO BE MATCHED *)
		VAR POS, LEN: QSTRINGIDX;  (* STARTING POSITION AND LENGTH OF SUBSTRING *)
		VAR ERR: QERRCODE	(* ERROR REPORT *)
				): BOOLEAN;




(* SPATDISPOSE disposes of the doubly-linked list in the given SPATTERN *)

EXTERNAL PROCEDURE SPATDISPOSE (PATTERN: SPATTERN);

(* SPATSETDEFAULT sets the default string pattern - i.e. // or equivalent.
   Initially it has no value; an attempt to reference it causes an error to
   be reported by SPATMATCH. *)


EXTERNAL PROCEDURE SPATSETDEFAULT (PATTERN: SPATTERN);

(* SPATGETDEFAULT returns the value of the current
default spattern. Useful for preventing sub-commands of FIND from
changing it. Will not report an error if the pattern
is null. *)

EXTERNAL FUNCTION SPATGETDEFAULT: SPATTERN;

(* QTOKENGET searches for the next non-blank character in line and sets idx
to that position. The function returns FALSE if no token is found *)

EXTERNAL FUNCTION QTOKENGET(LINE : CMDLINE; VAR IDX : CMDLINEIDX) : BOOLEAN;
$SYSTEM qspred.inc
(* SPREDPARSE extracts a string predicate from an input line.  The parse begins
   at a caller supplied index. If the first token at or following the index is
   not a valid initial token for a string predicate, false is returned. No
   additional parameters, including the line index, are modified. If the
   token is acceptable, an attempt is made to parse a complete string predicate
   An error occurs if there is a syntactic error, an error in a contained
   string pattern, or an AND operator whose operands are both ::, @@, or ##
   form patterns.  If there is no error, then a pointer to the base of the
   predicate tree is filled in; the index is set to the character position
   following the end of the predicate; and true is returned.  If there is an
   error, true is returned with a nonnull error code.  The index is set to
   the start of the token at which the error occurred. The predicate tree
   base is not set. *)

EXTERNAL FUNCTION SPREDPARSE
	    (	LINE: QSTRING;			(* STRING TO BE PARSED *)
		VAR IDX: QSTRINGIDX;		(* PARSING CURSOR, ADVANCED IF SOMETHING FOUND *)
		VAR PRED: SPRED;		(* PREDICATE TREE, SET IF COMPLETE PATTERN FOUND *)
		WILDSWITCH: BOOLEAN;		(* ON/OFF SWITCH FOR WILDCARDING *)
		VAR ERR: QERRCODE		(* ERROR CODE, SET IF SOMETHING FOUND *)
				): BOOLEAN;



(* SPREDMATCH attempts to match a line against a string predicate. The rules
   for interpreting a predicate is given in the 'Definition of QED'.  A flag is
   returned indicating whether or not there was a match. *)

EXTERNAL FUNCTION SPREDMATCH
	    (	LINE: QSTRING;			(* LINE TO MATCH AGAINST *)
		PRED: SPRED;			(* PREDICATE TO MATCH *)
		VAR ERR: QERRCODE		(* ERROR REPORT *)
				  ): BOOLEAN;	(* TRUE IF MATCH FOUND *)



(* SPREDDISPOSE deletes the nodes of a string predicate tree. It is passed
   a possibly nil pointer to the base of a tree. *)

EXTERNAL PROCEDURE SPREDDISPOSE (PRED: SPRED);
$SYSTEM qederr.inc
(**********  QEDERR.inc  last modified 3/6/81  **********)

TYPE QEDERRLEVEL = 1..10;

EXTERNAL PROCEDURE QEDERROR
    (	VAR F: TEXT;				(* file to write error message to *)
	CODE: QERRCODE;				(* error to report *)
	LEVEL: QEDERRLEVEL  );			(* level of message desired *)

$SYSTEM qld.inc
(* QLDPARSE extracts a line designator from an input line. It returns a count
   of line addresses seen, and a representation of the LD.  The parse begins
   with the first token at or following a caller supplied index.  If no LD at
   all appears, a zero count and nil LD chain are returned. There is no error.
   If the start of an LD appears, then the parse continues until the first
   token, not a part of an LD, is seen or an error is detected. If a valid LD
   is found, the count of address (one for a single address, two for a
   range) and the LD chain built up are returned with a null error code. An
   error occurs if there is a syntactic error in a contained string predicate,
   star is used improperly, or a line address or offset is out of range. In
   such a case, the count and LD chain parameters are not returned; and a
   nonnull error code is returned. The index is set to the start of the first
   token following the LD if there is no error; otherwise is set to point to
   the start of the erroneous token.  The caller should insure that the LD
   chain has been initialized by QLDDISPOSEing any records on it, and then
   setting LD to nil.   *)


EXTERNAL PROCEDURE QLDPARSE
	    (	LINE: CMDLINE;			(* LINE TO BE PARSED *)
		VAR IDX: CMDLINEIDX;		(* PARSING CURSOR *)
		VAR NLD: LDCOUNT;		(* NUMBER OF LINE ADDRESSES SEEN *)
		VAR LD: LDCHAIN;		(* REPRESENTATION *)
		WILDSWITCH: BOOLEAN;		(* SWITCH FOR WILDCARDING *)
		VAR ERR: QERRCODE	);	(* INDICATES SUCCESS OR NATURE OF FAILURE *)



(* QLDEVAL resolves a line designation within the range of lines specified,
   and returns the line numbers of the addressed lines.  The rules governing
   the evaluation of an LD are given in the 'Definition of QED'.  If an error
   is detected, the error code is set nonnull, and the range left undefined. *)

EXTERNAL PROCEDURE QLDEVAL
	    (	VAR BUF: QBUFFER;		(* BUFFER TO SEARCH *)
		LD: LDCHAIN;			(* PARSED LD TO EVALUATE *)
		SEARCHRANGE: LDRANGE;		(* LIMITS OF SEARCH *)
		VAR RANGE: LDRANGE;		(* CONTAINS RETURNED LINE ADDRESSES *)
		VAR ERR: QERRCODE	);	(* INDICATES SUCCESS OR NATURE OF FAILURE *)



(* QLDDISPOSE frees the storage associated with an LD chain. It is passed a
   pointer to the start of the chain to free. This pointer may be nil. *)

EXTERNAL PROCEDURE QLDDISPOSE (LD: LDCHAIN );
$SYSTEM qread.inc
(* QREAD reads a line of input, but will no longer allow intraline editing.
   Naturally assumes that the terminal is open. This routine will also
   print a '?' when a control-G is type at the terminal.      *)

EXTERNAL FUNCTION QREAD : QSTRING;
$SYSTEM qedln.inc
(* QEDLN.INC - last modified 9/21/81 by djm to remove external references to
               procedures QTAG, QTAGSET, and QTAGCLEAR.  These procedures are 
               still present in the ANC version. *)


EXTERNAL PROCEDURE QDELBUF			(* clear and re-initialize buffer *)
(	VAR BUFFER: QBUFFER);			(* buffer to be purged *)

EXTERNAL FUNCTION QGETLINE			(* returns text of specified line *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	L: QLINENO;				(* number of line to be gotten *)
	VAR ERR: QERRCODE			(* what went wrong? *)
		): QSTRING;			(* text of line *)

EXTERNAL PROCEDURE QMODLINE			(* change text of specified line *)
(	VAR BUFFER: QBUFFER;			(* in this buffer *)
	L: QLINENO;				(* line to be changed *)
	NEWTEXT: QSTRING;			(* new line's text *)
	VAR ERR: QERRCODE);			(* in case of trouble *)

EXTERNAL PROCEDURE QDELLINES			(* delete specified range of lines *)
(	VAR BUFFER: QBUFFER;			(* from this buffer *)
	F, L: QLINENO;				(* from F to L *)
	VAR ERR: QERRCODE);			(* everything QOK? *)

EXTERNAL PROCEDURE QADDLINE			(* add line of text to specified place *)
(	VAR BUFFER: QBUFFER;			(* in this buffer *)
	L: QLINENO;				(* after this line *)
	TEXT: QSTRING;				(* line to add *)
	VAR ERR: QERRCODE);			(* did it work? *)

EXTERNAL FUNCTION QBUFLENGTH			(* return length of bounded buffer *)
(	VAR BUFFER: QBUFFER
		): QLINENO;

EXTERNAL FUNCTION QDOLLAR_VAL			(* return the value of a $ ld *)
(       VAR BUFFER: QBUFFER
		): QLINENO;

EXTERNAL FUNCTION QFIRST_VAL			(* return the value of the 1st bounded line*)
(       VAR BUFFER: QBUFFER
		): QLINENO;

EXTERNAL PROCEDURE QMOVELINES			(* relocate a section of the buffer *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	FIRST, LAST: QLINENO;			(* range of lines to be moved *)
	DEST: QLINENO;				(* where to move them to *)
	VAR ERR: QERRCODE);			(* error report *)

EXTERNAL PROCEDURE QCOPYLINES
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	FIRST, LAST: QLINENO;			(* range of lines to copy *)
	DEST: QLINENO;				(* where to copy them to *)
	VAR ERR: QERRCODE);			(* error report *)

EXTERNAL PROCEDURE QSETOFFSET			(* sets offset for addressing bounded lines *)
(	NEWOFFSET: QLINENO;			(* new buffer offset *)
	VAR BUFFER: QBUFFER);			(* working buffer *)

EXTERNAL PROCEDURE QSETBOUNDS			(* set up bounded region in buffer *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	F, L: QLINENO;				(* bounds (in bounded linenos) *)
	ABSOLUTE: BOOLEAN;			(* flag for line addressing mode *)
	VAR ERR: QERRCODE);			(* any problem? *)

EXTERNAL PROCEDURE QUNBOUND			(* resets buffer bounds to whole buffer *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	VAR ERR: QERRCODE);			(* done? *)

EXTERNAL PROCEDURE QFILEAPPEND			(* read text from file *)
(	VAR BUFFER: QBUFFER;			(* in this buffer *)
	s940id: file_id;			(* from this file	*)
	wmod: wmodifier;			(* 940 file type modifier	*)
	WHERE: QLINENO;				(* append to this line *)
	VAR CNT: QLINENO;			(* number of lines appended *)
	VAR ERR: QERRCODE);			(* and report any problems *)

EXTERNAL PROCEDURE QTTYAPPEND			(* append text from the TTY *)
(	VAR BUFFER: QBUFFER;			(* in this buffer *)
	WHERE: QLINENO;				(* append to this line *)
	VAR CNT: QLINENO;			(* number of lines appended *)
	VAR ERR: QERRCODE);			(* report failures *)

EXTERNAL PROCEDURE QFILEWRITE			(* write text to file *)
(	VAR BUFFER: QBUFFER;			(* buffer to write from *)
	s940id: file_id;			(* file to write to	*)
	wmod: wmodifier;			(* 940 file type modifier	*)
	FNO, LNO: QLINENO;			(* range to write *)
	CONFIRM: BOOLEAN;			(* new/old file prompting desired? *)
	VAR ERR: QERRCODE);			(* error report *)
$SYSTEM qmark.inc
EXTERNAL PROCEDURE QMARKMATCH			(* matches lines to the markstring *)
(	VAR BUFFER: QBUFFER;			(* in this buffer *)
	MARK: SPRED;				(* predicate to match *)
	SECT_NAME: SPRED;			(* name predicate to AND in *)
	START: QLINENO;				(* place to start looking *)
	VAR FNO, LNO: QLINENO;			(* return args, new bounds *)
	BACKWARD: BOOLEAN;			(* search direction flag *)
	WRAP: BOOLEAN;				(* wrap/no-wrap around buffer flag *)
	VAR ERR: QERRCODE);			(* error report *)
$SYSTEM qprint.inc
EXTERNAL PROCEDURE PRLINE (VAR OUTFILE: TEXT; LINE: QSTRING;
	TAB_PRINT: BOOLEAN; VAR ERR: QERRCODE);

(* outputs the line to the specified file, converting
   all control characters except &G through &M to &x form. *)

EXTERNAL PROCEDURE QLISTLINES (VAR BUFFER: QBUFFER; LOW, HIGH: QLINENO;
	VAR F: TEXT; CTL_CHAR, NUMBER, TAB_PRINT: BOOLEAN; VAR ERR: QERRCODE);

(* qlistlines outputs the specified range of lines to
   the tty. uses prline. *)
$SYSTEM qsubst.inc
(* QSUBSTITUTE replaces zero or more occurrences of a pattern within a line
   with a caller supplied string.  The caller may select among the following
   options:

     confirm - display the line, bracketing the matched pattern with BACKSLASH,
       and query the user to confirm the substitution. It is assumed that
       the teletype has been opened.

     all - substitute for all occurences of the pattern in the line; if not
       selected, only the first occurrence may be replaced.

     print - after applying all substitutions, display the resulting line
       if modifications have been made.

     number - if a match for the pattern is found, display a caller supplied
       line number before the first substitution.

   This routine returns the modified line, a count of the number of substitutions
   performed, and a flag indication that a match was found for the pattern. The
   flag may be true when the count is zero if the substitutions were not confirmed
   by the user.

   An error is reported if insertion of the replacement string would result in
   a line that is too long. If on return, the error code is nonnull, then
   the flag function value is true, but the values of the other values are
   undefined. 
   N_PAR is the Nth occurrance parameter. This parameter instructs QSUBST on 
   which occurrance of matching the pattern substitution(s) should begin.
   Variable NTH is a count of the number of matches found and not 
   substituted for. If NTH is less than N_PAR, then no substitution will be
   made and QSUBST will return false.
   CMD must be either SUBSTITUTE, BEFORE, or AFTER qedcmd. This instructs
   QSUBST whether to substitute or insert the replacement string in the
   line.         *)

EXTERNAL FUNCTION QSUBSTITUTE
	    (	VAR LINE: QSTRING;		(* LINE TO BE MODIFIED *)
		LINENO: QLINENO;		(* NUMBER OF ABOVE *)
		PAT: SPATTERN;			(* PATTERN TO BE REPLACED *)
		RPLMTSTR: QSTRING;		(* STRING TO REPLACE PATTERN *)
		OPTION_LIST: SUB_OPT_SET;
		VAR CNT: QSTRINGIDX;		(* COUNT OF SUBSTITUTIONS PERFORMED *)
		CMD : QEDCMDS;			(* PARSED COMMAND *)
		VAR NTH : QLINENO;		(* COUNT OF OCCURRANCES *)
		N_PAR : QLINENO;		(* OCCURRANCE PARAMETER *)
		VAR ERR: QERRCODE		(* INDICATES IF RESULT TOO LONG *)
					): BOOLEAN; (* INDICATES MATCH FOUND *)

$SYSTEM qjoin.inc
EXTERNAL PROCEDURE QJOINLINES			(* turn two (or more) lines into one *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	FIRST,
	LAST: QLINENO;				(* range of lines to join *)
	CONTMARK: QSTRING;			(* string to replace CRs with *)
	VAR ERR: QERRCODE);			(* error report *)
$SYSTEM qsplit.inc
EXTERNAL FUNCTION QSPLITLINES			(* split selected lines into smaller ones *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	LINENO: QLINENO;			(* addr of line to split *)
	PAT: SPATTERN;				(* where to split each line *)
	OPTION_LIST: SPLIT_OPT_SET;		(* various action-controllers *)
	VAR CNT: QLINENO;			(* number of splits - new lines created *)
	VAR ERR: QERRCODE			(* error report *)
		): BOOLEAN;			(* true if a match for pat found *)
$SYSTEM qopen.inc
TYPE
  QIOMODE = (QINPUT_MODE, QOUTPUT_MODE);
  QIOOPTIONS = (QIO_APPEND, QIO_CONFIRM, QIO_ASCII);
  QIOOPTION_SET = SET OF QIOOPTIONS;

EXTERNAL PROCEDURE QOPENFILE ( VAR F: TEXT;	(* file to open *)
			     FID: FILE_NAME;	(* name of file *)
			     EXT: STRING[3];	(* default extension *)
			     MODE: QIOMODE;	(* I/O mode *)
			     OPTS: QIOOPTION_SET;   (* options *)
			     VAR ERR: QERRCODE );   (* error code *)
$SYSTEM qed.inc
$PAGE QED.INC, last modified 2/16/84, zw

CONST
tab = CHR(#O11);
lf = CHR(#O12);
cr = CHR(#O15);
esc = CHR(#O33);






EXTERNAL PROCEDURE qexecute (*parse and execute one command line*)
  (VAR buffer: qbuffer; (*working buffer*)
  VAR line: cmdline; (*command line to parse*)
  VAR lindex: cmdlineidx; (*place marker*)
  VAR execrange: ldrange; (*limits of execution*)
  VAR ble: qlineno; (*bottommost line examined*)
  findflag: BOOLEAN; (*running under find?*)
  allowed_cmds: qed_cmd_set; (*commands permitted to be executed*)
  VAR err: qerrcode); (*errors?*)
$SYSTEM qlabel.inc
external function qlabelfind
(	var buffer:	qbuffer;	  
	start:		qlineno;	(* where to start search *)
	top:		qlineno;	(* upper search limit *)
	var qlabel:	qstring;	(* label name, if found *)
	var displ:	qlineno;	(* disp. of label from start *)
	var err:	qerrcode):	(* error code *)
			boolean;	(* true if label found, else false *)

(* QLABELFIND searches the buffer from the start line backwards to the
   TOP line for a line with a label. If one is found, the label's name
   and its displacement from the START line are returned, with the value
   TRUE. Otherwise, if no label is found, FALSE is returned. 
   A label begins in column one with a character in the set
   ['A'..'Z','a'..'z','0'..'9','$'] and ends with the character
   preceding the next tab, blank, or end of line.  *)

$TITLE EDIT.PAS, last modified 6/24/85, zw
PROGRAM edit OPTIONS STORAGE(3072);
(*text editor*)
type query_string = string[256];

public function query (question: query_string): boolean;

 procedure ask;
  begin
   write (tty, question, '?  '); break
  end;

 var response: (good, bad);
 var line: query_string;
 const eoln_chars : set of char :=
                   [chr(15b), chr(33b), chr(32b), chr(12b), chr(7b)];
                (* [  <cr>,     <esc>,   <^Z>,     <lf>,    <^G>  ]  *)

 begin
   ask;						(* print question *)
   repeat
     readln (tty);
     read (tty, line);
     line := uppercase (line);
     if (line <> '') andif
	(line [length (line)] in eoln_chars) then (* possible if TTY opened ASCII *)
       line := substr (line, 1, length(line)-1);

     response := good;				(* process, assume ok *)
     if (line = 'YES') or (line = 'Y') or (line = '')
	then query := true
     else if (line = 'NO') or (line = 'N')
	then query := false
     else if (line = 'REPEAT')
	then begin
	  ask;
	  response := bad
	end
     else
	begin
	  write (tty, 'YES/NO?  '); break;
	  clear (tty);				(* don't want user typing ahead of error *)
	  response := bad
	end
   until response = good
 end.

PUBLIC PROCEDURE QINITBUF(VAR BUF: QBUFFER);
{init a buffer for first time}
BEGIN
  WITH BUF DO
  BEGIN
    LASTLINENO := 0;    GETLINENO := 0;    LBOUND := 1;
    OFFSET := 1;    OLDOFFSET := 1;    HBOUND := 0;    CURLINENO := 0;
    NEW (FIRSTLINEP);	(* dummy zeroth line to make things easier *)
    WITH FIRSTLINEP^ DO BEGIN
      PREVLINEP := NIL;      NEXTLINEP := NIL
    END;
    LASTLINEP := FIRSTLINEP;    GETLINEP := FIRSTLINEP;
    LBOUNDP := NIL;    HBOUNDP := FIRSTLINEP;
    GARBLINEP := NIL;    CURFILE := '';
    CURFILEOK := FALSE;    CHANGES := FALSE;
    MARK := NIL;
  END
END;						

public procedure qsetmarkdefault (line: cmdline);
{default SET MARK value}
begin
  qdata.markstring := line
end;


public procedure qsettabdefault (default: boolean);
{default SET TABS value}
begin
  qdata.tabprint := default
end;

public procedure qinit (var buffer: qbuffer);
  var didx: qstringidx;  derr: qerrcode;
begin with qdata do
  begin
    linecount := 1;
    maxdel := 10;
    wildswitch := false;                     (* New for OPS version *)
    s940 := false;                           (* No 940 file yet     *)
    openfileopen := false;
    lasterr := qok;
    errlevel := 1
  end;
  qsetcase(true);
  didx := 1;
  if spredparse (qdata.markstring, didx, buffer.mark, false, derr) then ;
end;

procedure qinitexec( var buffer: qbuffer);
{call other init routines in correct order}
BEGIN
  qinitbuf(buffer);
  qsetmarkdefault(':$PAGE:');
  qsettabdefault( true );
  qinit( buffer )
end;

procedure command_cycle;
(var buffer:qbuffer; allowed_cmds:qed_cmd_set);
var
line: cmdline;
lindex: cmdlineidx;
execrng: ldrange;
ble: qlineno;
err: qerrcode;
lp: qlinep;
emergency_heap_space: ^array[1..160] of integer;

begin
  line := ''; new (emergency_heap_space);      
(* we get this space now so that in the case of a heap overflow,
 we can dispose of it and thus let the user save his buffer.  *)
  saved_bounds := false;
  loop begin
    write (tty, '*'); break; line := qread;
    execrng.lbound := 0; execrng.hbound := qdollar_val (buffer);
    lindex := 1; ble := buffer.curlineno;
    qexecute (buffer, line, lindex, execrng, ble, false, allowed_cmds, err);
  exit if err = qquit;
    if err <> qok then begin clear(tty); qederror (ttyoutput, err, 1) end;
    exception
      storage_overflow: begin
	err := qheapfull;
	buffer.curfileok := false;
        buffer.curfile := '';  (*don't let him ruin file unless he wnats to*)
	writeln (tty,'?Error -- the heap has overflowed.');
	writeln (tty,'Save any unwritten changes in a new file.');
	writeln (tty,'The next heap overflow will be fatal.');
	dispose (emergency_heap_space);
	(* give the user his last piece of the pie...   *)
	if saved_bounds then popbounds(buffer)
      end;
      attention: begin	
	clear(tty); clear(ttyoutput); writeln (tty, '__'); break;
	if saved_bounds then popbounds(buffer)
      end;
    end;
  end;
end;

VAR buffer: qbuffer; (*working buffer*)

BEGIN
  OPEN(TTY,[ASCII]); REWRITE(TTYOUTPUT);
  WRITELN(TTY, 'Text Editor ', version()); WRITELN(TTY);
  TTY^ := cr; (*initialize fake line end*)
  qinitexec(buffer); (*init buffer and editor parameters*)
  REPEAT command_cycle(buffer,[MINIMUM(qedcmds)..MAXIMUM(qedcmds)])
  UNTIL (NOT buffer.changes) ORIF query('Unwritten changes, OK')
END.
 y[1Å