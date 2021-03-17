type
   cstrdim = 1..256;			(* length of a command string *)
   cstridx = 0..257;			(* index for a command string *)
   cstring = record
	       length:	cstridx;	(* number of chars in string *)
	       chars:	packed array[cstrdim] of char
	     end;

type
   ctokidx = 0..11;			(* lexical tokens *)
   ctokdim = 1..10;
   ctoken = packed array[ctokdim] of char;

type
   cinteger = -32768..32767;	(* should be supportable on most machines *)
   charset = set of char;	(* type for delimiter sets *)






(* Global state information for parser *)

type prstate = record
  stmt: cstring;			(* statement being parsed *)
  stmtidx: cstridx;			(* index of current character in stmt *)
  numeric_class,
    alpha_class: charset		(* class of tokens beginning
					  with a number or alphabetic char *)
 end;

var state: prstate;


(* Character constants *)

const
  cr = chr (015B);
   tab = chr (011B);



$page
(* PRINIT establishes the parsing modes to be used in processing subsequent
   statements.  In effect, it is used to define the lexical grammer of the
   language being parsed.

   alpha_chars	      defines the characters which may appear in a token
		      that begins with an alphabetic character.
   numeric_chars      defines the characters which may appear in a token
		      beginning with a number.
									*)

procedure prinit (alpha_chars, numeric_chars: charset);
begin
  with state do begin
    alpha_class := alpha_chars;
    numeric_class := numeric_chars
  end
end;





(* PRSTMT, passed a string to be parsed, initializes processing of that
   string. Parsing begins at the start of the string. *)

procedure prstmt (astmt: cstring);
begin
  with state do begin
    stmt := astmt;			(* copy the string *)
    stmtidx := minimum (cstrdim);	(* start with the first character *)
  end
end;

$page
(* PRIDX and PRSIDX allow the caller to access or set, respectively,
   the current character index within the statement being parsed.
   This provides a primative facility for backup. *)

function pridx: cstridx;
begin
  with state do begin
    pridx := stmtidx
  end
end;


procedure prsidx (idx: cstridx);
begin
  with state do begin
    stmtidx := idx
  end
end;






$page
(* PRCHR returns the character at the current position within the
   statement being parsed.  If the index is past the end of the statement,
   a carriage returned is used. *)

function prchr: char;
begin
  with state do begin
    if stmtidx <= stmt.length
      then prchr := stmt.chars [stmtidx]
      else prchr := cr
  end
end;





(* PRADVCHR advances one character position in the statement. No action
   is taken if already positioned after the end of the statement. *)

procedure pradvchr;
begin
  with state do begin
    if stmtidx <= stmt.length then stmtidx := succ (stmtidx)
  end
end;





(* PRNCHR returns the current character in the input line, and then 
   advances to the next character. If positioned at the end of the statement,
   the cursor is not moved, and a carriage return is returned. *)

function prnchr: char;
begin
  with state do begin
    prnchr := prchr;
    pradvchr
  end
end;

$page
(* SCANBLANKS is an internal procedure that scans white space and comments
   from a particular index in the statement and returns the index of the
   first non-blank or comment character. *)

function scanblanks (aidx: cstridx): cstridx;
 var idx: cstridx;
 var blanks: boolean;
begin
  with state do begin
    idx := aidx;
    blanks := true;
    while (idx <= stmt.length) and blanks do begin
      if (stmt.chars [idx] <> ' ') and (stmt.chars [idx] <> tab)
        then blanks := false
        else idx := idx + 1
    end;
    scanblanks := idx
  end
end;





(* PRSKIPBLANKS advances the current character index to the next nonblank
   character in the statement. *)

procedure prskipblanks;
begin
  with state do begin
    stmtidx := scanblanks (stmtidx)
  end
end;

$page
(* SCANTOKEN is a local procedure which extracts the lexical token, i.e.,
   word, number or punctuation mark at the current position within the
   line.  The starting index and the length of the token are returned. If at
   the end of the statement, a token of length zero is returned. *)

procedure scantoken (var start_idx: cstridx; var len: cstridx);
    var idx: cstridx;

   function verify (aidx: cstridx; class: charset): cstridx;
    var len, idx: cstridx; inclass: boolean;
    begin
     with state do begin		(* scan to last char of class *)
       len := 0; idx := aidx; inclass := true;
       while (idx <= stmt.length) and inclass do begin
	 if not (uppercase (stmt.chars [idx]) in class)
	   then inclass := false
	   else begin
	     idx := idx + 1;
	     len := len + 1
	   end
       end;
       verify := len			(* return length of substring *)
     end
    end;

begin
  with state do begin
    idx := scanblanks (stmtidx);	(* ignore leading white space *)
    start_idx := idx;			(* first non-blank is start of token *)

    if idx > stmt.length then begin	(* last end of line *)
      len := 0;				(* at EOL, null token is result *)
    end

    else begin				(* type token and scan for end *)
      case uppercase (stmt.chars [idx]) of
        'A'..'Z':  len := verify (idx, alpha_class);
        '0'..'9':  len := verify (idx, numeric_class);
        others:    len := 1
      end
    end;
  end
end;

$page
(* PRTOKEN is a function which returns the token at the current position
   within the statement. *)

function prtoken: ctoken;
 var sidx, len: cstridx;
     token: ctoken; i: cstridx;
begin
  with state do begin
    scantoken (sidx, len);
    token := '          ';
    if len > maximum (ctokdim) then len := maximum (ctokdim);
    for i := 1 to len do token [i] := stmt.chars [sidx-1+i];
    prtoken := token
  end
end;




(* PRADVTOKEN advances past the current token. If the parser is
   positioned at the end of the statement, no action is performed. *)

procedure pradvtoken;
  var sidx,len: cstridx;
begin
  scantoken (sidx, len);
  prsidx (sidx+len)
end;

$page
(* PRNTOKEN returns the current token and advances past it in the line.
   If at the end of the statement, the cursor is not moved, and a null
   token is returned. *)

function prntoken: ctoken;
 var sidx, len: cstridx;
 var token: ctoken; i: cstridx;
begin
  with state do begin
    scantoken (sidx, len);
    token := '          ';
    if len > maximum (ctokdim) then len := maximum (ctokdim);
    for i := 1 to len do token [i] := stmt.chars [sidx-1+i];
    prntoken := token;
    prsidx (sidx + len)
  end
end;

$page
(* PCALPHA is a function returning a boolean value indicating whether or not
   a token beginning with an alphabetic character is present at the cursor
   position. If so, the text of the token is returned, and the cursor advanced
   past the token. *)

function pcalpha (var token: ctoken): boolean;
 var sidx: cstridx;
 begin
  with state do begin
   sidx := stmtidx;			(* save location to backup *)
   prskipblanks;
   if uppercase (prchr) in ['A'..'Z']
     then begin				(* alphabetic token found *)
       token := prntoken;
       pcalpha := true
     end
     else begin				(* no found, backup *)
       pcalpha := false;
       stmtidx := sidx
     end
  end
 end;

$page
(* PCINTEGER is a function returing a boolean value indicating whether or
   not the current token is an integer value. If so, the converted value
   is returned, and the cursor advanced past the token. *)

function pcinteger (var intval: cinteger): boolean;
 var sidx: cstridx;
 var temp: cinteger;
 var okay: boolean;
 begin
  with state do begin
   sidx := stmtidx;
   temp := 0; okay := false;
   prskipblanks;			(* skip white space *)
   while prchr in ['0'..'9'] do begin
     temp := (temp * 10) + (ord (prchr) - ord ('0'));
     okay := true;			(* have at least on digit *)
     pradvchr
   end;
   if okay then begin			(* check that all numeric chars proc. *)
     if prchr in numeric_class		(* remaining number class chars *)
       then okay := false;		(* must have been a real *)
   end;
   pcinteger := okay;
   if okay
     then intval := temp		(* return only if good *)
     else stmtidx := sidx		(* backup if bad *)
  end
 end;

$page
(* PCNUMBER is a function returning a boolean value indicating whether or not
   a number is present.  The user supplies a default radix which may be over-
   ridden by a suffixed base designator (B = binary, Q or O = octal,
   D = decimal, H = hexadecimal). Hex constants must begin with
   a number.  The number must be followed by delimiters (defined to be not
   in the alpha or numeric class). If found, the value is returned. *)

function pcnumber (default_base: cinteger; var intval: cinteger): boolean;
 var sidx, sridx, lidx: cstridx;
 var temp, digit, biggest_digit, base: cinteger;
 var ch: char;
 label 1;

 procedure error;
  begin
    pcnumber := false;
    state.stmtidx := sidx;	(* backup *)
    goto 1
  end;

 function cvt (ch: char): cinteger;	(* converts radix n chars to integer *)
  begin
   if ch in ['0'..'9']
     then cvt := ord (ch) - ord ('0')
     else cvt := ord (uppercase (ch)) - ord ('A') + 10
  end;

 begin
  with state do begin
    sidx := stmtidx;
    prskipblanks;

    if not (prchr in ['0'..'9']) then error;  (* must begin with a digit *)

    biggest_digit := 0;		(* biggest digit seen *)
    sridx := stmtidx;			(* start of number *)
    loop
      ch := uppercase (prnchr);
      if ch in ['0'..'9'] then begin
        digit := cvt (ch);
	if digit > biggest_digit then biggest_digit := digit
      end;
    exit if not (uppercase (prchr) in (alpha_class + numeric_class));
      if ch in ['0'..'9','A'..'F'] then begin
	digit := cvt (ch);
	if digit > biggest_digit then biggest_digit := digit
      end
      else error
    end;
    lidx := stmtidx - 2;		(* assume last char is base tag *)
    case ch of				(* check for tag *)
      'B':    base := 2;
      'Q','O':base := 8;
      'D':    base := 10;
      'H':    base := 16;
      '0'..'9': begin
		if biggest_digit >= default_base 
		  then base := 10	(* case of 89 with base = 8 *)
		  else base := default_base;
		lidx := stmtidx - 1	(* there was no tag char *)
	      end;
      others: error
    end;

    if biggest_digit >= base then error;	(* will not convert *)

    temp := 0;				(* perform conversion *)
    for sidx := sridx to lidx
      do temp := (temp * base) + cvt (stmt.chars [sidx]);

    pcnumber := true;			(* successful return *)
    intval := temp
  end;
 1:end;

$page
(* PCPUNCTUATION is a function returning a boolean value indicating whether
   or not the current token is a designated, single character punctuation
   mark.  If so, the cursor is position after the punctionation. *)

function pcpunctuation (punct: char): boolean;
 var sidx: cstridx;
 var token: ctoken;
 begin
  with state do begin
    sidx := stmtidx;
    token := prntoken;			(* get the next token *)
    if (token [1] = punct) and (token [2] = ' ')	(* make sure it is single *)
      then pcpunctuation := true
      else begin			(* failed, backup *)
        stmtidx := sidx;
	pcpunctuation := false
      end
  end
 end;




(* PCKEYWORD is a function returning a boolean value indicating whether or
   not the current token is a designated keyword, value, or multi-character
   punctuation mark. If the keyword is present, the parser is advanced past
   the token before return. *)

function pckeyword (word: ctoken): boolean;
 var sidx: cstridx;
 begin
  with state do begin
    sidx := stmtidx;
    if word = prntoken
      then pckeyword := true		(* correct match *)
      else begin			(* match fails, backup *)
        stmtidx := sidx;
	pckeyword := false
      end
  end
 end;

$page
(* PCEND is a function returning a boolean value indicating whether or not
   the end of the statement has been reached. *)

function pcend: boolean;
 var sidx: cstridx;
 begin
   pcend := (prtoken = '          ')
 end;

$page
(* PCSTRING is a function returning a boolean value indicating whether or
   not a quoted string is present at the current token position. The caller
   may specify the set of acceptable quote characters, and whether or
   not the quote character may be doubled within the string. If a string
   is found, the cursor is moved past the balancing quote mark and the string
   value returned.  A value will be returned even if the quote mark is not
   balanced; a separate parameter indicates whether or the string is correctly
   terminated. *)

function pcstring (delimiters: charset; doubled: boolean;
			var str: cstring; var balanced: boolean): boolean;
 var sidx: cstridx;
 var quote: char;
 var ch: char;
 begin
  with state do begin
    sidx := stmtidx;
    prskipblanks;			(* ignore blanks before quote *)
    if prchr in delimiters then begin
      quote := prnchr;			(* string must be closed by same char *)
      str.length := 0;
      balanced := false;
      loop
	ch := prnchr;			(* get next char, and step *)
	if ch = quote then begin
	  if (prchr = quote) and doubled  (* if doubled quote, and okay *)
	    then pradvchr		(* then pass char as part of string *)
	    else balanced := true
	end;
      exit if balanced or (ch = cr);	(* if end of string or end of line *)
	str.length := str.length + 1;	(* append char to string *)
	str.chars [str.length] := ch
      end;
      pcstring := true			(* record success *)
    end
    else begin				(* no quote mark *)
      pcstring := false;
      stmtidx := sidx
    end
  end
 end;

$page
(* PCPPN is a function returning a boolean indicating whether or not a
   project/programmer number has been found. If so, the converted value
   of the ppn is returned. An additional flag is returned to indicate if the
   entire ppn has been correctly given. *)

type ppntype = 0..77777777777B;

function pcppn (var ppn: ppntype; var valid: boolean): boolean;
 var proj, prog: ppntype;
 begin
  pcppn := false;			(* assume failure *)
  if pcpunctuation ('[') then begin		(* have start of a ppn *)
     pcppn := true; valid := false;	(* but not yet complete *)
    if pcnumber (8, proj) then begin
      if pcpunctuation (',') then begin
        if pcnumber (8, prog) then begin
	  if pcpunctuation (']') then begin
	    valid := true;
	    ppn := (proj * 1000000B) + prog
	  end
        end
      end
    end
  end
 end;

$page
(* PCFILEID is a function returning a boolean indicating whether or not a
   filename is present in the statement. If so, the components of the file
   designator (device, name, extension, ppn) are extracted and returned
   as files of a record type.  A separate flag is returned to indicate whether a
   syntax error has occured within the file name. *)

type file_designator = record
  device: packed array [1..6] of char;
  name_ext: packed array [1..9] of char;
  ppn: ppntype
 end;
type extension = packed array[1..3] of char;

function pcfileid (ext: extension;
              var fileid: file_designator; var valid: boolean): boolean;
 var newfileid: file_designator;
 var token: ctoken; i: ctokidx;
 label 1;
 begin
  with newfileid do begin
    device := 'DSK   ';			(* null return value *)
    name_ext := '         ';
    ppn := 0;

    if pcalpha (token) then begin;
      pcfileid := true;			(* have found partial fileid *)
      valid := true;			(* assume that all is okay *)
      if prchr = ':' then begin		(* token was a device name *)
        device := '      '; for i := 1 to 6 do device [i] := token [i];
        pradvchr;
	if not (uppercase (prchr) in ['A'..'Z']) then goto 1;
        token := prntoken		(* known that a name token follows *)
      end;
      for i := 1 to 6 do name_ext [i] := token [i];	(* get name *)
      if prchr = '.'
	then begin			(* ext given, may be null *)
	  pradvchr;
	  if uppercase (prchr) in ['A'..'Z'] then begin
	    token := prntoken;
	    for i := 7 to 9 do name_ext [i] := token [i-6]
	  end
	end
	else begin			(* apply default extension *)
	  for i := 7 to 9 do name_ext [i] := ext [i-6]
        end;
      if prchr = '[' then begin		(* get ppn *)
	if pcppn (ppn, valid) then ;
      end;
1:    fileid := newfileid		(* copy result *)
    end
    else pcfileid := false
  end
 end;
