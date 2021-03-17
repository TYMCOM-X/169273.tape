(*$m-,c- *)
(**********************************************************************)

		 (*analyzer lexical analysis package*)

(* symbols and variables globally available *)

$INCLUDE anscan.typ

public var tokenid: tokendesc;			(*variable returned by scan*)
	   optionlist: set of options;		(*compiler options*)
	   symfile, namfile, semfile, tabfile: text;		(*shared non-standard files used*)

var errormessages: array[error] of string[30];	(*error messages*)

(* variables preserving lexical analyzer state *)

const maxlinelen = 256;
type  lineidx = 0..maxlinelen;


var tokenptr,					(*index of current token in line*)
    lineptr: lineidx;				(* index of current char in line *)

    linecnt: 0..10000;				(*count of source lines read*)

    ch: char;					(*the current character*)

    line: string[maxlinelen];			(* current line *)

    saveid: tokendesc;				(*used to save current token during error recovery*)

    specials: array[char] of symbols;		(*used by scan for special chars*)

const maxerrcnt = 4;				(*storage used to preserve errors in line*)
var toterrcnt: 0..1000;				(*count of errors in program*)
    linerrcnt: 0..maxerrcnt;			(*count of errors in line*)
    errorptr: lineidx;				(*position of previous error in line*)

const tab = chr (11b);

(* special scanner states due to error recovery --
   normalscan -- advance to next token.
   nopscan -- set by inserttoken. causes scanner to merely
	      return current token. transitions to popscan state.
   popscan -- set by skipuntil. causes saved token in saveid to be
	      returned. transitions to normalscan state. *)

type scanstates = (normalscan, nopscan, popscan);
var scanstate: scanstates;

$PAGE
(*    initialization of lexical package    *)

public procedure initscan;

  procedure initlex;				(*initialize lexical state information*)
    var i: char;
  begin
    line:= '';
    with tokenid do begin
      tokentext:= '';
      tokensym:= badsymbol
    end;
    for i:= minimum(char) to maximum(char) do specials[i]:= badsymbol;
    specials[';']:= semicolon;
    specials[',']:= comma;
    specials['|']:= comma;			(*synonym for comma*)
    specials['(']:= leftparen;
    specials[')']:= rightparen;
    specials['[']:= leftbracket;
    specials[']']:= rightbracket;
    specials['*']:= star;
    specials['=']:= equals;
    scanstate:= normalscan;
  (* prime the scanner with a line and 1st character *)
    linecnt:= 0;
    lineptr:= 1;
    ch := ' '
  end (*initlex*);

  procedure initerror;
  begin
    toterrcnt:= 0; linerrcnt:= 0;
    errormessages[badchar] := 'INVALID CHARACTER';
    errormessages[noprime] := 'MISSING "''" AT END';
    errormessages[badsym] :=  'INVALID SYMBOL';
    errormessages[toomany] := 'TOO MANY ERRORS ON THIS LINE';
    errormessages[nocarat] := 'MISSING ">" AT END';
    errormessages[gnsoflo] := 'TOO MANY NESTED SYMBOLS';
    errormessages[gncodcnflct] := 'CODE CONFLICTS WITH TERMINAL SYMBOL';
    errormessages[gnredef] := 'ATTEMPT TO REDEFINE SYMBOL';
    errormessages[gnsymcnflct] := 'SYMBOL CONFLICTS WITH SEMANTIC CODE';
    errormessages[synerror] := 'SYNTAX ERROR';
    errormessages[gncodincomp] := 'CONFLICTS WITH TARGET LANGUAGE';
    errormessages[expgramsy] := 'EXPECTED $GRAMMAR';
    errormessages[exprule] := 'EXPECTED RULE';
    errormessages[expcolonequals] := 'EXPECTED ":="';
    errormessages[expsemicolon]:= 'EXPECTED ";"';
    errormessages[expsymbol] := 'EXPECTED VALID SYMBOL';
    errormessages[exprightparen] := 'EXPECTED ")"';
    errormessages[exprightbracket] := 'EXPECTED "]"';
    errormessages[expsemcode] := 'EXPECTED SEMANTIC CODE';
    errormessages[scalarnameexp] := 'EXPECTED SCALAR TYPE NAME';
    errormessages[gnundefter] := 'NOT DEFINED IN $TERMINALS';
    errormessages[gnundefcod] := 'NOT DEFINED IN $CODES';
    errormessages[gnundefcls] := 'CLASSNAME NOT IN $TERMINALS';
  end (*initerror*);

begin						(*initscan*)
  initerror;
  initlex
end (*initscan*);
$page
(*    diagnostic routines    *)

public procedure noteerror(code: error);

  (*routine prints diagnostic with uparrow on terminal and
    listing file, if called for *)

  var realcode: error;

    (*internal routine to print the uparrow on specified file*)

  procedure printuparrow(pad: lineidx; var f: text);
    var chpos, blankpad: lineidx;
  begin
    blankpad:= pad+tokenptr-1;			(*number of blanks before ^*)
    chpos:= 0;					(*char posn in print blanks loop*)
    while chpos<blankpad do
      if chpos+8 < blankpad then		(*use tab's for speed*)
	begin write(f,tab); chpos:= chpos+8 end
      else begin
	writeln(f,' ':blankpad-chpos,'^');
	chpos:= blankpad
      end
  end (*printuparrow*);

begin						(*noteerror*)

  (* suppress error if current token already diagnosed (in an attempt
     to avoid cascading diagnostics, or if too many diagnostics already
     printed on current line *)

  if (tokenptr>errorptr) and (linerrcnt<maxerrcnt) then begin
    if not (code in [gnundefter,gnundefcod,gnundefcls]) then
      optionlist:= optionlist-[gentables];	(*kill table generation*)
    if linerrcnt = 0 then			(*on first error in line, echo to tty*)
      writeln(tty,linecnt:5,tab,line);
    linerrcnt:= linerrcnt+1;			(*count diagnostic about to be printed*)
    if linerrcnt<maxerrcnt then begin		(*put ^ out if not too many errors yet*)
      if genlist in optionlist then
	printuparrow(8,output);			(*to listing file if called for*)
      printuparrow(8,ttyoutput);		(*to terminal*)
      realcode:= code				(*and print passed diagnostic*)
    end
    else realcode:= toomany;			(*no further diags after std message*)
    if genlist in optionlist then		(*message to listing file*)
      writeln(output,errormessages[realcode]);
    writeln(tty,errormessages[realcode]);
    break;
    errorptr:= tokenptr				(*save posn of this error*)
  end
end (*noteerror*);
$page
(*    procedure to read the next source line    *)

procedure nextline;
begin
  toterrcnt:= toterrcnt+linerrcnt;		(*keep running total of diagnostics*)
  linerrcnt:= 0;				(*clear line error count total*)
  errorptr:= 0;					(*and position of last error in line*)
  linecnt:= linecnt+1;				(*keep count of lines*)
  if eof(input) then begin
    if genlist in optionlist then
      writeln(output,'***MISSING $END');
    writeln(tty,'***MISSING $END');
    break(tty);
    line := '$END';
  end
  else begin
    line := '';
    readln (input);
    while not eoln (input) do begin
      if length (line) < maxlinelen then begin
	line := line || input^
      end;
      get (input)
    end;
    if genlist in optionlist then begin
      write(output,linecnt:5,tab);
      writeln (output, line)
    end
  end;
  lineptr:= 0
end (*nextline*);
$page
(*    error recovery procedures    *)

public procedure inserttoken(insertion:symbols);	(* insert a token, saving current *)
begin
  saveid:= tokenid;
  scanstate:= nopscan;
  tokenid.tokensym:= insertion
end (*inserttoken*);


public function scan: symbols; forward;

public procedure skipuntil(targets: setofsymbols);
begin
  while not (tokenid.tokensym in targets) do tokenid.tokensym := scan;
  saveid:= tokenid;
  scanstate:= popscan
end (*skipuntil*);
$page
(*    the lexical analyzer    *)

public function scan;				(* returns next token in tokenid *)
  const eolchar = chr(15b);			(*suitable stopper char used for endofline*)
  label 1;
  var litptr: lineidx;				(* used in literal scan *)


  procedure nextch;
  begin
    if lineptr>length (line) then nextline;
    lineptr:= lineptr+1;
    if lineptr>length (line) then ch:= eolchar
    else ch := line[lineptr]
  end (*nextch*);

begin						(*scan*)
  if scanstate=nopscan then scanstate:= popscan
  else if scanstate=popscan then begin
    tokenid:= saveid;
    scanstate:= normalscan
  end
  else with tokenid do begin

1:  while (ch=' ')or(ch=eolchar) do nextch;	(*skip white space*)
    tokenptr:= lineptr;				(*save token start*)

    if ch='$' then begin			(* $ symbol *)
      nextch; ch:= uppercase(ch);
      if not (ch in ['A'..'Z']) then begin
	noteerror(badchar);
	goto 1
      end
	   (* decide on basis of 1st character for now *)
      else if ch='S' then tokensym:= succsy
      else if ch='F' then tokensym:= failsy
      else if ch='E' then tokensym:= endsy
      else if ch='T' then tokensym:= tersy
      else if ch='G' then tokensym:= gramsy
      else if ch='C' then tokensym:= codesy
      else if ch='N' then tokensym:= nontersy
      else begin
	noteerror(badsym);
	tokensym:= badsymbol
      end;
      while uppercase(ch) in ['A'..'Z'] do nextch
    end						(* $ symbol *)
    else if ch='<' then begin			(* class name *)
      nextch;
      while (ch<>'>') and (ch<>eolchar) do nextch;
      tokentext:= substr(line,tokenptr+1,lineptr-tokenptr-1);
      if ch=eolchar then noteerror(nocarat)
      else nextch;				(* skip terminating carat *)
      if length(tokentext) > 0 then tokensym:= class
      else begin
	noteerror(badsym);
	tokensym:= badsymbol
      end
    end						(* class name *)
    else if uppercase(ch) in ['A'..'Z'] then begin  (* identifier *)
      tokentext := '';
      repeat
	tokentext := tokentext || uppercase (ch);
	nextch
      until not (uppercase(ch) in ['A'..'Z','0'..'9','_']);
      tokensym:= identifier
    end						(* identifier *)
    else if ch='''' then begin			(* literal *)
      tokentext := '';
      loop
	litptr := lineptr + 1;
	repeat nextch until (ch = '''') or (ch = eolchar);
	tokentext := tokentext || substr (line, litptr, lineptr-litptr);
	if ch = eolchar
	  then noteerror (noprime)
	  else nextch;
      exit if ch <> '''';
	tokentext := tokentext || ''''
      end;
      if length (tokentext) = 0 then begin
	noteerror(badsym);
	tokensym:= badsymbol
      end
      else tokensym:= literal
    end						(* literal *)
    else if ch=':' then begin
      nextch;
      if ch='=' then begin			(* := *)
	nextch;
	tokensym:= colonequals
      end
      else if ch=':' then begin
	nextch;
	if ch = '=' then begin			(* ::= *)
	  nextch;
	  tokensym:= colonequals
	end
	else begin
	  noteerror(badchar);
	  goto 1
	end
      end
      else begin
	noteerror(badchar);
	goto 1
      end
    end
    else if ch in ['0'..'9'] then begin		(*number*)
      tokentext:= '';
      repeat					(* return text, don't convert to internal form *)
	tokentext:= tokentext || ch;
	nextch
      until not (ch in ['0'..'9']);
      tokensym:= number
    end
    else if ch='(' then begin
      nextch;					(*check for comment*)
      if ch<>'*' then tokensym:= leftparen
      else begin nextch;			(*eat comment without worrying about eof*)
	repeat
	  while ch<>'*' do nextch;
	  nextch
	until ch=')';
	nextch;
	goto 1
      end
    end
    else begin
      tokensym:= specials[ch];
      nextch;
      if tokensym=badsymbol then begin
	noteerror(badchar);
	goto 1
      end
    end
  end;
  scan := tokenid.tokensym
end						(*scan*)

(* the *) .					(* end *)
    