(*$m-,c-******************************************************************)
	  (* ll(1) compiler generator semantic routine *)

$INCLUDE anscan.typ
$INCLUDE anscan.var
$INCLUDE anscan.ext
$INCLUDE anal.ext
$INCLUDE angen.typ

(*    variables preserving generator state   *)

var
  rootsym: symrecptr;				(*symbol table root*)
  curvindex:  0..maxvindex;			(*current vocabulary index*)
  curgencode: 0..maxgencode;			(*current $s/$f code*)
  curpmaddr: 1..1000;				(*current pm address*)
  firstnonter: symrecptr;			(*head ptr of non-terminal chain*)
  firstnamednt: symrecptr;			(* chained of named non terminal symbol *)
  lastnamednt: symrecptr;
  firstgencode: symrecptr;			(*head ptr of $s/$f codes*)
  firstter: symrecptr;				(*head ptr of terminal chain*)
  sawcodessection,				(*flags $codes section seen*)
  sawntsection,					(*flags $nonterminals section*)
  sawtersection: boolean;			(*flags $terminals section seen*)
    (* interim state preservation variables *)
  cursym: symrecptr;				(* preserves current <symbol> in parse *)
  curlhs: symrecptr;				(* preserves <class> on left side of rule *)
  curgsym: gramrecptr;				(* preserves grammar rec of current <symbol> *)
    (* stack used to preserve headptrs of nested metabrackets and/or
       alternative lists *)
const maxdefncnt = 10;
var
  defncnt: -1..1000;				(*index of current definition being parsed*)
  defnstack: array[0..maxdefncnt] of		(*note size less than defncnt range*)
    record
      altormb,					(*points to alt or mb gramrec being defined*)
      lastdefn: gramrecptr			(*points to last block on chain*)
    end;

(*    list of semantic operations   *)

$INCLUDE anal.sem


(* routine to initialize state variables *)

public procedure initgen;
begin
  rootsym:= nil;				(*empty symbol table without releasing storage*)
  sawtersection:= false;
  sawntsection := false;
  sawcodessection:= false;

  (* for fortran tables, pm addresses start from 1 so that indicies are
     one-based.  special pm codes are in fact negative, i.e. e=-3,
     f=-2, and t=-1.  pascal tables start from 3, so that special
     codes may be 0..2.  this allows pascal tables to be packed (as
     pascal does not pack fields which may be negative) *)

  if fortables in optionlist then curpmaddr:= 1
  else curpmaddr:= 3;
  curvindex:=  0;				(*account for std symbol badsymbol*)
  curgencode:= 0;				(*account for std code nosemop*)
  firstnonter:= nil;				(*zap head ptrs*)
  firstnamednt := nil;
  lastnamednt := nil;
  firstter:= nil;
  firstgencode:= nil;
end (*initgen*);


(*   symbol table lookup routine  *)

public function findsymrec (s: tokenstring; text: symtexttype;
  var sptr: symrecptr): boolean;
 var
   cursym, lastsym: symrecptr;
   aside: boolean; tests: string[maxsymlen];

 begin
  lastsym := nil; cursym := rootsym;
  tests:= s;					(*to truncate to maxsymlen if necessary for = comparison*)
  while cursym <> nil do
   with cursym^ do begin
     if (symtext=tests) and (text=texttype) then begin
       findsymrec := true;
       sptr := cursym;
       return
     end;
     lastsym := cursym;
     aside := (symtext < tests);
     if aside
       then cursym := aptr
       else cursym := zptr
   end;

  (* symbol not found, create new entry *)
  new (cursym);
  with cursym^ do begin
    aptr := nil; zptr := nil;
    symtext := tests;
    texttype := text;
    scalar_name := '';
  end;
  if lastsym = nil
    then rootsym := cursym			(* thread onto tree *)
    else if aside
      then lastsym^.aptr := cursym
      else lastsym^.zptr := cursym;
  sptr := cursym;
  findsymrec := false
 end (*findsymrec*);



(* here begins the semantic routine itself *)

public procedure generate(sgfgcode: pmsemop);

var texttype: symtexttype;


  (* push a definition header (metabracket or alternative list) on
     the definition stack *)

  procedure pushdefn(defntype: gramrectype);
  begin
    defncnt:= defncnt+1;
    (* merely increment count if beyond stack end *)
    if defncnt<=maxdefncnt then
      with defnstack[defncnt] do begin
	case defntype of
	  grammb: new(altormb,grammb);
	  gramalt: new(altormb,gramalt)
	end;
	lastdefn:= nil;
	with altormb^ do begin			(*set up mb/alt record*)
	  nextptr:= nil;
	  instaddr := curpmaddr;
	  closure:= false
	end
      end
    else if defncnt=maxdefncnt+1 then		(*give diagnostic at point of oflo*)
      noteerror(gnsoflo)
  end (*pushdefn*);


  (* append chain of grammar record(s) whose head pointer is passed to
     definition chain of symbol on top of stack *)

  procedure addgrec(headptr: gramrecptr);
    var tptr1, tptr2: gramrecptr;
  begin
    (* don't add if past top or stack or if lhs not defined due to
       previous error *)
    if (defncnt<=maxdefncnt) and (curlhs<>nil) then
      with defnstack[defncnt] do begin
	if lastdefn=nil then			(*update defnptr in this mb/alt record*)
	  altormb^.defnptr:= headptr
	else					(*update nextptr of last record on current chain*)
	  lastdefn^.nextptr:= headptr;
	if headptr<>nil then begin		(*find end of new chain to update lastdefn*)
	  lastdefn:= headptr;
	  while lastdefn^.nextptr<>nil do lastdefn:= lastdefn^.nextptr
	end
      end
    else begin					(* if we didn't append ... *)
      tptr1:= headptr;				(*instead, throw chain away*)
      while tptr1<>nil do begin
	tptr2:= tptr1^.nextptr;
	dispose(tptr1);
	tptr1:= tptr2
      end
    end
  end (*addgrec*);


  (* pop current definition header and append to enclosing definition
     chain, checking for useless metabrackets or alternative lists *)

  procedure popdefn;
    var tptr: gramrecptr;
  begin
    (* decrement defncnt now, so addgrec can be used to append to
       enclosing definition *)
    defncnt:= defncnt-1;
    if defncnt<maxdefncnt then
      with defnstack[defncnt+1] do
	if lastdefn<>nil then			(*non-nil definition*)
	  with altormb^ do
	    if rectype=gramalt then
	    (* alternative list consisting solely of zpart can be dropped *)
	      if (defnptr^.rectype<>gramsym) or
	      (not defnptr^.ypart) then begin
		addgrec(defnptr);
		dispose(altormb)
	      end
	      else addgrec(altormb)
	    else				(*metabracket*)
	    (* metabracket consisting solely of metabracket or alternative
	       list is equivalent to single metabracket.  however, if
	       inner mb is starred, so is outer *)
	      if (defnptr^.nextptr=nil) and
	      (defnptr^.rectype<>gramsym) then begin
		closure:= closure or defnptr^.closure;
		tptr:= defnptr;
		defnptr:= defnptr^.defnptr;
		addgrec(altormb);
		dispose(tptr)
	      end
	      else addgrec(altormb)
  end (*popdefn*);


  (* set up grammar record for <symbol> *)

  procedure creategsym;
    (* creates curgsym, assumes symtab ptr for current symbol in cursym *)
  begin
    new(curgsym,gramsym);
    with curgsym^ do begin
      nextptr:= nil;
      instaddr := curpmaddr;
      curpmaddr := curpmaddr + 1;		(* <symbol> generates pm instruction but not alt or mb *)
      symptr:= cursym;
      sgptr:= nil; fgptr:= nil;
      ypart:= false
    end;
  end (*createsym*);


  (* look up and define if necessary $s/$f code in symbol table *)

  procedure findgencode;			(*result left in cursym*)
  begin
    if findsymrec(tokenid.tokentext,symlit,cursym) then begin
      if cursym^.rectype<>symgencode then begin
	noteerror(gncodcnflct);
	cursym:= nil
      end
    end
    else begin					(*define it*)
      if sawcodessection then noteerror(gnundefcod);
      curgencode:= curgencode+1;
      with cursym^ do begin
	gencode:= curgencode;
	rectype:= symgencode;
	nextsymptr:= firstgencode;
	firstgencode:= cursym
      end
    end
  end (*findgencode*);


  (* procedure to scan symbol table and assign vindex's to all
     undefined class names, making them terminal classes *)

  procedure changeundef(cursym: symrecptr);
  begin
    with cursym^ do begin
      if aptr<>nil then changeundef(aptr);
      if (rectype=symundef) orif ((rectype = symnonter) andif (defnptr=nil)) then begin
	if sawtersection then begin
	  noteerror (gnundefcls);
	  writeln (tty, ' ':8, '<', symtext, '>')
	end;
	curvindex:= curvindex+1;
	rectype:= symter;
	vindex:= curvindex;
	nextsymptr:= firstter;
	firstter:= cursym
      end;
      if zptr<>nil then changeundef(zptr)
    end
  end (*changeundef*);


  (* procedure to reverse a singly linked list of symbol records *)

  procedure flipsymlist(var headptr: symrecptr);
    var nextblk,curblk,prevblk: symrecptr;
  begin
    curblk:= headptr; prevblk:= nil;
    while curblk<>nil do begin
      nextblk:= curblk^.nextsymptr;
      curblk^.nextsymptr:= prevblk;
      prevblk:= curblk;
      curblk:= nextblk
    end;
    headptr:= prevblk
  end (*flipsymlist*);


  (* procedure to generate parsing machine for a symbol by filling in
     at and af fields of symbol records in definition *)

  procedure genpm(altormb: gramrecptr;		(* head record of current symbol *)
		  truebranch,			(* pm address to goto when symbol recognized *)
		  falsebranch: pmaddr);		(* goto addr for non-recognition *)
    var curptr: gramrecptr;			(*current symbol in definition being examined*)

    procedure genzpart(falsebranch: pmaddr);
    begin
      with curptr^ do
	case rectype of
	gramsym: begin

	  (* if final zpart, then at is either head of symbol if closed,
	     else, it is passed truebranch *)

	  if nextptr=nil then
	    if altormb^.closure then at:= altormb^.instaddr
	    else at:= truebranch

	  (* if not final, continue with next zpart symbol on recognition *)

	  else at:= instaddr+1;
	  af:= falsebranch
	end;
	gramalt:
	  if nextptr<>nil then genpm(curptr,nextptr^.instaddr,falsebranch)
	  else if altormb^.closure then
	    genpm(curptr,altormb^.instaddr,falsebranch)
	  else genpm(curptr,truebranch,falsebranch);
	grammb:					(* falsebranch of mb is truebranch since it cannot fail *)
	  if nextptr<>nil then
	    genpm(curptr,nextptr^.instaddr,nextptr^.instaddr)
	  else if altormb^.closure then
	    genpm(curptr,altormb^.instaddr,truebranch)
	  else genpm(curptr,truebranch,truebranch)
	end					(*case*)
    end (*genzpart*);

  begin						(*genpm*)
    curptr:= altormb^.defnptr;
    if curptr<>nil then begin

	(* first we do initial symbol(s) marked as yparts *)

      while (curptr^.rectype=gramsym) and curptr^.ypart do
	with curptr^ do begin			(* no need to check for nil, as zpart follows *)
	  if altormb^.closure then		(* symbol we are working on is starred *)
	    at:= altormb^.instaddr
	  else at:= truebranch;
	  af:= instaddr+1;			(*symbol always follows ypart*)
	  curptr:= nextptr
	end;

	(* now do first zpart, which requires special treatment, as
	   af of symbol and falsebranch of alternative list is
	   different than subsequent zparts *)

      genzpart(falsebranch);

	(* now do subsequent zparts *)

      curptr:= curptr^.nextptr;
      while curptr<>nil do begin
	genzpart(errorpmret);
	curptr:= curptr^.nextptr
      end

    end						(*non-nil symbol definition*)
  end (*genpm*);


  (* procedure to write on listing file the parsing machine for symbol *)

  procedure writepm(altormb: gramrecptr);
    var curptr: gramrecptr;
  begin
    curptr:= altormb^.defnptr;
    while curptr<>nil do begin
      if curptr^.rectype=gramsym then with curptr^ do begin
	write(instaddr:4,' ');
	if symptr<>nil then
	  if symptr^.texttype=symclass then
	    write('<'||symptr^.symtext||'>':maxsymlen+3,' ')
	  else write(symptr^.symtext:maxsymlen+3,' ')
	else write(' ':maxsymlen+4);
	case at of
	  errorpmret: write('   E');
	  falsepmret: write('   F');
	  truepmret:  write('   T');
	  others: write(at:4)
	end;
	case af of
	  errorpmret: write('    E ');
	  falsepmret: write('    F ');
	  truepmret:  write('    T ');
	  others: write(af:5,' ')
	end;
	if sgptr<>nil then write(sgptr^.symtext:maxsymlen+1,' ')
	else write(' ':maxsymlen+2);
	if fgptr<>nil then writeln(fgptr^.symtext:maxsymlen+1)
	else writeln
      end
      else writepm(curptr);
      curptr:= curptr^.nextptr
    end
  end (*writepm*);


(* procedure to write symbol table on listing file *)

  procedure writesyms;
    var curptr: symrecptr;
  begin
    if firstter<>nil then begin
      writeln; writeln('TERMINAL SYMBOLS'); writeln;
      curptr:= firstter;
      while curptr<>nil do with curptr^ do begin
	write (vindex:4, ' ');
	if texttype = symclass
	  then write ('<', symtext, '>')
	  else write (symtext);
	if scalar_name <> '' then begin
	  if texttype = symclass
	    then write (' ':maxsymlen-length (symtext)-2+3)
	    else write (' ':maxsymlen-length (symtext)+3);
	  write (scalar_name)
	end;
	writeln;
	curptr:= nextsymptr
      end;
      writeln
    end;
    if (firstgencode<>nil) and not (fortables in optionlist) then begin
      writeln; writeln('SEMANTIC OPERATIONS'); writeln;
      curptr:= firstgencode;
      while curptr<>nil do with curptr^ do begin
	writeln (gencode:4, ' ', symtext);
	curptr:= nextsymptr
      end;
      writeln
    end;
    if firstnonter<>nil then begin
      writeln; writeln('NON-TERMINAL SYMBOLS'); writeln;
      curptr:= firstnonter;
      while curptr<>nil do with curptr^ do begin
	if defnptr = nil
	  then write ('******',' ')
	  else write (defnptr^.instaddr:6);
	write (' <', symtext, '>');
	if scalar_name <> '' then
	  write (' ':maxsymlen-length(symtext)+3, scalar_name);
	writeln;
	curptr:= nextsymptr
      end;
      writeln
    end
  end (*writesyms*);


  (* procedure to dump parsing tables to tabfile in pascal format *)

  procedure pasdumppm(altormb: gramrecptr);

    procedure writepmaddr (addr: pmaddr);
     begin
      case addr of
	errorpmret: write (tabfile, 'err_rt');
	falsepmret: write (tabfile, 'failrt');
	truepmret:  write (tabfile, 'succrt');
	others:     write (tabfile, addr:6)
      end;
      write (tabfile, ', ')
     end;

    procedure writesemop (sym: symrecptr);
     begin
      if sym = nil
	then write (tabfile, 'nosemop', ' ':maxsymlen-7)
	else write (tabfile, sym^.symtext, ' ':maxsymlen-length (sym^.symtext));
      write (tabfile, ', ');
     end;

    var curptr: gramrecptr;
  begin
    curptr:= altormb^.defnptr;
    while curptr<>nil do begin
      if curptr^.rectype=gramsym then with curptr^ do begin
	if instaddr = 3
	  then write (tabfile, '     (', chr (11b))
	  else write (tabfile, chr (11b));
	write (tabfile, '(* ', instaddr:6, ' *)  (');
	writepmaddr (at); writepmaddr (af);
	writesemop (sgptr); writesemop (fgptr);

	if symptr = nil then begin		(* who knows *)
	  write (tabfile, ' ', 'badsymbol');
	end
	else if symptr^.rectype = symnonter then begin
	  with symptr^ do begin
	   if scalar_name = ''
	     then write (tabfile, 'nonterminal', ' ':maxsymlen-11)
	     else write (tabfile, scalar_name, ' ':maxsymlen-length (scalar_name));
	    write (tabfile, ',');
	    if defnptr = nil
	      then write (tabfile, 0:6)
	      else write (tabfile, defnptr^.instaddr:6)
	  end
	end
	else (* rectype = symter *) begin
	  with symptr^ do begin
	    if scalar_name = ''
	      then write (tabfile, 'term', vindex:4:o, ' ':maxsymlen-8+7)
	      else write (tabfile, scalar_name, ' ':maxsymlen-length (scalar_name)+7)
	  end
	end;
	if instaddr = (curpmaddr - 1)
	  then writeln (tabfile, ')  );')
	  else writeln (tabfile, '),');
      end
      else pasdumppm(curptr);
      curptr:= curptr^.nextptr
    end
  end(*pasdumppm*);


  (* procedure to dump parsing tables to tabfile in fortran format *)

  procedure fordumppm(altormb: gramrecptr);
    var curptr: gramrecptr;
	pval: 0..1; opval: 0..1000; atval, afval: -3..1000;
  begin
    curptr:= altormb^.defnptr;
    while curptr<>nil do begin
      if curptr^.rectype=gramsym then with curptr^ do begin
	if symptr=nil then begin
	  pval:= 0; opval:= 0
	end
	else if symptr^.rectype=symnonter then begin
	  pval:= 1;
	  if symptr^.defnptr<>nil then
	    opval:= symptr^.defnptr^.instaddr
	  else opval:= 0
	end
	else begin
	  pval:= 0; opval:= symptr^.vindex
	end;
	writeln(tabfile,' ':6,'DATA P(',instaddr:3,'),OP(',instaddr:3,
	'),AT(',instaddr:3,'),AF(',instaddr:3,'),SG(',instaddr:3,
	'),FG(',instaddr:3,')/');
	write(tabfile,' ':5,'* ',pval:3,',',opval:3,',',at:3,',',
	af:3);
	if sgptr<>nil then write(tabfile,',',sgptr^.symtext:3)
	else write(tabfile,',  0');
	if fgptr<>nil then writeln(tabfile,',',fgptr^.symtext:3,'/')
	else writeln(tabfile,',  0/')
      end
      else fordumppm(curptr);
      curptr:= curptr^.nextptr
    end
  end (*fordumppm*);


  (* procedure to dump symbols type declaration to type file *)

  procedure dump_symbols_type;
   var lidx: 0..255;
   var cur: symrecptr;

   procedure writename (sym: symrecptr);
    var l: 0..255;
    begin
      with sym^ do begin
	if rectype = symter
	  then if scalar_name = ''
	    then l := 8
	    else l := length (scalar_name)
	  else l := length (scalar_name);
	if (lidx + l + 2) > 80 then begin
	  writeln (symfile);
	  write (symfile, chr(11b));
	  lidx := 8
	end;
	if (rectype = symter) and (scalar_name = '')
	  then write (symfile, 'term', vindex:4:o)
	else write (symfile, lowercase (scalar_name));
	lidx := lidx + l + 2;
      end
    end;

   begin
    writeln (symfile, 'type');
    writeln (symfile, '  symbols =');
    if (firstter = nil) and (firstnamednt = nil)
      then write (symfile, '     (  badsymbol  );')
      else write (symfile, '     (  badsymbol, ');
    lidx := 19;
    cur := firstter;
    while cur <> nil do begin
      writename (cur);
      cur := cur^.nextsymptr;
      write (symfile, ', ');
    end;
    writeln (symfile);
    writeln (symfile);
    write (symfile, '        nonterminal');
    lidx := 20;
    cur := firstnamednt;
    while cur <> nil do begin
      if cur^.scalar_name <> '' then begin
	write (symfile, ', ');
	writename (cur)
      end;
      cur := cur^.scalar_chain;
    end;
    writeln (symfile, '  );');
   end;


   (* procedure to dump a name array for the symbols type *)  procedure dump_names;
    var cur: symrecptr;
    begin
     writeln (namfile, 'type symnamtable = array[symbols] of string[',maxsymlen+2:3,'];');
     writeln (namfile, 'const symbol_names: symnamtable :=');
     writeln (namfile, '     (  ''*badsymbol*'',');
     cur := firstter;
     while cur <> nil do begin
	write (namfile, chr (11b), '''');
	with cur^ do begin
	  if scalar_name = ''
	    then write (namfile, 'term', vindex:4:o)
	    else write (namfile, symtext);
	  cur := nextsymptr
	end;
	writeln (namfile, ''',');
     end;
     write (namfile, chr(11b), '''*nonterminal*''');
     if firstnamednt = nil then writeln (namfile, '   );')
	else writeln (namfile, ',');
     cur := firstnamednt;
     while cur <> nil do begin
	with cur^ do begin
	  if scalar_name <> '' then begin
	    write (namfile, chr(11b), '''<', symtext, '>''');
	  end;
	  cur := scalar_chain;
	  if cur = nil
	    then writeln (namfile, '    );')
	    else writeln (namfile, ',')
	end
     end;
   end;


  (* procedure to dump the semantic operation scalar type *)

  procedure dumptyp;
    var curptr: symrecptr;
    var lidx: 0..255;
  begin
    if firstgencode<>nil
      then begin
	writeln (semfile, 'type pmsemop =');
	write (semfile, '      ( nosemop, ')
      end
    else writeln(semfile,'type pmsemop = (nosemop);');
    curptr:= firstgencode;
    lidx := 17;
    while curptr<>nil do begin
      if (lidx + 1 + length (curptr^.symtext)) > 72 then begin
	writeln (semfile);
	write (semfile, chr(11b) (* tab *));
	lidx := 8
      end;
      with curptr^ do write (semfile, lowercase (symtext));
      if curptr^.nextsymptr = nil
	then writeln (semfile, ' );')
	else write (semfile, ', ');
      lidx := lidx + length (curptr^.symtext) + 1;
      curptr:= curptr^.nextsymptr
    end;
  end (*dumptyp*);


  (* procedure to write header of .tab file: contains all necessary declarations
     for the parse machine instruction table *)

  procedure write_tab_header;
   begin
    writeln (tabfile, 'const');
    writeln (tabfile, '  err_rt = 0;');
    writeln (tabfile, '  failrt = 1;');
    writeln (tabfile, '  succrt = 2;');
    writeln (tabfile, '  minpmaddr = 3;');
    writeln(tabfile,'  maxpmaddr = ',curpmaddr-1:3,';');
    writeln (tabfile);
    writeln (tabfile, 'type');
    writeln (tabfile, '  pmaddr = 0..maxpmaddr;');
    writeln (tabfile);
    writeln (tabfile, '  pmtabletype = array[minpmaddr..maxpmaddr] of');
    writeln (tabfile, '      packed record');
    writeln (tabfile, '        at, af: pmaddr;');
    writeln (tabfile, '        sg, fg: pmsemop;');
    writeln (tabfile, '        case sym: symbols of');
    write (tabfile, '          nonterminal');
      if (lastnamednt <> nil) andif (lastnamednt^.scalar_name <> '')	(* output upb of nonterminals *)
	then with lastnamednt^ do write (tabfile, '..', lowercase (scalar_name));
      writeln (tabfile, ':');
    writeln (tabfile, '            ( ntaddr: pmaddr )');
    writeln (tabfile, '      end;');
    writeln (tabfile, '');
    writeln (tabfile, 'const');
    writeln (tabfile, '  pmtable: pmtabletype :=');
   end;


begin						(* generate *)


  case sgfgcode of

  settercls: if gentables in optionlist then begin
    changeundef(rootsym);
    flipsymlist(firstter);
    flipsymlist(firstnonter);
    if not (fortables in optionlist) then	(* don't print in fortran mode *)
      flipsymlist(firstgencode);		(* so don't waste time flipping list *)
    if genlist in optionlist then begin
      writeln;
      writeln('ADDR ',' ':(maxsymlen+2)div 2,'OP',' ':(maxsymlen+2)div 2,
	'  AT   AF ',' ':maxsymlen div 2,'SG',' ':maxsymlen,'FG');
      writeln
    end;
    cursym:= firstnonter;
    while cursym<>nil do begin
      if cursym^.defnptr<>nil then begin
	genpm(cursym^.defnptr,truepmret,falsepmret);
	if genlist in optionlist then
	  writepm(cursym^.defnptr)
      end;
      cursym:= cursym^.nextsymptr
    end;
    if genlist in optionlist then writesyms;
    if not (fortables in optionlist) then begin
      dump_symbols_type;
      if symnames in optionlist then dump_names;
      dumptyp;
      write_tab_header;
    end;
    cursym:= firstnonter;
    if (cursym<>nil) and (fortables in optionlist) then
      writeln(tabfile,'      INTEGER P(',curpmaddr-1:3,'),OP(',curpmaddr-1:3,
      '),AT(',curpmaddr-1:3,'),AF(',curpmaddr-1:3,'),SG(',curpmaddr-1:3,
      '),FG(',curpmaddr-1:3,')');
    while cursym<>nil do begin
      if cursym^.defnptr<>nil then
	if fortables in optionlist then fordumppm(cursym^.defnptr)
	else pasdumppm(cursym^.defnptr);
      cursym:= cursym^.nextsymptr
    end;
  end (*settercls*);

  savelhs: begin				(* remember classname on left side of <rule> *)
    if findsymrec(tokenid.tokentext,symclass,curlhs) then
      if (curlhs^.rectype = symnonter)
	andif (curlhs^.defnptr = nil) then	(* defining nonter, whose symbol code is specified *)
      else if curlhs^.rectype<>symundef then begin
	noteerror(gnredef);
	curlhs:= nil
      end;
    if curlhs<>nil then				(*initialize symbol record*)
      with curlhs^ do begin
	if rectype <> symnonter then begin	(* not specified in $nonterminals section *)
	  rectype := symnonter;
	  defnptr:= nil;
	  scalar_chain := nil;
	end;
	nextsymptr:= firstnonter;		(*stack on non-terminal chain*)
	firstnonter:= curlhs;			(*is reversed after $end seen*)
      end;
    defncnt:= -1;				(*push phony alt, left after <rule> parse*)
    pushdefn(gramalt);
    pushdefn(gramalt)				(*push alt popped in <rule> after <complete symbol>*)
  end (*savelhs*);

  chainrhs: begin				(*complete definition after <rule> parse*)
    with defnstack[0] do begin
      (*will have single alt at stack 0 whose definition is rhs of rule*)
      if curlhs<>nil then			(*hook to symbol table entry*)
	curlhs^.defnptr:= altormb
    end
  end (*saverhs*);

  savelit: begin				(*set up symbol table and grammar records for literal or id*)
    if findsymrec(tokenid.tokentext,symlit,cursym) then begin
      if cursym^.rectype<>symter then begin
	noteerror(gnsymcnflct);
	cursym:= nil
      end
    end
    else begin					(*define new terminal symbol*)
      if sawtersection then noteerror(gnundefter);
      curvindex:= curvindex+1;
      with cursym^ do begin
	vindex:= curvindex;
	rectype:= symter;
	nextsymptr:= firstter;
	firstter:= cursym
      end
    end;
    creategsym
  end (*savelit*);

  savecls: begin				(*set up symbol table and grammar records for classname*)
    if not findsymrec(tokenid.tokentext,symclass,cursym) then
      cursym^.rectype:= symundef;
    creategsym
  end (*savecls*);

  savesgen: begin				(*save a pascal-style $s code*)
    if fortables in optionlist then noteerror(gncodincomp)
    else begin
      findgencode;
      curgsym^.sgptr:= cursym
    end
  end (*savesgen*);

  savefgen: begin				(*save a pascal-style $f code*)
    if fortables in optionlist then noteerror(gncodincomp)
    else begin
      findgencode;
      curgsym^.fgptr:= cursym
    end
  end (*savefgen*);

  savesnum: begin				(* save a fortran-style $s code*)
    if fortables in optionlist then begin

      (* note that cheap shot is taken in that text of number is saved
	 in binary tree symbol table rather than internal representation *)

      findgencode;  curgsym^.sgptr:= cursym
    end
    else noteerror(gncodincomp)
  end (*savesnum*);

  savefnum: begin				(* save a fortran-style $f code *)
    if fortables in optionlist then begin
      findgencode; curgsym^.fgptr:= cursym
    end
    else noteerror(gncodincomp)
  end (*savefnum*);

  chainsym: addgrec(curgsym);			(*hook current symbol to current definition*)

  markypart: if defncnt<=maxdefncnt then	(*mark previous symbol as ypart*)
	       if defnstack[defncnt].lastdefn <> nil then
		 defnstack[defncnt].lastdefn^.ypart:= true;

  pushalt: pushdefn(gramalt);			(*set up for alt parse (after "(")*)

  pushmb: pushdefn(grammb);			(*set up for mb parse (after "[")*)

  chainmbalt: popdefn;				(*end of mb/alt parse*)

  markstar: if defncnt<=maxdefncnt then		(*mark previous mb as starred*)
	      if defnstack[defncnt].altormb <> nil then
		defnstack[defncnt].altormb^.closure:= true;

  defnt:					(* specify a pm addr for the nonterminal *)
   begin sawntsection := true;
    if findsymrec (tokenid.tokentext, symclass, cursym)
      then noteerror (gnredef)
    else begin
      with cursym^ do begin			(* see savelhs for init of nonter rec *)
	rectype := symnonter;
	defnptr := nil;
	scalar_chain := nil;
      end;
      if firstnamednt = nil then firstnamednt := cursym
	else lastnamednt^.scalar_chain := cursym;
      lastnamednt := cursym;
    end;
   end;

  defcls,
  deflit: begin					(*stuff an explicitly defined terminal (via $terminals) into table*)
    sawtersection:= true;			(*remember that $terminals section has been seen*)
    if sgfgcode=defcls then texttype:= symclass else texttype:= symlit;
    if findsymrec(tokenid.tokentext,texttype,cursym) then
      noteerror(gnredef)
    else begin
      curvindex:= curvindex+1;
      with cursym^ do begin
	vindex:= curvindex;
	rectype:= symter;
	nextsymptr:= firstter;
	firstter:= cursym
      end
    end
  end (*deflit,defcls*);

  setname:					(* set scalar name of current symbol *)
    cursym^.scalar_name := tokenid.tokentext;

  defgen: begin					(*stuff explicitly defined code (via $codes) into table*)
    sawcodessection:= true;			(*remember that $codes section has been seen*)
    if findsymrec(tokenid.tokentext,symlit,cursym) then
      if cursym^.rectype=symgencode then noteerror(gnredef)
      else noteerror(gncodcnflct)
    else begin
      curgencode:= curgencode+1;
      with cursym^ do begin
	gencode:= curgencode;
	rectype:= symgencode;
	nextsymptr:= firstgencode;
	firstgencode:= cursym
      end
    end
  end (*defgen*);


(* syntactical error recovery entries *)

  gramerr: begin				(*failed to see expected $grammar after $codes/$terminals*)
    noteerror(expgramsy);
    skipuntil([gramsy,class,endsy]);
    if tokenid.tokensym=gramsy then forceloop
    else forcenext
  end;

  ruleerr: begin				(*failure to recognize <rule>*)
    if tokenid.tokensym<>endsy then noteerror(exprule);
    skipuntil([semicolon,endsy]);
    if tokenid.tokensym=semicolon then begin
      tokenid.tokensym:= scan;			(*skip the semicolon*)
      forceloop					(*and go for another loop*)
    end						(*else, let <rule> loop fail and go for $end*)
  end;

  ruleceerr: begin				(*missing colonequals after <class> in <rule>*)
    noteerror(expcolonequals);
    skipuntil([endsy,semicolon]);		(*eat this "rule"*)
    forcenext					(*as following <complete symbol> prints no diagnostic*)
  end;

  rulecserr: begin				(*<complete symbol> in <rule> fails*)
    skipuntil([tokenid.tokensym]);		(*save current symbol*)
    forcenext					(*and let following ';' recognition diagnose, if necessary*)
  end;

  rulesemierror: begin				(*missing ';' at end of <rule>*)
    noteerror(expsemicolon);
     (* be savage, and eat next rule if only ; missing *)
    skipuntil([endsy,semicolon]);
    if tokenid.tokensym=semicolon then forceloop    (*recognize it this time*)
    else forcenext
  end;

  symerr: begin					(*<complete symbol> in mb, alt, or after ',' fails*)
    noteerror(expsymbol);
    skipuntil([tokenid.tokensym]);		(*save current symbol*)
    forcenext					(*and let somebody else decide how to recover*)
  end;

  alterr: begin					(*missing ) at end of alternative list*)
    noteerror(exprightparen);
    skipuntil([rightparen,rightbracket,semicolon,endsy]);
    if tokenid.tokensym=rightparen then forceloop
    else forcenext
  end;

  mberr: begin					(*missing ] at end of metabracket*)
    noteerror(exprightbracket);
    skipuntil([rightparen,rightbracket,semicolon,endsy]);
    if tokenid.tokensym=rightbracket then forceloop
    else forcenext
  end;

  nameerr: begin				(* missing scalar type name for terminal or nonterminal *)
    noteerror (scalarnameexp);
    skipuntil ([endsy,semicolon]);
    forcenext
  end;

  codeerr: begin				(*bad semantic code after $s or $f*)
    noteerror(expsemcode);
    skipuntil([tokenid.tokensym]);		(*save current symbol*)
    forcenext					(*and go on*)
  end


  end						(*case*)

end (*generate*) .
    Q35<