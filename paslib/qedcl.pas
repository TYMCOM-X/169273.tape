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

module qedcl;

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


$PAGE QINIT
public procedure qsetmarkdefault (line: cmdline);
begin
  qdata.markstring := line
end;

public procedure qsettabdefault (default: boolean);
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
$PAGE QINITEXEC
(* initialization routine which calls four other initialization
   routines in the mandantory order *)

public procedure qinitexec( var buffer: qbuffer);

begin
  qinitbuf( buffer );
  qsetmarkdefault( ':$PAGE:');
  qsettabdefault( true );
  qinit( buffer )
end;
$PAGE QEXECUTE
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
	  qunds (buffer, fno, lno, true, err);
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
$PAGE Begin Main Command Loop
begin						(* begin body of qexecute *)
  saved_bounds := false;

1:						(* each pass over following code parses and executes one
						   command in the command line. *)
  qldparse (line, lindex, nld, ld, qdata.wildswitch, err);
  chkerr;
  if not command (line, lindex, cmd) then begin
    if (lindex > length (line)) orif (line[lindex] = ';') then begin
      if nld = 0 then begin
	if findflag then
	  return;		                (* but don't do anything under find *)
	if buffer.curlineno = qdollar_val (buffer) then
	  seterr (qbadln);		        (* nothing to print *)
	fno := buffer.curlineno + 1;	        (* get range of lines to print *)
	lno := buffer.curlineno + qdata.linecount;
	if lno > qdollar_val (buffer) then
	  lno := qdollar_val (buffer);          (* use remainder if too few *)
	qlistlines (buffer, fno, lno, ttyoutput, true, false, qdata.tabprint, err);
	chkerr;
	buffer.curlineno := lno;
	goto 2;				        (* to exit interpretation loop *)
      end
      else
	cmd := print			        (* Ld <eoln> - assume print and eval Ld normally *)
    end
    else
      seterr (qbadcmd);			        (* no known command name, not eoln *)
  end;
  if not (cmd in allowed_cmds) then
    seterr (qnocmd);
  qldeval (buffer, ld, execrange, cmdrange, err);
  qlddispose (ld);
  chkerr;
  chkrng (cmd, nld, cmdrange, findflag, err);
  chkerr;
$PAGE Command Dispatcher and Action Routines
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
      end;					(* bound *)

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
      writeln(tty,'No longer implemented - use SUBSTITUTE');

    list:
      begin
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
	  qlistlines (buffer, cmdrange.lbound, cmdrange.hbound, list_file, false, true, true, err);
          chkerr;
	  buffer.curlineno := cmdrange.hbound
	end

	(* list entire file, numbered by section *)
	else begin
	  pushbounds(buffer, err);				(* save current bounds *)
	  idx := 0; fno := 1;
	  loop					(* one pass certain, since no addr error => nonzero line cnt *)
	    repeat
	      idx := idx + 1
	    until (idx > qdollar_val (buffer)) orif
		    spredmatch (qgetline (buffer, idx, err), buffer.mark, err);
	    lno := idx - 1;
	    if lno >= fno then begin		(* may have zero length section at start *)
	      qsetbounds (buffer, fno, lno, true, err);   (* address new region *)
	      chkerr;
	      if fno <> 1 then begin    (* page between parts, not at begin and end *)
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

	if list_file <> ttyoutput then  (* don't close the teletype !! *)
	  close (list_file)
      end;

    find:
      begin
	find_cmds := allowed_cmds - [why, quit, exitcmd, resetcmd, load, save, writecmd,
				     opencmd, closecmd, setcmd, bound, uparrow];
	cnt := 0;
	if not spredparse (line, lindex, predi, qdata.wildswitch, err) then err := qnofindpred;
	chkerr;
	confirm := query ('Confirm');
	idx := cmdrange.lbound;
	while idx <= cmdrange.hbound do
	begin
	  source := qgetline (buffer, idx, err);
	  chkerr;
	  if spredmatch (source, predi, err) then
	  begin
	    pat := spatgetdefault;
	    findble := idx;
	    buffer.curlineno := idx;		(* set if matched *)
	    doit := true;
	    if confirm then begin
	      writeln (tty, source);
	      doit := query ('OK')
	    end;
	    if doit then
	    begin
	      cnt := cnt + 1;			(* count confirmed matches, not executions *)
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
          then write (tty, 'Found and confirmed ')
          else write (tty, 'Found ');
        write (tty, numtochar (cnt), ' time' );
        if cnt <> 1 then write ( tty, 's' );
        writeln ( tty, '.' );
	lindex := length (line) + 1;		(* find uses rest of line *)
      end;					(* find *)

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
	if (total <= 2) orif query (numtochar(total) || ' lines, OK') then
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
      end;					(* join *)

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

$PAGE QEDCL
public procedure qedcl
(       var buffer:     qbuffer;		(* working buffer *)
	allowed_cmds:   qed_cmd_set);		(* only commands allowed *)

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
  new (emergency_heap_space);      (* we get this space now so that in the
				      case of a heap overflow, we can dispose
				      of it and thus let the user save his
				      buffer.  *)

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
      qederror (ttyoutput, err, 1)
    end;
    exception
      storage_overflow: begin
			  err := qheapfull;
			  buffer.curfileok := false;
			  buffer.curfile := '';  (* don't let him ruin his file unless he wnats to *)

			  writeln (tty,'?Error -- the heap has overflowed.');
			  writeln (tty,'Save any unwritten changes in a new file.');
			  writeln (tty,'The next heap overflow will be fatal.');

			  dispose (emergency_heap_space);	(* give the user his last piece of
								 the pie...   *)
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
end.						(* qedcl *)
   f %[