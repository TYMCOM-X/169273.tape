program slrtbl;

const
    max_syms = 128;
    max_prods = 200;
    max_states = 200;
    max_table = 5000;

type
    sym_ind = 0 .. max_syms;
    prod_ind = 0 .. max_prods;
    state_ind = 0 .. max_states;
    table_ind = 0 .. max_table;

    table_ptr = record
	first, count: table_ind
    end;

var
    nsyms: sym_ind;
    nterms: sym_ind;
    nprods: prod_ind;
    nstates: state_ind;
    nacts: table_ind;
    ngotos: table_ind;
    ntable: table_ind;

    prod: array [prod_ind] of record
	lhs: sym_ind;
	nrhs: 0 .. 8;
	rhs: array [1..8] of sym_ind
    end;

    act_list: array [state_ind] of table_ptr;

    goto_list: array [sym_ind] of table_ptr;

    table: array [table_ind] of record
	match, arg: integer
    end;

$INCLUDE getiof
$PAGE readtables

procedure readtables;

var i, j, incode: integer;

begin
    readln (nsyms,nterms);
    for i := 1 to (nsyms+7) div 8 do readln;
    readln (nprods);
    for i := 1 to nprods do
	with prod [i] do begin
	    read (lhs,nrhs);
	    for j := 1 to nrhs do
		read (rhs[j]);
	    readln;
	end;
    readln (nstates,nacts);
    for i := 1 to nstates do
	with act_list [i] do begin
	    read (incode,count);
	    if incode >= 0
		then first := incode
		else first := act_list[-incode].first;
	end;
    readln;
    for i := 1 to nacts do
	with table [i] do
	    read (match,arg);
    readln;
    readln (nterms,ngotos);
    for i := 1 to nterms do
	with goto_list [i] do begin
	    read (incode,count);
	    if incode >= 0
		then first := incode + nacts
		else first := act_list[-incode].first;
	end;
    readln;
    for i := 1 to ngotos do
	with table [nacts+i] do
	    read (match,arg);
    ntable := nacts + ngotos;

end (* readtables *);
$PAGE writefile

const tab = chr(9);

var template: text;
    val_type: string;

procedure writefile;

var n: integer;
    i: integer;

procedure copy;
begin
    while not eof(template) andif (template^ <> '%') do
	if eoln(template) then begin
	    writeln;
	    readln (template);
	end
	else begin
	    write (output,template^);
	    get (template);
	end;
    get (template);
end (* copy *);

procedure comma ( n, counter, line_size: integer );
begin
    write (',',' ':n-1);
    if counter mod line_size = 0 then begin
	writeln;
	write (tab);
    end;
end (* comma *);

begin
    reset (template,'SLRTBL.TEM');

(***********     STATE LIST     ***********)

    copy;
    write (nstates+nterms-1:4);
    copy;
    for i := 1 to nstates do begin
	with act_list [i] do
	    write ('(',first:5,',',first+count-1:5,')');
	comma (2,i,4);
    end;
    for i := 1 to nterms - 1 do begin
	with goto_list [i+1] do
	    write ('(',first:5,',',first+count-1:5,')');
	if i <> nterms - 1 then
	    comma (2,nstates+i,4);
    end;

(***********     LOOKUP TABLE     ***********)

    copy;
    write (ntable:5);
    copy;
    for i := 1 to ntable do begin
	if i <= nacts
	    then write (max(table[i].match-nterms,0):5)
	    else write (table[i].match:5);
	if i <> ntable then
	    comma (1,i,10);
    end;

(***********     ACTION TABLE     ***********)

    copy;
    write (ntable:5);
    copy;
    for i := 1 to ntable do begin
	n := table[i].arg;
	if n = 1001 then
	    write ('(ac     )')
	else case n div 1000 of
	    1:  write ('(rd,',n mod 1000 - 1:4,')');
	    2:  if i <= nacts
		    then write ('(sr,',n mod 1000 - 1:4,')')
		    else write ('(gr,',n mod 1000 - 1:4,')');
	    3:  if i <= nacts
		    then write ('(sh,',n mod 1000:4,')')
		    else write ('(go,',n mod 1000:4,')');
	    4:  write ('(er     )')
	end (* case *);
	if i <> ntable then
	    comma (2,i,5);
    end;

(***********     PRODUCTIONS     ***********)

    copy;
    write (nprods-1:4);
    copy;
    for i := 1 to nprods - 1 do begin
	with prod [i+1] do
	    write ('(',lhs-1+nstates:4,',',nrhs:4,')');
	if i <> nprods - 1 then
	    comma (2,i,5);
    end;

(***********     REDUCTIONS     ***********)

    copy;
(*    reset (input,filename(input) || '.TBL');*)
reset(input,'A.TBL');
    while not eof andif (input^ <> '%') do readln;
    readln;
    while not eof andif (input^ <> '%') do readln;
    readln;
    while not eof andif (input^ <> '%') do readln;
    readln;
    for i := 1 to nprods - 1 do begin
	writeln (i:5,':  begin');
	write (tab,'  ');
	while not eof andif (input^ <> '%') do begin
	  while not eoln do begin
	    if input^ <> '#' then begin
	      write (input^);
	      get (input);
	    end
	    else begin
	      get (input);
	      if eoln then
		write ('#')
	      else if input^ = '#' then begin
		write ('result');
		get (input);
	      end
	      else if input^ = '[' then begin
		write ('value[stk_ind+');
		get (input);
	      end
	      else
		write (output, '#');
	    end;
	  end;
	  writeln;
	  write (tab, '  ');
	  readln;
	end;
	readln;
	write ('end');
	if i <> nprods - 1 then begin
	  writeln (';');
	  writeln;
	end;
    end;
    copy;
end (* writefile *);
$PAGE main program

begin
    open (tty);
    rewrite (tty);
    if not getiofiles (input, 'TBL', output, 'PAR') then stop;
    readtables;
    writefile;
end.
