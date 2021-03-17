program IMODST;

$include opstat.typ

$include opnams.con

external procedure readinstr(var f:text; var p:p_instr);
external procedure killinstr(var p:p_instr);

var
	inf,outf: text;
	infilename,outfilename: string;
	p_ins: p_instr;
	p_op: p_opspec;
	p_case: p_casedis;

	(* counting variables *)

	i,maxop: ubyte;
	distrib: array[ubyte] of integer;
	total: integer;
	maxfreq,nstars: integer;
	perstar: real;

const linelen = 60;
type chrline = packed array[1..linelen] of char;
const
	starline: chrline :=
'************************************************************';
	spaceline: chrline :=
'                                                            ';

begin
  open(tty);
  rewrite(tty);
  writeln(tty,'IMODST');
  writeln(tty);
  write(tty,'Input file (default .OPS): ');
  break;
  readln(tty);
  read(tty,infilename);
  write(tty,'Output file (default .LST): ');
  break;
  readln(tty);
  read(tty,outfilename);

  reset(inf,'.OPS ' || infilename);
  rewrite(outf,'.LST ' || outfilename);

(* initialize *)

  for i := minimum(i) to maximum(i) do distrib[i] := 0;
  total := 0;
  maxfreq := 0;

  while not eof(inf) do begin
    readinstr(inf,p_ins);

(* count statistics *)

    p_op := p_ins^.o_list;
    while p_op <> nil do begin
      if (p_op^.mode in [0..3]) or ((p_op^.mode=8) and (p_op^.reg=15)) then
	distrib[p_ins^.opcode] := distrib[p_ins^.opcode] + 1;
      p_op := p_op^.o_next
      end;
    if maxfreq < distrib[p_ins^.opcode] then maxfreq := distrib[p_ins^.opcode];
    total := total + 1;
    killinstr(p_ins)
    end;

(* output summary *)

  writeln(outf,'Opcodes with literal/immediate operands: file ',filename(inf));
  writeln(outf,'Total opcodes counted: ',total);
  writeln(outf);
  writeln(outf,' Opcode ',spaceline,'Total Freq');

  perstar := maxfreq / linelen;

  loop
    maxfreq := 0;
    for i := minimum(i) to maximum(i) do if maxfreq < distrib[i]
      then begin
	maxfreq := distrib[i];
	maxop := i
	end;
    exit if maxfreq = 0;

    nstars := round(maxfreq / perstar);
    writeln(outf,opnames[maxop],starline:nstars,spaceline:linelen-nstars,
      maxfreq:5,maxfreq * 100.0 / total :6:1, '%');
    distrib[maxop] := 0
    end
  end.
