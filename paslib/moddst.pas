program MODDST;

$include opstat.typ

$include opnams.con

$include opmode.con

$include opsort.con

external procedure readinstr(var f:text; var p:p_instr);
external procedure killinstr(var p:p_instr);

var
	inf,outf: text;
	infilename,outfilename: string;
	p_ins: p_instr;
	p_op: p_opspec;
	p_case: p_casedis;

	(* counting variables *)

type
	accum_accmodes = array[1..*,1..21] of integer;
var
	accvec: array[ubyte] of record
		count: integer;
		paccum: ^accum_accmodes
		end;
	i,j,slot_num,numops,opnd_num: integer;

begin
  open(tty);
  rewrite(tty);
  writeln(tty,'MODDST');
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

  for i := 0 to 255 do begin accvec[i].count := 0; accvec[i].paccum := nil end;

  while not eof(inf) do begin
    readinstr(inf,p_ins);

(* count statistics *)

    if accvec[p_ins^.opcode].paccum = nil then begin
      numops := 0;
      for opnd_num := 1 to 6 do begin
	exit if opmodes[p_ins^.opcode][opnd_num].access in [ac_branch,ac_none];
	numops := opnd_num
	end;
      new(accvec[p_ins^.opcode].paccum,numops);
      for opnd_num := 1 to numops do for slot_num := 1 to 21 do
	accvec[p_ins^.opcode].paccum^[opnd_num,slot_num] := 0
      end;
    accvec[p_ins^.opcode].count := accvec[p_ins^.opcode].count + 1;
    p_op := p_ins^.o_list;
    opnd_num := 0;
    while p_op <> nil do begin
      opnd_num := opnd_num + 1;
      case p_op^.mode of
	0..3: slot_num := 1;
	4..7: slot_num := p_op^.mode - 2;
	8..15: begin
		if p_op^.reg = 15 then slot_num := p_op^.mode + 6
		else slot_num := p_op^.mode - 2
		end
	end;
      accvec[p_ins^.opcode].paccum^[opnd_num,slot_num] :=
	accvec[p_ins^.opcode].paccum^[opnd_num,slot_num] + 1;
      if p_op^.mode = 4 then opnd_num := opnd_num - 1;
      p_op := p_op^.o_next
      end;

    killinstr(p_ins)
    end;

(* output summary *)

  writeln(outf,'Distribution of operand access modes by opcode: file ',
    filename(inf));
  writeln(outf);
  writeln(outf,'    lit  ndx  reg @reg  dec  inc @inc  b^D @b^D ' ||
    ' w^D @w^D  l^D @l^D  imm  abs  b^A @b^A  w^A @w^A  l^A @l^A');
  for i := 0 to 255 do begin
    j := alforder[i];
    if accvec[j].paccum <> nil then begin
      writeln(outf,opnames[j],accvec[j].count:6);
      for opnd_num := 1 to upperbound(accvec[j].paccum^) do begin
	write(outf,accessnames[opmodes[j][opnd_num].access][1],
	  datanames[opmodes[j][opnd_num].data][1]);
	for slot_num := 1 to 21 do
	  write(outf,accvec[j].paccum^[opnd_num,slot_num]:5);
	writeln(outf)
	end
      end
    end

  end.
    