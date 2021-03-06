
    procedure iosts_print(pnt_num: integer; iostsp: io_status);
      begin writeln(output);
	write(output,' point =',pnt_num:3,'     iostatus = ');
	case iostsp of
	  io_ok  : writeln(output,'IO_OK  ');
	  io_novf: writeln(output,'IO_NOVF');
	  io_povf: writeln(output,'IO_POVF');
	  io_dgit: writeln(output,'IO_DGIT');
	  io_govf: writeln(output,'IO_GOVF');
	  io_intr: writeln(output,'IO_INTR');
	  io_rewr: writeln(output,'IO_REWR');
	  io_eof : writeln(output,'IO_EOF ');
	  io_outf: writeln(output,'IO_OUTF');
	  io_inpf: writeln(output,'IO_INPF');
	  io_seek: writeln(output,'IO_SEEK');
	  io_illc: writeln(output,'IO_ILLC');
	  io_nemp: writeln(output,'IO_NEMP');
	  io_opnf: writeln(output,'IO_OPNF') 
	end; break(output)
      end;

    