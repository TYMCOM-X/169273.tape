
    procedure iosts_print(pnt_num: integer; iostsp: io_status);
      begin writeln(tty);
	write(tty,' ',pnt_num:2,'     iostatus = ');
	case iostsp of
	  io_ok  : writeln(tty,'IO_OK  ');
	  io_novf: writeln(tty,'IO_NOVF');
	  io_povf: writeln(tty,'IO_POVF');
	  io_dgit: writeln(tty,'IO_DGIT');
	  io_govf: writeln(tty,'IO_GOVF');
	  io_intr: writeln(tty,'IO_INTR');
	  io_rewr: writeln(tty,'IO_REWR');
	  io_eof : writeln(tty,'IO_EOF ');
	  io_outf: writeln(tty,'IO_OUTF');
	  io_inpf: writeln(tty,'IO_INPF');
	  io_seek: writeln(tty,'IO_SEEK');
	  io_illc: writeln(tty,'IO_ILLC');
	  io_nemp: writeln(tty,'IO_NEMP');
	  io_opnf: writeln(tty,'IO_OPNF') 
	end; break(tty)
      end;

  