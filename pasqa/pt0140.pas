  program pt0140;

	type	sng_type = string[80];
		dtfl_tp = file of sng_type;
		proc_tp = procedure( file_name );

	const	ar_bnd = 30;

	var	sw,wrt_bln: boolean;
		sng_vr: string[256];
		inptx: text;
		dt_fl: dtfl_tp;
		inptx_flnm,out_flnm: file_name;
		dt_flnm: file_name := 'pt0140.tmp';
		vr_er,inpfl_er: io_status;
		ar_cnt,dt_cnt: integer;
		ar_vr: array[1..ar_bnd] of integer;

	exception excp_1,excp_2,excp_3,finish;

    procedure print(msg: string[50]);
      begin writeln(output); 
	writeln(output,msg); break(output)
      end;

$include ptiost.inc

    procedure rd_sng(var sngp:sng_type);

	var	sng_hdr: packed array[1..3] of char;

      begin readln(inptx); 
	      read(inptx,sng_hdr);
	  assert(sng_hdr='***'); read(inptx,sng_hdr);
	if sng_hdr='end' then signal(excp_1) 
	  else begin if sng_hdr='wrt' then wrt_bln:=true;
		 signal(excp_2) 
	       end;
	exception
	  program_error:
	    if programstatus=program_assertion then
	      begin print(' 1 - PROGRAM_ASSERTION signalled');
	        if (sng_hdr='   ') or wrt_bln then 
		  begin if wrt_bln then
			  begin ar_cnt:=ar_cnt+1;
			    ar_vr[ar_cnt]:=dt_cnt+1; wrt_bln:=false
			  end;
		    read(inptx,sngp)
		  end else signal(excp_3)
	      end
	    else begin print(' Unexpected PROGRAM_ERROR in RD_SNG');
		   exception_message; signal()
		 end;
	  others: begin print(' 2 - OTHERS signalled in RD_SNG');
		    signal()
		  end;
      end;

    procedure wr_file(var data_flnm: file_name);

	label	10;

	var	sng: sng_type;
		data_fl: file of sng_type;
		bl_vr: boolean;

      begin bl_vr:=true;
    10: begin if bl_vr then begin reset(data_fl,data_flnm);
			      vr_er:=iostatus(data_fl);
				iosts_print(3,vr_er); bl_vr:=false;
			      if vr_er=io_opnf then
				rewrite(data_fl,data_flnm)
			    end;
	  data_fl^:='   new data file   '; put(data_fl); 
					dt_cnt:=dt_cnt+1;
	  loop begin
	    rd_sng(sng); 
		data_fl^:=sng; put(data_fl); dt_cnt:=dt_cnt+1;
	    exception
	      excp_2:
		begin print(' 4 - EXCP_2 signalled');
		  if wrt_bln then print(' 5 - WRT command given')
			else begin print(' 6 - wrong command given');
		  	       data_fl^:=' ??? wrong command'; 
			       put(data_fl); dt_cnt:=dt_cnt+1
			     end
		end;
	      io_error:
		begin vr_er:=exiostatus;
			iosts_print(7,vr_er);
		  close(inptx); open(inptx,inptx_flnm)
		end;
	      others: begin print(' 8 - OTHERS signalled in WR_FILE');
			signal()
		     end
	  end end;
	  exception
	    io_error:
	      begin vr_er:=exiostatus;
		iosts_print(9,vr_er);
		  if vr_er<>io_opnf then
		    close(data_fl);
		rewrite(data_fl,data_flnm,[preserve]); bl_vr:=false
	      end
	end; goto 10;
	exception 
	    others: begin print(' 10 - OTHERS signalled in WR_FILE');
			    close(data_fl); signal()
		    end
      end;

    procedure dtfl_crtn (var flnmp: file_name; procp: proc_tp);

      begin
	    procp(flnmp);
	exception
	  excp_3: begin print(' 11 - EXCP_3 signalled; wrong line');
		    sw:=true
		  end;
	  excp_1: begin
	       print(' 12 - EXCP_1 signalled; read the rest of INPTX');
		    loop 
			readln(inptx)
		    end 
		  end
      end;

    procedure dtfl_seek (var dtflp: dtfl_tp);
	
	label	101,102;

	var	bln_sw: boolean;
		cnt: integer;
		dtflp_er: io_status;

      begin bln_sw:=false;
	  open(inptx,dt_flnm);
	    inpfl_er:=iostatus(inptx);
	      iosts_print(13,inpfl_er);
	  readln(inptx);
	    inpfl_er:=iostatus(inptx);
	      iosts_print(14,inpfl_er);
	  while not eof(inptx) do
	    begin read(inptx,sng_vr);
	   writeln(output); writeln(output,sng_vr); 
			writeln(output); break(output);
		readln(inptx) 
	    end; close(inptx);
    101:begin if bln_sw then goto 102;
	      reset(dtflp,dt_flnm);
	    seek(dtflp,ar_vr[1]);
    102:  begin 
	    for cnt:=1 to ar_bnd do
	      begin seek(dtflp,ar_vr[cnt]);
		print(dtflp^) 
	      end;
	    exception
	      io_error:
	        begin vr_er:=exiostatus;
			iosts_print(15,vr_er);
		  signal()
	        end;
	      others: begin print(' Unexpected error in DTFL_SEEK');
			exception_message; signal()
		      end
	  end; 
	  exception
	      io_error:
		begin vr_er:=exiostatus;
			iosts_print(16,vr_er);
		      if bln_sw then
		        begin print(' 17 - It is here for 2nd time');
				signal()
			    end;
		      close(dtflp);
			reset(dtflp,dt_flnm,[seekok]);
		  bln_sw:=true
		end 
	end; goto 101;
	exception
	  io_error:
	    begin vr_er:=exiostatus;
			iosts_print(18,vr_er);
		close(dtflp)
	    end
      end;

  begin
    begin open(tty); rewrite(tty); writeln(tty); 
	write(tty,' give an input file name: '); break(tty);
	  readln(tty); read(tty,inptx_flnm); writeln(tty);
	write(tty,' give an output file name: '); break(tty);
      	  readln(tty); read(tty,out_flnm);
	rewrite(output,out_flnm);
      begin ar_cnt:=0; dt_cnt:=0; wrt_bln:=false; sw:=true;
	  rewrite(inptx,inptx_flnm,[preserve]);
        while sw do
	  begin sw:=false;
	    dtfl_crtn(dt_flnm,wr_file);
	  end;
        exception
	  io_error:
	    begin vr_er:=exiostatus;
			iosts_print(19,vr_er);
	        close(inptx);
	      dtfl_seek(dt_fl);
		writeln(output); writeln(output,' data file contents');
		reset(dt_fl,dt_flnm); 
		  while not eof(dt_fl) do
		    begin sng_vr:=dt_fl^; writeln(output,sng_vr);
		      get(dt_fl)
		    end; 
	      empty(dt_fl) 
	    end
      end;
      exception
	io_error: begin vr_er:=exiostatus;
			  iosts_print(20,vr_er);
		      scratch(dt_fl);
			vr_er:=iostatus(dt_fl);
			  iosts_print(21,vr_er);
		    reset(dt_fl,dt_flnm);
		      vr_er:=iostatus(dt_fl);
			iosts_print(22,vr_er);
		      if vr_er=io_opnf then signal(finish)
		  end 
    end;
      exception
	finish:
	  print(' 23 - *** THE END ***');
	allconditions:
	  begin print(' Unexpected ALLCONDITIONS signalled');
		exception_message; close
	  end
    end.
