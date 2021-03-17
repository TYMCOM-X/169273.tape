  program pte040;

	type	io_try_type=(try_int,try_sng,try_real,put_sng);

	var	subint: -20..20;
		int,int1,int2: integer;
		rl,rl1,rl2: -1..2.0**120 prec 7;
		sng: string[28];
		io_vr: io_try_type;
		io_er_vr: io_status;
		math_vr: math_status;
		outtx,inptx: text;
		outtx_flnm,inptx_flnm: file_name;
		bl_vr: boolean;

    procedure print(numb: integer; mesgp: string[50]);
	begin writeln(outtx);
	  writeln(outtx,' point =',numb:3,'     ',mesgp);
	  break(outtx)
	end;

    begin open(tty); rewrite(tty); writeln(tty);
        write(tty,' give an input file name : '); break(tty);
        readln(tty); read(tty,inptx_flnm);
      open(inptx,inptx_flnm);	   writeln(tty);
	write(tty,' give an output file name : '); break(tty);
	readln(tty); read(tty,outtx_flnm);
      rewrite(outtx,outtx_flnm);
	io_vr:=try_int; math_vr:=math_flt_und; bl_vr:=true;
      	readln(inptx);
      loop begin
	while bl_vr do
	  begin case io_vr of
		  try_int: read(inptx,subint);
		  try_sng: begin read(inptx,sng);
			     getstring(sng,int1,int2)
			   end;
		 try_real: read(inptx,rl1,rl2);
		  put_sng: putstring(sng,rl1:14:4,rl2:14:4,
						int1:5,int2:5) 
		end;
	    exception
	      io_error:
		begin io_er_vr:=iostatus(inptx);
		  case io_er_vr of
		    io_dgit: begin print(1,' IO_DGIT signalled');
			       if io_vr=try_int then
					print(2,' for integer var')
			       else begin print(3,' for real var');
				      read(inptx,rl2); sng:='';
				      io_vr:=put_sng
				    end
			     end;
		    io_novf: begin print(4,' IO_NOVF signalled');
			       io_vr:=try_sng
			     end;
		    io_govf: begin print(5,' IO_GOVF signalled');
			       readln(inptx); read(inptx,int2);
			       io_vr:=try_real
			     end;
		     others: begin print(6,' IO_ERROR signalled');
			       signal()
			     end
	          end
		end;
	      others: begin print(7,' OTHERS signalled');
			exception_message; signal()
		      end
	  end;
	case math_vr of
	  math_flt_und: begin subint:=0; rl2:=rl2**int2;
			  rl:=rl1**(-int1); rl:=rl/rl2
			end;
	  math_flt_ovf: begin rl1:=rl1**int1; rl:=rl1*rl2 end;
	  math_int_ovf: int:=int1**int2;
	  math_zero_divide: rl:=int1/subint;
	  math_arg_arcsin: rl:=arcsin(int1);
	  math_arg_arccos: rl:=arccos(int1)
	end;
	exception
	  io_error:
	    if iostatus=io_povf then
	      begin print(10,' IO_POVF signalled'); bl_vr:=false;
		writeln(outtx,sng,int1:5,int2:5); break(outtx)
	      end
	    else begin print(11,' IO_ERROR signalled');
		   exception_message; signal()
		 end;
	  math_error:
	    case mathstatus of
	      math_flt_und: 
		begin print(12,' MATH_FLT_UND signalled');
		  math_vr:=math_flt_ovf
		end;
	      math_flt_ovf:
		begin print(13,' MATH_FLT_OVF signalled');
		  math_vr:=math_int_ovf
		end;
	      math_int_ovf:
		begin print(14,' MATH_INT_OVF signalled');
		  math_vr:=math_zero_divide
		end;
	      math_zero_divide:
		begin print(15,' MATH_ZERO_DIVIDE signalled');
		  math_vr:=math_arg_arcsin
		end;
	      math_arg_arcsin:
		begin print(16,' MATH_ARG_ARCSIN signalled');
		  math_vr:=math_arg_arccos
		end;
	      others: begin print(17,' MATH_ERROR signalled');
			signal()
		      end
	    end;
	  others: begin print(18,' OTHERS signalled');
		    exception_message; signal()
		  end
      end end;
      exception
	math_error:
	  if mathstatus=math_arg_arccos then
	    print(20,' MATH_ARG_ARCCOS signalled') 
	  else begin print(21,' MATH_ERROR signalled');
		 exception_message
	       end;
	allconditions: begin print(22,' ALLCONDITIONS signalled');
			 exception_message
		       end
    end.
