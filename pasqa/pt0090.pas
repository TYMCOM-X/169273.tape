  program pt0090;

	const	int1_cst=8;
		int2_cst=567435275b;
		int3_cst=731354436b;
		rl1_cst=481.035;
		rl2_cst=893.12;

	var	subint: -20..20;
		int,int1,int2,int3,int4: integer;
		rl,rl1,rl2: -1..2.0**110 prec 7;
		math_vr: math_status;
		out_flnm: file_name;

    procedure print(numb: integer; mesgp: string[50]);
	begin writeln(output);
	  writeln(output,' point =',numb:3,'     ',mesgp);
	  break(output)
	end;

    procedure init_vars(var intp1,intp2,intp3,intp4: integer;
			var subintp: -20..20;
			var rlp1,rlp2: -1..2.0**110 prec 7);
      begin intp1:=int1_cst; intp2:=int2_cst; intp3:=int3_cst;
	    intp4:=9; subintp:=0; rlp1:=rl1_cst; rlp2:=rl2_cst
      end;

    begin open(tty); rewrite(tty); writeln(tty);
      write(tty,' give an output file name: '); break(tty);
      readln(tty); read(tty,out_flnm);
	rewrite(output,out_flnm);
	math_vr:=math_flt_und;
	init_vars(int1,int2,int3,int4,subint,rl1,rl2);
      loop begin
	case math_vr of
	  math_flt_und: begin print(1,' MATH_FLT_UND');
			  rl2:=rl2**int4;
			  rl:=rl1**(-int1); 
			  rl:=rl/rl2 
			end;
	  math_flt_ovf: begin print(2,' MATH_FLT_OVF');
			  rl1:=rl1**int1; 
			  rl:=rl1*rl2 
			end;
	  math_int_ovf: begin print(3,' MATH_INT_OVF');
			  int:=int2*int3
			end;
	  math_zero_divide: begin print(4,' MATH_ZERO_DIVIDE');
				rl:=int1/subint 
			    end 
	end;
	exception
	  math_error:
	    case mathstatus of
	      math_flt_und: 
		begin print(5,' MATH_FLT_UND signalled');
		  math_vr:=math_flt_ovf
		end;
	      math_flt_ovf:
		begin print(6,' MATH_FLT_OVF signalled');
		  math_vr:=math_int_ovf
		end;
	      math_int_ovf:
		begin print(7,' MATH_INT_OVF signalled');
		  math_vr:=math_zero_divide
		end;
	      others: begin print(8,' MATH_ERROR signalled');
			signal()
		      end
	    end;
	  others: begin print(100,' Unexpected OTHERS signalled'); 
		    exception_message; signal()
		  end
      end end;
      exception
	math_error:
	  if mathstatus=math_zero_divide then
	    print(9,' MATH_ZERO_DIVIDE signalled') 
	  else begin print(101,' Unexpected MATH_ERROR signalled');
		 exception_message; close
	       end;
	allconditions: 
	  begin print(102,' Unexpected ALLCONDITIONS signalled');
			 exception_message; close
	  end
    end.
 