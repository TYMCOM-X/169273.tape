  program pte020;

	type	full_hdr_type=(nons,endl,line,numb);
		hdr_type=endl..numb;
		hdr_sng_type=packed array[1..4] of char;
		wrd_type=packed array[1..7] of char;
		flex_ar_type=array[1..*] of wrd_type;

	var	inptx,outtx: text;
		flnm,outtx_flnm: file_name;
		wrd_ar_len,ar_bnd: integer;
		ptr_wrdar,ptr_new_wrdar: ^flex_ar_type;

	exception excp_end;

    procedure print(numbp: integer; mesgp: string[50]);
	begin writeln(outtx);
	   writeln(outtx,' point =',numbp:3,'     ',mesgp);
	   break(outtx)
	end;

    procedure rd_ln;
	var	i: integer;
		sng: string[256];
      begin i:=0; readln(inptx); read(inptx,sng);
	loop begin i:=i+1;
	  ptr_wrdar^[i]:=substr(sng,1,7);
	  sng:=substr(sng,8);
	  exception
	    program_error:
	      if programstatus=program_subscript then
(* 1 *)		begin print(1,' PROGRAM_SUBSCRIPT signalled');
		  sng:=substr(sng,8)
		end
	      else if programstatus=program_pointer then
(* 2 *)		    begin print(2,' PROGRAM_POINTER signalled');
		      new(ptr_wrdar,ar_bnd); i:=i-1
                    end
              else
(* 3 *)		begin print(3,' PROGRAM_ERROR signalled; resignal it');
			signal()
		end
	end end;
	exception
	  program_error:
	    if programstatus=program_substring then
(* 4 *)	      begin print(4,' PROGRAM_SUBSTRING signalled; line read');
		wrd_ar_len:=i
	      end
	    else begin exception_message; signal() end
      end;

    procedure wr_ln;

	var	hdr_knd: hdr_type;
		i,j: integer;

	function rd_hdr: full_hdr_type;
		var	hdr_vr: hdr_sng_type;
	  begin read(inptx,hdr_vr);
	    assert(hdr_vr='****'); read(inptx,hdr_vr);
	    if hdr_vr='endl' then rd_hdr:=endl
	    else if hdr_vr='line' then rd_hdr:=line
	    else if hdr_vr='numb' then rd_hdr:=numb
	    else rd_hdr:=nons
	  end;

      begin hdr_knd:=rd_hdr;
	case hdr_knd of
(* 10 *)  endl:begin print(10,' end line; signal EXCP_END');
		 signal(excp_end)
	       end;
	  line:begin rd_ln;
		 new(ptr_new_wrdar,wrd_ar_len);
		   if wrd_ar_len>=ar_bnd then j:=ar_bnd
		     else j:=wrd_ar_len;
		 ptr_new_wrdar^:=ptr_wrdar^;
		   writeln(outtx); writeln(outtx,' next line: ');
		   for i:=1 to j do write(outtx,ptr_new_wrdar^[i]);
		   writeln(outtx); break(outtx);
	    dispose(ptr_wrdar); dispose(ptr_new_wrdar); ptr_wrdar:=nil;
		 exception
		   others:
		     begin
(* 11 *)	       print(11,' OTHERS SIGNALLED; resignal it');
	               signal()
		     end
	       end
	end;
	exception
	  program_error:
	    case programstatus of
	      program_compatibility:
(* 12 *)	begin print(12,' PROGRAM_COMPATIBILITY signalled');
		  writeln(outtx); writeln(outtx,' next line: ');
		  for i:=1 to j do write(outtx,ptr_wrdar^[i]);
		  writeln(outtx); break(outtx); dispose(ptr_wrdar);
		  dispose(ptr_new_wrdar); ptr_wrdar:=nil 
		end;
      	      program_assertion: 
(* 13 *)	print(13,' PROGRAM_ASSERTION signalled');
(* 14 *)      program_value: print(14,' PROGRAM_VALUE signalled');
(* 15 *)      program_case: print(15,' PROGRAM_CASE signalled');
	      others: exception_message
	    end;
(* 16 *)  others: begin print(16,' OTHERS signalled; resignal it');
		    signal()
		  end
      end;

    begin rewrite(tty); open(tty);
      writeln(tty,' give an input file name:'); break(tty);
      readln(tty); read(tty,flnm);
	writeln(tty,' give an output file name'); break(tty);
	readln(tty); read(tty,outtx_flnm);
      rewrite(outtx,outtx_flnm); ar_bnd:=2;
      loop begin readln(inptx); 
	wr_ln; ar_bnd:=wrd_ar_len;
	exception
	  program_error:
	    if programstatus=program_file then
(* 20 *)      begin print(20,' PROGRAM_FILE signalled');
		open(inptx,flnm) 
	      end else exception_message
      end end;
      exception
(* 21 *)excp_end: print(21,' EXCP_END signalled');
	allconditions: exception_message
    end.
