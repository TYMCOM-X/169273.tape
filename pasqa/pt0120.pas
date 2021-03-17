  program pt0120;

	const	sngsize_const=120000b;

	type	str=record r_ptr: ^str;
			   r_strg: string[sngsize_const]
		     end;
		strptr_type=^str;

	var     str_p1,str_p2: strptr_type;
		stp: integer;
		sng: string[10];
		bl_vr: boolean;
		outtx: text;
		outtx_flnm: file_name;
		sngsize: integer;

    procedure print(numb:integer; mesgp: string[50]);
      begin writeln(outtx); 
	writeln(outtx,' point =',numb:3,'     ',mesgp);
	break(outtx)
      end;

    procedure alloc_mem;
	
	var	loc: integer;
	static var	full_size: integer := 0;

      begin 
	if str_p1=nil then
	  begin new(str_p2); str_p1:=str_p2 end
	else
	  begin new(str_p2^.r_ptr);
	    str_p2:=str_p2^.r_ptr; str_p2^.r_ptr:=nil
	  end;
	loc:=ord(str_p2); full_size:=full_size+sngsize+3;
	if masked(attention) then
	  begin print(1,' ATTENTION masked; signal it');
	    writeln(tty);
	    write(tty,' signal ATTENTION; then press [c/r]'); 
	    break(tty); readln(tty); read(tty,sng) 
	  end
	else if stp=5 then
	  begin print(2,' ATTENTION unmasked; signal it');
	    writeln(tty);
	    write(tty,' signal ATTENTION'); 
	    break(tty); readln(tty); read(tty,sng) 
	  end;
	  writeln(outtx);
	  write(outtx,' step =',stp:4,'     location =',loc:7:o);
	  writeln(outtx,'     full size =',full_size:7:o); 
	  break(outtx);
	exception 
	  others:  
	    begin print(3,' OTHERS signalled'); 
	      writeln(outtx,' for location =',loc:7:o); break(outtx);
	      signal() 
	    end
      end;

    procedure stack_ovf(var ptrp: ^str);

	var 	str_vr: str;
		loc: integer;
		bl_vr:boolean;

      begin str_vr:=ptrp^; loc:=ord(ptrp); bl_vr:=true;
	while bl_vr do
	  begin bl_vr:=false;
	    dispose(str_p2); ptrp:=str_vr.r_ptr; str_p2:=ptrp;
	      writeln(outtx); 
	      writeln(outtx,' location =',loc:7:o,'  - memory freed');
	      break(outtx);
	    exception
	      special_error:
		if specialstatus=special_disp_ptr then
		  begin print(10,' SPECIAL_DISP_PTR signalled');
		    str_p2:=ptrp; bl_vr:=true
		  end
		else begin exception_message; signal() end
	  end;
	if ptrp<>nil then stack_ovf(ptrp);
	exception
	  others: begin print(11,' OTHERS signalled');
		    writeln(outtx,' for location =',loc:7:o); 
		    break(outtx); signal()
		  end
      end;

    begin open(tty); rewrite(tty); 
        writeln(tty); write(tty,' give an output file name: ');
	break(tty); readln(tty); read(tty,outtx_flnm);
	rewrite(outtx,outtx_flnm);
      str_p1:=nil; str_p2:=nil; stp:=0; bl_vr:=true; 
      sngsize:=sngsize_const div 5;
      loop begin
	while bl_vr do
	  begin
	    loop stp:=stp+1;
	      if stp=3 then mask(attention);
	        alloc_mem;
	      if stp=3 then
		begin if pending(attention) then
			print(20,' ATTENTION pending')
		end
	      else if stp=4 then
		begin print(21,' unmask ATTENTION');
		  unmask(attention)
		end
	    end;
	    exception
		attention: print(22,' ATTENTION signalled');
		others: begin print(23,' OTHERS signalled');
			  signal()
			end
	  end; str_p2:=nil;
	stack_ovf(str_p1);
	exception
	  storage_overflow:
	    begin print(24,' STORAGE_OVERFLOW signalled'); 
		bl_vr:=false
	    end;
	  others: 
	    begin print(25,' OTHERS signalled'); signal() end
      end end;
      exception
	stack_overflow: print(26,' STACK_OVERFLOW signalled');
	allconditions: begin close; exception_message end
    end.
