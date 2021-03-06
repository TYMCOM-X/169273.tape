  module pt007j;

	external var	outtx: text;

    public procedure print(message: string[50]);
      begin writeln(outtx); writeln(outtx,message); break(outtx) end;

    public procedure p_integer( sngp: sng_type; intp: integer);
      begin writeln(outtx);
	writeln(outtx,' ',sngp,' = ',intp:10); break(outtx) 
      end;

    public procedure p_char( sngp: sng_type; chrp: char);
      begin writeln(outtx);
	writeln(outtx,' ',sngp,' = ',chrp); break(outtx)
      end;

    public procedure p_boolean( sngp: sng_type; boolp: boolean);
      begin writeln(outtx);
	write(outtx,' ',sngp,' = '); 
	  if boolp then writeln(outtx,'true') 
		   else writeln(outtx,'false');
	break(outtx)
      end;

    public procedure p_sclr_type( sngp: sng_type; sclrp: sclr_type);
      begin writeln(outtx);
	write(outtx,' ',sngp,' = '); 
	  case sclrp of
	    zero : writeln(outtx,'zero');
	    one  : writeln(outtx,'one');
	    two  : writeln(outtx,'two');
	    three: writeln(outtx,'three');
	    four : writeln(outtx,'four');
	    five : writeln(outtx,'five') 
	  end; break(outtx)
      end;

    public procedure p_ind_type( sngp: sng_type; indp: ind_type);
      begin writeln(outtx);
	write(outtx,' ',sngp,' = '); 
	  case indp of
	    blue : writeln(outtx,'blue');
	    rose : writeln(outtx,'rose');
	    red  : writeln(outtx,'red');
	    green: writeln(outtx,'green');
	    brown: writeln(outtx,'brown') 
	  end; break(outtx)
      end;

    public procedure p_stsclr_type( sngp:sng_type; stscp:stsclr_type);
	
	var	sc: sclr_type;

      begin writeln(outtx);
	write(outtx,' ',sngp,' = '); 
	  for sc:=zero to five do
	    if sc in stscp then case sc of
				  zero : write(outtx,'zero ');
				  one  : write(outtx,'one ');
				  two  : write(outtx,'two ');
				  three: write(outtx,'three ');
				  four : write(outtx,'four ');
				  five : write(outtx,'five ') 
				end;	
	writeln(outtx); break(outtx)
      end;

    public procedure p_bl_ar_type( sngp: sng_type; blarp: bl_ar_type);

	var	sc: sclr_type;

      begin writeln(outtx); writeln(outtx,' ',sngp,' : ');
	for sc:=zero to five do
	  begin write(outtx,'      ');
	    case sc of
	      zero : write(outtx,'zero  -> ');
	      one  : write(outtx,'one   -> ');
	      two  : write(outtx,'two   -> ');
	      three: write(outtx,'three -> ');
	      four : write(outtx,'four  -> ');
	      five : write(outtx,'five  -> ') 
	    end;
	    if blarp[sc] then writeln(outtx,'true')
			 else writeln(outtx,'false')
	  end; break(outtx)
      end;

    public procedure p_st_char( sngp: sng_type; stchp: st_char);

	var 	chrt: char;

      begin writeln(outtx); write(outtx,' ',sngp,' = '); 
	for chrt:=minimum(char) to maximum(char) do
	  if chrt in stchp then
	    write(outtx,chrt,' ');
	writeln(outtx); break(outtx)
      end;

    public procedure p_sng_type( sngp: sng_type; sngpar: sng_type);
      begin writeln(outtx);
	writeln(outtx,' ',sngp,' = ',sngpar); break(outtx)
      end;

    public procedure p_rl_type( sngp: sng_type; rlp: rl_type);
      begin writeln(outtx);
	writeln(outtx,' ',sngp,' = ',rlp:0:7:e); break(outtx)
      end;

    public procedure p_drl_type( sngp: sng_type; drlp: rl_type);
      begin writeln(outtx);
	writeln(outtx,' ',sngp,' = ',drlp:0:14:e); break(outtx)
      end;

    public procedure p_rcd_type( sngp: sng_type; rcdp: rcd_type);

	var	ind: ind_type;
		sbsc: subsclr_type;
		sc: sclr_type;
		chrt: char;

      begin writeln(outtx); writeln(outtx,' ',sngp,' : ');
	with rcdp do
	  begin
	    writeln(outtx,'       srlnum = ',srlnum:0:7:e);
	    writeln(outtx,'       rlnum  = ',rlnum:0:14:e);
	    writeln(outtx,'       subrcd : ');
	    with subrcd do
	      begin
 		writeln(outtx,'	          intar : ');
		  for ind:=blue to brown do
		    begin write(outtx,'   	    ');
		      case ind of
			blue  : write(outtx,'blue  -> ');
			rose  : write(outtx,'rose  -> ');
			red   : write(outtx,'red   -> ');
			green : write(outtx,'green -> ');
			brown : write(outtx,'brown -> ') 
		      end;
		      for sbsc:=one to four do
			write(outtx,intar[ind,sbsc]:10);
		      writeln(outtx)
		    end;
	        write(outtx,'   		        ^          ^');
		  writeln(outtx,'	   ^          ^');
		write(outtx,'			        |	   |');
		  writeln(outtx,'	   |	      |');
	       write(outtx,'			      one        two');
		  writeln(outtx,'        three       four');
		writeln(outtx,'	        stindar : ');
		  for ind:=blue to brown do
		    begin write(outtx,'   	    ');
		      case ind of
			blue  : write(outtx,'blue  -> ');
			rose  : write(outtx,'rose  -> ');
			red   : write(outtx,'red   -> ');
			green : write(outtx,'green -> ');
			brown : write(outtx,'brown -> ') 
		      end;
		      for sbsc:=one to four do
			if sbsc in stindar[ind] then
			  case sbsc of
			    one   : write(outtx,'one ');
			    two   : write(outtx,'two ');
			    three : write(outtx,'three ');
			    four  : write(outtx,'four ') 
			  end;
		      writeln(outtx)
	       	    end 
	      end;
	    writeln(outtx,'       wrd    = ',wrd);
	      write(outtx,'	  numb   = ');
	        case numb of
		  zero  : writeln(outtx,'zero ');
		  one   : writeln(outtx,'one  ');
		  two   : writeln(outtx,'two  ');
		  three : writeln(outtx,'three');
		  four  : writeln(outtx,'four ');
		  five  : writeln(outtx,'five ') 
		end;
	      write(outtx,'	  stsclr = ');
		for sc:=zero to five do
		  if sc in stsclr then 
		    case sc of
		      zero  : write(outtx,'zero ');
		      one   : write(outtx,'one ');
		      two   : write(outtx,'two ');
		      three : write(outtx,'three ');
		      four  : write(outtx,'four ');
		      five  : write(outtx,'five ') 
		    end;
		writeln(outtx);
	      write(outtx,'	  lngst  = ');
		for chrt:=minimum(char) to maximum(char) do
		  if chrt in lngst then write(outtx,chrt,' ');
		writeln(outtx);
	    writeln(outtx,'	  bl_ar  : ');
		for sc:=zero to five do
		  begin write(outtx,'	        ');
		    case sc of
		      zero  : write(outtx,'zero  -> ');
		      one   : write(outtx,'one   -> ');
		      two   : write(outtx,'two   -> ');
		      three : write(outtx,'three -> ');
		      four  : write(outtx,'four  -> ');
		      five  : write(outtx,'five  -> ') 
		    end;
		    if bl_ar[sc] then writeln(outtx,'true')
				 else writeln(outtx,'false') 
		  end
	  end;
	break(outtx)
      end.
    