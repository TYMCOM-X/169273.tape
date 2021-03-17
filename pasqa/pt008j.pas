  module pt008j;

	external var	outtx: text;

    public procedure print(message: string[50]);
      begin writeln(outtx); 
	writeln(outtx,message); break(outtx) 
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
		    sc:=zero;
		loop
		        write(outtx,'	        ');
		    case sc of
		      zero  : write(outtx,'zero  -> ');
		      one   : write(outtx,'one   -> ');
		      two   : write(outtx,'two   -> ');
		      three : write(outtx,'three -> ');
		      four  : write(outtx,'four  -> ');
		      five  : write(outtx,'five  -> ') 
		    end;
		    if bl_ar[sc] then writeln(outtx,'true')
				 else writeln(outtx,'false');
		    sc:=succ(sc) 
		end
	  end;
	break(outtx)
      end.
