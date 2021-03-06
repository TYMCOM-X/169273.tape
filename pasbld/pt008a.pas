  module pt008a;

	external var	ptr1,ptr2: ptr_main_rcd_type;
			outtx: text;

    public procedure rd_inp(flnmp: file_name);

	label	10,11,12;

	const	drl_const=111.111111111;
		rl_const=3.3;

	var	fl: text;
		bvr: boolean;
		sng: sng_type;
		sng_hdr: packed array[1..4] of char;
		sclr_vr: sclr_type;
		stsclr_vr: stsclr_type;
		bl_ar_vr: bl_ar_type;
		st_vr: st_char;
		chrt,chrt1: char;
		i,i1,i2,ps,strt_pos,len_vr,cnt_vr: integer;
		ind_vr: ind_type;
		sub_vr: subsclr_type;
		drl_vr1,drl_vr2: drl_type;
		rl_vr: rl_type;
		ptr3: ptr_main_rcd_type;
		radix: integer;
		bln_mth: boolean;

	function rl_convrt(ip: integer): drl_type;
		var	drl_vr: drl_type;
	  begin drl_vr:=ip;
	    while abs(drl_vr)>=1 do drl_vr:=drl_vr/radix;
	    rl_convrt:=drl_vr
	  end;

      begin 
	open(fl,flnmp); readln(fl);	radix:=0; bln_mth:=false;
	for sclr_vr:=zero to five do bl_ar_vr[sclr_vr]:=false;
        sclr_vr:=zero; ptr1:=nil; ptr2:=nil; stsclr_vr:=[];
	while not eof(fl) do
	  begin read(fl,sng_hdr);
	    if sng_hdr='name' then
	      begin readln(fl); read(fl,sng); len_vr:=length(sng);
		bvr:=true; cnt_vr:=1;
		while bvr do
		  begin chrt:=sng[cnt_vr]; st_vr:=[];
		    while chrt=' ' do
		      begin cnt_vr:=cnt_vr+1;
			if cnt_vr>len_vr then bvr:=false
			  else chrt:=sng[cnt_vr]
		      end;  strt_pos:=cnt_vr;
		    while chrt<>' ' do
		      begin if not ([chrt]<=st_vr) then
			       st_vr:=st_vr+[chrt];
                        cnt_vr:=cnt_vr+1;
			if cnt_vr>len_vr then bvr:=false
			  else chrt:=sng[cnt_vr];
		      end;
		    if strt_pos<>cnt_vr then
		      begin new(ptr3);
			if ptr2<>nil then ptr2^.ptr:=ptr3;
			ptr2:=ptr3;
			if ptr1=nil then ptr1:=ptr3;
			with ptr3^ do
			  begin ptr:=nil;
				elmt.numb:=sclr_vr;
				elmt.lngst:=st_vr;
				elmt.wrd:=substr(sng,strt_pos,
						 cnt_vr-strt_pos)
			  end;
			if bvr then
			  if sclr_vr=five then bvr:=false
			    else sclr_vr:=succ(sclr_vr)
		      end
		  end;  ptr2:=ptr1;  sclr_vr:=zero; readln(fl)
	      end
	    else if sng_hdr='line' then
	      begin if ptr1=nil then
		      begin print(' no names'); jump_error end;
		if ptr2=nil then
		      begin print(' too many lines'); jump_error end;
	      10: begin if bln_mth then goto 11;
			readln(fl); chrt:=fl^;
		    while chrt<>':' do
		      begin get(fl); chrt:=fl^ end; get(fl); bvr:=true;
	      11:   with ptr2^.elmt do
		      begin if bln_mth then
			      begin bln_mth:=false; goto 12 end;
			with subrcd do
			  for ind_vr:=blue to brown do
			    begin stindar[ind_vr]:=[];
			      for sub_vr:=one to four do
			       begin
				if bvr then
				  begin read(fl,i:5);
				    if i<>0 then stindar[ind_vr]:=
                                              stindar[ind_vr]+[sub_vr];
				    intar[ind_vr,sub_vr]:=i; chrt:=fl^;
				    while (chrt<>',')and(chrt<>';') do
				      begin get(fl);
					if eoln(fl) then get(fl);
					chrt:=fl^
				      end; 
				    if chrt=';' then bvr:=false
				      else get(fl)
				  end
				else intar[ind_vr,sub_vr]:=0
			       end	
			    end; readln(fl); read(fl,sng);
			st_vr:=['+','-','*','/'];
			ps:=index(sng,'.'); cnt_vr:=search(sng,st_vr);
			getstring(substr(sng,1,cnt_vr-1),
				  i1:ps-1,chrt1,i2:cnt_vr-ps-1);
		12:	drl_vr1:=rl_convrt(i2); 
			drl_vr1:=drl_vr1+i1;
			drl_vr1:=drl_vr1+drl_const;
			chrt:=sng[cnt_vr]; sng:=substr(sng,cnt_vr+1);
			ps:=index(sng,'.');
			getstring(sng,i1:ps-1,chrt1,i2);
			drl_vr2:=rl_convrt(i2); 
			rl_vr:=drl_vr2+i1;
			rl_vr:=rl_vr/rl_const;
			case chrt of
			  '+': drl_vr1:=drl_vr1+rl_vr;
			  '-': drl_vr1:=drl_vr1-rl_vr;
			  '*': drl_vr1:=drl_vr1*rl_vr;
			  '/': drl_vr1:=drl_vr1/rl_vr;
			end;   rlnum:=drl_vr1;  srlnum:=sin(rl_vr);
			bl_ar_vr[sclr_vr]:=true; bl_ar:=bl_ar_vr;
			stsclr_vr:=stsclr_vr+[sclr_vr];
			stsclr:=stsclr_vr;
			if sclr_vr<five then sclr_vr:=succ(sclr_vr);
		      end;  
		p_rcd_type('ptr2^.elmt',ptr2^.elmt);
	exception
	  math_error:
	    begin if mathstatus=math_zero_divide then
	      excmes_print(' RD_INP - MATH_ZERO_DIVIDE signalled') 
		  else
	      excmes_print(' RD_INP - Unexpected MATH_ERROR signalled');
			    radix:=10; bln_mth:=true
	    end
		  end; goto 10;
		exception
		  program_error:
		    begin if programstatus=program_value then
		      excmes_print(' RD_INP - PROGRAM_VALUE signalled')
			  else
		      excmes_print(' RD_INP - PROGRAM_ERROR signalled');
		        ptr2:=ptr2^.ptr; readln(fl) 
		    end;
		  others:
		    begin 
		  excmes_print(' RD_INP - Unexpected OTHERS signalled');
			signal()
		    end
	      end
	    else
	      begin readln(fl,sng);
		sng:=sng_hdr||sng; print(sng)
	      end
	  end;
	if ptr1=nil then
	  begin print(' no data at all'); jump_error end;
	if ptr2<>nil then
	  begin print(' too few lines'); jump_error end
      end.

    