  program pt0030 options special(word,coercions);

	label	1;

	const	max_pr_mem=1;
		mchwrd_ar_len=35;

	type	sclr_type=(zero,one,two,three,four,five);
		subsclr_type=one..four;
		ind_type=(blue,rose,red,green,brown);
		stsclr_type=set of sclr_type;
		bl_ar_type=packed array[sclr_type] of boolean;
		stsub_type=set of subsclr_type;
		st_char=set of char;
		wrd_type=packed array[1..7] of char;
		sng_type=string[256];
		mchwrd_ar_type=array[1..mchwrd_ar_len] of machine_word;
		blar_pr_mem_type=packed array[1..max_pr_mem] 
							of boolean;
		drl_type=-10e6..10e6 prec 13;
		subrcd_type=packed record
				intar: packed array
				    [ind_type,subsclr_type] of integer;
				stindar: packed array[ind_type]
						of stsub_type
			    end;
		rcd_type=packed record
				rlnum: drl_type;
				subrcd: subrcd_type;
				wrd: wrd_type;
				numb: sclr_type;
				stsclr: stsclr_type;
				lngst: st_char;
				bl_ar: bl_ar_type
			 end;
		main_rcd=record ptr: ^main_rcd;
				case boolean of
				  true:(main_ar: mchwrd_ar_type);
				  false:(elmt: rcd_type)
			 end;
		ptr_mchwrd_ar_type=^mchwrd_ar_type;

	const	blar_pr_mem_ct: blar_pr_mem_type = (true);

	var	blar_pr_mem: blar_pr_mem_type;
		ptr1,ptr2: ^main_rcd;
		rcd_vr: main_rcd;
		mem_ptr: ptr_mchwrd_ar_type;
		tp_flnm,bn_flnm: file_name;
		sng_vr: sng_type;


$include ptprio.inc

    procedure jump_error;
      begin goto 1 end;

    procedure pr_mem(var ptrp: ptr_mchwrd_ar_type; 
                     wrdnum,blindp: integer; mesgp: string[35]);
	var	i,j,addr_vr: integer;
      begin
	if blar_pr_mem[blindp] then
	  begin print(' this is PR_MEM procedure');
	    print(mesgp);
	      j:=1; writeln(output);
	    for i:=1 to wrdnum do
	      begin write(output,'  ',ptrp^[i]:12:o);
		if j=5 then begin writeln(output); j:=1 end
		 else j:=j+1
	      end; 
	    writeln(output); writeln(output,' ***'); break(output);
	    blar_pr_mem[blindp]:=false
          end
      end;

    procedure rd_inp(flnmp: file_name);

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
		rl_vr: -10e6..10e6 prec 6;
		ptr3: ^main_rcd;

	function rl_convrt(ip: integer): drl_type;
		var	drl_vr: drl_type;
	  begin drl_vr:=ip;
	    while abs(drl_vr)>=1 do drl_vr:=drl_vr/10;
	    rl_convrt:=drl_vr
	  end;

      begin open(fl,flnmp); readln(fl);
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
		      begin print(' too many lines'); jump_error end
		else
		  begin readln(fl); chrt:=fl^;
		    while chrt<>':' do
		      begin get(fl); chrt:=fl^ end; get(fl); bvr:=true;
		    with ptr2^.elmt do
		      begin
			with subrcd do
			  for ind_vr:=blue to brown do
			    begin stindar[ind_vr]:=[];
			      for sub_vr:=one to four do
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
			    end; readln(fl); read(fl,sng);
			st_vr:=['+','-','*','/'];
			ps:=index(sng,'.'); cnt_vr:=search(sng,st_vr);
			getstring(substr(sng,1,cnt_vr-1),
				  i1:ps-1,chrt1,i2:cnt_vr-ps-1);
			drl_vr1:=rl_convrt(i2); drl_vr1:=drl_vr1+i1;
			drl_vr1:=drl_vr1+drl_const;
			chrt:=sng[cnt_vr]; sng:=substr(sng,cnt_vr+1);
			ps:=index(sng,'.');
			getstring(sng,i1:ps-1,chrt1,i2);
			drl_vr2:=rl_convrt(i2); rl_vr:=drl_vr2+i1;
			rl_vr:=rl_vr/rl_const;
			case chrt of
			  '+': drl_vr1:=drl_vr1+rl_vr;
			  '-': drl_vr1:=drl_vr1-rl_vr;
			  '*': drl_vr1:=drl_vr1*rl_vr;
			  '/': drl_vr1:=drl_vr1/rl_vr;
			end;   rlnum:=drl_vr1;
			bl_ar_vr[sclr_vr]:=true; bl_ar:=bl_ar_vr;
			stsclr_vr:=stsclr_vr+[sclr_vr];
			stsclr:=stsclr_vr;
			if sclr_vr<five then sclr_vr:=succ(sclr_vr);
		      end;  ptr2:=ptr2^.ptr; readln(fl)
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
      end;


    procedure rd_typed(rcd_num,rcd_qnt: integer;
		       var fl_rcd: file of rcd_type; var fl_txt: text);

	procedure out_rcd(var rcdp: rcd_type);
		var	chrt: char;
			sng: sng_type;
			sng_vr: string[20];
			ind_vr: ind_type;
			sub_vr: subsclr_type;

	  begin writeln(fl_txt);
	    with rcdp do
	      begin case numb of
		      zero: write(fl_txt,'   zero        ');
		       one: write(fl_txt,'    one        ');
		       two: write(fl_txt,'    two        ');
		     three: write(fl_txt,'  three        ');
		      four: write(fl_txt,'   four        ');
		      five: write(fl_txt,'   five        ');
		    end; write(fl_txt,wrd,'     '); sng:='';
		for chrt:=minimum(char) to maximum(char) do
		  if chrt in lngst then sng:=sng||chrt;
		writeln(fl_txt,sng,'     ',rlnum:0:13:e);
		with subrcd do
		  begin sng:='';
		    for ind_vr:=blue to brown do
		      for sub_vr:=one to four do
			if sub_vr in stindar[ind_vr] then
			  begin
                            putstring(sng_vr,intar[ind_vr,sub_vr]:5);
			    sng:=sng||sng_vr
			  end else sng:=sng||'.'
		  end
	      end; writeln(fl_txt,sng); writeln(fl_txt); break(fl_txt)
	  end;

      begin if rcd_qnt=1 then seek(fl_rcd,rcd_num)
	    else begin readrn(fl_rcd,rcd_num,rcd_vr.elmt);
		       out_rcd(rcd_vr.elmt)
		 end;
	    out_rcd(fl_rcd^)
      end;

    procedure tp_fl_ops(flnmp: file_name);

	var	fl: file of rcd_type;

      begin rewrite(fl,flnmp); ptr2:=ptr1;
	while ptr1<>nil do
	  begin fl^:=ptr1^.elmt; put(fl);
	    ptr1:=ptr2^.ptr; dispose(ptr2); ptr2:=ptr1
	  end; close(fl);
	update(fl,flnmp);
	rd_typed(2,1,fl,output);
	rd_typed(4,2,fl,output);
	close(fl)
      end;

    procedure bin_fl_ops(var bin_arp: mchwrd_ar_type;
				flnmp: file_name; var fl_txt: text);

	var	fl: file of *;
		i: integer;
		ar_vr: array[1..3] of machine_word;

      begin rewrite(fl,flnmp); i:=2;
	repeat write(fl,bin_arp[i]:2);
	       i:=i+4
	  until i>mchwrd_ar_len;  close(fl);
	writeln(fl_txt); writeln(fl_txt,' binary file contents');
	writeln(fl_txt);
	reset(fl,flnmp); read(fl,ar_vr);
	for i:=1 to 3 do write(fl_txt,'  ',ar_vr[i]:12:o);
	  writeln(fl_txt); writeln(fl_txt); close(fl);
	update(fl,flnmp); readrn(fl,11,ar_vr);
	for i:=1 to 3 do write(fl_txt,'  ',ar_vr[i]:12:o);
	  writeln(fl_txt); writeln(fl_txt); close(fl) 
      end;

    begin blar_pr_mem:=blar_pr_mem_ct;
	  tp_flnm:='typed.tmp'; bn_flnm:='binary.tmp';

$include ptinou.inc

        rewrite(output,out_flnm);
      rd_inp(inp_flnm);
      tp_fl_ops(tp_flnm);
	new(mem_ptr); mem_ptr^:=rcd_vr.main_ar;
	pr_mem(mem_ptr,35,1,' called to type memory for RCD_VR');
      bin_fl_ops(rcd_vr.main_ar,bn_flnm,output); 
	sng_vr:=' *** current date is '||date();
	print(sng_vr);
    1: print(' *** finish ***');
	 close
    end.

 