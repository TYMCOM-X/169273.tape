  program pt0070;

	label	1;

	public 	const	max_pr_mem: integer = mem_cst;
			mchwrd_ar_len: integer = len_cst;
			out_txm_flnm_cst: file_name = 'outtxm.tmp';

	        const	blar_pr_mem_ct: blar_pr_mem_type = 
						(true,true);

	public  var	blar_pr_mem: blar_pr_mem_type;
			ptr1,ptr2: ptr_main_rcd_type;
			rcd_vr: main_rcd;
			outtx,outtxm: text;

	var	mem_ptr: ptr_mchwrd_ar_type;
		tp_flnm,inptx_flnm,outtx_flnm,bn_flnm: file_name;
		sng_vr: sng_type;

    public procedure jump_error;
	      begin goto 1 end;

    begin open(tty); rewrite(tty); blar_pr_mem:=blar_pr_mem_ct;
	      tp_flnm:='typed.tmp'; bn_flnm:='binary.tmp';
	writeln(tty); write(tty,' give input file name: '); break(tty);
	readln(tty); read(tty,inptx_flnm);
       writeln(tty); write(tty,' give output file name: '); break(tty);
	readln(tty); read(tty,outtx_flnm);
      rewrite(outtx,outtx_flnm);
      rd_inp(inptx_flnm); print(' *** input file is read');
      tp_fl_ops(tp_flnm); close(outtx);
	new(mem_ptr); mem_ptr^:=rcd_vr.main_ar;
	pr_mem(mem_ptr,32,1,' called to type memory for RCD_VR');
      rewrite(outtx,outtx_flnm,[preserve]);
      bin_fl_ops(rcd_vr.main_ar,bn_flnm,outtx);
	print(' *** data from the OUTTXM file:'); writeln(outtx);
	  open(outtxm,out_txm_flnm_cst); readln(outtxm);
	while not eof(outtxm) do
	  begin readln(outtxm,sng_vr); writeln(outtx,sng_vr) end;
	sng_vr:=' *** current date is '||date();
	print(sng_vr);
    1: print(' *** finish ***'); close
    end.

    