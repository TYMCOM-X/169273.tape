  program pt0012;

	var	rcd_vr: rcd_tp;
		out_flnm: file_name;

      begin reset(rcd_file,rcd_fl_name,[seekok]);

$include pt0out.inc
        rewrite(output,out_flnm);

	seek(rcd_file,2); rcd:=rcd_file^;
	with rcd do
	  begin mem_prnt(11,main_mem_ar);
	        mem_prnt(9,mem_ar)
	  end;

	get(rcd_file); rcd:=rcd_file^;
	with rcd do mem_prnt(9,mem_ar);

	readrn(rcd_file,1,rcd);
	with rcd do mem_prnt(9,mem_ar);

	  rcd_vr:=rcd_file^;
	with rcd_vr do
	  begin rcd.mem_prnt(11,main_mem_ar);
	        mem_prnt(9,mem_ar)
	  end

      end.
  