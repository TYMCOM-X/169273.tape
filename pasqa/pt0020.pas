  program pt0020;

	type	sng_type=string[256];

	var	
		tp_flnm: file_name;
		tp_fl1,tp_fl2: file of sng_type;
		sng_vr: sng_type;

$include ptprou.inc

    begin tp_flnm:='typ020.tmp'; rewrite(tp_fl1,tp_flnm);
	tp_fl1^:='string 1   ';	put(tp_fl1);
	tp_fl1^:='string 2   ';	put(tp_fl1);
	tp_fl1^:='string 3   ';	put(tp_fl1);
	tp_fl1^:='string 4   ';	put(tp_fl1);
	tp_fl1^:='string 5   ';	put(tp_fl1);
      close(tp_fl1);

$include pt0out.inc
      rewrite(output,out_flnm);

      reset(tp_fl1,tp_flnm,[seekok]);
      seek(tp_fl1,3); sng_vr:=tp_fl1^; print(sng_vr);
      reset(tp_fl2,tp_flnm,[seekok]);
	print('second file is open');
	sng_vr:=tp_fl2^; print(sng_vr);
      seek(tp_fl1,5); sng_vr:=tp_fl1^; print(sng_vr);
	seek(tp_fl2,4); sng_vr:=tp_fl2^; print(sng_vr);
      close
    end.
