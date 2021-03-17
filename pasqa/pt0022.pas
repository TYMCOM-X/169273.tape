  program pt0022;

	type	sng_type=string[256];

	var	
		tp_flnm: file_name;
		tp_fl1,tp_fl2: file of sng_type;
		sng_vr: sng_type;

$include ptprou.inc

    begin tp_flnm:='typ022.tmp'; rewrite(tp_fl1,tp_flnm);
	tp_fl1^:='string 1   ';	put(tp_fl1);
	tp_fl1^:='string 2   ';	put(tp_fl1);
	tp_fl1^:='string 3   ';	put(tp_fl1);
	tp_fl1^:='string 4   ';	put(tp_fl1);
	tp_fl1^:='string 5   ';	put(tp_fl1);
      close(tp_fl1);

$include pt0out.inc
      rewrite(output,out_flnm);

      update(tp_fl1,tp_flnm);
      seek(tp_fl1,3); print(tp_fl1^);
	tp_fl1^:='string 15   '; put(tp_fl1); break(tp_fl1);
      update(tp_fl2,tp_flnm);
	if eof(tp_fl2) then print(' association failure for tp_fl2')
      else begin
	print('second file is open');
      readrn(tp_fl2,2,sng_vr); print(sng_vr); print(tp_fl2^);
      tp_fl2^:='string 10'; put(tp_fl2); break(tp_fl2);
      read(tp_fl1,sng_vr); print(sng_vr); print(tp_fl1^);
	sng_vr:='string 12 ';
      writern(tp_fl2,4,sng_vr); break(tp_fl2);
      print(tp_fl1^)  
	   end;
	close
    end.
