  module pt007c;

	external var	ptr1,ptr2: ptr_main_rcd_type;
			outtx: text;

    external procedure rd_typed( integer; integer;
		                var file of rcd_type; var text);

    public procedure tp_fl_ops(flnmp: file_name);

	var	fl: file of rcd_type;

      begin rewrite(fl,flnmp); ptr2:=ptr1;
	while ptr1<>nil do
	  begin p_rcd_type('ptr1^.elmt',ptr1^.elmt);
		fl^:=ptr1^.elmt; put(fl);
	    ptr1:=ptr2^.ptr; dispose(ptr2); ptr2:=ptr1
	  end; close(fl);
	update(fl,flnmp);
	rd_typed(2,1,fl,outtx);
	rd_typed(4,2,fl,outtx);
	close(fl)
      end.

    