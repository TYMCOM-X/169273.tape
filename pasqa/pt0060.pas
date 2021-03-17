  program pt0060;

	type	proc_tp1 = procedure(char; var string[80]);
		rcd_tp	 = record
			     prc: proc_tp1;
			     sng: string[80]
			   end;
		proc_tp2 = procedure(var ^rcd_tp; proc_tp2);
	
	var	pt1,pt2: ^rcd_tp;
		bln1,bln2: boolean;
	      first_comment: string[80] := ' ** 1st allocation of PT1';

$include ptprio.inc

    procedure proc_prnt(var ptp: ^rcd_tp; prp: proc_tp2);

	var	adr_val,siz_val: integer;

      begin print(' PROC_PRNT entered'); writeln(output);
	  adr_val:=ord(ptp); siz_val:=extent(ptp);
	write(output,' addr = ',adr_val:6:o,'     size = ',siz_val:3);
	writeln(output,'     ',ptp^.sng); break(output);
	  if bln1 then prp(ptp,prp);
	print(' PROC_PRNT finishing') 
      end;

    procedure proc_rd_cmd(var ptp: ^rcd_tp; prp: proc_tp2);

	var 	c: char;

      begin print(' PROC_RD_CMD entered');
	  readln(input);
	while not eoln(input) do
	  with ptp^ do
	    begin read(input,c); 
	      prc(c,sng) 
	    end;
	print(' PROC_RD_CMD still running; status:');
	  writeln(output,' PTP^.SNG = ',ptp^.sng); break(output);
	if bln2 then proc_prnt(pt2,prp)
		else proc_prnt(pt1,prp);
	print(' PROC_RD_CMD finishing')
      end;

    procedure proc_realloc(cp: char; var rtnp: string[80]); forward;

    procedure proc_alloc(cp: char; var rtnp: string[80]); 

      begin 
	if cp='I' then
	  begin print(' PROC_ALLOC entered');
	      allocate(pt1,40);
	    pt1^.prc:=proc_realloc;
	    pt1^.sng:=rtnp;
		bln1:=true;
	    proc_prnt(pt1,proc_rd_cmd)
	  end
	else if cp='A' then
	  begin print(' PROC_ALLOC entered');
	    dispose(pt2);
	    allocate(pt1,30);
	      read(input,pt1^.sng:30);
	    pt1^.prc:=proc_alloc; bln1:=false
	  end
	else return;
	  print(' PROC_ALLOC finishing')
      end;

    procedure proc_realloc(cp: char; var rtnp: string[80]);

      begin 
	if cp='R' then
	  begin bln2:=true;
	      print(' PROC_REALLOC entered');
	    pt2:=pt1; allocate(pt1,55);
	      read(input,rtnp:30);
	    pt1^.prc:=proc_alloc;
	    pt1^.sng:=' ** 2nd allocation of PT1'
	  end
	else if cp='M' then
	  begin bln2:=false;
	      print(' PROC_REALLOC entered');
	    read(input,rtnp:30)
	  end
	else return;
	  print(' PROC_REALLOC finishing') 
      end;

    begin

$include ptopio.inc

	proc_alloc('I',first_comment)

    end.
