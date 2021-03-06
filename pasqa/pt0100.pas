  program pt0100;

	const	maxnum=9;
		len_of_wrd=10;
		max_arcompt=7;

	type	sng50_type=string[50];
		sngwrd_type=string[len_of_wrd];
		sng_ar_type=array[1..maxnum] of sng50_type;

	const	sng_ar: sng_ar_type = (
		'this  is a test   ',
		'  t1234hi87s is a   te654st',
		'too@  long  words    abcdefg5hijkl  abcde3fghijkl',
		'first@illegal@word   is +-/a12*, second  :&a1*',
		'     one    word@',
		'  one  word and#  second word',
		'one word  and+-132a/  second word',
		'  too  many  words - abc abc abc abc abc abc ',
		'              ');

	var	i,sng_num,arcompt_num,sng_len: integer;
		sng: sng50_type;
		ar_wrd: array[1..max_arcompt] of sngwrd_type;
		outtx_flnm: file_name;
		outtx: text;

	exception excp_0,excp_1,excp_2,excp_3,excp_4;

    procedure print(numb: integer; mesgp: string[40]);
      begin writeln(outtx);
	writeln(outtx,' point =',numb:3,'     ',mesgp);
	break(outtx)
      end;

    function sng_end(var countp: integer): boolean;
      begin countp:=countp+1;
	if countp>sng_len then sng_end:=true
	  else sng_end:=false
      end;

    procedure procss_word(var sngp: sng50_type;var cntp:integer;
				var wrdp: sngwrd_type);

	var	cnt: integer;
		cmp_val: boolean;
		chrt: char;
	exception excp_11,excp_12,excp_13;

	function comp_char(chrtp: char): boolean;
	  begin
	    if (chrtp>='a') and (chrtp<='z') then comp_char:=true
	    else if (chrtp>='0') and (chrtp<='9') then
(* 1 *)		begin print(1,' digit in the word; signal EXCP_11');
		  signal(excp_11)
		end
	    else if chrtp=' ' then comp_char:=false
	    else if chrtp='@' then
(* 2 *)		begin print(2,' character -@-; signal EXCP_12');
		  signal(excp_12)
		end
	    else if chrtp='#' then
(* 3 *)		begin print(3,' character -#-; signal EXCP_0');
		  signal(excp_0)
		end
(* 4 *)	    else begin print(4,' illegal character; signal EXCP_1');
		   signal(excp_1)
		 end;
(* 5 *)	    exception excp_11: begin print(5,' EXCP_11 signalled');
				 cmp_val:=false; comp_char:=true
			       end
	  end;

      begin cnt:=0; cmp_val:=true; chrt:=sngp[cntp];
	while comp_char(chrt) do
	  begin
		if cmp_val then
		  begin cnt:=cnt+1;
		    if cnt>len_of_wrd then
(* 10 *)	      begin print(10,' too long word; signal EXCP_13');
			signal(excp_13)
		      end; wrdp:=wrdp||chrt 
		  end;
	    if sng_end(cntp) then
(* 11 *)      begin print(11,' string end; signal EXCP_2');
		signal(excp_2)
	      end else chrt:=sngp[cntp];
	    cmp_val:=true;
(* 12 *)    exception others: begin print(12,' OTHERS signalled');
				signal()
			      end
	  end;
	exception
(* 13 *)  excp_12: begin print(13,' EXCP_12 signalled');
		    if sng_end(cntp) then
(* 14 *)	      begin print(14,' string end; signal EXCP_2');
			signal(excp_2)
		      end
		   end;
(* 15 *)  excp_13: begin print(15,' EXCP_13 signalled');
		    while chrt<>' ' do
		      if sng_end(cntp) then
(* 16 *)		begin print(16,' string end; signal EXCP_2');
			  signal(excp_2)
			end else chrt:=sngp[cntp]
		   end;
(* 17 *)  others: begin print(17,' OTHERS signalled');
			signal()
		  end
      end;

    procedure procss_strng(var stng: sng50_type;var arcompt_nump: 
							integer);

	var	count: integer;
		chrt: char;
		wrd: sngwrd_type;
		bl_vr: boolean;
	exception excp_21;

      begin count:=1; arcompt_nump:=1; bl_vr:=false;
	repeat
	  begin
	    begin chrt:=stng[count];
	      while (chrt<'a') or (chrt>'z') do
		begin if chrt<>' ' then
(* 20 *)	  begin print(20,
			'illegal start character; signal EXCP_21');
		    signal(excp_21)
		  end;
		  if sng_end(count) then
(* 21 *)	    begin print(21,' string end; signal EXCP_3');
			signal(excp_3)
		    end else chrt:=stng[count]
		end; wrd:='';
	      procss_word(stng,count,wrd);
	      exception
		excp_0,excp_2: 
(* 24 *)	    begin print(24,' EXCP_0 or EXCP_2 signalled');
			bl_vr:=true
		    end;
(* 25 *)	others: begin print(25,' OTHERS signalled');
			  signal()
			end
	    end;
	    if arcompt_nump>max_arcompt then
(* 26 *)      begin print(26,' too many words; signal EXCP_4');
		signal(excp_4)
	      end;  ar_wrd[arcompt_nump]:=wrd;
	    arcompt_nump:=arcompt_nump+1
	    exception
(* 22 *)      excp_21: begin print(22,' EXCP_21 signalled');
			while chrt<>' ' do
			  if sng_end(count) then
			    begin 
(* 23 *)		      print(23,' string end; signal EXCP_3');
			      signal(excp_3)
			    end else chrt:=stng[count]
		       end
	  end until bl_vr
      end;

    begin rewrite(tty);	open(tty); 
      writeln(tty); writeln(tty,' give an output file name :'); 
      break(tty); readln(tty); read(tty,outtx_flnm);
      rewrite(outtx,outtx_flnm);
      for sng_num:=1 to maxnum do
	begin
	    begin sng:=sng_ar[sng_num]; sng_len:=length(sng);
	      writeln(outtx);
	      writeln(outtx,' sng_len =',sng_len:3,'     ',sng); 
	      break(outtx);
	      procss_strng(sng,arcompt_num);
(* 30 *)      exception excp_3: begin print(30,' EXCP_3 signalled');
				  if arcompt_num=1 then
(* 31 *)			    begin print(31,
				      ' last string: resignal EXCP_3');
				      signal()
				    end	
				end
	    end; writeln(outtx);
		writeln(outtx,' *** next line ***');
		for i:=1 to arcompt_num-1 do writeln(outtx,ar_wrd[i]);
		writeln(outtx); break(outtx);
	    exception
		excp_3: begin
(* 32 *)	      print(32,' EXCP_3 resignalled; signal it again');
			  signal()
			end;
(* 33 *)	allconditions: print(33,' ALLCONDITIONS signalled')
	end;
(* 34 *)
      exception allconditions: begin
			print(34,' last ALLCONDITIONS signalled');
				 close
			       end
    end.
 