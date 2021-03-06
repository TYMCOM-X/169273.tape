program dirtst;
$include dtime.typ
$include dtime.inc
$include pasdir.typ
$include pasdir.inc

var
fn: dir_fname;
pat: dir_m_str;
dir: dir_ext_id;
err: dir_errors;
ch: dir_int_id;
com: char;
att: dir_attrs;

begin
open(tty);rewrite(ttyoutput);
loop
  write(tty,'>>');break(tty);readln(tty);read(tty,com);
  case uppercase(com) of
    'Q': stop;

    'O': begin
      write(tty,'Dir [n,n]:');break(tty);readln(tty);
      dir:='';
      while not eoln(tty) do begin
	dir:=dir || tty^;
	get(tty)
	end;
      dir_open(err,ch,dir);
      writeln(tty,'err=',ord(err):2, '  ch=',ord(ch):2);
      end;

    'G':begin
      dir_next(err,ch,fn);
      writeln(tty,'err=',ord(err):2, '  fn=[',fn,']')
      end;

    'A':begin
      write(tty,'Enter file:');break(tty);readln(tty);
      if not eoln(tty) then begin
	fn:='';
	while not eoln(tty) do begin
	  fn:=fn||tty^;
	  get(tty)
	  end
	end;
      dir_attr(err,fn,att);
      writeln(tty,'err=',ord(err):2);
      with att do begin
(*	writeln(tty,'Full xform: ',dc_ext(creation));*)
	writeln(tty,'name[',name,'], pro=',protect);
	writeln(tty,'Cre= ',ns_d2(extr_date(creation)),' at ',
	  ns_t1(extr_time(creation)), ' acc= ',
	  ns_d2(accessed), ' size=',size:6)
	end
      end;

    'C':begin
      dir_close(err,ch);
      writeln(tty,'err=',ord(err):2)
      end;

    'M':begin
      pat:='';
      write(tty,'Enter pattern:');break(tty);readln(tty);
      while not eoln(tty) do begin
	pat:=pat || uppercase(tty^);
	get(tty)
	end;
      dir_next (ERR,ch,fn);
      if err <> dir_ok then
	writeln(tty,'No directory open.')
      else
	while err = dir_ok do begin
(*	  writeln (tty, 'Returned from dir_next: [',fn, ']');  *)
	  if dir_match (fn,pat) then
	    writeln(tty,'[',fn,'] matches [',pat,']');
	  dir_next(err,ch,fn)
	  end
      end;

    others: writeln(tty,'NFG command.')
    end
  end
end.
 