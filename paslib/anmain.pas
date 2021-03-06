(*$c- *)
		  (* ll(1) analyzer mainline *)

$OPTIONS STORAGE=4096
program anmain;

$INCLUDE anscan.typ
$INCLUDE anscan.var

type parseret = (parsecomplete, parsestackoflo, parseerror);

procedure parse ( var code: parseret ); extern;	(*external procedures*)
procedure noteerror( code: error ); extern;
procedure initscan; extern;
procedure initparse; extern;
procedure initgen; extern;

var code: parseret;
    buf,opt: string[30];

function prompt: boolean;

  (* function to prompt user for file name at terminal, open it on
     input, and open standard output files output (with extension
     .lst) and tabfile (extension .tab) in current directory. default
     extension of input is .bnf.  prompt returns true if file name
     entered, and false if only carraige return typed.   *)

  label 1;
  var ch: char; i: 0..30;

begin
1:write(tty,'*'); break; readln(tty);		(*initialize*)
  buf:= ''; opt:= '';				(*global string vars returned*)
  while not eoln(tty) do begin
    read(tty,ch); buf:= buf || uppercase (ch)
  end;
  if length(buf)>0 then begin			(*if user typed something*)
    i:= search(buf,['/']);			(*strip options into opt, if any*)
    if i>0 then begin
      if i<length(buf) then
	opt:= substr(buf,i+1,length(buf)-i);
      buf:= substr(buf,1,i-1)
    end;
    open(input,'.BNF '||buf);			(*add default extension and open*)
    if eof then begin				(*on error, reprompt*)
      writeln(tty,'BAD FILE, RE-ENTER');
      goto 1
    end;
    i:= search(buf,['[']);			(*eliminate ppn from buf, if any*)
    if i>0 then					(*assume ppn is at end of buf*)
      buf:= substr(buf,1,i-1);
    rewrite(output,buf||'.LST');		(*now open standard files with*)
    rewrite(tabfile,buf||'.TAB');		(*std. ext. in current directory*)
    prompt:= true				(*success return*)
  end
  else prompt:= false				(*only cr typed*)
end (*prompt*);


begin						(*the mainline*)
  open(tty); rewrite(ttyoutput);
  while prompt do begin
    optionlist:= [gentables,genlist];
    if search(opt,['F'])<>0 then optionlist:= optionlist+[fortables];
    if search(opt,['N'])<>0 then optionlist := optionlist+[symnames];
    if not (fortables in optionlist) then begin
      rewrite(symfile,buf||'.sym');
      rewrite(semfile,buf||'.sem');
      if symnames in optionlist then rewrite (namfile, buf||'.nam');
    end;
    initscan; initparse; initgen;
    parse( code );
    case code of
      parsecomplete: writeln(tty,'PARSE COMPLETE');
      parsestackoflo: writeln(tty,'PARSE STACK OVERFLOW');
      parseerror: noteerror(synerror)
    end
  end
end.
 