program detab;
$include (pasdev27)getiof.inc
var inf,outf: text;
    line: string[132];
begin
open(tty); rewrite(tty);
loop
    getiofiles(inf,outf,'TXT','TXT');
    while not eof(inf) do begin
	readln(inf,line);
	writeln(outf,line)
	end;
    close(inf); close(outf)
    end
end.
