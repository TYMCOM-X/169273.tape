
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(*       This program attemts to read from a text file which has been
        'rewrite'-ed.  It should fail.
*)
program sui330;

var f : text;
    i : integer := 0;
        
begin
        rewrite(output,'suite.txt',[preserve]);
        rewrite(f,'tmpfil:',[retry]);
        read(f,i);
        if iostatus(f) = io_eof then
                writeln('sui330 conforms')
        else
                writeln('sui330 deviates (read after rewrite)')
end.    

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

