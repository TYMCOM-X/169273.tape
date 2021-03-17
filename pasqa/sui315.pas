
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(*       This program attemts to read from a text file which has been
        'rewrite'-ed.  It should fail.
*)
program sui315;

var f : text;
    i : integer := 0;
        
begin
        rewrite(output,'suite.txt',[preserve]);
        rewrite(f,[retry]);
        read(f,i);
        if iostatus(f) = io_eof then
                writeln('sui315 conforms')
        else
                writeln('sui315 deviates (read after rewrite)')
end.    

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

