
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(*       This program attempts to open a file of 'rec'.  This should fail
        because only texts can be 'open'-ed.
*)
program sui334;
const str_val : array[io_status] of string := ('io_ok',',io_novf',
        'io_povf', 'io_dgit', 'io_govf', 'io_intr', 'io_rewr',
        'io_eof', 'io_outf', 'io_inpf', 'io_seek', 'io_illc',
        'io_nepf', 'io_opnf');

type rec = record
        int : integer;
        rea : real;
        boo : boolean
     end;       
var f : file of rec;
    i : rec;
        
begin
        rewrite(output,'suite.txt',[preserve]);
        open(f,'tmpfil:',[retry]);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

        if iostatus(f) = io_opnf then
                writeln('sui334 conforms')
        else
                writeln('sui334 deviates (open on typed file)',
                        ' iostatus = ',str_val[iostatus(f)])
end.    
