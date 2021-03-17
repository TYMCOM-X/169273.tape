
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(*       This program attemts to reset a file of 'rec'. It should succeed.
*)
program sui321;
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
        reset(f,[retry]);
        if iostatus(f) = io_ok then

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

                writeln('sui321 conforms')
        else
                writeln('sui321 deviates (reset on typed file)',
                        ' iostatus = ',str_val[iostatus(f)])
end.    
