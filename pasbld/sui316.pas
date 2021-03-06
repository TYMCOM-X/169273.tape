
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(*       This program attempts to read from a file of integer that has
        been 'rewrite'-ed. It should fail.
*)
program sui316;

const str_val : array[io_status] of string := ('io_ok',',io_novf',
        'io_povf', 'io_dgit', 'io_govf', 'io_intr', 'io_rewr',
        'io_eof', 'io_outf', 'io_inpf', 'io_seek', 'io_illc',
        'io_nepf', 'io_opnf');

var f : file of integer;
    i : integer := 0;
        
begin
        rewrite(output,'suite.txt',[preserve]);
        rewrite(f,[retry]);
        read(f,i);
        if iostatus(f) = io_eof then
                writeln('sui316 conforms')
        else

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

                writeln('sui316 deviates (read after rewrite)',
                        ' iostatus = ',str_val[iostatus(f)])
end.
  