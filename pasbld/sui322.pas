
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(*       This program writes 15 records to a file of 'rec' and then
        reads them back in and compares what was read with what was
        written.  Program deviates if eof is encountered or if any
        of the records read is different from what was written.
*)
program sui322;
const str_val : array[io_status] of string := ('io_ok',',io_novf',
        'io_povf', 'io_dgit', 'io_govf', 'io_intr', 'io_rewr',
        'io_eof', 'io_outf', 'io_inpf', 'io_seek', 'io_illc',
        'io_nepf', 'io_opnf');
      Max = 15;
type rec = record
        int : integer;
        rea : real;
        boo : boolean
     end;       
var f : file of rec;
    r : rec;
    i : integer;
    a : array[1 .. Max] of rec;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

        
begin
        rewrite(output,'suite.txt',[preserve]);

        rewrite(f);
        for i := 1 to Max do begin
                with a[i] do begin
                        int := i;
                        rea := sqrt(i);
                        boo := (i mod 2 = 0)
                end;
                write(f,a[i])
        end;
        close(f);

        reset(f,[retry]);
        if extent(f) <> Max then
                writeln('deviates: sui322 (extent)');
        i := 1;
        loop

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

                read(f,r);
                exit if iostatus(f)<> io_ok do
                        writeln('deviates: sui322 iostatus = ',
                                str_val[iostatus(f)]);
                exit if (r.int <> a[i].int) or (r.rea <> a[i].rea)
                        or (r.boo <> a[i].boo) do
                        writeln('deviates: sui322  (read or write error)');
                exit if i = Max do writeln('conforms: sui322');
                i := i + 1;
        end
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    