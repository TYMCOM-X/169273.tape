
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* this program tests the file association functions with all possible
        parameter combinations. The iostatus is printed along with the
        combination of parameters that generated it.
  file type: text
*)

program sui337;

const str_val : array[io_status] of string := ('io_ok',',io_novf',
        'io_povf', 'io_dgit', 'io_govf', 'io_intr', 'io_rewr',
        'io_eof', 'io_outf', 'io_inpf', 'io_seek', 'io_illc',
        'io_nepf', 'io_opnf');

var f : text;
        
begin
        rewrite(output,'suite.txt',[preserve]);

        WRITELN('SUI337');
        writeln('TEST OF FILE ASSOCIATION PROCEDURES ON A ''TEXT''');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

        WRITELN;


(**************** open ****************)
    writeln('TEST OF OPEN:');
      open(f,'test.tmp',[seekok, image, preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii, control, retry');
      close(f);

      open(f,'test.tmp',[image, preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii, control, retry');
      close(f);

      open(f,'test.tmp',[seekok, preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii, control, retry');
      close(f);

      open(f,'test.tmp',[preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii, control, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[seekok, image, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii, control, retry');
      close(f);

      open(f,'test.tmp',[image, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii, control, retry');
      close(f);

      open(f,'test.tmp',[seekok, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii, control, retry');
      close(f);

      open(f,'test.tmp',[ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii, control, retry');
      close(f);

      open(f,'test.tmp',[seekok, image, preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, control, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[image, preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, control, retry');
      close(f);

      open(f,'test.tmp',[seekok, preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, control, retry');
      close(f);

      open(f,'test.tmp',[preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, control, retry');
      close(f);

      open(f,'test.tmp',[seekok, image, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, control, retry');
      close(f);

      open(f,'test.tmp',[image, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, control, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[seekok, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, control, retry');
      close(f);

      open(f,'test.tmp',[control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' control, retry');
      close(f);

      open(f,'test.tmp',[seekok, image, preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii, retry');
      close(f);

      open(f,'test.tmp',[image, preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii, retry');
      close(f);

      open(f,'test.tmp',[seekok, preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii, retry');
      close(f);

      open(f,'test.tmp',[seekok, image, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii, retry');
      close(f);

      open(f,'test.tmp',[image, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii, retry');
      close(f);

      open(f,'test.tmp',[seekok, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii, retry');
      close(f);

      open(f,'test.tmp',[ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[seekok, image, preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, retry');
      close(f);

      open(f,'test.tmp',[image, preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, retry');
      close(f);

      open(f,'test.tmp',[seekok, preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, retry');
      close(f);

      open(f,'test.tmp',[preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, retry');
      close(f);

      open(f,'test.tmp',[seekok, image, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[image, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, retry');
      close(f);

      open(f,'test.tmp',[seekok, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, retry');
      close(f);

      open(f,'test.tmp',[retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' retry');
      close(f);

      open(f,'test.tmp',[seekok, image, preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii, control');
      close(f);

      open(f,'test.tmp',[image, preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii, control');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[seekok, preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii, control');
      close(f);

      open(f,'test.tmp',[preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii, control');
      close(f);

      open(f,'test.tmp',[seekok, image, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii, control');
      close(f);

      open(f,'test.tmp',[image, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii, control');
      close(f);

      open(f,'test.tmp',[seekok, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii, control');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii, control');
      close(f);

      open(f,'test.tmp',[seekok, image, preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, control');
      close(f);

      open(f,'test.tmp',[image, preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, control');
      close(f);

      open(f,'test.tmp',[seekok, preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, control');
      close(f);

      open(f,'test.tmp',[preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, control');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[seekok, image, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, control');
      close(f);

      open(f,'test.tmp',[image, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, control');
      close(f);

      open(f,'test.tmp',[seekok, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, control');
      close(f);

      open(f,'test.tmp',[control]);
        writeln('iostatus = ',str_val[iostatus(f)],' control');
      close(f);

      open(f,'test.tmp',[seekok, image, preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[image, preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii');
      close(f);

      open(f,'test.tmp',[seekok, preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii');
      close(f);

      open(f,'test.tmp',[preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii');
      close(f);

      open(f,'test.tmp',[seekok, image, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii');
      close(f);

      open(f,'test.tmp',[image, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[seekok, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii');
      close(f);

      open(f,'test.tmp',[ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii');
      close(f);

      open(f,'test.tmp',[seekok, image, preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve');
      close(f);

      open(f,'test.tmp',[image, preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve');
      close(f);

      open(f,'test.tmp',[seekok, preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      open(f,'test.tmp',[preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve');
      close(f);

      open(f,'test.tmp',[seekok, image]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image');
      close(f);

      open(f,'test.tmp',[image]);
        writeln('iostatus = ',str_val[iostatus(f)],' image');
      close(f);

      open(f,'test.tmp',[seekok]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok');
      close(f);

(**************** reset ****************)
    writeln;
    writeln('TEST OF RESET');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[seekok, image, preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii, control, retry');
      close(f);

      reset(f,'test.tmp',[image, preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii, control, retry');
      close(f);

      reset(f,'test.tmp',[seekok, preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii, control, retry');
      close(f);

      reset(f,'test.tmp',[preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii, control, retry');
      close(f);

      reset(f,'test.tmp',[seekok, image, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii, control, retry');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[image, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii, control, retry');
      close(f);

      reset(f,'test.tmp',[seekok, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii, control, retry');
      close(f);

      reset(f,'test.tmp',[ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii, control, retry');
      close(f);

      reset(f,'test.tmp',[seekok, image, preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, control, retry');
      close(f);

      reset(f,'test.tmp',[image, preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, control, retry');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[seekok, preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, control, retry');
      close(f);

      reset(f,'test.tmp',[preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, control, retry');
      close(f);

      reset(f,'test.tmp',[seekok, image, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, control, retry');
      close(f);

      reset(f,'test.tmp',[image, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, control, retry');
      close(f);

      reset(f,'test.tmp',[seekok, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, control, retry');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' control, retry');
      close(f);

      reset(f,'test.tmp',[seekok, image, preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii, retry');
      close(f);

      reset(f,'test.tmp',[image, preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii, retry');
      close(f);

      reset(f,'test.tmp',[seekok, preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii, retry');
      close(f);

      reset(f,'test.tmp',[preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii, retry');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[seekok, image, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii, retry');
      close(f);

      reset(f,'test.tmp',[image, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii, retry');
      close(f);

      reset(f,'test.tmp',[seekok, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii, retry');
      close(f);

      reset(f,'test.tmp',[ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii, retry');
      close(f);

      reset(f,'test.tmp',[seekok, image, preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, retry');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[image, preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, retry');
      close(f);

      reset(f,'test.tmp',[seekok, preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, retry');
      close(f);

      reset(f,'test.tmp',[preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, retry');
      close(f);

      reset(f,'test.tmp',[seekok, image, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, retry');
      close(f);

      reset(f,'test.tmp',[image, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, retry');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[seekok, retry]);
        writeln('iostatus =r_val[iostatus(f)],' seekok, retry');
      close(f);

      reset(f,'test.tmp',[retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' retry');
      close(f);

      reset(f,'test.tmp',[seekok, image, preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii, control');
      close(f);

      reset(f,'test.tmp',[image, preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii, control');
      close(f);

      reset(f,'test.tmp',[seekok, preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii, control');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii, control');
      close(f);

      reset(f,'test.tmp',[seekok, image, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii, control');
      close(f);

      reset(f,'test.tmp',[image, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii, control');
      close(f);

      reset(f,'test.tmp',[seekok, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii, control');
      close(f);

      reset(f,'test.tmp',[ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii, control');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[seekok, image, preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, control');
      close(f);

      reset(f,'test.tmp',[image, preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, control');
      close(f);

      reset(f,'test.tmp',[seekok, preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, control');
      close(f);

      reset(f,'test.tmp',[preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, control');
      close(f);

      reset(f,'test.tmp',[seekok, image, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, control');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[image, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, control');
      close(f);

      reset(f,'test.tmp',[seekok, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, control');
      close(f);

      reset(f,'test.tmp',[control]);
        writeln('iostatus = ',str_val[iostatus(f)],' control');
      close(f);

      reset(f,'test.tmp',[seekok, image, preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii');
      close(f);

      reset(f,'test.tmp',[image, preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[seekok, preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii');
      close(f);

      reset(f,'test.tmp',[preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii');
      close(f);

      reset(f,'test.tmp',[seekok, image, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii');
      close(f);

      reset(f,'test.tmp',[image, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii');
      close(f);

      reset(f,'test.tmp',[seekok, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii');
      close(f);

      reset(f,'test.tmp',[seekok, image, preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve');
      close(f);

      reset(f,'test.tmp',[image, preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve');
      close(f);

      reset(f,'test.tmp',[seekok, preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve');
      close(f);

      reset(f,'test.tmp',[preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve');
      close(f);


(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      reset(f,'test.tmp',[seekok, image]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image');
      close(f);

      reset(f,'test.tmp',[image]);
        writeln('iostatus = ',str_val[iostatus(f)],' image');
      close(f);

      reset(f,'test.tmp',[seekok]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok');
      close(f);

(**************** rewrite ****************)
   writeln;
   writeln('TEST OF REWRITE:');
      rewrite(f,'test.tmp',[seekok, image, preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii, control, retry');
      rewrite(f,'test.tmp',[image, preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii, control, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[seekok, preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii, control, retry');
      close(f);

      rewrite(f,'test.tmp',[preserve, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii, control, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, image, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii, control, retry');
      close(f);

      rewrite(f,'test.tmp',[image, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii, control, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii, control, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[ascii, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii, control, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, image, preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, control, retry');
      close(f);

      rewrite(f,'test.tmp',[image, preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, control, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, control, retry');
      close(f);

      rewrite(f,'test.tmp',[preserve, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, control, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[seekok, image, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, control, retry');
      close(f);

      rewrite(f,'test.tmp',[image, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, control, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, control, retry');
      close(f);

      rewrite(f,'test.tmp',[control, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' control, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, image, preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[image, preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii, retry');
      close(f);

      rewrite(f,'test.tmp',[preserve, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, image, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii, retry');
      close(f);

      rewrite(f,'test.tmp',[image, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[seekok, ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii, retry');
      close(f);

      rewrite(f,'test.tmp',[ascii, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, image, preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, retry');
      close(f);

      rewrite(f,'test.tmp',[image, preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[preserve, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, image, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, retry');
      close(f);

      rewrite(f,'test.tmp',[image, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, retry');
      close(f);

      rewrite(f,'test.tmp',[seekok, retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, retry');
      close(f);

      rewrite(f,'test.tmp',[retry]);
        writeln('iostatus = ',str_val[iostatus(f)],' retry');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[seekok, image, preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii, control');
      close(f);

      rewrite(f,'test.tmp',[image, preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii, control');
      close(f);

      rewrite(f,'test.tmp',[seekok, preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii, control');
      close(f);

      rewrite(f,'test.tmp',[preserve, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii, control');
      close(f);

      rewrite(f,'test.tmp',[seekok, image, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii, control');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[image, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii, control');
      close(f);

      rewrite(f,'test.tmp',[seekok, ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii, control');
      close(f);

      rewrite(f,'test.tmp',[ascii, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii, control');
      close(f);

      rewrite(f,'test.tmp',[seekok, image, preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, control');
      close(f);

      rewrite(f,'test.tmp',[image, preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, control');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[seekok, preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, control');
      close(f);

      rewrite(f,'test.tmp',[preserve, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, control');
      close(f);

      rewrite(f,'test.tmp',[seekok, image, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, control');
      close(f);

      rewrite(f,'test.tmp',[image, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, control');
      close(f);

      rewrite(f,'test.tmp',[seekok, control]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, control');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[control]);
        writeln('iostatus = ',str_val[iostatus(f)],' control');
      close(f);

      rewrite(f,'test.tmp',[seekok, image, preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve, ascii');
      close(f);

      rewrite(f,'test.tmp',[image, preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve, ascii');
      close(f);

      rewrite(f,'test.tmp',[seekok, preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve, ascii');
      close(f);

      rewrite(f,'test.tmp',[preserve, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve, ascii');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[seekok, image, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, ascii');
      close(f);

      rewrite(f,'test.tmp',[image, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, ascii');
      close(f);

      rewrite(f,'test.tmp',[seekok, ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, ascii');
      close(f);

      rewrite(f,'test.tmp',[ascii]);
        writeln('iostatus = ',str_val[iostatus(f)],' ascii');
      close(f);

      rewrite(f,'test.tmp',[seekok, image, preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image, preserve');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[image, preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' image, preserve');
      close(f);

      rewrite(f,'test.tmp',[seekok, preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, preserve');
      close(f);

      rewrite(f,'test.tmp',[preserve]);
        writeln('iostatus = ',str_val[iostatus(f)],' preserve');
      close(f);

      rewrite(f,'test.tmp',[seekok, image]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok, image');
      close(f);

      rewrite(f,'test.tmp',[image]);
        writeln('iostatus = ',str_val[iostatus(f)],' image');
      close(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)


      rewrite(f,'test.tmp',[seekok]);
        writeln('iostatus = ',str_val[iostatus(f)],' seekok');
      close(f);
      writeln('END OF TEST')
end.
   	 7