(*PA2DMP.INC, last modified 1/11/84, zw*)
$IFNOT pa2dmpinc

EXTERNAL PROCEDURE dump_on;
EXTERNAL PROCEDURE dmptuples(title: line_string);
EXTERNAL PROCEDURE dmpfgraph(blocks: tpl_vector; n_blocks: index_range);
EXTERNAL PROCEDURE dmpvlsyms;
EXTERNAL PROCEDURE dmpset
  (s: svector; n_elems: elem_number; title: line_string);
EXTERNAL PROCEDURE dmpfrtable(f: fre_vector; n_fre: fre);
EXTERNAL PROCEDURE dmpdominators
  (idom, dom_son, dom_brother: index_vector; reducible: BOOLEAN);
EXTERNAL PROCEDURE dmpwenv(tpl_list_vec; index_range);
EXTERNAL PROCEDURE dmprefcounts(ref_c_vector; index_range);

$ENABLE pa2dmpinc
$ENDIF

   