$PAGE PASVAL.INC, last modified 1/16/84, zw
$IFNOT pasvalinc

EXTERNAL FUNCTION makestring(len: char_range): val;
EXTERNAL FUNCTION makeset(min_range, max_range: bit_range): val;
EXTERNAL FUNCTION makearray(n_comps: int_type): val;
EXTERNAL FUNCTION makerecord(n_fields: int_type): val;
EXTERNAL FUNCTION cst_scalar(svalue: int_type): val;
EXTERNAL FUNCTION mkreal(rval: real_type; rprec: prec_type): val;

$ENABLE pasvalinc
$ENDIF
 