$PAGE UTLSET.INC, last modified 1/13/84, zw
$IFNOT utlsetinc

$INCLUDE UTLSET.TYP

EXTERNAL FUNCTION new_svector(set_number; elem_number): svector;
EXTERNAL PROCEDURE clr_set(svector; set_number);
EXTERNAL PROCEDURE unv_set(svector; set_number);
EXTERNAL PROCEDURE add_elem(svector; set_number; elem_number);
EXTERNAL PROCEDURE del_elem(svector; set_number; elem_number);
EXTERNAL PROCEDURE mov_set(svector; set_number; svector; set_number);
EXTERNAL PROCEDURE cpy_set(svector; set_number; set_number);
EXTERNAL PROCEDURE union(svector; set_number; set_number);
EXTERNAL PROCEDURE intersect(svector; set_number; set_number);
EXTERNAL PROCEDURE subtract(svector; set_number; set_number);
EXTERNAL FUNCTION set_eq(svector; set_number; set_number): BOOLEAN;
EXTERNAL FUNCTION in_set(svector; set_number; elem_number): BOOLEAN;
EXTERNAL FUNCTION is_empty(svector; set_number): BOOLEAN;
EXTERNAL PROCEDURE del_svector(svector);

$ENABLE utlsetinc
$ENDIF
    