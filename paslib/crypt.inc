$IFNOT cryptinc

$SYSTEM setup.typ
$SYSTEM ask.typ
$SYSTEM err.typ
$SYSTEM dbg.typ
$SYSTEM copy.typ

$INCLUDE crypt.typ

EXTERNAL PROCEDURE crypt(i_nam, o_nam, key: STRING[*]);
(*encrypt input file to output file according to key*)

EXTERNAL PROCEDURE do_crypt(key: STRING[*]);
(*encrypt input to output according to key*)

$ENABLE cryptinc
$ENDIF
    