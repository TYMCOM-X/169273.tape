$PAGE PASRDR.INC, last modified 3/9/84, zw
$IFNOT pasrdrinc

$SYSTEM UTLTYP.TYP
$SYSTEM UTLLST.TYP
$SYSTEM UTLSRC.TYP

EXTERNAL VAR
maittl: page_title; (*first title which appears in input*)
glbttl: page_title; (*current title*)
pagttl: page_title; (*currently applicable $PAGE title*)
linlst: yn; (*line should be listed?*)
explst: yn; (*list file name from ",list=source"*)
srclst: source_ptr; (*start of list of files referenced*)
filnum: src_range; (*number of current file*)
finsrc: src_id; (*last line read*)
srclin: src_lin_str; (*uppercased input line + 2 blanks*)
litlin: src_lin_str; (*input line as read*)
linena: yn; (*line can be compiled?*)
srceof: yn; (*last source line been read?*)
srcsel: yn; (*source on somewhere in program?*)

$ENABLE pasrdrinc
$ENDIF
   