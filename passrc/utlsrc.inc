$PAGE UTLSRC.INC, last modified 3/21/84, zw
$IFNOT utlsrcinc

$SYSTEM UTLTYP.TYP
$SYSTEM UTLSRC.TYP

EXTERNAL PROCEDURE newsrch(VAR source_search_list; file_name);
EXTERNAL PROCEDURE zapsrch(VAR source_search_list);
EXTERNAL PROCEDURE wrsrch(source_search_list; VAR binary_file);
EXTERNAL FUNCTION rdsrch(VAR binary_file): source_search_list;
EXTERNAL FUNCTION opnsrc(source_search_list; VAR text_file; file_name): yes_no;

$ENABLE utlopninc
$ENDIF
  