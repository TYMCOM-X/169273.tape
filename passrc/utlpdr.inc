(*UTLPDR.INC, last modified 1/9/84, zw*)
$IFNOT utlpdrinc

EXTERNAL PROCEDURE prgm_ppn(VAR INTEGER); (*from PRGDIR.MAC*)
EXTERNAL FUNCTION user_ppn: MACHINE_WORD; (*from where??*)
EXTERNAL FUNCTION prgm_dir: STRING[15]; (*from PRGDIR.MAC*)
FUNCTION pdr(nam: FILE_NAME): FILE_NAME;
BEGIN pdr := nam || prgm_dir() END;

$ENABLE utlpdrinc
$ENDIF   