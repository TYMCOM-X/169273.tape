$PAGE PASGLB.INC, last modified 1/16/84, zw
$IFNOT pasglbinc

EXTERNAL VAR
glob_file: TEXT;
in_body: BOOLEAN;

EXTERNAL PROCEDURE glob_init;
EXTERNAL PROCEDURE glob_term;

$ENABLE pasglbinc
$ENDIF

    