(*PASCFM.INC, last modified 12/20/83, zw*)
$IFNOT pascfminc

$SYSTEM PASCFM.TYP

EXTERNAL VAR
cmd_name_stack: cmd_stack;
cmd_list: pending_command;
last_cmd: pending_command;

EXTERNAL PROCEDURE rd_cmd_file(VAR f: TEXT);
EXTERNAL PROCEDURE rd_tmpcor(parm_string; tmp_buffer; INTEGER);
EXTERNAL PROCEDURE cmd_save(VAR FILE OF * );
EXTERNAL PROCEDURE cmd_restore(VAR FILE OF * );
EXTERNAL PROCEDURE cmd_clear;

$ENABLE pascfminc
$ENDIF
   