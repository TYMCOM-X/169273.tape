$PAGE PASCFM.INC, last modified 5/11/84, zw
$INCLUDE pascfm.typ
EXTERNAL VAR
cmd_name_stack: cmd_stack;
cmd_list: pending_command;
last_cmd: pending_command;
EXTERNAL
PROCEDURE rd_cmd_file ( VAR f: TEXT );
EXTERNAL
PROCEDURE rd_tmpcor ( name: parm_string; buf: tmp_buffer; len: INTEGER );
EXTERNAL
PROCEDURE cmd_save ( VAR f: FILE OF * );
EXTERNAL
PROCEDURE cmd_restore ( VAR f: FILE OF * );
EXTERNAL
PROCEDURE cmd_clear;
  