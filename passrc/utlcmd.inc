$PAGE UTLCMD.INC, last modified 4/3/84, zw
$IFNOT utlcmdinc

$SYSTEM UTLTYP.TYP
$SYSTEM UTLTCR.TYP
$SYSTEM UTLCMD.TYP

EXTERNAL VAR
cmdstk: command_stack_pointer;
cmdlst: pending_command_pointer;
lstcmd: pending_command_pointer;

EXTERNAL PROCEDURE rdcmd(VAR command_line_string; VAR command_line_cursor);
EXTERNAL FUNCTION cmdtcr(string_argument; tmpcor_buffer): yes_no;
EXTERNAL FUNCTION cmdfil(file_name): yes_no;
EXTERNAL PROCEDURE stcmd(VAR binary_file);
EXTERNAL PROCEDURE ldcmd(VAR binary_file);
EXTERNAL PROCEDURE clrcmd;
EXTERNAL PROCEDURE cmderr
  (command_line_string; command_line_cursor; generic_string);
EXTERNAL FUNCTION chkeol(command_line_string; command_line_cursor): yes_no;
EXTERNAL PROCEDURE skpspc(command_line_string; VAR command_line_cursor);
EXTERNAL FUNCTION chkchr
  (command_line_string; VAR command_line_cursor; CHAR): yes_no;
EXTERNAL FUNCTION scnfil
  (command_line_string; VAR command_line_cursor; VAR file_name): yes_no;

$ENABLE utlcmdinc
$ENDIF
 