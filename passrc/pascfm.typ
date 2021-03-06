$PAGE PASCFM.TYP, last modified 1/13/84, zw
$IFNOT pascfmtyp

TYPE (*pending command list declarations*)
pending_command = ^cmd_list_entry;
cmd_list_entry = RECORD
  next: pending_command;	(*next pending command*)
  eof: boolean;			(*dummy entry following last line*)
  too_long: BOOLEAN;		(*if line from file wouldn't fit*)
  text: PACKED ARRAY [1..*] OF CHAR (*text of command line*)
end;
cmd_stack = ^ cmd_stack_entry;
cmd_stack_entry = RECORD
  next: cmd_stack;		(*links stack nodes*)
  cmd_file_name: FILE_NAME	(*filename of command file*)
END;
tmp_buf_array = PACKED ARRAY [1..*] OF CHAR;
tmp_buffer = ^tmp_buf_array;

$ENABLE pascfmtyp
$ENDIF 