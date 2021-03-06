$PAGE PASCFM.TYP, lst modified 5/11/84, zw
(*  Pending Command List Declarations  *)

TYPE
pending_command = ^ cmd_list_entry;
cmd_list_entry = PACKED RECORD
  next: pending_command;
  EOF: BOOLEAN; (* dummy entry following the last line *)
  too_long: BOOLEAN; (* if the line from the file wouldn't fit *)
  TEXT: PACKED ARRAY [1..*] OF CHAR
END;
cmd_stack = ^ cmd_stack_entry;
cmd_stack_entry = RECORD
  next: cmd_stack; (* links the stack nodes *)
  cmd_file_name: FILE_NAME (* the filename of the command file *)
END;
tmp_buf_array= PACKED ARRAY [1..*] OF CHAR;
tmp_buffer= ^ tmp_buf_array;
  