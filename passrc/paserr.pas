$WIDTH=100
$LENGTH=55
$TITLE PASERR.PAS, last modified 3/26/84, zw
MODULE paserr;
(*TYM-Pascal compiler -- error reporting utilities*)

$PAGE system modules

$SYSTEM UTLTYP.TYP
$SYSTEM PASCAL.INC
$SYSTEM PASIST.INC
$SYSTEM PASPT.TYP
$SYSTEM PASFIL.INC

$INCLUDE PASERR.TYP

$INCLUDE PASERR.TAB

$PAGE definitions

STATIC VAR err_log_file: binary_file;

PUBLIC EXCEPTION abort; (*signaled on fatal error*)

PUBLIC PROCEDURE errsrc
  (code: error_code; src: source_id; msg: string; pos: line_index);
(*records the error information and outputs it to the error log
  file.  This may run in any of several passes;  however, if the log file has
  not been explicitly opened, then a log record is not written.  Advantage is
  taken of this fact to suppress duplicate errors from being reported by the
  file reader during the listing pass.  In all cases, fatal errors cause the
  current abort subroutine to be invoked. *)
VAR i, len: INTEGER; txt: PACKED ARRAY[1 .. 128] OF CHAR;
TYPE str = PACKED ARRAY[1 ..*] OF CHAR;
BEGIN
  IF elf_status = now_open THEN BEGIN
    max_severity := MAX(max_severity, err_table[code].level);
    i := CURSOR(err_log_file) + 1; len := LENGTH(msg);
    txt := msg; (* To avoid writing the string length word. *)
    WRITE(err_log_file, i, code, src, pos, len, txt: SIZE(str, len));
    err_count := err_count + 1;
    IF err_table[code].level <= 1 THEN warnings := warnings + 1
  END;
  IF max_severity = 3 THEN abort (* fatal error *)
END;

$PAGE error, err_node

PUBLIC PROCEDURE error(code: error_code);
(*a wrapper for the err_print. It supplies the current line as the
  location, and no additional text.*)
BEGIN
  err_print(code, cur_source, '', 0)
END;

PUBLIC PROCEDURE err_token(code: error_code; token: token_type);
(*is another wrapper. It records an error occuring on the specified
  parse node. The node's line and text are supplied.  Suppresses multiple
  errors  on the same token.*)
BEGIN
  IF token.dummy then err_print(code, cur_source, '', 0)
  ELSE err_print(code, token.source, '', token.column)
END;

PUBLIC PROCEDURE err_node(code: error_code; node: parse_node);
(*a wrapper for err_token.  This accepts a pointer to a parse tree
  node and dereferences it to a token.*)
BEGIN
  err_token(code, node^)
END;

$PAGE err_text, err_op

PUBLIC PROCEDURE err_text(code: error_code; text: string);
(*another wrapper. It records an error occuring on the current
  line and supplies special additional text.*)
BEGIN
  err_print(code, cur_source, text, 0)
END;

PUBLIC PROCEDURE err_op(code: error_code; op_text: STRING; operand: parse_node);
(*another wrapper.  It is the same as ERR NODE, except
  that instead of recording the text of the parse node, it takes text
  from a parameter.*)
BEGIN
  WITH operand^ DO BEGIN
    IF dummy THEN err_text(code, op_text)
    ELSE err_print(code, source, op_text, column)
  END
END;

$PAGE elf_open, elf_close

PUBLIC PROCEDURE elf_open;
(*opens the error log file to permit log entries to be written to the
  file.  This routine may be run in any pass.  It examines "elf_status" in
  order to determine if the file should be opened in append mode.  The status
  is set to indicate that the file is open. *)
BEGIN
  IF elf_status = prev_opened THEN REWRITE(err_log_file, err_tmp, [PRESERVE])
  ELSE REWRITE(err_log_file, err_tmp);
  elf_status := now_open
END;

PUBLIC PROCEDURE elf_close;
(*closes the error log file.  It marks "elf_status" to show that
  subsequent opens require append mode. *)
BEGIN
  CLOSE(err_log_file);
  elf_status := prev_opened
END.
   