$TITLE paserr - first pass error utilities
$LENGTH 42

module paserr;

$PAGE includes
$include pascal.inc
$include paserr.typ
$include pasist.inc
$include paspt.typ
$include paserr.tab
$include corout.inc
$include pasfil.inc
$include tmpnam.inc
$PAGE declarations

static var
  err_log_file: file of *;

public var
  abort: environment;                   (* action to perform on fatal error *)
$PAGE err_print
(* ERR PRINT records the error information and outputs it to the error log
   file.  This may run in any of several passes;  however, if the log file has
   not been explicitly opened, then a log record is not written.  Advantage is
   taken of this fact to suppress duplicate errors from being reported by the
   file reader during the listing pass.  In all cases, fatal errors cause the
   current abort subroutine to be invoked. *)

public procedure err_print (code: err_codes; src: source_id;
			    added_text: string; line_posn: line_index);

var
    ix, len: int_type;
    text: packed array [1..128] of char;

type
    flex_str = packed array [1..*] of char;

begin
  if elf_status = now_open then begin
    max_severity := max (max_severity,err_table[code].level);
    ix := cursor (err_log_file) + 1;
    len := length (added_text);
    text := added_text; (* To avoid writing the string length word. *)
    write (err_log_file, ix, code, src, line_posn, len, text: size (flex_str, len));
    err_count := err_count + 1;
    if err_table[code].level <= 1
      then warnings := warnings + 1;
  end;
  if max_severity = 3 then      (* fatal error *)
    call (abort);
end;
$PAGE error, err_node
(* ERROR is a wrapper for the err_print. It supplies the current line as the
   location, and no additional text. *)

public procedure error (code: err_codes);
 begin
  err_print (code, cur_source, '', 0);
 end;



(* ERR TOKEN is another wrapper. It records an error occuring on the specified
   parse node. The node's line and text are supplied.  Suppresses multiple errors
   on the same token. *)

public procedure err_token (code: err_codes; token: token_type);
 begin
  if token.dummy
    then err_print (code, cur_source, '', 0)
    else err_print (code, token.source, '', token.column);
 end;



(* ERR NODE is a wrapper for err_token.  This accepts a pointer to a parse tree
   node and dereferences it to a token. *)

public procedure err_node ( code: err_codes; node: parse_node );
 begin
  err_token (code, node^)
 end;
$PAGE err_text, err_op
(* ERR TEXT is another wrapper. It records an error occuring on the current
   line and supplies special additional text. *)

public procedure err_text (code: err_codes; text: string);
 begin
  err_print (code, cur_source, text, 0);
 end;



(*  ERR OP is another wrapper.  It is the same as ERR NODE, except
    that instead of recording the text of the parse node, it takes text
    from a parameter.  *)

public procedure err_op ( code: err_codes; op_text: string; operand: parse_node );
begin
  with operand^ do begin
    if dummy
      then err_text (code, op_text)
      else err_print (code, source, op_text, column);
  end;
end (* err_op *) ;
$PAGE elf_open, elf_close
(* ELF OPEN opens the error log file to permit log entries to be written to the
   file.  This routine may be run in any pass.  It examines "elf_status" in
   order to determine if the file should be opened in append mode.  The status
   is set to indicate that the file is open. *)

public procedure elf_open;
 begin
  if elf_status = prev_opened then
   rewrite (err_log_file, tempname ('ERR'), [preserve])
  else
   rewrite (err_log_file, tempname ('ERR'));
  elf_status := now_open;
 end;



(* ELF CLOSE closes the error log file.  It marks "elf_status" to show that
   subsequent opens require append mode. *)

public procedure elf_close;
 begin
  close (err_log_file);
  elf_status := prev_opened;
 end.
 