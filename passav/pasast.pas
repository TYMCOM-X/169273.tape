module pasast;
$system pascal.inc
$system pasfil.inc
$system pasist.inc
external function cv_source_id (source_id): string;
external procedure err_failure;
$PAGE ass_failure, assert
  
procedure ass_failure;
  
var n: nam;
  
begin
  if cur_block^.kind = subr_blk
    then n := cur_block^.subr_sym^.name
    else n := cur_block^.id;
$IFNOT PRODUCTION
  writeln (tty, 'Continuing after assertion failure in block ' || n^.text || ' at ' ||
							     cv_source_id (cur_source));
  break (tty);
$ENDIF
$IF PRODUCTION
  writeln (tty, 'Assertion failure in block ' || n^.text || ' at ' || cv_source_id (cur_source));
  writeln (tty, '  please report with stack trace and line number information');
  break (tty);
  err_failure;
$ENDIF
end;

  
(* ASSERT is defined here in lieu of the new compiler's assertion checking.  *)

public procedure assert ( b: boolean );
  begin
    if not b then begin
      trace;
      ass_failure;
    end;
  end.
