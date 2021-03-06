module q10ass;
$system pascal.inc
$system pasfil.inc
$system pasist.inc
external function cv_source_id (source_id): string;
$system query.inc
external procedure err_failure;
external var bypass: boolean;
external var code_area: integer;
external procedure gen_cmt (var integer; string);
procedure gen_trace;

type
  stack_ptr = ^ stack_frame;
  trace_link = ^alfa;
  link_ptr = ^trace_link;
  stack_frame = packed record
    ret: integer;
    caller: stack_ptr;
    parent: stack_ptr;
    old_frame: integer;
    fill: stack_ptr;
    link: link_ptr
  end;

var
  l: trace_link;
  p: stack_ptr;
  s: string;

const
  maxline = 60;

begin
  gen_cmt (code_area,'Assertion failure trace:');
  p := ptr (16b);
  p := ptr (p^.ret);
  p := p^.caller;
  p := p^.caller;
  s := '';
  while p <> ptr (0) do begin
    l := p^.link^;
    if l <> nil then begin
      if length (s) >= maxline then begin
        gen_cmt (code_area, s);
        s := '';
      end;
      s := s || substr(l^,1,search(l^,[' '],11)-1) || ' ';
    end;
    p := p^.caller;
  end;
  gen_cmt (code_area, s);
end;
public procedure ass_failure;
var n: nam;
    s: string;
begin
  open (tty);
  if cur_block^.kind = subr_blk
    then n := cur_block^.subr_sym^.name
    else n := cur_block^.id;
  s := ' after assertion failure in block ' || substr(n^.text,1,n^.len)||' at '||cv_source_id (cur_source);
  if bypass then
    writeln (tty,'Continuing',s);
  if bypass orif query ('Continue'||s) then begin
    break (tty);
    close (tty);
    return;
  end;
  close (tty);
  err_failure;
end;

(* ASSERT is defined here in lieu of the new compiler's assertion
   checking.  *)

public procedure assert ( b: boolean );
  begin
    if not b then begin
      trace;
      if assembly_opt in cur_block^.semantic_options then
        gen_trace;
      ass_failure;
    end;
  end.
 