$TITLE PASGLB -- Pascal Compiler Global Cross Reference File

module pasglb;

$INCLUDE pascal.inc
$INCLUDE pasfil.inc

public var glob_file: text;
           in_body: boolean;

public procedure glob_init;
begin
  rewrite (glob_file, list_file || '.SYM');
  if not eof (glob_file) then begin
    writeln (tty, '? Unable to open .SYM file');
    prog_options.global_opt := false;
  end;
  in_body := false;
end;

public procedure glob_term;
begin
  close (glob_file);
end.
