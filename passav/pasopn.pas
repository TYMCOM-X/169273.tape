$TITLE PASOPN -- Pascal Compiler Source File Opener

module pasopn;

$SYSTEM pascal.inc
$PAGE open_search
(* OPEN SEARCH will attempt to open a specified file in the current directory.
   If this fails, it will retry the open operation with each directory in the
   file search list.  It returns true if the open succeeds, false if it fails. *)

public function open_search ( var f: text; name: file_name ): boolean;

var list: search_ptr;

begin
  reset (f, name);
  list := prog_options.search_list;
  while eof (f) and (list <> nil) do begin
    reset (f, list^.name || name);
    list := list^.next;
  end;
  open_search := not eof (f);
end (* open_search *).
   