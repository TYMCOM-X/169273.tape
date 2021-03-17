$TITLE PASOPN.PA, last modified 5/11/84, zw
MODULE pasopn;
(*TYM-Pascal compiler Source File Opener*)
$SYSTEM pascal.inc
$PAGE open_search
(* OPEN SEARCH will attempt to open a specified file in the current directory.
   If this fails, it will retry the open operation with each directory in the
   file search list.  It returns true if the open succeeds, false if it fails. *)
PUBLIC
FUNCTION open_search ( VAR f: TEXT; name: FILE_NAME ): BOOLEAN;
VAR
list: search_ptr;
BEGIN
  RESET (f, name);
  list := prog_options.search_list;
  WHILE EOF (f) AND (list <> NIL) DO BEGIN
    RESET (f, list^.name || name);
    list := list^.next;
  END;
  open_search := NOT EOF (f);
END (* open_search *).
