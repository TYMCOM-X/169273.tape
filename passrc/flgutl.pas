$TITLE FLGUTL.PAS, last modified 4/11/84, zw
MODULE flgutl;
(*TYM-Pascal flag manipulation utility*)

(*HEADER FLGUTL.HDR*)

$SYSTEM TYPUTL.TYP
$INCLUDE FLGUTL.TYP

FUNCTION find_flag(list: flag_pointer; name: flag_name): flag_pointer;
(*Search for the specified flag, return NIL if it is not found.*)
BEGIN
  find_flag := list;
  WHILE find_flag <> NIL DO BEGIN
    EXIT IF find_flag^.name = name;
    find_flag := find_flag^.next
  END
END;

PUBLIC FUNCTION flag(list: flag_pointer; name: flag_name): yes_no;
(*Return enable value of flag in list or no.*)
VAR tmp: flag_pointer;
BEGIN
  tmp := find_flag(list, name);
  IF tmp = NIL THEN flag := no ELSE flag := tmp^.enabled
END;

PUBLIC PROCEDURE setflg
  (VAR flags: flag_pointer; name: flag_name; enable: yes_no);
(*Set a new flag at the head of the list.*)
VAR flag: flag_pointer;
BEGIN
  NEW(flag, LENGTH(name));
  flag^.name[1:LENGTH(name)] := name;
  flag^.enabled := enable;
  flag^.next := flags;
  flags := flag
END;

PUBLIC PROCEDURE delflg(VAR head: flag_pointer; final: flag_pointer);
(*Delete flags up to specified flag.*)
VAR flag, next_flag: flag_pointer;
BEGIN
  flag := head;
  WHILE (flag <> NIL) ANDIF (flag <> final) DO BEGIN
    next_flag := flag^.next; DISPOSE(flag); flag := next_flag
  END;
  head := final
END;

PUBLIC PROCEDURE ldflgs(VAR flags: flag_pointer; VAR f: binary_file);
(*Load flags from binary file.*)
VAR flag, last_flag: flag_pointer; len: INTEGER;
BEGIN
  flags := NIL;
  LOOP
    READ(f, len);
    EXIT IF len = 0;
    NEW(flag, len);
    READ(f, flag^: SIZE(flag^, len));
    flag^.next := NIL;
    IF flags = NIL THEN flags := flag
    ELSE last_flag^.next := flag;
    last_flag := flag
  END
END;

PUBLIC PROCEDURE stflgs(VAR flags: flag_pointer; VAR f: binary_file);
(*Store flags to binary file.*)
VAR flag: flag_pointer; len: INTEGER;
BEGIN
  flag := flags;
  WHILE flag <> NIL DO BEGIN
    len := LENGTH(flag^.name);
    WRITE(f, len, flag^: SIZE(flag^, len));
    flag := flag^.next
  END;
  len := 0; 
  WRITE(f, len);
  delflg(flags, NIL)
END.
