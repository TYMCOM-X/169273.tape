$IFNOT askinc

$SYSTEM setup.typ
$SYSTEM err.typ
$SYSTEM dbg.typ
$SYSTEM copy.typ

$INCLUDE ask.typ

EXTERNAL PROCEDURE ask_tty(q: STRING[*]; l: ask_lst; VAR i: INTEGER);
(*ask TTY a question, return index of response word in list*)

EXTERNAL PROCEDURE ask(q: STRING[*]; l: ask_lst; VAR i: INTEGER);
(*ask question, return index of response word in list*)

EXTERNAL FUNCTION abbrev(s1, s2: STRING[*]): BOOLEAN;
(*return TRUE if s1 is equal to or an abbreviation of s2*)

$ENABLE askinc
$ENDIF
  