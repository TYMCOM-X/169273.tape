$TITLE PASSW.PAS, last modified 5/11/84, zw
MODULE passw OPTIONS special(word);
(*TYM-Pascal switch manipulation utilities*)
$PAGE includes
$SYSTEM pascal
$PAGE find_switch
(* FIND SWITCH searches a switch list for a switch with a specified name. If
   found the pointer to the switch node is returned; otherwise nil is returned. *)

FUNCTION find_switch ( head: switch_ptr; (* head of switch list *)
switch_name: switch_string (* name to be found *)
): switch_ptr; (* node for switch or nil *)
BEGIN
  find_switch := head;
  WHILE find_switch <> NIL DO BEGIN
    WITH find_switch^ DO BEGIN
      IF name = switch_name THEN RETURN; (* <---- exit for switch found *)
      find_switch := next_switch;
    END;
  END;
END;
$PAGE switch
(* SWITCH searches a list of known switches and returns the current value if
   found, otherwise returns false. *)
PUBLIC
FUNCTION switch ( head: switch_ptr; (* head of list *)
switch_name: switch_string (* switch to be found *)
): BOOLEAN; (* interpreted value of switch *)
VAR
sw: switch_ptr;
BEGIN
  sw := find_switch (head, switch_name);
  IF sw = NIL THEN switch := FALSE
  ELSE switch := sw^.enabled;
END;
$PAGE enable_switch
(* ENABLE SWITCH initializes a new switch record, placing it at the start of a
   caller-supplied switch list. *)
PUBLIC
FUNCTION enable_switch ( head: switch_ptr; (* start of switch list *)
switch_name: switch_string; (* name of switch to set *)
enable: BOOLEAN (* enable or disable switch *)
): switch_ptr; (* to updated start of list *)
VAR
sw: switch_ptr;
BEGIN
  NEW (sw, LENGTH (switch_name));
  WITH sw^ DO BEGIN
    enabled := enable;
    name[1:LENGTH(switch_name)] := switch_name;
    next_switch := head;
  END;
  enable_switch := sw;
END;
$PAGE pop_switches
(* POP SWITCHES deletes entries in a switch list up to a specified node. *)
PUBLIC
PROCEDURE pop_switches ( head: switch_ptr; (* first in list *)
final: switch_ptr ); (* stopping point, this node is left in list *)
VAR
sw, next_sw: switch_ptr;
BEGIN
  sw := head;
  WHILE sw <> final DO BEGIN
    next_sw := sw^.next_switch;
    DISPOSE (sw);
    sw := next_sw;
  END;
END;
$PAGE sw_save
(* SW SAVE will write the switches in a specified list out to a specified file. *)
PUBLIC
PROCEDURE sw_save ( head: switch_ptr; VAR f: FILE OF * );
VAR
sw: switch_ptr;
len: INTEGER;
BEGIN
  sw := head;
  WHILE sw <> NIL DO BEGIN
    len := LENGTH (sw^.name);
    WRITE (f, len, sw^: SIZE (sw^, len));
    sw := sw^.next_switch;
  END;
  len := 0;
  WRITE (f, len);
END;
$PAGE sw_load
(* SW LOAD reads a switch list from a specified file and returns a pointer to it. *)
PUBLIC
FUNCTION sw_load ( VAR f: FILE OF * ): switch_ptr;
VAR
sw, last_sw: switch_ptr;
len: INTEGER;
BEGIN
  sw_load := NIL;
  LOOP
    READ (f, len);
    EXIT IF len = 0;
    NEW (sw, len);
    READ (f, sw^: SIZE (sw^, len));
    sw^.next_switch := NIL;
    IF sw_load = NIL THEN sw_load := sw
    ELSE last_sw^.next_switch := sw;
    last_sw := sw;
  END;
END.
   