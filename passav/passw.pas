$TITLE PASSW - switch manipulation utilities

module passw;
$PAGE includes
$SYSTEM pascal
$PAGE find_switch
(* FIND SWITCH searches a switch list for a switch with a specified name. If
   found the pointer to the switch node is returned; otherwise nil is returned. *)

function find_switch
     (	head: switch_ptr;	(* head of switch list *)
	switch_name: switch_string	(* name to be found *)
	   ): switch_ptr;		(* node for switch or nil *)

begin
  find_switch := head;
  while find_switch <> nil do begin
    with find_switch^ do begin
      if name = switch_name then return;	(* <---- exit for switch found *)
      find_switch := next_switch;
    end;
  end;
end;
$PAGE switch
(* SWITCH searches a list of known switches and returns the current value if
   found, otherwise returns false. *)

public function switch
     (	head: switch_ptr;		(* head of list *)
	switch_name: switch_string	(* switch to be found *)
		): boolean;		(* interpreted value of switch *)

  var sw: switch_ptr;

begin
  sw := find_switch (head, switch_name);
  if sw = nil
    then switch := false
    else switch := sw^.enabled;
end;
$PAGE enable_switch
(* ENABLE SWITCH initializes a new switch record, placing it at the start of a
   caller-supplied switch list. *)

public function enable_switch
      (	head: switch_ptr;		(* start of switch list *)
	switch_name: switch_string;	(* name of switch to set *)
	enable: boolean			(* enable or disable switch *)
			 ): switch_ptr;	(* to updated start of list *)

var sw: switch_ptr;
begin
  new (sw, length (switch_name));
  with sw^ do begin
    enabled := enable;
    name[1:length(switch_name)] := switch_name;
    next_switch := head;
  end;
  enable_switch := sw;
end;
$PAGE pop_switches
(* POP SWITCHES deletes entries in a switch list up to a specified node. *)

public procedure pop_switches
     (	head: switch_ptr;		(* first in list *)
	final: switch_ptr  );		(* stopping point, this node is left in list *)

var sw, next_sw: switch_ptr;
begin
  sw := head;
  while sw <> final do begin
    next_sw := sw^.next_switch;
    dispose (sw);
    sw := next_sw;
  end;
end;
$PAGE sw_save
(* SW SAVE will write the switches in a specified list out to a specified file. *)

public procedure sw_save ( head: switch_ptr; var f: file of * );

var sw: switch_ptr;
    len: integer;

begin
  sw := head;
  while sw <> nil do begin
    len := length (sw^.name);
    write (f, len, sw^: size (sw^, len));
    sw := sw^.next_switch;
  end;
  len := 0;
  write (f, len);
end;
$PAGE sw_load
(* SW LOAD reads a switch list from a specified file and returns a pointer to it. *)

public function sw_load ( var f: file of * ): switch_ptr;

var sw, last_sw: switch_ptr;
    len: integer;

begin
  sw_load := nil;
  loop
    read (f, len);
  exit if len = 0;
    new (sw, len);
    read (f, sw^: size (sw^, len));
    sw^.next_switch := nil;
    if sw_load = nil
      then sw_load := sw
      else last_sw^.next_switch := sw;
    last_sw := sw;
  end;
end.
  