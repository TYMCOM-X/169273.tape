$IFNOT setupinc

$SYSTEM ask.typ
$SYSTEM err.typ
$SYSTEM dbg.typ
$SYSTEM copy.typ

$INCLUDE setup.typ

EXTERNAL PROCEDURE tty_on;
(*direct INPUT and OUTPUT to TTY*)

EXTERNAL PROCEDURE tty_off;
(*restore INPUT and OUTPUT to state before tty_on*)

EXTERNAL PROCEDURE close_io;
(*close INPUT and OUTPUT files, except TTY*)

EXTERNAL PROCEDURE open_io(i_nam, o_nam: STRING[*]);
(*open INPUT and OUTPUT files*)

EXTERNAL PROCEDURE rd_lin(VAR lin: STRING[*]);
(*read line of text from INPUT*)

EXTERNAL PROCEDURE rd_cmd(prompt: STRING[*]; VAR cmd_lin: STRING[*]);
(*read command line from INPUT, prompt if TTY*)

EXTERNAL PROCEDURE arg(n: INTEGER; VAR val: STRING[*]);
(*return the nth argument from the command line*)

EXTERNAL FUNCTION set_up(name: STRING[*]): BOOLEAN;
(*set up TTY, INPUT and OUTPUT files*)

EXTERNAL VAR end_input: BOOLEAN;
(*set TRUE if at end of INPUT file*)

$ENABLE setupinc
$ENDIF
