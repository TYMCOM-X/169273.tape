$PAGE ED.INC, last modified 2/16/84, zw

CONST
tab = CHR(#O11);
lf = CHR(#O12);
cr = CHR(#O15);
esc = CHR(#O33);

EXTERNAL PROCEDURE edcl (*main command loop for ED*)
  (VAR buffer: buftyp; (*pre-setup buffer*)
  allowed_cmds: ed_cmd_set); (*command set to work with*)

EXTERNAL PROCEDURE initexec (*equiv to init and initbuf*)
  (VAR buffer: buftyp); (*buffer to be used*)

EXTERNAL PROCEDURE init (*setup ED to run*)
  (VAR buffer: buftyp); (*buffer to initialize*)

EXTERNAL PROCEDURE settabdefaultn (*SET TABS switch default set*)
  (default: BOOLEAN); (*true for SET TABS ON default*)

EXTERNAL PROCEDURE setmarkdefault (*mark string default setter*)
  (line: cmdline); (*the default mark string*)

EXTERNAL PROCEDURE execute (*parse and execute one command line*)
  (VAR buffer: buftyp; (*working buffer*)
  VAR line: cmdline; (*command line to parse*)
  VAR lindex: cmdlineidx; (*place marker*)
  VAR execrange: ldrange; (*limits of execution*)
  VAR ble: lineno; (*bottommost line examined*)
  findflag: BOOLEAN; (*running under find?*)
  allowed_cmds: ed_cmd_set; (*commands permitted to be executed*)
  VAR err: errcode); (*errors?*)
   