$PAGE QED.INC, last modified 2/16/84, zw

CONST
tab = CHR(#O11);
lf = CHR(#O12);
cr = CHR(#O15);
esc = CHR(#O33);

EXTERNAL PROCEDURE qedcl (*main command loop for QED*)
  (VAR buffer: qbuffer; (*pre-setup buffer*)
  allowed_cmds: qed_cmd_set); (*command set to work with*)

EXTERNAL PROCEDURE qinitexec (*equiv to init and initqbuf*)
  (VAR buffer: qbuffer); (*buffer to be used*)

EXTERNAL PROCEDURE qinit (*setup QED to run*)
  (VAR buffer: qbuffer); (*buffer to initialize*)

EXTERNAL PROCEDURE qsettabdefaultn (*SET TABS switch default set*)
  (default: BOOLEAN); (*true for SET TABS ON default*)

EXTERNAL PROCEDURE qsetmarkdefault (*mark string default setter*)
  (line: cmdline); (*the default mark string*)

EXTERNAL PROCEDURE qexecute (*parse and execute one command line*)
  (VAR buffer: qbuffer; (*working buffer*)
  VAR line: cmdline; (*command line to parse*)
  VAR lindex: cmdlineidx; (*place marker*)
  VAR execrange: ldrange; (*limits of execution*)
  VAR ble: qlineno; (*bottommost line examined*)
  findflag: BOOLEAN; (*running under find?*)
  allowed_cmds: qed_cmd_set; (*commands permitted to be executed*)
  VAR err: qerrcode); (*errors?*)
