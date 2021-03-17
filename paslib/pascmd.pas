$WIDTH=100
$LENGTH=55
$TITLE PASCMD.PAS, last modified 10/26/83, zw

PROGRAM pascmd;
(*TYM-Pascal Compiler Command Processor*)

$HEADER PASCMD.DOC

$PAGE modules and declarations

$SYSTEM USRUTL.MOD
$SYSTEM CMDUTL.MOD
$SYSTEM ERRUTL.MOD
$SYSTEM RUNUTL.MOD
$SYSTEM DBGUTL.MOD
$SYSTEM PASENV.MOD

VAR
exit_mode: (exit_compiler, run_program, begin_compilation);
program_name: FILE_NAME;

$PAGE process_command

PROCEDURE process_command;
BEGIN
  exit_mode := exit_compiler
END;

$PAGE main block

BEGIN
  dbg := TRUE;
  resume(banner);
  getenv;
  IF NOT err THEN BEGIN
    bgncmd(command_file, TRUE);
    process_command;
    CASE exit_mode OF
      exit_compiler: BEGIN
        zapenv;
	STOP
      END;
      run_program: BEGIN
        zapenv;
	run(program_name)
      END;
      begin_compilation: chain('PASPAR')
    END
  END;
  fatal('compiler command processor fails')
END.
